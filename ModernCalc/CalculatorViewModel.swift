//
//  CalculatorViewModel.swift
//  ModernCalc
//
//  Created by Manu Heiskanen on 28.8.25.
//

import Foundation
import Combine
import SwiftUI

// MODIFIED: The Calculation struct now stores two versions of the result.
struct Calculation: Identifiable, Hashable {
    let id = UUID()
    let expression: String
    let displayResult: String      // The pretty string for display
    let parsableResult: String     // The string the parser can understand
    let isDefinition: Bool         // Tracks if it was a variable or function definition

    var displayString: String {
        if isDefinition {
            return expression
        } else {
            if displayResult.contains("\n") {
                return "\(expression) =\n\(displayResult)"
            } else {
                return "\(expression) = \(displayResult)"
            }
        }
    }
}


class CalculatorViewModel: ObservableObject {

    @Published var rawExpression: String = ""
    @Published var history: [Calculation] = []
    @Published var liveResult: String = ""
    @Published var previewText: String = ""

    // Central stores for variables and user-defined functions.
    @Published var variables: [String: MathValue] = [:]
    @Published var functions: [String: FunctionDefinitionNode] = [:]

    private let evaluator = Evaluator()
    private var lastSuccessfulValue: MathValue?
    private var cancellable: AnyCancellable?
    
    private let navigationManager = NavigationManager()
    private let ansVariable = "ans"

    init() {
        cancellable = $rawExpression
            .sink { [weak self] newExpression in
                guard let self = self else { return }
                DispatchQueue.main.async {
                    self.calculate(expression: newExpression)
                }
            }
    }

    private func calculate(expression: String) {
        guard !expression.trimmingCharacters(in: .whitespaces).isEmpty else {
            DispatchQueue.main.async {
                self.liveResult = ""
                self.lastSuccessfulValue = nil
            }
            return
        }

        do {
            let lexer = Lexer(input: expression)
            let tokens = lexer.tokenize()
            let parser = Parser(tokens: tokens)
            let expressionTree = try parser.parse()
            
            var tempVars = self.variables
            var tempFuncs = self.functions
            
            let value = try evaluator.evaluate(node: expressionTree, variables: &tempVars, functions: &tempFuncs)
            
            DispatchQueue.main.async {
                self.variables = tempVars
                self.functions = tempFuncs
                self.lastSuccessfulValue = value
                
                if case .functionDefinition(let name) = value {
                    self.liveResult = "Function '\(name)' defined."
                } else {
                    self.liveResult = self.formatLivePreview(value)
                }
            }
        } catch let error {
            DispatchQueue.main.async {
                self.lastSuccessfulValue = nil
                if let displayError = error as? CustomStringConvertible {
                    self.liveResult = displayError.description
                } else {
                    self.liveResult = "An unknown error occurred."
                }
            }
        }
    }
    
    func commitCalculation() {
        if let selectedItem = history.first(where: { $0.id == navigationManager.selectedHistoryId }) {
            DispatchQueue.main.async {
                if selectedItem.isDefinition {
                    self.rawExpression += selectedItem.expression.replacingOccurrences(of: " ", with: "")
                } else {
                    if self.navigationManager.selectedPart == .equation {
                        self.rawExpression += selectedItem.expression.replacingOccurrences(of: " ", with: "")
                    } else {
                        self.rawExpression += selectedItem.parsableResult
                    }
                }
                self.resetNavigation()
            }
        } else {
            guard !rawExpression.isEmpty, let valueToCommit = lastSuccessfulValue else { return }
            
            let isDefinition = rawExpression.contains(":=") || (valueToCommit.typeName == "FunctionDefinition")

            if !isDefinition {
                self.variables[ansVariable] = valueToCommit
            }
            
            let displayResultString = isDefinition ? "" : formatForHistory(valueToCommit)
            let parsableResultString = isDefinition ? "" : formatForParsing(valueToCommit)

            let newCalculation = Calculation(
                expression: rawExpression,
                displayResult: displayResultString,
                parsableResult: parsableResultString,
                isDefinition: isDefinition
            )
            
            DispatchQueue.main.async {
                self.history.append(newCalculation)
                self.rawExpression = ""
            }
        }
    }

    func handleKeyPress(keys: Set<KeyEquivalent>) -> Bool {
        if let selectedText = navigationManager.handleKeyPress(keys: keys, history: history) {
            DispatchQueue.main.async { self.previewText = selectedText }
            return true
        } else {
            DispatchQueue.main.async { self.previewText = "" }
            return false
        }
    }

    func resetNavigation() {
        navigationManager.resetSelection()
        previewText = ""
    }

    var selectedHistoryId: UUID? { navigationManager.selectedHistoryId }
    var selectedHistoryPart: SelectionPart { navigationManager.selectedPart }
    
    var sortedVariables: [(String, MathValue)] { variables.sorted { $0.key < $1.key } }
    var sortedFunctions: [(String, FunctionDefinitionNode)] { functions.sorted { $0.key < $1.key } }
    
    func deleteVariable(name: String) { variables.removeValue(forKey: name) }
    func deleteFunction(name: String) { functions.removeValue(forKey: name) }

    // --- FORMATTING HELPERS ---
    
    func formatLivePreview(_ value: MathValue) -> String {
        switch value {
        case .scalar(let doubleValue): return formatScalar(doubleValue)
        case .complex(let complexValue): return "Result: \(formatComplex(complexValue))"
        case .vector(let vector): return "Result: Vector [\(vector.dimension)]"
        case .matrix(let matrix): return "Result: Matrix [\(matrix.rows)x\(matrix.columns)]"
        case .tuple(let values): return "Result: \(values.count) possible values"
        case .complexVector(let cVector): return "Result: Complex Vector [\(cVector.dimension)]"
        case .complexMatrix(let cMatrix): return "Result: Complex Matrix [\(cMatrix.rows)x\(cMatrix.columns)]"
        case .functionDefinition(let name): return "Function '\(name)' defined."
        }
    }
    
    private func formatForHistory(_ value: MathValue) -> String {
        switch value {
        case .scalar(let doubleValue): return formatScalar(doubleValue)
        case .complex(let complexValue): return formatComplex(complexValue)
        case .vector(let vector): return formatVector(vector)
        case .matrix(let matrix): return formatMatrix(matrix)
        case .tuple(let values): return values.map { formatForHistory($0) }.joined(separator: " or ")
        case .complexVector(let cVector): return formatComplexVector(cVector)
        case .complexMatrix(let cMatrix): return formatComplexMatrix(cMatrix)
        case .functionDefinition: return ""
        }
    }
    
    private func formatForParsing(_ value: MathValue) -> String {
        switch value {
        case .scalar(let doubleValue): return formatScalar(doubleValue)
        // FIX: Removed spaces from complex number formatting
        case .complex(let complexValue):
            let sign = complexValue.imaginary < 0 ? "" : "+"
            return "(\(formatScalar(complexValue.real))\(sign)\(formatScalar(complexValue.imaginary))i)"
        case .vector(let vector): return "vector(\(vector.values.map { formatScalar($0) }.joined(separator: ";")))"
        case .matrix(let matrix): return formatMatrixForParsing(matrix)
        case .tuple(let values): return values.map { formatForParsing($0) }.first ?? ""
        case .complexVector(let cVector): return "cvector(\(cVector.values.map { formatForParsing(.complex($0)) }.joined(separator: ";")))"
        case .complexMatrix(let cMatrix): return formatComplexMatrixForParsing(cMatrix)
        case .functionDefinition: return ""
        }
    }

    private func formatMatrixForParsing(_ matrix: Matrix) -> String {
        let rows = (0..<matrix.rows).map { r in
            (0..<matrix.columns).map { c in
                formatScalar(matrix[r, c])
            }.joined(separator: ",")
        }.joined(separator: ";")
        return "matrix(\(rows))"
    }
    
    private func formatComplexMatrixForParsing(_ matrix: ComplexMatrix) -> String {
        let rows = (0..<matrix.rows).map { r in
            (0..<matrix.columns).map { c in
                formatForParsing(.complex(matrix[r, c]))
            }.joined(separator: ",")
        }.joined(separator: ";")
        return "cmatrix(\(rows))"
    }

    // --- Shared Formatting Helpers ---
    private func formatScalar(_ value: Double) -> String {
        return value.truncatingRemainder(dividingBy: 1) == 0 ? String(format: "%.0f", value) : String(format: "%.4f", value)
    }
    private func formatComplex(_ value: Complex) -> String {
        if value.real != 0 && value.imaginary != 0 {
            return "\(formatScalar(value.real)) \(value.imaginary < 0 ? "-" : "+") \(formatScalar(abs(value.imaginary)))i"
        } else if value.real != 0 { return formatScalar(value.real) }
        else if value.imaginary != 0 { return "\(formatScalar(value.imaginary))i" }
        else { return "0" }
    }
    private func formatVector(_ vector: Vector) -> String {
        return (0..<vector.dimension).map { "[ \(formatScalar(vector[$0])) ]" }.joined(separator: "\n")
    }
    private func formatMatrix(_ matrix: Matrix) -> String {
        if matrix.rows == 0 || matrix.columns == 0 { return "[]" }

        var columnWidths = [Int](repeating: 0, count: matrix.columns)
        for c in 0..<matrix.columns {
            var maxWidth = 0
            for r in 0..<matrix.rows {
                let formattedNumber = formatScalar(matrix[r, c])
                if formattedNumber.count > maxWidth {
                    maxWidth = formattedNumber.count
                }
            }
            columnWidths[c] = maxWidth
        }

        return (0..<matrix.rows).map { r in
            let rowContent = (0..<matrix.columns).map { c in
                let formattedNumber = formatScalar(matrix[r, c])
                let padding = String(repeating: " ", count: columnWidths[c] - formattedNumber.count)
                return padding + formattedNumber
            }.joined(separator: "  ")
            return "[ \(rowContent) ]"
        }.joined(separator: "\n")
    }
    private func formatComplexVector(_ vector: ComplexVector) -> String {
        return (0..<vector.dimension).map { "[ \(formatComplex(vector[$0])) ]" }.joined(separator: "\n")
    }
    private func formatComplexMatrix(_ matrix: ComplexMatrix) -> String {
        if matrix.rows == 0 || matrix.columns == 0 { return "[]" }

        var columnWidths = [Int](repeating: 0, count: matrix.columns)
        for c in 0..<matrix.columns {
            var maxWidth = 0
            for r in 0..<matrix.rows {
                let formattedNumber = "(\(formatComplex(matrix[r, c])))"
                if formattedNumber.count > maxWidth {
                    maxWidth = formattedNumber.count
                }
            }
            columnWidths[c] = maxWidth
        }

        return (0..<matrix.rows).map { r in
            let rowContent = (0..<matrix.columns).map { c in
                let formattedNumber = "(\(formatComplex(matrix[r, c])))"
                let padding = String(repeating: " ", count: columnWidths[c] - formattedNumber.count)
                return padding + formattedNumber
            }.joined(separator: "  ")
            return "[ \(rowContent) ]"
        }.joined(separator: "\n")
    }
}

