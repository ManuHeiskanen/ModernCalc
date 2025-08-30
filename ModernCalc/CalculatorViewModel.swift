//
//  CalculatorViewModel.swift
//  ModernCalc
//
//  Created by Manu Heiskanen on 28.8.25.
//

import Foundation
import Combine
import SwiftUI

// The Calculation struct now has smarter formatting logic.
struct Calculation: Identifiable, Hashable {
    let id = UUID()
    let expression: String
    let result: String
    let isDefinition: Bool // Tracks if it was a variable or function definition

    var displayString: String {
        if isDefinition {
            // For assignments or definitions, show the full expression.
            return expression
        } else {
            // For calculations, check if the result is multi-line.
            if result.contains("\n") {
                // If it's a matrix or vector, put the result on a new line.
                return "\(expression) =\n\(result)"
            } else {
                // If it's a scalar, keep it on the same line.
                return "\(expression) = \(result)"
            }
        }
    }
}


class CalculatorViewModel: ObservableObject {

    @Published var rawExpression: String = "sqrt(-4) * (1+2i)"
    @Published var history: [Calculation] = []
    @Published var liveResult: String = ""
    @Published var previewText: String = ""

    // Central stores for variables and user-defined functions.
    @Published var variables: [String: MathValue] = [:]
    @Published var functions: [String: FunctionDefinitionNode] = [:]

    private let evaluator = Evaluator()
    private var lastSuccessfulValue: MathValue?
    private var cancellable: AnyCancellable?
    
    // NEW: Navigation Manager instance
    private let navigationManager = NavigationManager()

    init() {
        cancellable = $rawExpression
            .sink { [weak self] newExpression in
                guard let self = self else { return }
                
                // Fix: All state changes must be on the main queue
                DispatchQueue.main.async {
                    self.calculate(expression: newExpression)
                }
            }
    }

    private func calculate(expression: String) {
        guard !expression.trimmingCharacters(in: .whitespaces).isEmpty else {
            // Fix: All state changes must be on the main queue
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
            
            // Fix: All state changes must be on the main queue
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
            // Fix: All state changes must be on the main queue
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
                // CORRECTED: Add special handling for definitions.
                if selectedItem.isDefinition {
                    // If the selected item is a definition, always add the expression.
                    self.rawExpression += selectedItem.expression
                } else {
                    // Otherwise, use the existing logic for calculations.
                    if self.navigationManager.selectedPart == .equation {
                        self.rawExpression += selectedItem.expression
                    } else {
                        self.rawExpression += selectedItem.result
                    }
                }
                self.resetNavigation()
            }
        } else {
            // Normal commit logic
            guard !rawExpression.isEmpty, let valueToCommit = lastSuccessfulValue else { return }
            
            let isDefinition: Bool
            if case .functionDefinition = valueToCommit {
                isDefinition = true
            } else {
                isDefinition = rawExpression.contains(":=")
            }
            
            let resultString = isDefinition ? "" : formatForHistory(valueToCommit)
            
            let newCalculation = Calculation(expression: rawExpression, result: resultString, isDefinition: isDefinition)
            
            DispatchQueue.main.async {
                self.history.append(newCalculation)
                self.rawExpression = ""
            }
        }
    }

    // NEW: Handle key presses for navigation
    func handleKeyPress(keys: Set<KeyEquivalent>) -> Bool {
        if let selectedText = navigationManager.handleKeyPress(keys: keys, history: history) {
            // Fix: All state changes must be on the main queue
            DispatchQueue.main.async {
                self.previewText = selectedText
            }
            return true
        } else {
            // Fix: All state changes must be on the main queue
            DispatchQueue.main.async {
                self.previewText = ""
            }
            return false
        }
    }

    // NEW: Reset the navigation manager's state
    func resetNavigation() {
        navigationManager.resetSelection()
        previewText = ""
    }

    // NEW: Get the currently selected ID from the navigation manager
    var selectedHistoryId: UUID? {
        navigationManager.selectedHistoryId
    }

    // NEW: Get the currently selected part
    var selectedHistoryPart: SelectionPart {
        navigationManager.selectedPart
    }
    
    // --- NEW: Computed properties for the Variable Editor View ---
    var sortedVariables: [(String, MathValue)] {
        variables.sorted { $0.key < $1.key }
    }
    
    var sortedFunctions: [(String, FunctionDefinitionNode)] {
        functions.sorted { $0.key < $1.key }
    }
    
    // --- NEW: Deletion logic ---
    func deleteVariable(name: String) {
        variables.removeValue(forKey: name)
    }
    
    func deleteFunction(name: String) {
        functions.removeValue(forKey: name)
    }

    // --- FORMATTING HELPERS ---

    // CORRECTED: Removed 'private' to make it accessible to VariableEditorView
    func formatLivePreview(_ value: MathValue) -> String {
        switch value {
        case .scalar(let doubleValue):
            return formatScalar(doubleValue)
        case .complex(let complexValue):
            return "Result: \(formatComplex(complexValue))"
        case .vector(let vector):
            return "Result: Vector [\(vector.dimension)]"
        case .matrix(let matrix):
            return "Result: Matrix [\(matrix.rows)x\(matrix.columns)]"
        case .tuple(let values):
            return "Result: \(values.count) possible values"
        case .complexVector(let cVector):
            return "Result: Complex Vector [\(cVector.dimension)]"
        case .complexMatrix(let cMatrix):
            return "Result: Complex Matrix [\(cMatrix.rows)x\(cMatrix.columns)]"
        case .functionDefinition(let name):
            return "Function '\(name)' defined."
        }
    }

    private func formatForHistory(_ value: MathValue) -> String {
        switch value {
        case .scalar(let doubleValue):
            return formatScalar(doubleValue)
        case .complex(let complexValue):
            return formatComplex(complexValue)
        case .vector(let vector):
            return formatVector(vector)
        case .matrix(let matrix):
            return formatMatrix(matrix)
        case .tuple(let values):
            return values.map { formatForHistory($0) }.joined(separator: " or ")
        case .complexVector(let cVector):
            return formatComplexVector(cVector)
        case .complexMatrix(let cMatrix):
            return formatComplexMatrix(cMatrix)
        case .functionDefinition:
            return ""
        }
    }
    
    private func formatScalar(_ value: Double) -> String {
        if value.truncatingRemainder(dividingBy: 1) == 0 {
            return String(format: "%.0f", value)
        } else {
            return String(format: "%.4f", value)
        }
    }
    
    private func formatComplex(_ value: Complex) -> String {
        let realPart = formatScalar(value.real)
        let imagPart = formatScalar(abs(value.imaginary))
        
        if value.real != 0 && value.imaginary != 0 {
            let sign = value.imaginary < 0 ? "-" : "+"
            return "\(realPart) \(sign) \(imagPart)i"
        } else if value.real != 0 {
            return realPart
        } else if value.imaginary != 0 {
            return "\(formatScalar(value.imaginary))i"
        } else {
            return "0"
        }
    }

    private func formatVector(_ vector: Vector) -> String {
        var result = ""
        for i in 0..<vector.dimension {
            result += "[ \(formatScalar(vector[i])) ]\n"
        }
        return String(result.dropLast())
    }

    private func formatMatrix(_ matrix: Matrix) -> String {
        var result = ""
        for r in 0..<matrix.rows {
            var rowString = "[ "
            for c in 0..<matrix.columns {
                rowString += "\(formatScalar(matrix[r, c])) "
            }
            rowString += "]\n"
            result += rowString
        }
        return String(result.dropLast())
    }
    
    private func formatComplexVector(_ vector: ComplexVector) -> String {
        var result = ""
        for i in 0..<vector.dimension {
            result += "[ \(formatComplex(vector[i])) ]\n"
        }
        return String(result.dropLast())
    }

    private func formatComplexMatrix(_ matrix: ComplexMatrix) -> String {
        var result = ""
        for r in 0..<matrix.rows {
            var rowString = "[ "
            for c in 0..<matrix.columns {
                rowString += "(\(formatComplex(matrix[r, c]))) "
            }
            rowString += "]\n"
            result += rowString
        }
        return String(result.dropLast())
    }
}

