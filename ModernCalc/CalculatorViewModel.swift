//
//  CalculatorViewModel.swift
//  ModernCalc
//
//  Created by Manu Heiskanen on 28.8.25.
//

import Foundation
import Combine
import SwiftUI

enum AngleMode {
    case degrees, radians
}

enum CalculationType {
    case evaluation
    case variableAssignment
    case functionDefinition
}

struct Calculation: Identifiable, Hashable {
    let id = UUID()
    let expression: String
    let result: MathValue
    let type: CalculationType
    let usedAngleSensitiveFunction: Bool
    let angleMode: AngleMode

    static func == (lhs: Calculation, rhs: Calculation) -> Bool {
        return lhs.id == rhs.id
    }

    func hash(into hasher: inout Hasher) {
        hasher.combine(id)
    }
}

// NEW: A struct to hold information about built-in functions for the help view.
struct BuiltinFunction: Identifiable, Hashable {
    let id = UUID()
    let name: String
    let signature: String
    let description: String
}


class CalculatorViewModel: ObservableObject {

    @Published var rawExpression: String = ""
    @Published var history: [Calculation] = []
    @Published var liveResult: String = ""
    @Published var previewText: String = ""
    @Published var variables: [String: MathValue] = [:]
    @Published var functions: [String: FunctionDefinitionNode] = [:]
    @Published var angleMode: AngleMode = .degrees

    private let evaluator = Evaluator()
    private var lastSuccessfulValue: MathValue?
    private var lastUsedAngleFlag: Bool = false
    private var cancellable: AnyCancellable?
    
    private let navigationManager = NavigationManager()
    private let ansVariable = "ans"
    
    // NEW: The complete list of built-in functions for the help view.
    let builtinFunctions: [BuiltinFunction] = [
        .init(name: "sin", signature: "sin(angle)", description: "Calculates the sine of an angle."),
        .init(name: "cos", signature: "cos(angle)", description: "Calculates the cosine of an angle."),
        .init(name: "tan", signature: "tan(angle)", description: "Calculates the tangent of an angle."),
        .init(name: "asin", signature: "asin(value)", description: "Calculates the inverse sine (arcsin)."),
        .init(name: "acos", signature: "acos(value)", description: "Calculates the inverse cosine (arccos)."),
        .init(name: "atan", signature: "atan(value)", description: "Calculates the inverse tangent (arctan)."),
        .init(name: "sqrt", signature: "sqrt(number)", description: "Calculates the square root. Handles complex numbers."),
        .init(name: "abs", signature: "abs(value)", description: "Calculates the absolute value or magnitude."),
        .init(name: "log", signature: "log(number)", description: "Calculates the common (base-10) logarithm."),
        .init(name: "lg", signature: "lg(number)", description: "Alias for the common (base-10) logarithm."),
        .init(name: "ln", signature: "ln(number)", description: "Calculates the natural (base-e) logarithm."),
        .init(name: "round", signature: "round(number)", description: "Rounds a number to the nearest integer."),
        .init(name: "floor", signature: "floor(number)", description: "Rounds a number down to the nearest integer."),
        .init(name: "ceil", signature: "ceil(number)", description: "Rounds a number up to the nearest integer."),
        .init(name: "fact", signature: "fact(integer)", description: "Calculates the factorial of a non-negative integer."),
        .init(name: "sum", signature: "sum(a, b, ...)", description: "Calculates the sum of a list of numbers or a vector/matrix."),
        .init(name: "avg", signature: "avg(a, b, ...)", description: "Calculates the average of a list of numbers or a vector/matrix."),
        .init(name: "min", signature: "min(a, b, ...)", description: "Finds the minimum value in a list of numbers or a vector/matrix."),
        .init(name: "max", signature: "max(a, b, ...)", description: "Finds the maximum value in a list of numbers or a vector/matrix."),
        .init(name: "median", signature: "median(a, b, ...)", description: "Finds the median of a list of numbers or a vector/matrix."),
        .init(name: "stddev", signature: "stddev(a, b, ...)", description: "Calculates the sample standard deviation of a list of numbers or a vector/matrix."),
        .init(name: "dot", signature: "dot(vectorA, vectorB)", description: "Calculates the dot product of two vectors."),
        .init(name: "cross", signature: "cross(vectorA, vectorB)", description: "Calculates the cross product of two 3D vectors."),
        .init(name: "det", signature: "det(matrix)", description: "Calculates the determinant of a square matrix."),
        .init(name: "inv", signature: "inv(matrix)", description: "Calculates the inverse of a square matrix."),
        .init(name: "polar", signature: "polar(complex)", description: "Converts a complex number to its polar form (R ∠ θ)."),
        .init(name: "real", signature: "real(complex)", description: "Extracts the real part of a complex number."),
        .init(name: "imag", signature: "imag(complex)", description: "Extracts the imaginary part of a complex number."),
        .init(name: "conj", signature: "conj(complex)", description: "Calculates the complex conjugate."),
        .init(name: "arg", signature: "arg(complex)", description: "Calculates the argument (phase) of a complex number."),
        .init(name: "nCr", signature: "nCr(n, k)", description: "Calculates the number of combinations."),
        .init(name: "nPr", signature: "nPr(n, k)", description: "Calculates the number of permutations.")
    ]

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
            
            let (value, usedAngle) = try evaluator.evaluate(node: expressionTree, variables: &tempVars, functions: &tempFuncs, angleMode: self.angleMode)
            
            DispatchQueue.main.async {
                self.variables = tempVars
                self.functions = tempFuncs
                self.lastSuccessfulValue = value
                self.lastUsedAngleFlag = usedAngle
                
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
                if selectedItem.type == .functionDefinition {
                    self.rawExpression += selectedItem.expression.replacingOccurrences(of: " ", with: "")
                } else {
                    if self.navigationManager.selectedPart == .equation {
                        self.rawExpression += selectedItem.expression.replacingOccurrences(of: " ", with: "")
                    } else {
                        self.rawExpression += self.formatForParsing(selectedItem.result)
                    }
                }
                self.resetNavigation()
            }
        } else {
            guard !rawExpression.isEmpty, let valueToCommit = lastSuccessfulValue else { return }
            
            let calcType: CalculationType
            if valueToCommit.typeName == "FunctionDefinition" {
                calcType = .functionDefinition
            } else if rawExpression.contains(":=") {
                calcType = .variableAssignment
            } else {
                calcType = .evaluation
            }

            if calcType != .functionDefinition {
                DispatchQueue.main.async {
                    self.variables[self.ansVariable] = valueToCommit
                }
            }
            
            let newCalculation = Calculation(
                expression: rawExpression,
                result: valueToCommit,
                type: calcType,
                usedAngleSensitiveFunction: self.lastUsedAngleFlag,
                angleMode: self.angleMode
            )
            
            DispatchQueue.main.async {
                self.history.append(newCalculation)
                self.rawExpression = ""
            }
        }
    }

    func handleKeyPress(keys: Set<KeyEquivalent>) -> Bool {
        if let selectedText = navigationManager.handleKeyPress(keys: keys, history: history, viewModel: self) {
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
        case .scalar(let doubleValue): return formatScalarForDisplay(doubleValue)
        case .complex(let complexValue): return "Result: \(formatComplexForDisplay(complexValue))"
        case .vector(let vector): return "Result: Vector [\(vector.dimension)]"
        case .matrix(let matrix): return "Result: Matrix [\(matrix.rows)x\(matrix.columns)]"
        case .tuple(let values): return "Result: \(values.count) possible values"
        case .complexVector(let cVector): return "Result: Complex Vector [\(cVector.dimension)]"
        case .complexMatrix(let cMatrix): return "Result: Complex Matrix [\(cMatrix.rows)x\(cMatrix.columns)]"
        case .functionDefinition(let name): return "Function '\(name)' defined."
        case .polar(let complexValue): return "Result: \(formatPolarForDisplay(complexValue))"
        }
    }
    
    func formatForHistory(_ value: MathValue) -> String {
        switch value {
        case .scalar(let doubleValue): return formatScalarForDisplay(doubleValue)
        case .complex(let complexValue): return formatComplexForDisplay(complexValue)
        case .vector(let vector): return formatVectorForDisplay(vector)
        case .matrix(let matrix): return formatMatrixForDisplay(matrix)
        case .tuple(let values): return values.map { formatForHistory($0) }.joined(separator: " or ")
        case .complexVector(let cVector): return formatComplexVectorForDisplay(cVector)
        case .complexMatrix(let cMatrix): return formatComplexMatrixForDisplay(cMatrix)
        case .functionDefinition: return ""
        case .polar(let complexValue): return formatPolarForDisplay(complexValue)
        }
    }
    
    func formatForParsing(_ value: MathValue) -> String {
        switch value {
        case .scalar(let doubleValue): return formatScalarForParsing(doubleValue)
        case .complex(let complexValue): return formatComplexForParsing(complexValue)
        case .vector(let vector): return "vector(\(vector.values.map { formatScalarForParsing($0) }.joined(separator: ";")))"
        case .matrix(let matrix): return formatMatrixForParsing(matrix)
        case .tuple(let values): return values.map { formatForParsing($0) }.first ?? ""
        case .complexVector(let cVector): return "cvector(\(cVector.values.map { formatForParsing(.complex($0)) }.joined(separator: ";")))"
        case .complexMatrix(let cMatrix): return formatComplexMatrixForParsing(cMatrix)
        case .functionDefinition: return ""
        case .polar(let complexValue): return formatPolarForParsing(complexValue)
        }
    }

    private func formatMatrixForParsing(_ matrix: Matrix) -> String {
        let rows = (0..<matrix.rows).map { r in
            (0..<matrix.columns).map { c in
                formatScalarForParsing(matrix[r, c])
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

    // --- Display Formatting Helpers (Low Precision) ---
    private func formatScalarForDisplay(_ value: Double) -> String {
        return value.truncatingRemainder(dividingBy: 1) == 0 ? String(format: "%.0f", value) : String(format: "%.4f", value)
    }
    
    private func formatComplexForDisplay(_ value: Complex) -> String {
        if value.real != 0 && value.imaginary != 0 {
            return "\(formatScalarForDisplay(value.real)) \(value.imaginary < 0 ? "-" : "+") \(formatScalarForDisplay(abs(value.imaginary)))i"
        } else if value.real != 0 { return formatScalarForDisplay(value.real) }
        else if value.imaginary != 0 { return "\(formatScalarForDisplay(value.imaginary))i" }
        else { return "0" }
    }
    
    private func formatPolarForDisplay(_ value: Complex) -> String {
        let magnitude = value.abs()
        let angle = value.argument()
        
        if self.angleMode == .degrees {
            let angleDegrees = angle * (180.0 / .pi)
            return String(format: "%.4f ∠ %.2f°", magnitude, angleDegrees)
        } else {
            return String(format: "%.4f ∠ %.4f rad", magnitude, angle)
        }
    }

    private func formatVectorForDisplay(_ vector: Vector) -> String {
        return (0..<vector.dimension).map { "[ \(formatScalarForDisplay(vector[$0])) ]" }.joined(separator: "\n")
    }
    
    private func formatMatrixForDisplay(_ matrix: Matrix) -> String {
        if matrix.rows == 0 || matrix.columns == 0 { return "[]" }
        var columnWidths = [Int](repeating: 0, count: matrix.columns)
        for c in 0..<matrix.columns {
            var maxWidth = 0
            for r in 0..<matrix.rows {
                let formattedNumber = formatScalarForDisplay(matrix[r, c])
                if formattedNumber.count > maxWidth { maxWidth = formattedNumber.count }
            }
            columnWidths[c] = maxWidth
        }
        return (0..<matrix.rows).map { r in
            let rowContent = (0..<matrix.columns).map { c in
                let formattedNumber = formatScalarForDisplay(matrix[r, c])
                let padding = String(repeating: " ", count: columnWidths[c] - formattedNumber.count)
                return padding + formattedNumber
            }.joined(separator: "  ")
            return "[ \(rowContent) ]"
        }.joined(separator: "\n")
    }
    
    private func formatComplexVectorForDisplay(_ vector: ComplexVector) -> String {
        return (0..<vector.dimension).map { "[ \(formatComplexForDisplay(vector[$0])) ]" }.joined(separator: "\n")
    }
    
    private func formatComplexMatrixForDisplay(_ matrix: ComplexMatrix) -> String {
        if matrix.rows == 0 || matrix.columns == 0 { return "[]" }
        var columnWidths = [Int](repeating: 0, count: matrix.columns)
        for c in 0..<matrix.columns {
            var maxWidth = 0
            for r in 0..<matrix.rows {
                let formattedNumber = "(\(formatComplexForDisplay(matrix[r, c])))"
                if formattedNumber.count > maxWidth { maxWidth = formattedNumber.count }
            }
            columnWidths[c] = maxWidth
        }
        return (0..<matrix.rows).map { r in
            let rowContent = (0..<matrix.columns).map { c in
                let formattedNumber = "(\(formatComplexForDisplay(matrix[r, c])))"
                let padding = String(repeating: " ", count: columnWidths[c] - formattedNumber.count)
                return padding + formattedNumber
            }.joined(separator: "  ")
            return "[ \(rowContent) ]"
        }.joined(separator: "\n")
    }
    
    // --- Parsing Formatting Helpers (High Precision) ---
    private func formatScalarForParsing(_ value: Double) -> String {
        return String(value)
    }
    
    private func formatComplexForParsing(_ value: Complex) -> String {
        let sign = value.imaginary < 0 ? "" : "+"
        return "(\(formatScalarForParsing(value.real))\(sign)\(formatScalarForParsing(value.imaginary))i)"
    }
    
    private func formatPolarForParsing(_ value: Complex) -> String {
        let magnitude = value.abs()
        let angleDegrees = value.argument() * (180.0 / .pi)
        return "\(formatScalarForParsing(magnitude))∠\(formatScalarForParsing(angleDegrees))"
    }
}

