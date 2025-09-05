//
//  CalculatorViewModel.swift
//  ModernCalc
//
//  Created by Manu Heiskanen on 28.8.25.
//

import Foundation
import Combine
import SwiftUI

enum AngleMode: String, Codable {
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

struct BuiltinFunction: Identifiable, Hashable {
    let id = UUID()
    let name: String
    let signature: String
    let description: String
}

struct PhysicalConstant: Identifiable, Hashable {
    let id = UUID()
    let symbol: String
    let name: String
    let value: Double
}

struct MathSymbol: Identifiable {
    let id = UUID()
    let symbol: String
    let name: String
    let insertionText: String?

    init(symbol: String, name: String, insertionText: String? = nil) {
        self.symbol = symbol
        self.name = name
        self.insertionText = insertionText
    }
}

struct HelpTopic: Identifiable, Hashable {
    let id = UUID()
    let title: String
    let content: String
}


class CalculatorViewModel: ObservableObject {

    @Published var rawExpression: String = ""
    @Published var history: [Calculation] = []
    @Published var liveResult: String = ""
    @Published var liveLaTeXPreview: String = "" // For the live formatted expression
    @Published var previewText: String = ""
    @Published var variables: [String: MathValue] = [:]
    @Published var functions: [String: FunctionDefinitionNode] = [:]
    
    @Published var angleMode: AngleMode = .degrees {
        didSet {
            saveState()
        }
    }
    @Published var userFunctionDefinitions: [String: String] = [:]
    
    @Published var cursorPosition = NSRange()
    
    private var settings: UserSettings

    private let evaluator = Evaluator()
    private var lastSuccessfulValue: MathValue?
    private var lastUsedAngleFlag: Bool = false
    private var cancellable: AnyCancellable?
    
    private let navigationManager = NavigationManager()
    private let ansVariable = "ans"
    
    let siPrefixes: Set<String> = [
        "yotta", "zetta", "exa", "peta", "tera", "giga", "mega", "kilo",
        "hecto", "deca", "deci", "centi", "milli", "micro", "nano",
        "pico", "femto", "atto", "zepto", "yocto"
    ]
    
    let operatorSymbols: [MathSymbol]
    let greekSymbols: [MathSymbol]
    let constantSymbols: [MathSymbol]
    
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
        .init(name: "nPr", signature: "nPr(n, k)", description: "Calculates the number of permutations."),
        .init(name: "hypot", signature: "hypot(sideA, sideB)", description: "Calculates the hypotenuse of a right triangle."),
        .init(name: "side", signature: "side(hyp, sideA)", description: "Calculates the missing side of a right triangle."),
        .init(name: "area_rect", signature: "area_rect(width, height)", description: "Calculates the area of a rectangle."),
        .init(name: "area_tri", signature: "area_tri(base, height)", description: "Calculates the area of a triangle."),
        .init(name: "area_circle", signature: "area_circle(radius)", description: "Calculates the area of a circle."),
        .init(name: "circum_circle", signature: "circum_circle(radius)", description: "Calculates the circumference of a circle."),
        .init(name: "vol_sphere", signature: "vol_sphere(radius)", description: "Calculates the volume of a sphere."),
        .init(name: "vol_cube", signature: "vol_cube(side)", description: "Calculates the volume of a cube."),
        .init(name: "vol_cylinder", signature: "vol_cylinder(radius, height)", description: "Calculates the volume of a cylinder."),
        .init(name: "vol_cone", signature: "vol_cone(radius, height)", description: "Calculates the volume of a cone.")
    ]
    
    let physicalConstants: [PhysicalConstant] = [
        .init(symbol: "π", name: "Pi", value: Double.pi),
        .init(symbol: "e", name: "Euler's number", value: M_E),
        .init(symbol: "c", name: "Speed of light", value: 299792458),
        .init(symbol: "g", name: "Standard gravity", value: 9.80665),
        .init(symbol: "G", name: "Gravitational constant", value: 6.67430e-11),
        .init(symbol: "h", name: "Planck constant", value: 6.62607015e-34),
        .init(symbol: "ħ", name: "Reduced Planck constant", value: 1.054571817e-34),
        .init(symbol: "μ0", name: "Vacuum permeability", value: 1.25663706212e-6),
        .init(symbol: "ε0", name: "Vacuum permittivity", value: 8.8541878128e-12),
        .init(symbol: "me", name: "Electron mass", value: 9.1093837015e-31),
        .init(symbol: "mp", name: "Proton mass", value: 1.67262192369e-27),
        .init(symbol: "mn", name: "Neutron mass", value: 1.67492749804e-27),
        .init(symbol: "e0", name: "Elementary charge", value: 1.602176634e-19),
        .init(symbol: "NA", name: "Avogadro constant", value: 6.02214076e23),
        .init(symbol: "R", name: "Gas constant", value: 8.314462618),
        .init(symbol: "kB", name: "Boltzmann constant", value: 1.380649e-23),
        .init(symbol: "F", name: "Faraday constant", value: 96485.33212),
        .init(symbol: "Rinf", name: "Rydberg constant", value: 10973731.568160),
        .init(symbol: "σ", name: "Stefan-Boltzmann constant", value: 5.670374419e-8),
        .init(symbol: "b", name: "Wien's displacement constant", value: 2.897771955e-3),
        .init(symbol: "atm", name: "Standard atmosphere", value: 101325),
        .init(symbol: "Vm", name: "Molar volume (STP)", value: 22.41396954e-3)
    ]

    let helpTopics: [HelpTopic] = [
        .init(title: "Quick Start", content:
            """
            - **Basic Calculations:** Type any mathematical expression and the result will update live. Press `Return` to commit it to history.
            - **History Navigation:** Use the `Up` and `Down` arrow keys to navigate your calculation history. Use `Left` and `Right` to select either the expression or the result. Press `Return` to insert the selected part into the input field.
            - **LaTeX Export:** Hover over any history row and click the copy icon (`doc.on.doc`) to copy the entire equation to your clipboard as a formatted LaTeX string.
            - **`ans` Variable:** The result of the last calculation is always stored in a variable called `ans`.
            """
        ),
        .init(title: "Syntax Reference", content:
            """
            This calculator supports advanced syntax for complex operations. Use the following formats:
            - **Variable Assignment:** `x := 5 * 2`
            - **Function Definition:** `f(x, y) := x^2 + y^2`
            - **Complex Numbers:** `3 + 4i` or `(3+4i)`
            - **Vectors:** `vector(1; 2; 3)`
            - **Matrices:** `matrix(1, 2; 3, 4)`
            - **Polar Coordinates:** `5 ∠ 30` (respects DEG/RAD mode)
            """
        ),
        .init(title: "Operators", content:
            """
            - **Standard:** `+`, `-`, `*`, `/`
            - **Power:** `^`
            - **Modulus:** `%`
            - **Implicit Multiplication:** Supported for cases like `2x`, `(2)(3)`, or `2sqrt(9)`.
            """
        )
    ]


    init(settings: UserSettings) {
        self.settings = settings
        
        self.operatorSymbols = [
            .init(symbol: "±", name: "Plus-Minus"),
            .init(symbol: "∠", name: "Angle"),
            .init(symbol: "√", name: "Square Root", insertionText: "√("),
            .init(symbol: "×", name: "Multiply"),
            .init(symbol: "÷", name: "Divide"),
            .init(symbol: "^", name: "Power")
        ]
        
        self.greekSymbols = [
            .init(symbol: "α", name: "Alpha (lowercase)"), .init(symbol: "Α", name: "Alpha (uppercase)"),
            .init(symbol: "β", name: "Beta (lowercase)"), .init(symbol: "Β", name: "Beta (uppercase)"),
            .init(symbol: "γ", name: "Gamma (lowercase)"), .init(symbol: "Γ", name: "Gamma (uppercase)"),
            .init(symbol: "δ", name: "Delta (lowercase)"), .init(symbol: "Δ", name: "Delta (uppercase)"),
            .init(symbol: "ε", name: "Epsilon (lowercase)"), .init(symbol: "Ε", name: "Epsilon (uppercase)"),
            .init(symbol: "ζ", name: "Zeta (lowercase)"), .init(symbol: "Ζ", name: "Zeta (uppercase)"),
            .init(symbol: "η", name: "Eta (lowercase)"), .init(symbol: "Η", name: "Eta (uppercase)"),
            .init(symbol: "θ", name: "Theta (lowercase)"), .init(symbol: "Θ", name: "Theta (uppercase)"),
            .init(symbol: "ι", name: "Iota (lowercase)"), .init(symbol: "Ι", name: "Iota (uppercase)"),
            .init(symbol: "κ", name: "Kappa (lowercase)"), .init(symbol: "Κ", name: "Kappa (uppercase)"),
            .init(symbol: "λ", name: "Lambda (lowercase)"), .init(symbol: "Λ", name: "Lambda (uppercase)"),
            .init(symbol: "μ", name: "Mu (lowercase)"), .init(symbol: "Μ", name: "Mu (uppercase)"),
            .init(symbol: "ν", name: "Nu (lowercase)"), .init(symbol: "Ν", name: "Nu (uppercase)"),
            .init(symbol: "ξ", name: "Xi (lowercase)"), .init(symbol: "Ξ", name: "Xi (uppercase)"),
            .init(symbol: "ο", name: "Omicron (lowercase)"), .init(symbol: "Ο", name: "Omicron (uppercase)"),
            .init(symbol: "ρ", name: "Rho (lowercase)"), .init(symbol: "Ρ", name: "Rho (uppercase)"),
            .init(symbol: "σ", name: "Sigma (lowercase)"), .init(symbol: "Σ", name: "Sigma (uppercase)"),
            .init(symbol: "τ", name: "Tau (lowercase)"), .init(symbol: "Τ", name: "Tau (uppercase)"),
            .init(symbol: "υ", name: "Upsilon (lowercase)"), .init(symbol: "Υ", name: "Upsilon (uppercase)"),
            .init(symbol: "φ", name: "Phi (lowercase)"), .init(symbol: "Φ", name: "Phi (uppercase)"),
            .init(symbol: "χ", name: "Chi (lowercase)"), .init(symbol: "Χ", name: "Chi (uppercase)"),
            .init(symbol: "ψ", name: "Psi (lowercase)"), .init(symbol: "Ψ", name: "Psi (uppercase)"),
            .init(symbol: "ω", name: "Omega (lowercase)"), .init(symbol: "Ω", name: "Omega (uppercase)")
        ]
        
        self.constantSymbols = physicalConstants.map {
            MathSymbol(symbol: $0.symbol, name: $0.name, insertionText: $0.symbol)
        }
        
        cancellable = $rawExpression
            .sink { [weak self] newExpression in
                guard let self = self else { return }
                DispatchQueue.main.async {
                    self.calculate(expression: newExpression)
                }
            }
        loadState()
    }

    private func calculate(expression: String) {
        guard !expression.trimmingCharacters(in: .whitespaces).isEmpty else {
            DispatchQueue.main.async {
                self.liveResult = ""
                self.liveLaTeXPreview = "" // Clear LaTeX preview
                self.lastSuccessfulValue = nil
            }
            return
        }

        do {
            let lexer = Lexer(input: expression, decimalSeparator: settings.decimalSeparator)
            let tokens = lexer.tokenize()
            let parser = Parser(tokens: tokens)
            let expressionTree = try parser.parse()
            
            // Generate live LaTeX from the parsed expression tree
            let expressionLaTeX = LaTeXEngine.formatNode(expressionTree, evaluator: self.evaluator, settings: self.settings)
            
            var tempVars = self.variables
            var tempFuncs = self.functions
            
            let (value, usedAngle) = try evaluator.evaluate(node: expressionTree, variables: &tempVars, functions: &tempFuncs, angleMode: self.angleMode)
            
            DispatchQueue.main.async {
                self.variables = tempVars
                self.functions = tempFuncs
                self.lastSuccessfulValue = value
                self.lastUsedAngleFlag = usedAngle
                
                // Update the live LaTeX preview
                self.liveLaTeXPreview = expressionLaTeX
                
                if case .functionDefinition(let name) = value {
                    self.liveResult = "Function '\(name)' defined."
                } else {
                    self.liveResult = self.formatLivePreview(value)
                }
            }
        } catch let error {
            DispatchQueue.main.async {
                self.lastSuccessfulValue = nil
                // On error, show raw expression in preview
                self.liveLaTeXPreview = self.rawExpression
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
                    self.insertTextAtCursor(selectedItem.expression.replacingOccurrences(of: " ", with: ""))
                } else {
                    if self.navigationManager.selectedPart == .equation {
                        self.insertTextAtCursor(selectedItem.expression.replacingOccurrences(of: " ", with: ""))
                    } else {
                        self.insertTextAtCursor(self.formatForParsing(selectedItem.result))
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
            
            if calcType == .variableAssignment {
                saveState()
            } else if calcType == .functionDefinition, case .functionDefinition(let name) = valueToCommit {
                userFunctionDefinitions[name] = rawExpression
                saveState()
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
    
    func deleteVariable(name: String) {
        variables.removeValue(forKey: name)
        saveState()
    }
    
    func deleteFunction(name: String) {
        functions.removeValue(forKey: name)
        userFunctionDefinitions.removeValue(forKey: name)
        saveState()
    }
    
    func insertTextAtCursor(_ textToInsert: String) {
        guard let range = Range(cursorPosition, in: rawExpression) else {
            rawExpression += textToInsert
            let newLocation = rawExpression.utf16.count
            cursorPosition = NSRange(location: newLocation, length: 0)
            return
        }
        
        rawExpression.replaceSubrange(range, with: textToInsert)
        
        let newLocation = cursorPosition.location + textToInsert.utf16.count
        cursorPosition = NSRange(location: newLocation, length: 0)
    }

    // --- PERSISTENCE ---

    private func saveState() {
        do {
            let variablesData = try JSONEncoder().encode(variables)
            let functionsData = try JSONEncoder().encode(userFunctionDefinitions)
            UserDefaults.standard.set(variablesData, forKey: "userVariables")
            UserDefaults.standard.set(functionsData, forKey: "userFunctionDefinitions")
            
            UserDefaults.standard.set(angleMode.rawValue, forKey: "angleModeSetting")
            
        } catch {
            print("Error saving state: \(error.localizedDescription)")
        }
    }

    private func loadState() {
        do {
            if let variablesData = UserDefaults.standard.data(forKey: "userVariables") {
                let decodedVars = try JSONDecoder().decode([String: MathValue].self, from: variablesData)
                self.variables = decodedVars.filter { $0.key != ansVariable }
            }
            if let functionsData = UserDefaults.standard.data(forKey: "userFunctionDefinitions") {
                self.userFunctionDefinitions = try JSONDecoder().decode([String: String].self, from: functionsData)
                rebuildFunctionsFromDefinitions()
            }
        } catch {
            print("Error loading variable/function state: \(error.localizedDescription)")
            self.variables = [:]
            self.userFunctionDefinitions = [:]
        }
        
        if let savedAngleMode = UserDefaults.standard.string(forKey: "angleModeSetting") {
            self.angleMode = AngleMode(rawValue: savedAngleMode) ?? .degrees
        }
    }
    
    private func rebuildFunctionsFromDefinitions() {
        let definitionsToRebuild = self.userFunctionDefinitions
        guard !definitionsToRebuild.isEmpty else { return }
        
        var tempVars = self.variables
        var tempFuncs: [String: FunctionDefinitionNode] = [:]

        for (_, definitionString) in definitionsToRebuild {
            do {
                let lexer = Lexer(input: definitionString, decimalSeparator: settings.decimalSeparator)
                let tokens = lexer.tokenize()
                let parser = Parser(tokens: tokens)
                let expressionTree = try parser.parse()
                
                _ = try evaluator.evaluate(node: expressionTree, variables: &tempVars, functions: &tempFuncs, angleMode: self.angleMode)
            } catch {
                print("Error rebuilding function '\(definitionString)': \(error)")
            }
        }

        DispatchQueue.main.async {
            self.functions = tempFuncs
            self.variables = tempVars
            self.liveResult = ""
        }
    }
    
    // --- LATEX FORMATTING ---
    
    func formatCalculationAsLaTeX(_ calculation: Calculation) -> String {
        return LaTeXEngine.format(calculation: calculation, evaluator: self.evaluator, angleMode: self.angleMode, settings: self.settings)
    }


    // --- FORMATTING HELPERS ---
    
    func formatLivePreview(_ value: MathValue) -> String {
        switch value {
        case .scalar(let doubleValue): return formatScalarForDisplay(doubleValue)
        case .complex(let complexValue): return formatComplexForDisplay(complexValue)
        case .vector(let vector): return formatForHistory(.vector(vector))
        case .matrix(let matrix): return formatForHistory(.matrix(matrix))
        case .tuple(let values): return "Result: \(values.count) possible values"
        case .complexVector(let cVector): return formatForHistory(.complexVector(cVector))
        case .complexMatrix(let cMatrix): return formatForHistory(.complexMatrix(cMatrix))
        case .functionDefinition(let name): return "Function '\(name)' defined."
        case .polar(let complexValue): return formatPolarForDisplay(complexValue)
        }
    }
    
    func formatForHistory(_ value: MathValue) -> String {
        switch value {
        case .scalar(let doubleValue): return formatScalarForDisplay(doubleValue)
        case .complex(let complexValue): return formatComplexForDisplay(complexValue)
        case .vector(let vector): return formatVectorForDisplay(vector)
        case .matrix(let matrix): return formatMatrixForDisplay(matrix)
        case .tuple(let values): return values.map { formatForHistory($0) }.joined(separator: " OR ")
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

    func formatScalarForDisplay(_ value: Double) -> String {
        let formattedString: String
        switch settings.displayMode {
        case .auto:
            if value.truncatingRemainder(dividingBy: 1) == 0 {
                formattedString = String(format: "%.0f", value)
            } else {
                let absValue = abs(value)
                if absValue > 0 && (absValue < 1e-4 || absValue >= 1e15) {
                    formattedString = String(format: "%.4g", value)
                } else {
                    let tempFormatted = String(format: "%.10f", value)
                    if let regex = try? NSRegularExpression(pattern: "\\.?0+$") {
                        let nsString = tempFormatted as NSString
                        let range = NSRange(location: 0, length: nsString.length)
                        let modString = regex.stringByReplacingMatches(in: tempFormatted, options: [], range: range, withTemplate: "")
                        let finalString = modString.isEmpty ? "0" : modString
                        formattedString = finalString.hasSuffix(".") ? String(finalString.dropLast()) : finalString
                    } else {
                        formattedString = tempFormatted
                    }
                }
            }
        case .scientific:
            formattedString = String(format: "%.*e", settings.fixedDecimalPlaces, value)
        case .fixed:
            formattedString = String(format: "%.\(settings.fixedDecimalPlaces)f", value)
        }
        
        if settings.decimalSeparator == .comma {
            return formattedString.replacingOccurrences(of: ".", with: ",")
        }
        return formattedString
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
            return "\(formatScalarForDisplay(magnitude)) ∠ \(formatScalarForDisplay(angleDegrees))°"
        } else {
            return "\(formatScalarForDisplay(magnitude)) ∠ \(formatScalarForDisplay(angle)) rad"
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
    
    private func formatScalarForParsing(_ value: Double) -> String {
        let stringValue = String(value)
        if stringValue.contains("e") {
            return "(\(stringValue.replacingOccurrences(of: "e", with: "*10^")))"
        }
        return stringValue.replacingOccurrences(of: ",", with: ".")
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

