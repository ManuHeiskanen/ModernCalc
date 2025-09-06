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
    @Published var liveLaTeXPreview: String = ""
    @Published var previewText: String = ""
    @Published var variables: [String: MathValue] = [:]
    @Published var functions: [String: FunctionDefinitionNode] = [:]
    @Published var isTallExpression: Bool = false
    
    @Published var angleMode: AngleMode = .degrees {
        didSet { saveState() }
    }
    @Published var userFunctionDefinitions: [String: String] = [:]
    @Published var cursorPosition = NSRange()
    
    private var settings: UserSettings
    private let evaluator = Evaluator()
    private var lastSuccessfulValue: MathValue?
    private var lastUsedAngleFlag: Bool = false
    private var cancellables = Set<AnyCancellable>()
    private let navigationManager = NavigationManager()
    private let ansVariable = "ans"
    
    let siPrefixes: Set<String> = [
        "yotta", "zetta", "exa", "peta", "tera", "giga", "mega", "kilo", "hecto", "deca", "deci",
        "centi", "milli", "micro", "nano", "pico", "femto", "atto", "zepto", "yocto"
    ]
    
    let operatorSymbols: [MathSymbol]
    let greekSymbols: [MathSymbol]
    let constantSymbols: [MathSymbol]
    
    let builtinFunctions: [BuiltinFunction] = [
        // Trig
        .init(name: "sin", signature: "sin(angle)", description: "Calculates the sine of an angle."),
        .init(name: "cos", signature: "cos(angle)", description: "Calculates the cosine of an angle."),
        .init(name: "tan", signature: "tan(angle)", description: "Calculates the tangent of an angle."),
        .init(name: "asin", signature: "asin(value)", description: "Calculates the inverse sine (arcsin)."),
        .init(name: "acos", signature: "acos(value)", description: "Calculates the inverse cosine (arccos)."),
        .init(name: "atan", signature: "atan(value)", description: "Calculates the inverse tangent (arctan)."),
        // Calculus
        .init(name: "derivative", signature: "derivative(expr, var, point, [order])", description: "Finds the instantaneous rate of change (slope). Can calculate higher-order derivatives (e.g., order 2 for concavity)."),
        .init(name: "integral", signature: "integral(expr, var, from, to)", description: "Calculates the total area under a function's curve between two points."),
        // General
        .init(name: "sqrt", signature: "sqrt(number)", description: "Calculates the square root. Handles complex numbers."),
        .init(name: "root", signature: "root(number, degree)", description: "Calculates the nth root of a number."),
        .init(name: "abs", signature: "abs(value)", description: "Calculates the absolute value or magnitude."),
        .init(name: "log", signature: "log(number)", description: "Calculates the common (base-10) logarithm."),
        .init(name: "lg", signature: "lg(number)", description: "Alias for the common (base-10) logarithm."),
        .init(name: "ln", signature: "ln(number)", description: "Calculates the natural (base-e) logarithm."),
        .init(name: "round", signature: "round(number)", description: "Rounds a number to the nearest integer."),
        .init(name: "floor", signature: "floor(number)", description: "Rounds a number down to the nearest integer."),
        .init(name: "ceil", signature: "ceil(number)", description: "Rounds a number up to the nearest integer."),
        .init(name: "fact", signature: "fact(integer)", description: "Calculates the factorial of a non-negative integer."),
        .init(name: "matrix", signature: "matrix(a, b ; c, d ; ...)", description: "Forms a matrix of given real numbers."),
        .init(name: "cmatrix", signature: "cmatrix(a, b ; c, d ; ...)", description: "Forms a complex matrix of given real or imaginary numbers."),
        .init(name: "vector", signature: "vector(a ; b ; c ; ...)", description: "Forms a vector of given real numbers."),
        .init(name: "cvector", signature: "cvector(a ; b ; c ; ...)", description: "Forms a complex vector of given real or imaginary numbers."),
        // Stats
        .init(name: "sum", signature: "sum(a, b, ...)", description: "Calculates the sum of a list of numbers or a vector/matrix."),
        .init(name: "avg", signature: "avg(a, b, ...)", description: "Calculates the average of a list of numbers or a vector/matrix."),
        .init(name: "min", signature: "min(a, b, ...)", description: "Finds the minimum value in a list of numbers or a vector/matrix."),
        .init(name: "max", signature: "max(a, b, ...)", description: "Finds the maximum value in a list of numbers or a vector/matrix."),
        .init(name: "median", signature: "median(a, b, ...)", description: "Finds the median of a list of numbers or a vector/matrix."),
        .init(name: "stddev", signature: "stddev(a, b, ...)", description: "Calculates the sample standard deviation."),
        // Linear Algebra
        .init(name: "dot", signature: "dot(vectorA, vectorB)", description: "Calculates the dot product of two vectors (real or complex)."),
        .init(name: "cross", signature: "cross(vectorA, vectorB)", description: "Calculates the cross product of two 3D vectors."),
        .init(name: "det", signature: "det(matrix)", description: "Calculates the determinant of a square matrix."),
        .init(name: "inv", signature: "inv(matrix)", description: "Calculates the inverse of a square matrix."),
        .init(name: "unit", signature: "unit(vector)", description: "Returns the unit vector (vector with magnitude 1)."),
        .init(name: "angle", signature: "angle(vectorA, vectorB)", description: "Calculates the angle between two vectors."),
        .init(name: "transpose", signature: "transpose(matrix)", description: "Transposes a matrix (rows become columns). For complex matrices, this is not the conjugate transpose."),
        .init(name: "trace", signature: "trace(matrix)", description: "Calculates the trace of a square matrix (sum of diagonal elements)."),
        // Complex
        .init(name: "polar", signature: "polar(complex)", description: "Converts a complex number to its polar form (R ∠ θ)."),
        .init(name: "real", signature: "real(complex)", description: "Extracts the real part of a complex number."),
        .init(name: "imag", signature: "imag(complex)", description: "Extracts the imaginary part of a complex number."),
        .init(name: "conj", signature: "conj(complex)", description: "Calculates the complex conjugate."),
        .init(name: "arg", signature: "arg(complex)", description: "Calculates the argument (phase) of a complex number."),
        // Combinatorics
        .init(name: "nCr", signature: "nCr(n, k)", description: "Calculates the number of combinations."),
        .init(name: "nPr", signature: "nPr(n, k)", description: "Calculates the number of permutations."),
        // Geometry
        .init(name: "hypot", signature: "hypot(sideA, sideB)", description: "Calculates the hypotenuse of a right triangle."),
        .init(name: "side", signature: "side(hyp, sideA)", description: "Calculates the missing side of a right triangle."),
        .init(name: "area_rect", signature: "area_rect(w, h)", description: "Area of a rectangle."),
        .init(name: "area_tri", signature: "area_tri(b, h)", description: "Area of a triangle."),
        .init(name: "area_circle", signature: "area_circle(r)", description: "Area of a circle."),
        .init(name: "circum_circle", signature: "circum_circle(r)", description: "Circumference of a circle."),
        .init(name: "vol_sphere", signature: "vol_sphere(r)", description: "Volume of a sphere."),
        .init(name: "vol_cube", signature: "vol_cube(s)", description: "Volume of a cube."),
        .init(name: "vol_cylinder", signature: "vol_cylinder(r, h)", description: "Volume of a cylinder."),
        .init(name: "vol_cone", signature: "vol_cone(r, h)", description: "Volume of a cone.")
    ]
    
    let physicalConstants: [PhysicalConstant] = [
        .init(symbol: "π", name: "Pi", value: Double.pi), .init(symbol: "e", name: "Euler's number", value: M_E),
        .init(symbol: "c", name: "Speed of light", value: 299792458), .init(symbol: "g", name: "Standard gravity", value: 9.80665),
        .init(symbol: "G", name: "Gravitational constant", value: 6.67430e-11), .init(symbol: "h", name: "Planck constant", value: 6.62607015e-34),
        .init(symbol: "ħ", name: "Reduced Planck constant", value: 1.054571817e-34), .init(symbol: "μ0", name: "Vacuum permeability", value: 1.25663706212e-6),
        .init(symbol: "ε0", name: "Vacuum permittivity", value: 8.8541878128e-12), .init(symbol: "me", name: "Electron mass", value: 9.1093837015e-31),
        .init(symbol: "mp", name: "Proton mass", value: 1.67262192369e-27), .init(symbol: "mn", name: "Neutron mass", value: 1.67492749804e-27),
        .init(symbol: "e0", name: "Elementary charge", value: 1.602176634e-19), .init(symbol: "NA", name: "Avogadro constant", value: 6.02214076e23),
        .init(symbol: "R", name: "Gas constant", value: 8.314462618), .init(symbol: "kB", name: "Boltzmann constant", value: 1.380649e-23),
        .init(symbol: "F", name: "Faraday constant", value: 96485.33212), .init(symbol: "Rinf", name: "Rydberg constant", value: 10973731.568160),
        .init(symbol: "σ", name: "Stefan-Boltzmann", value: 5.670374419e-8), .init(symbol: "b", name: "Wien's displacement", value: 2.897771955e-3),
        .init(symbol: "atm", name: "Standard atmosphere", value: 101325), .init(symbol: "Vm", name: "Molar volume (STP)", value: 22.41396954e-3)
    ]

    let helpTopics: [HelpTopic] = [
        .init(title: "Quick Start", content: "Basic calculations, history navigation, LaTeX export, and the `ans` variable."),
        .init(title: "Syntax Reference", content: "Variable assignment (`:=`), function definition (`f(x) := ...`), complex numbers (`3+4i`), vectors (`vector(1;2;3)`), matrices (`matrix(1,2;3,4)`), and polar coordinates (`5∠30`)."),
        .init(title: "Operators", content: "Standard operators `+ - * / ^ %`, transpose `'`, element-wise `.* ./`, and implicit multiplication (`2x`).")
    ]


    init(settings: UserSettings) {
        self.settings = settings
        self.operatorSymbols = [
            .init(symbol: "±", name: "Plus-Minus"), .init(symbol: "∠", name: "Angle"), .init(symbol: "√", name: "Square Root", insertionText: "√("),
            .init(symbol: "×", name: "Multiply"), .init(symbol: "÷", name: "Divide"), .init(symbol: "^", name: "Power"),
            .init(symbol: "'", name: "Transpose"), .init(symbol: ".*", name: "Element-wise Multiply"), .init(symbol: "./", name: "Element-wise Divide")
        ]
        self.greekSymbols = [
            .init(symbol: "α", name: "Alpha"), .init(symbol: "Α", name: "Alpha"), .init(symbol: "β", name: "Beta"), .init(symbol: "Β", name: "Beta"),
            .init(symbol: "γ", name: "Gamma"), .init(symbol: "Γ", name: "Gamma"), .init(symbol: "δ", name: "Delta"), .init(symbol: "Δ", name: "Delta"),
            .init(symbol: "ε", name: "Epsilon"), .init(symbol: "Ε", name: "Epsilon"), .init(symbol: "ζ", name: "Zeta"), .init(symbol: "Ζ", name: "Zeta"),
            .init(symbol: "η", name: "Eta"), .init(symbol: "Η", name: "Eta"), .init(symbol: "θ", name: "Theta"), .init(symbol: "Θ", name: "Theta"),
            .init(symbol: "ι", name: "Iota"), .init(symbol: "Ι", name: "Iota"), .init(symbol: "κ", name: "Kappa"), .init(symbol: "Κ", name: "Kappa"),
            .init(symbol: "λ", name: "Lambda"), .init(symbol: "Λ", name: "Lambda"), .init(symbol: "μ", name: "Mu"), .init(symbol: "Μ", name: "Mu"),
            .init(symbol: "ν", name: "Nu"), .init(symbol: "Ν", name: "Nu"), .init(symbol: "ξ", name: "Xi"), .init(symbol: "Ξ", name: "Xi"),
            .init(symbol: "ο", name: "Omicron"), .init(symbol: "Ο", name: "Omicron"), .init(symbol: "ρ", name: "Rho"), .init(symbol: "Ρ", name: "Rho"),
            .init(symbol: "σ", name: "Sigma"), .init(symbol: "Σ", name: "Sigma"), .init(symbol: "τ", name: "Tau"), .init(symbol: "Τ", name: "Tau"),
            .init(symbol: "υ", name: "Upsilon"), .init(symbol: "Υ", name: "Upsilon"), .init(symbol: "φ", name: "Phi"), .init(symbol: "Φ", name: "Phi"),
            .init(symbol: "χ", name: "Chi"), .init(symbol: "Χ", name: "Chi"), .init(symbol: "ψ", name: "Psi"), .init(symbol: "Ψ", name: "Psi"),
            .init(symbol: "ω", name: "Omega"), .init(symbol: "Ω", name: "Omega")
        ]
        self.constantSymbols = physicalConstants.map { .init(symbol: $0.symbol, name: $0.name, insertionText: $0.symbol) }
        
        Publishers.CombineLatest($rawExpression, $cursorPosition)
            .debounce(for: .milliseconds(50), scheduler: RunLoop.main)
            .sink { [weak self] (expression, position) in
                guard let self = self else { return }
                self.calculate(expression: expression, cursor: position)
            }
            .store(in: &cancellables)
            
        loadState()
    }

    private func getContextualHelp(expression: String, cursor: NSRange) -> String? {
        guard cursor.location <= expression.utf16.count else { return nil }
        
        let startIndex = expression.startIndex
        let cursorIndex = expression.index(startIndex, offsetBy: cursor.location, limitedBy: expression.endIndex) ?? expression.endIndex
        let textBeforeCursor = expression[..<cursorIndex]

        var openParenCount = 0
        var lastOpenParenIndex: String.Index?
        
        for index in textBeforeCursor.indices.reversed() {
            let char = textBeforeCursor[index]
            if char == ")" {
                openParenCount += 1
            } else if char == "(" {
                if openParenCount == 0 {
                    lastOpenParenIndex = index
                    break
                } else {
                    openParenCount -= 1
                }
            }
        }
        
        guard let parenIndex = lastOpenParenIndex else { return nil }
        
        let textBeforeParen = textBeforeCursor[..<parenIndex]
        
        let pattern = "\\b([a-zA-Z_][a-zA-Z0-9_]*)$"
        if let range = textBeforeParen.range(of: pattern, options: .regularExpression),
           let function = builtinFunctions.first(where: { $0.name == textBeforeParen[range] }) {
            return "\(function.signature)\n\(function.description)"
        }
        
        return nil
    }

    private func calculate(expression: String, cursor: NSRange) {
        let helpText = getContextualHelp(expression: expression, cursor: cursor)
        
        let isMultiRow = (expression.contains("vector(") || expression.contains("matrix(")) && expression.contains(";")
        let parts = expression.components(separatedBy: CharacterSet(charactersIn: "+-()"))
        let hasNestedFraction = parts.contains { $0.filter { $0 == "/" }.count >= 2 }
        let isTall = isMultiRow || hasNestedFraction
        if self.isTallExpression != isTall { self.isTallExpression = isTall }

        guard !expression.trimmingCharacters(in: .whitespaces).isEmpty else {
            DispatchQueue.main.async {
                self.liveResult = helpText ?? ""
                self.liveLaTeXPreview = ""
                self.lastSuccessfulValue = nil
            }
            return
        }

        var tempVars = self.variables
        var tempFuncs = self.functions
        do {
            let lexer = Lexer(input: expression, decimalSeparator: settings.decimalSeparator)
            let tokens = lexer.tokenize()
            let parser = Parser(tokens: tokens)
            let expressionTree = try parser.parse()
            let expressionLaTeX = LaTeXEngine.formatNode(expressionTree, evaluator: self.evaluator, settings: self.settings)

            let (value, usedAngle) = try evaluator.evaluate(node: expressionTree, variables: &tempVars, functions: &tempFuncs, angleMode: self.angleMode)
            
            let isSimpleVariableDefinition = expressionTree is AssignmentNode && ((expressionTree as! AssignmentNode).expression is NumberNode || (expressionTree as! AssignmentNode).expression is UnaryOpNode)
            
            DispatchQueue.main.async {
                self.variables = tempVars
                self.functions = tempFuncs
                self.lastSuccessfulValue = value
                self.lastUsedAngleFlag = usedAngle
                
                if case .functionDefinition = value {
                    self.liveLaTeXPreview = expressionLaTeX
                } else if isSimpleVariableDefinition {
                    self.liveLaTeXPreview = expressionLaTeX
                } else {
                    let resultLaTeX: String
                    if self.settings.enableLiveRounding {
                        let liveSettings = UserSettings(); liveSettings.displayMode = .fixed
                        liveSettings.fixedDecimalPlaces = self.settings.livePreviewDecimalPlaces; liveSettings.decimalSeparator = self.settings.decimalSeparator
                        resultLaTeX = LaTeXEngine.formatMathValue(value, angleMode: self.angleMode, settings: liveSettings)
                    } else {
                        resultLaTeX = LaTeXEngine.formatMathValue(value, angleMode: self.angleMode, settings: self.settings)
                    }
                    self.liveLaTeXPreview = "\(expressionLaTeX) = \(resultLaTeX)"
                }
                
                self.liveResult = helpText ?? ""
            }
        } catch let error {
            let errorMessage = (error as? CustomStringConvertible)?.description ?? "An unknown error occurred."
            let finalMessage = [helpText, errorMessage].compactMap { $0 }.joined(separator: "\n")
            
            let partialLaTeX = LaTeXEngine.formatExpression(expression, evaluator: self.evaluator, settings: self.settings)
            
            DispatchQueue.main.async {
                self.lastSuccessfulValue = nil
                self.liveLaTeXPreview = partialLaTeX
                self.liveResult = finalMessage
            }
        }
    }
    
    func commitCalculation() {
        if let selectedItem = history.first(where: { $0.id == navigationManager.selectedHistoryId }) {
            DispatchQueue.main.async {
                if selectedItem.type == .functionDefinition { self.insertTextAtCursor(selectedItem.expression.replacingOccurrences(of: " ", with: "")) }
                else {
                    if self.navigationManager.selectedPart == .equation { self.insertTextAtCursor(selectedItem.expression.replacingOccurrences(of: " ", with: "")) }
                    else { self.insertTextAtCursor(self.formatForParsing(selectedItem.result)) }
                }
                self.resetNavigation()
            }
        } else {
            guard !rawExpression.isEmpty, let valueToCommit = lastSuccessfulValue else { return }
            
            let calcType: CalculationType
            if valueToCommit.typeName == "FunctionDefinition" { calcType = .functionDefinition }
            else if rawExpression.contains(":=") { calcType = .variableAssignment }
            else { calcType = .evaluation }

            if calcType != .functionDefinition { DispatchQueue.main.async { self.variables[self.ansVariable] = valueToCommit } }
            if calcType == .variableAssignment { saveState() }
            else if calcType == .functionDefinition, case .functionDefinition(let name) = valueToCommit { userFunctionDefinitions[name] = rawExpression; saveState() }
            
            let newCalculation = Calculation(expression: rawExpression, result: valueToCommit, type: calcType, usedAngleSensitiveFunction: self.lastUsedAngleFlag, angleMode: self.angleMode)
            DispatchQueue.main.async { self.history.append(newCalculation); self.rawExpression = "" }
        }
    }

    func handleKeyPress(keys: Set<KeyEquivalent>) -> Bool {
        if let selectedText = navigationManager.handleKeyPress(keys: keys, history: history, viewModel: self) {
            DispatchQueue.main.async { self.previewText = selectedText }; return true
        } else {
            DispatchQueue.main.async { self.previewText = "" }; return false
        }
    }

    func resetNavigation() { navigationManager.resetSelection(); previewText = "" }

    var selectedHistoryId: UUID? { navigationManager.selectedHistoryId }
    var selectedHistoryPart: SelectionPart { navigationManager.selectedPart }
    var sortedVariables: [(String, MathValue)] { variables.sorted { $0.key < $1.key } }
    var sortedFunctions: [(String, FunctionDefinitionNode)] { functions.sorted { $0.key < $1.key } }
    
    func deleteVariable(name: String) { variables.removeValue(forKey: name); saveState() }
    func deleteFunction(name: String) { functions.removeValue(forKey: name); userFunctionDefinitions.removeValue(forKey: name); saveState() }
    
    func insertTextAtCursor(_ textToInsert: String) {
        guard let range = Range(cursorPosition, in: rawExpression) else {
            rawExpression += textToInsert; let newLocation = rawExpression.utf16.count; cursorPosition = NSRange(location: newLocation, length: 0)
            return
        }
        rawExpression.replaceSubrange(range, with: textToInsert)
        let newLocation = cursorPosition.location + textToInsert.utf16.count; cursorPosition = NSRange(location: newLocation, length: 0)
    }

    private func saveState() {
        do {
            let variablesData = try JSONEncoder().encode(variables); let functionsData = try JSONEncoder().encode(userFunctionDefinitions)
            UserDefaults.standard.set(variablesData, forKey: "userVariables"); UserDefaults.standard.set(functionsData, forKey: "userFunctionDefinitions")
            UserDefaults.standard.set(angleMode.rawValue, forKey: "angleModeSetting")
        } catch { print("Error saving state: \(error.localizedDescription)") }
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
            print("Error loading variable/function state: \(error.localizedDescription)"); self.variables = [:]; self.userFunctionDefinitions = [:]
        }
        if let savedAngleMode = UserDefaults.standard.string(forKey: "angleModeSetting") { self.angleMode = AngleMode(rawValue: savedAngleMode) ?? .degrees }
    }
    
    private func rebuildFunctionsFromDefinitions() {
        let definitionsToRebuild = self.userFunctionDefinitions; guard !definitionsToRebuild.isEmpty else { return }
        var tempVars = self.variables; var tempFuncs: [String: FunctionDefinitionNode] = [:]

        for (_, definitionString) in definitionsToRebuild {
            do {
                let lexer = Lexer(input: definitionString, decimalSeparator: settings.decimalSeparator)
                let tokens = lexer.tokenize(); let parser = Parser(tokens: tokens); let expressionTree = try parser.parse()
                _ = try evaluator.evaluate(node: expressionTree, variables: &tempVars, functions: &tempFuncs, angleMode: self.angleMode)
            } catch { print("Error rebuilding function '\(definitionString)': \(error)") }
        }
        DispatchQueue.main.async { self.functions = tempFuncs; self.variables = tempVars; self.liveResult = "" }
    }
    
    func formatCalculationAsLaTeX(_ calculation: Calculation) -> String {
        return LaTeXEngine.format(calculation: calculation, evaluator: self.evaluator, angleMode: self.angleMode, settings: self.settings)
    }

    func formatForHistory(_ value: MathValue) -> String {
        switch value {
        case .scalar(let d): return formatScalarForDisplay(d); case .complex(let c): return formatComplexForDisplay(c); case .vector(let v): return formatVectorForDisplay(v)
        case .matrix(let m): return formatMatrixForDisplay(m); case .tuple(let t): return t.map { formatForHistory($0) }.joined(separator: " OR ")
        case .complexVector(let cv): return formatComplexVectorForDisplay(cv); case .complexMatrix(let cm): return formatComplexMatrixForDisplay(cm)
        case .functionDefinition: return ""; case .polar(let p): return formatPolarForDisplay(p)
        }
    }
    
    func formatForParsing(_ value: MathValue) -> String {
        switch value {
        case .scalar(let d): return formatScalarForParsing(d); case .complex(let c): return formatComplexForParsing(c)
        case .vector(let v): return "vector(\(v.values.map { formatScalarForParsing($0) }.joined(separator: ";")))"
        case .matrix(let m): return "matrix(\((0..<m.rows).map { r in (0..<m.columns).map { c in formatScalarForParsing(m[r, c]) }.joined(separator: ",") }.joined(separator: ";")))"
        case .tuple(let t): return t.map { formatForParsing($0) }.first ?? ""
        case .complexVector(let cv): return "cvector(\(cv.values.map { formatForParsing(.complex($0)) }.joined(separator: ";")))"
        case .complexMatrix(let cm): return "cmatrix(\((0..<cm.rows).map { r in (0..<cm.columns).map { c in formatForParsing(.complex(cm[r, c])) }.joined(separator: ",") }.joined(separator: ";")))"
        case .functionDefinition: return ""; case .polar(let p): return formatPolarForParsing(p)
        }
    }

    func formatScalarForDisplay(_ value: Double) -> String {
        let formattedString: String
        switch settings.displayMode {
        case .auto:
            if value.truncatingRemainder(dividingBy: 1) == 0 { formattedString = String(format: "%.0f", value) }
            else {
                let absValue = abs(value)
                if absValue > 0 && (absValue < 1e-4 || absValue >= 1e15) { formattedString = String(format: "%.4g", value) }
                else {
                    let tempFormatted = String(format: "%.10f", value)
                    if let regex = try? NSRegularExpression(pattern: "\\.?0+$") {
                        let nsString = tempFormatted as NSString
                        let range = NSRange(location: 0, length: nsString.length)
                        let modString = regex.stringByReplacingMatches(in: tempFormatted, options: [], range: range, withTemplate: "")
                        let finalString = modString.isEmpty ? "0" : modString
                        formattedString = finalString.hasSuffix(".") ? String(finalString.dropLast()) : finalString
                    } else { formattedString = tempFormatted }
                }
            }
        case .scientific: formattedString = String(format: "%.*e", settings.fixedDecimalPlaces, value)
        case .fixed: formattedString = String(format: "%.\(settings.fixedDecimalPlaces)f", value)
        }
        return settings.decimalSeparator == .comma ? formattedString.replacingOccurrences(of: ".", with: ",") : formattedString
    }
    
    private func formatComplexForDisplay(_ value: Complex) -> String {
        if value.real != 0 && value.imaginary != 0 { return "\(formatScalarForDisplay(value.real)) \(value.imaginary < 0 ? "-" : "+") \(formatScalarForDisplay(abs(value.imaginary)))i" }
        else if value.real != 0 { return formatScalarForDisplay(value.real) }
        else if value.imaginary != 0 { return "\(formatScalarForDisplay(value.imaginary))i" }
        else { return "0" }
    }
    
    private func formatPolarForDisplay(_ value: Complex) -> String {
        let magnitude = value.abs(); let angle = value.argument()
        if self.angleMode == .degrees { let angleDegrees = angle * (180.0 / .pi); return "\(formatScalarForDisplay(magnitude)) ∠ \(formatScalarForDisplay(angleDegrees))°" }
        else { return "\(formatScalarForDisplay(magnitude)) ∠ \(formatScalarForDisplay(angle)) rad" }
    }

    private func formatVectorForDisplay(_ vector: Vector) -> String { return (0..<vector.dimension).map { "[ \(formatScalarForDisplay(vector[$0])) ]" }.joined(separator: "\n") }
    
    private func formatMatrixForDisplay(_ matrix: Matrix) -> String {
        if matrix.rows == 0 || matrix.columns == 0 { return "[]" }
        var columnWidths = [Int](repeating: 0, count: matrix.columns)
        for c in 0..<matrix.columns {
            var maxWidth = 0
            for r in 0..<matrix.rows { let formattedNumber = formatScalarForDisplay(matrix[r, c]); if formattedNumber.count > maxWidth { maxWidth = formattedNumber.count } }
            columnWidths[c] = maxWidth
        }
        return (0..<matrix.rows).map { r in
            let rowContent = (0..<matrix.columns).map { c in
                let formattedNumber = formatScalarForDisplay(matrix[r, c]); let padding = String(repeating: " ", count: columnWidths[c] - formattedNumber.count)
                return padding + formattedNumber
            }.joined(separator: "  "); return "[ \(rowContent) ]"
        }.joined(separator: "\n")
    }
    
    private func formatComplexVectorForDisplay(_ vector: ComplexVector) -> String { return (0..<vector.dimension).map { "[ \(formatComplexForDisplay(vector[$0])) ]" }.joined(separator: "\n") }
    
    private func formatComplexMatrixForDisplay(_ matrix: ComplexMatrix) -> String {
        if matrix.rows == 0 || matrix.columns == 0 { return "[]" }
        var columnWidths = [Int](repeating: 0, count: matrix.columns)
        for c in 0..<matrix.columns {
            var maxWidth = 0
            for r in 0..<matrix.rows { let formattedNumber = "(\(formatComplexForDisplay(matrix[r, c])))"; if formattedNumber.count > maxWidth { maxWidth = formattedNumber.count } }
            columnWidths[c] = maxWidth
        }
        return (0..<matrix.rows).map { r in
            let rowContent = (0..<matrix.columns).map { c in
                let formattedNumber = "(\(formatComplexForDisplay(matrix[r, c])))"; let padding = String(repeating: " ", count: columnWidths[c] - formattedNumber.count)
                return padding + formattedNumber
            }.joined(separator: "  "); return "[ \(rowContent) ]"
        }.joined(separator: "\n")
    }
    
    private func formatScalarForParsing(_ value: Double) -> String {
        let stringValue = String(value)
        if stringValue.contains("e") { return "(\(stringValue.replacingOccurrences(of: "e", with: "*10^")))" }
        return stringValue.replacingOccurrences(of: ",", with: ".")
    }
    
    private func formatComplexForParsing(_ value: Complex) -> String {
        let sign = value.imaginary < 0 ? "" : "+"; return "(\(formatScalarForParsing(value.real))\(sign)\(formatScalarForParsing(value.imaginary))i)"
    }
    
    private func formatPolarForParsing(_ value: Complex) -> String {
        let magnitude = value.abs(); let angleDegrees = value.argument() * (180.0 / .pi)
        return "\(formatScalarForParsing(magnitude))∠\(formatScalarForParsing(angleDegrees))"
    }
}

