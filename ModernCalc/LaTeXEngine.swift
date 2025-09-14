//
//  LaTeXEngine.swift
//  ModernCalc
//
//  Created by Manu Heiskanen on 1.9.2025.
//

import Foundation

// A dedicated engine for converting calculations into LaTeX strings.
struct LaTeXEngine {
    
    /// Formats a complete `Calculation` object into a single LaTeX string.
    static func format(calculation: Calculation, evaluator: Evaluator, angleMode: AngleMode, settings: UserSettings) -> String {
        let expressionLaTeX = formatExpression(calculation.expression, evaluator: evaluator, settings: settings)
        
        switch calculation.type {
        case .evaluation, .variableAssignment:
            let resultLaTeX = formatMathValue(calculation.result, angleMode: angleMode, settings: settings, expression: calculation.expression)
            return "\(expressionLaTeX) = \(resultLaTeX)"
        case .functionDefinition, .plot:
            return expressionLaTeX
        }
    }
    
    static func formatExpression(_ expression: String, evaluator: Evaluator, settings: UserSettings) -> String {
        do {
            let lexer = Lexer(input: expression, decimalSeparator: settings.decimalSeparator)
            let tokens = lexer.tokenize()
            let parser = Parser(tokens: tokens)
            let node = try parser.parse()
            return formatNode(node, evaluator: evaluator, settings: settings)
        } catch {
            // Even on failure, try to do basic string replacement for a better preview.
            return fallbackFormatExpression(expression)
        }
    }
    
    // MARK: - AST-Based Formatting
    
    /// Converts an `ExpressionNode` from the AST into a LaTeX string.
    static func formatNode(_ node: ExpressionNode, evaluator: Evaluator, settings: UserSettings) -> String {
        switch node {
        case let numberNode as NumberNode:
            return formatScalar(numberNode.value, settings: settings)
            
        case let conversionNode as ConversionNode:
            let value = formatNode(conversionNode.valueNode, evaluator: evaluator, settings: settings)
            let target = formatNode(conversionNode.targetUnitNode, evaluator: evaluator, settings: settings)
            return "\(value) \\rightarrow \(target)"

        case let unitNode as UnitAndExponentNode:
            var latex = "\\text{\(unitNode.unitSymbol)}"
            if let exponent = unitNode.exponent {
                let expLaTeX = formatNode(exponent, evaluator: evaluator, settings: settings)
                if expLaTeX != "1" { latex += "^{\(expLaTeX)}" }
            }
            return latex

        case let constantNode as ConstantNode:
            // Use proper LaTeX command for pi, otherwise use the name.
            if constantNode.name == "pi" { return "\\pi" }
            return constantNode.name.replacingOccurrences(of: "π", with: "\\pi")
            
        case let unaryNode as UnaryOpNode:
            let child = formatNode(unaryNode.child, evaluator: evaluator, settings: settings)
            // Special handling for negation of boolean expressions.
            if unaryNode.op.rawValue == "~" {
                return "\\neg\(wrapInParentheses(child))"
            }
            // Add parentheses for clarity if the child is a binary operation.
            if unaryNode.child is BinaryOpNode {
                return "\(unaryNode.op.rawValue)\(wrapInParentheses(child))"
            }
            return "\(unaryNode.op.rawValue)\(child)"

        case let binaryNode as BinaryOpNode:
            var left = formatNode(binaryNode.left, evaluator: evaluator, settings: settings)
            var right = formatNode(binaryNode.right, evaluator: evaluator, settings: settings)

            // Add parentheses around child nodes if their precedence is lower.
            if let leftBinary = binaryNode.left as? BinaryOpNode, operatorPrecedence[leftBinary.op.rawValue, default: 0] < operatorPrecedence[binaryNode.op.rawValue, default: 0] {
                left = wrapInParentheses(left)
            }
            if let rightBinary = binaryNode.right as? BinaryOpNode, operatorPrecedence[rightBinary.op.rawValue, default: 0] <= operatorPrecedence[binaryNode.op.rawValue, default: 0] {
                right = wrapInParentheses(right)
            }
            
            // Use proper LaTeX symbols for operators.
            if let latexOp = operatorMap[binaryNode.op.rawValue] {
                return "\(left) \(latexOp) \(right)"
            }
            
            // Special cases for fractions and implicit multiplication.
            switch binaryNode.op.rawValue {
            case "/", "÷":
                return "\\frac{\(left)}{\(right)}"
            case "*", "×":
                // Logic for implicit multiplication (e.g., 2x, 2(x+y)).
                let leftIsNumber = binaryNode.left is NumberNode
                let leftIsParen = (binaryNode.left as? ConstantNode)?.name.hasSuffix(")") ?? false
                let rightIsIdentifier = binaryNode.right is ConstantNode
                let rightIsFunc = binaryNode.right is FunctionCallNode
                let rightIsParen = binaryNode.right is UnaryOpNode && (binaryNode.right as! UnaryOpNode).op.rawValue == "("
                
                if (leftIsNumber || leftIsParen) && (rightIsIdentifier || rightIsFunc || rightIsParen) {
                    return "\(left)\(right)"
                }
                return "\(left) \\cdot \(right)"
            case "^":
                return "{\(left)}^{\(right)}"
            default:
                return "\(left) \(binaryNode.op.rawValue) \(right)"
            }

        case let functionCallNode as FunctionCallNode:
            let args = functionCallNode.arguments.map { formatNode($0, evaluator: evaluator, settings: settings) }.joined(separator: ", ")
            
            if let latexFunc = functionMap[functionCallNode.name] {
                return "\(latexFunc){\(args)}"
            }
            // Use \text for user-defined or unknown functions.
            return "\\text{\(functionCallNode.name.replacingOccurrences(of: "_", with: "\\_"))}\\left(\(args)\\right)"

        case let assignmentNode as AssignmentNode:
            let expr = formatNode(assignmentNode.expression, evaluator: evaluator, settings: settings)
            return "\(assignmentNode.name) := \(expr)"
            
        case let funcDefNode as FunctionDefinitionNode:
            let params = funcDefNode.parameterNames.joined(separator: ", ")
            let body = formatNode(funcDefNode.body, evaluator: evaluator, settings: settings)
            return "\\text{\(funcDefNode.name)}\\left(\(params)\\right) := \(body)"

        case let vectorNode as VectorNode:
            let elements = vectorNode.elements.map { formatNode($0, evaluator: evaluator, settings: settings) }.joined(separator: " \\\\ ")
            return "\\begin{bmatrix} \(elements) \\end{bmatrix}"

        case let matrixNode as MatrixNode:
            let rows = matrixNode.rows.map { $0.map { formatNode($0, evaluator: evaluator, settings: settings) }.joined(separator: " & ") }.joined(separator: " \\\\ ")
            return "\\begin{bmatrix} \(rows) \\end{bmatrix}"
        
        case let postfixNode as PostfixOpNode:
            let child = formatNode(postfixNode.child, evaluator: evaluator, settings: settings)
            switch postfixNode.op.rawValue {
            case "'": return "{\(child)}^T"
            case "!": return needsParenthesesForFactorial(postfixNode.child) ? "\(wrapInParentheses(child))!" : "\(child)!"
            default: return "\(child)\(postfixNode.op.rawValue)"
            }

        case let cVectorNode as ComplexVectorNode:
            let elements = cVectorNode.elements.map { formatNode($0, evaluator: evaluator, settings: settings) }.joined(separator: " \\\\ ")
            return "\\begin{bmatrix} \(elements) \\end{bmatrix}"

        case let cMatrixNode as ComplexMatrixNode:
            let rows = cMatrixNode.rows.map { $0.map { formatNode($0, evaluator: evaluator, settings: settings) }.joined(separator: " & ") }.joined(separator: " \\\\ ")
            return "\\begin{bmatrix} \(rows) \\end{bmatrix}"
        
        case let derivativeNode as DerivativeNode:
            let body = formatNode(derivativeNode.body, evaluator: evaluator, settings: settings)
            let variable = derivativeNode.variable?.name ?? "x"
            let point = formatNode(derivativeNode.point, evaluator: evaluator, settings: settings)
            let order = formatNode(derivativeNode.order, evaluator: evaluator, settings: settings)
            
            let derivativePart = (order == "1") ?
                "\\frac{d}{d\(variable)}" :
                "\\frac{d^{\(order)}}{d\(variable)^{\(order)}}"
            
            return "\(derivativePart)\\left(\(body)\\right)\\Bigg|_{\(variable)=\(point)}"

        case let integralNode as IntegralNode:
            let body = formatNode(integralNode.body, evaluator: evaluator, settings: settings)
            let variable = integralNode.variable.name
            let lower = formatNode(integralNode.lowerBound, evaluator: evaluator, settings: settings)
            let upper = formatNode(integralNode.upperBound, evaluator: evaluator, settings: settings)
            return "\\int_{\(lower)}^{\(upper)} \(body) \\,\\mathrm{d}\(variable)"
            
        case let primeNode as PrimeDerivativeNode:
            let arg = formatNode(primeNode.argument, evaluator: evaluator, settings: settings)
            return "\\text{\(primeNode.functionName)}'\\left(\(arg)\\right)"

        default:
            return node.description
        }
    }
    
    // MARK: - MathValue Formatting

    /// Formats a final calculated `MathValue` into a LaTeX string.
    static func formatMathValue(_ value: MathValue, angleMode: AngleMode, settings: UserSettings, expression: String? = nil) -> String {
        switch value {
        case .dimensionless(let doubleValue):
            if let expr = expression, isBoolean(expression: expr) {
                if doubleValue == 1.0 { return "\\text{true}" }
                if doubleValue == 0.0 { return "\\text{false}" }
            }
            return formatScalar(doubleValue, settings: settings)
        case .unitValue(let u):
            if let preferredUnitSymbol = u.preferredDisplayUnit,
               let preferredUnitDef = UnitStore.units[preferredUnitSymbol] {
                let convertedValue = u.value / preferredUnitDef.conversionFactor
                return "\(formatScalar(convertedValue, settings: settings)) \\, \(formatUnitSymbol(preferredUnitSymbol))"
            }
            
            if u.dimensions.isEmpty { return formatScalar(u.value, settings: settings) }
            
            if let bestUnit = findBestUnitFor(dimensions: u.dimensions) {
                let convertedValue = u.value / bestUnit.conversionFactor
                return "\(formatScalar(convertedValue, settings: settings)) \\, \(formatUnitSymbol(bestUnit.symbol))"
            }
            // Fallback for complex units without a single symbol.
            let valStr = formatScalar(u.value, settings: settings)
            let unitStr = formatDimensions(u.dimensions)
            return "\(valStr) \\, \(unitStr)"
        case .complex(let c):
            return formatComplex(c, settings: settings)
        case .vector(let v):
            let elements = v.values.map { formatScalar($0, settings: settings) }.joined(separator: " \\\\ ")
            return "\\begin{bmatrix} \(elements) \\end{bmatrix}"
        case .matrix(let m):
            let rows = (0..<m.rows).map { r in
                (0..<m.columns).map { c in formatScalar(m[r, c], settings: settings) }.joined(separator: " & ")
            }.joined(separator: " \\\\ ")
            return "\\begin{bmatrix} \(rows) \\end{bmatrix}"
        case .tuple(let values):
            return values.map { formatMathValue($0, angleMode: angleMode, settings: settings, expression: expression) }.joined(separator: " \\text{ or } ")
        case .polar(let c):
            let mag = formatScalar(c.abs(), settings: settings)
            let angle = c.argument()
            let angleVal = angleMode == .degrees ? angle * (180.0 / .pi) : angle
            let angleUnit = angleMode == .degrees ? "^{\\circ}" : ""
            return "\(mag) \\angle \(formatScalar(angleVal, settings: settings))\(angleUnit)"
        case .functionDefinition(let name):
            return "\\text{Defined: } \(name)"
        case .complexVector(let cv):
            let elements = cv.values.map { formatComplex($0, settings: settings) }.joined(separator: " \\\\ ")
            return "\\begin{bmatrix} \(elements) \\end{bmatrix}"
        case .complexMatrix(let cm):
            let rows = (0..<cm.rows).map { r in
                (0..<cm.columns).map { c in formatComplex(cm[r, c], settings: settings) }.joined(separator: " & ")
            }.joined(separator: " \\\\ ")
            return "\\begin{bmatrix} \(rows) \\end{bmatrix}"
        case .regressionResult(let m, let b):
            return "\\text{m = } \(formatScalar(m, settings: settings)), \\text{ b = } \(formatScalar(b, settings: settings))"
        case .polynomialFit(let coeffs):
            return formatPolyFit(coeffs, settings: settings)
        case .plot(let data):
            return "\\text{Plot: \(data.expression.replacingOccurrences(of: "*", with: "\\cdot"))}"
        case .uncertain(let u):
            return "\(formatScalar(u.value, settings: settings)) \\pm \(formatScalar(u.totalUncertainty, settings: settings))"
        default:
            return ""
        }
    }

    // MARK: - Private Helpers & Mappings

    private static let operatorPrecedence: [String: Int] = [
        "||": 1, "&&": 2, ">": 3, "<": 3, ">=": 3, "<=": 3, "==": 3, "!=": 3,
        "+": 4, "-": 4, "*": 5, "/": 5, ".*": 5, "./": 5, "^": 6, "~": 7
    ]

    private static let operatorMap: [String: String] = [
        "±": "\\pm", "==": "=", "!=": "\\neq", ">=": "\\geq", "<=": "\\leq",
        "&&": "\\land", "||": "\\lor"
    ]

    private static let functionMap: [String: String] = [
        "sin": "\\sin", "cos": "\\cos", "tan": "\\tan", "sec": "\\sec", "csc": "\\csc", "cot": "\\cot",
        "asin": "\\arcsin", "acos": "\\arccos", "atan": "\\arctan", "asec": "\\arcsec", "acsc": "\\arccsc", "acot": "\\arccot",
        "sinh": "\\sinh", "cosh": "\\cosh", "tanh": "\\tanh", "coth": "\\coth",
        "asinh": "\\operatorname{arsinh}", "acosh": "\\operatorname{arcosh}", "atanh": "\\operatorname{artanh}",
        "log": "\\log", "ln": "\\ln", "det": "\\det", "sqrt": "\\sqrt"
    ]

    private static func isBoolean(expression: String) -> Bool {
        let boolOperators = ["==", "!=", ">", "<", ">=", "<=", "&&", "||", "~"]
        return boolOperators.contains { expression.contains($0) }
    }
    
    private static func findBestUnitFor(dimensions: UnitDimension) -> UnitDefinition? {
        if dimensions.isEmpty {
            return nil
        }
        
        // Special cases: The user prefers to see area/volume in base SI units (m^2, m^3)
        // by default. Returning nil prevents automatic conversion to other units like L or ha.
        if dimensions == [.meter: 3] || dimensions == [.meter: 2] {
            return nil
        }
        
        let preferredSymbols = ["m", "s", "kg", "A", "K", "mol", "cd", "N", "J", "W", "Pa", "Hz", "C", "V", "Ohm", "F", "H", "T", "L", "eV", "cal", "bar", "g"]

        var potentialMatches: [UnitDefinition] = []
        for (_, unitDef) in UnitStore.units {
            if unitDef.dimensions == dimensions {
                potentialMatches.append(unitDef)
            }
        }

        if potentialMatches.isEmpty {
            return nil
        }

        for symbol in preferredSymbols {
            if let match = potentialMatches.first(where: { $0.symbol == symbol }) {
                return match
            }
        }
        
        // Fallback to the shortest symbol if no preferred match is found
        return potentialMatches.min(by: { $0.symbol.count < $1.symbol.count })
    }

    private static func formatDimensions(_ dimensions: UnitDimension) -> String {
        let positive = dimensions.filter { $0.value > 0 }.sorted { $0.key.rawValue < $1.key.rawValue }
        let negative = dimensions.filter { $0.value < 0 }.sorted { $0.key.rawValue < $1.key.rawValue }

        let formatPart = { (dims: [(key: BaseUnit, value: Int)]) -> String in
            dims.map { (unit, exp) -> String in
                let symbol = UnitStore.baseUnitSymbols[unit] ?? unit.rawValue
                return abs(exp) == 1 ? "\\text{\(symbol)}" : "\\text{\(symbol)}^{\(abs(exp))}"
            }.joined(separator: " \\cdot ")
        }

        let num = formatPart(positive)
        let den = formatPart(negative)

        if den.isEmpty { return num }
        if num.isEmpty { return "\\frac{1}{\(den)}" }
        return "\\frac{\(num)}{\(den)}"
    }
    
    private static func formatUnitSymbol(_ symbol: String) -> String {
        if let caretIndex = symbol.firstIndex(of: "^") {
            let base = symbol[..<caretIndex]
            let exponent = symbol[symbol.index(after: caretIndex)...]
            return "\\text{\(base)}^{\(exponent)}"
        }
        return "\\text{\(symbol)}"
    }

    private static func formatScalar(_ value: Double, settings: UserSettings) -> String {
        let formattedString: String
        switch settings.displayMode {
        case .auto:
            if value.truncatingRemainder(dividingBy: 1) == 0 && abs(value) < 1e15 {
                formattedString = String(format: "%.0f", value)
            } else {
                let absValue = abs(value)
                if absValue > 0 && (absValue < 1e-4 || absValue >= 1e15) {
                    return formatScientificNotation(fromString: String(format: "%.4g", value), using: settings)
                }
                let temp = String(format: "%.10f", value).trimmingCharacters(in: ["0"])
                formattedString = temp.hasSuffix(".") ? String(temp.dropLast()) : (temp.isEmpty ? "0" : temp)
            }
        case .scientific:
            return formatScientificNotation(fromString: String(format: "%.*e", settings.fixedDecimalPlaces, value), using: settings)
        case .fixed:
            formattedString = String(format: "%.\(settings.fixedDecimalPlaces)f", value)
        }
        return settings.decimalSeparator == .comma ? formattedString.replacingOccurrences(of: ".", with: "{,}") : formattedString
    }
    
    private static func formatComplex(_ c: Complex, settings: UserSettings) -> String {
        if abs(c.real) < 1e-12 && abs(c.imaginary) < 1e-12 { return "0" }
        if abs(c.imaginary) < 1e-12 { return formatScalar(c.real, settings: settings) }
        if abs(c.real) < 1e-12 {
            if abs(c.imaginary - 1.0) < 1e-12 { return "i" }
            if abs(c.imaginary - -1.0) < 1e-12 { return "-i" }
            return "\(formatScalar(c.imaginary, settings: settings))i"
        }
        let sign = c.imaginary < 0 ? "-" : "+"
        let imagScalar = formatScalar(abs(c.imaginary), settings: settings)
        let imagPart = imagScalar == "1" ? "i" : "\(imagScalar)i"
        return "\(formatScalar(c.real, settings: settings)) \(sign) \(imagPart)"
    }
    
    private static func formatPolyFit(_ coeffs: Vector, settings: UserSettings) -> String {
        var result = "y = "
        for (i, coeff) in coeffs.values.enumerated().reversed() {
            if abs(coeff) < 1e-9 && coeffs.dimension > 1 { continue }
            let isFirst = (result == "y = ")
            if !isFirst { result += (coeff < 0) ? " - " : " + " }
            else if coeff < 0 { result += "- " }
            
            let formattedCoeff = formatScalar(abs(coeff), settings: settings)
            if abs(abs(coeff) - 1.0) > 1e-9 || i == 0 { result += formattedCoeff }
            
            if i > 0 { result += "x" }
            if i > 1 { result += "^{\(i)}" }
        }
        return result
    }

    private static func formatScientificNotation(fromString scientificString: String, using settings: UserSettings) -> String {
        let parts = scientificString.lowercased().split(separator: "e")
        guard parts.count == 2, let exponent = Int(parts[1]) else { return scientificString }
        var mantissa = String(parts[0])
        if settings.decimalSeparator == .comma { mantissa = mantissa.replacingOccurrences(of: ".", with: "{,}") }
        return "\(mantissa) \\times 10^{\(exponent)}"
    }

    private static func fallbackFormatExpression(_ expression: String) -> String {
        return expression.replacingOccurrences(of: "*", with: " \\cdot ").replacingOccurrences(of: "pi", with: "\\pi")
    }
    
    private static func wrapInParentheses(_ text: String) -> String {
        return "\\left( \(text) \\right)"
    }
    
    private static func needsParenthesesForFactorial(_ node: ExpressionNode) -> Bool {
        return node is BinaryOpNode || node is UnaryOpNode
    }
}
