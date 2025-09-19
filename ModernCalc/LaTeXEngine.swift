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
        case .evaluation:
            let resultLaTeX = formatMathValue(calculation.result, angleMode: angleMode, settings: settings, expression: calculation.expression)
            return "\(expressionLaTeX) = \(resultLaTeX)"
        case .variableAssignment, .functionDefinition, .plot:
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

            // Special formatting for the target unit to avoid showing an implicit "1".
            // E.g., for ".ft", this ensures we format it as "ft", not "1 ft".
            func formatTargetUnit(node: ExpressionNode) -> String {
                if let binaryNode = node as? BinaryOpNode,
                   let numberNode = binaryNode.left as? NumberNode,
                   numberNode.value == 1.0 {
                    // This is the case `1 * .unit`, so we only format the unit part.
                    return formatNode(binaryNode.right, evaluator: evaluator, settings: settings)
                }
                // Otherwise (e.g., just `.ft` or a complex expression like `2*.ft`), format normally.
                return formatNode(node, evaluator: evaluator, settings: settings)
            }
            
            let target = formatTargetUnit(node: conversionNode.targetUnitNode)
            return "\(value) \\rightarrow \(target)"

        case let unitNode as UnitAndExponentNode:
            var latex = "\\text{\(unitNode.unitSymbol)}"
            if let exponent = unitNode.exponent {
                let expLaTeX = formatNode(exponent, evaluator: evaluator, settings: settings)
                if expLaTeX != "1" { latex += "^{\(expLaTeX)}" }
            }
            return latex

        case let constantNode as ConstantNode:
            let name = constantNode.name
            // Use proper LaTeX command for pi.
            if name == "pi" { return "\\pi" }
            
            // Handle subscripts denoted by underscore.
            if let underscoreIndex = name.firstIndex(of: "_") {
                let base = name[..<underscoreIndex]
                let subscriptPart = name[name.index(after: underscoreIndex)...]
                
                // If the part after the underscore is longer than a single character,
                // it must be wrapped in curly braces for correct LaTeX rendering.
                if subscriptPart.count > 1 {
                    // Also, handle any other special characters within the base or subscript.
                    let formattedBase = String(base).replacingOccurrences(of: "π", with: "\\pi")
                    let formattedSubscript = String(subscriptPart).replacingOccurrences(of: "π", with: "\\pi")
                    return "\(formattedBase)_{\(formattedSubscript)}"
                } else {
                    return "\(String(base))_{\(String(subscriptPart))}"
                }
            }
            
            // Fallback for names without special subscript handling.
            return name.replacingOccurrences(of: "π", with: "\\pi")
            
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
            
            if functionCallNode.name == "ctranspose" {
                guard functionCallNode.arguments.count == 1 else { return "\\text{ctranspose()}" }
                return "{\(args)}^T"
            }
            
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
            
            if let compoundUnitString = UnitStore.commonCompoundUnits[u.dimensions] {
                // The value 'u.value' is already in base SI, which is what we need.
                // We just append the nicely formatted compound string.
                return "\(formatScalar(u.value, settings: settings)) \\, \(formatUnitSymbol(compoundUnitString))"
            }
            
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
            if let compoundUnitString = UnitStore.commonCompoundUnits[v.dimensions] {
                let elements = v.values.map { formatScalar($0, settings: settings) }.joined(separator: " \\\\ ")
                let matrix = "\\begin{bmatrix} \(elements) \\end{bmatrix}"
                return "\(matrix) \\, \(formatUnitSymbol(compoundUnitString))"
            }
            if let bestUnit = findBestUnitFor(dimensions: v.dimensions) {
                let convertedValues = v.values.map { $0 / bestUnit.conversionFactor }
                let elements = convertedValues.map { formatScalar($0, settings: settings) }.joined(separator: " \\\\ ")
                let matrix = "\\begin{bmatrix} \(elements) \\end{bmatrix}"
                return "\(matrix) \\, \\text{\(bestUnit.symbol)}"
            }

            let elements = v.values.map { formatScalar($0, settings: settings) }.joined(separator: " \\\\ ")
            let matrix = "\\begin{bmatrix} \(elements) \\end{bmatrix}"
            let unitStr = formatDimensions(v.dimensions)
            return unitStr.isEmpty ? matrix : "\(matrix) \\, \(unitStr)"
        case .matrix(let m):
            if let compoundUnitString = UnitStore.commonCompoundUnits[m.dimensions] {
                let rows = (0..<m.rows).map { r in
                    (0..<m.columns).map { c in formatScalar(m[r, c], settings: settings) }.joined(separator: " & ")
                }.joined(separator: " \\\\ ")
                let matrix = "\\begin{bmatrix} \(rows) \\end{bmatrix}"
                return "\(matrix) \\, \(formatUnitSymbol(compoundUnitString))"
            }
            if let bestUnit = findBestUnitFor(dimensions: m.dimensions) {
                let convertedValues = m.values.map { $0 / bestUnit.conversionFactor }
                let convertedMatrix = Matrix(values: convertedValues, rows: m.rows, columns: m.columns, dimensions: m.dimensions)
                let rows = (0..<convertedMatrix.rows).map { r in
                    (0..<convertedMatrix.columns).map { c in formatScalar(convertedMatrix[r, c], settings: settings) }.joined(separator: " & ")
                }.joined(separator: " \\\\ ")
                let matrix = "\\begin{bmatrix} \(rows) \\end{bmatrix}"
                return "\(matrix) \\, \\text{\(bestUnit.symbol)}"
            }
            
            let rows = (0..<m.rows).map { r in
                (0..<m.columns).map { c in formatScalar(m[r, c], settings: settings) }.joined(separator: " & ")
            }.joined(separator: " \\\\ ")
            let matrix = "\\begin{bmatrix} \(rows) \\end{bmatrix}"
            let unitStr = formatDimensions(m.dimensions)
            return unitStr.isEmpty ? matrix : "\(matrix) \\, \(unitStr)"
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
            let matrix = "\\begin{bmatrix} \(elements) \\end{bmatrix}"
            let unitStr = formatDimensions(cv.dimensions)
            return unitStr.isEmpty ? matrix : "\(matrix) \\, \(unitStr)"
        case .complexMatrix(let cm):
            let rows = (0..<cm.rows).map { r in
                (0..<cm.columns).map { c in formatComplex(cm[r, c], settings: settings) }.joined(separator: " & ")
            }.joined(separator: " \\\\ ")
            let matrix = "\\begin{bmatrix} \(rows) \\end{bmatrix}"
            let unitStr = formatDimensions(cm.dimensions)
            return unitStr.isEmpty ? matrix : "\(matrix) \\, \(unitStr)"
        case .complexUnitValue(let cu):
            let complexValue = cu.value
            let dimensions = cu.dimensions
            
            if dimensions.isEmpty {
                return formatComplex(complexValue, settings: settings)
            }
            
            let unitStr: String
            let complexStrToFormat: String

            if let bestUnit = findBestUnitFor(dimensions: dimensions) {
                let convertedReal = complexValue.real / bestUnit.conversionFactor
                let convertedImag = complexValue.imaginary / bestUnit.conversionFactor
                let convertedComplex = Complex(real: convertedReal, imaginary: convertedImag)
                complexStrToFormat = formatComplex(convertedComplex, settings: settings)
                unitStr = formatUnitSymbol(bestUnit.symbol)
            } else if let compoundUnitString = UnitStore.commonCompoundUnits[dimensions] {
                complexStrToFormat = formatComplex(complexValue, settings: settings) // Already in base SI
                unitStr = formatUnitSymbol(compoundUnitString)
            } else {
                complexStrToFormat = formatComplex(complexValue, settings: settings) // Already in base SI
                unitStr = formatDimensions(dimensions)
            }
            
            return "\\left( \(complexStrToFormat) \\right) \\, \(unitStr)"
        case .regressionResult(let slope, let intercept):
            let slopeLatex = formatMathValue(.unitValue(slope), angleMode: angleMode, settings: settings)
            let interceptLatex = formatMathValue(.unitValue(intercept), angleMode: angleMode, settings: settings)
            return "\\begin{cases} \\text{Slope} = \(slopeLatex) \\\\ \\text{Intercept} = \(interceptLatex) \\end{cases}"
        case .polynomialFit(let polyCoeffs):
            return formatPolyFitWithUnits(polyCoeffs, angleMode: angleMode, settings: settings)
        case .plot(let data):
            return "\\text{Plot: \(data.expression.replacingOccurrences(of: "*", with: "\\cdot"))}"
        case .uncertain(let u):
            let baseString = "\(formatScalar(u.value, settings: settings)) \\pm \(formatScalar(u.totalUncertainty, settings: settings))"
            if u.dimensions.isEmpty {
                return baseString
            }
            
            let unitValueForFormatting = UnitValue(value: 1.0, dimensions: u.dimensions)
            let unitStr = formatMathValue(.unitValue(unitValueForFormatting), angleMode: angleMode, settings: settings).trimmingCharacters(in: .whitespacesAndNewlines).replacingOccurrences(of: "1 ", with: "")
            
            return "\\left( \(baseString) \\right) \\, \(unitStr)"
        case .roots(let roots):
            if roots.isEmpty {
                return "\\text{No real roots found}"
            }
            
            var variableName = "x" // Default variable name
            if let expr = expression {
                let pattern = #"(?:solve|nsolve)\s*\([^,]+,\s*([a-zA-Z_][a-zA-Z0-9_]*)"#
                do {
                    let regex = try NSRegularExpression(pattern: pattern)
                    if let match = regex.firstMatch(in: expr, range: NSRange(expr.startIndex..., in: expr)),
                       let range = Range(match.range(at: 1), in: expr) {
                        variableName = String(expr[range])
                    }
                } catch {}
            }

            let rootsString = roots.map { rootValue -> String in
                let tolerance = 1e-7
                var valueToFormat = rootValue
                
                if let scalar = try? rootValue.asScalar() {
                    let roundedScalar = scalar.rounded()
                    if abs(scalar - roundedScalar) < tolerance {
                        switch rootValue {
                        case .dimensionless:
                            valueToFormat = .dimensionless(roundedScalar)
                        case .unitValue(let u):
                            var newUnitValue = u
                            newUnitValue.value = roundedScalar
                            valueToFormat = .unitValue(newUnitValue)
                        default:
                            break
                        }
                    }
                }
                return formatMathValue(valueToFormat, angleMode: angleMode, settings: settings, expression: nil)
            }.joined(separator: ", ")

            if roots.count > 1 {
                return "\(variableName) \\approx \\{ \(rootsString) \\}"
            } else {
                return "\(variableName) \\approx \(rootsString)"
            }
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
        
        return nil
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
        if symbol.contains("/") {
            let parts = symbol.split(separator: "/")
            return "\\frac{\\text{\(parts[0])}}{\\text{\(parts[1])}}"
        }
        if symbol.contains("·") {
            let parts = symbol.split(separator: "·")
            let latexParts = parts.map { "\\text{\($0)}" }
            return latexParts.joined(separator: " \\cdot ")
        }
        if let caretIndex = symbol.firstIndex(of: "^") {
            let base = symbol[..<caretIndex]
            let exponent = symbol[symbol.index(after: caretIndex)...]
            return "\\text{\(base)}^{\(exponent)}"
        }
        if symbol == "Ω" { return "\\Omega" }
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
    
    private static func formatPolyFitWithUnits(_ polyCoeffs: PolynomialCoefficients, angleMode: AngleMode, settings: UserSettings) -> String {
        var result = "y = "
        let coefficients = polyCoeffs.coefficients
        var isFirstTerm = true
        
        for (i, coeffUnitValue) in coefficients.enumerated().reversed() {
            if abs(coeffUnitValue.value) < 1e-9 && coefficients.count > 1 { continue }
            
            let coeffValue = coeffUnitValue.value
            
            if !isFirstTerm {
                result += (coeffValue < 0) ? " - " : " + "
            } else if coeffValue < 0 {
                result += "- "
            }
            isFirstTerm = false
            
            let coeffAbsValue = abs(coeffValue)
            let formattedCoeff = formatScalar(coeffAbsValue, settings: settings)
            
            let dimensions = coeffUnitValue.dimensions
            var unitStr: String
            if dimensions.isEmpty {
                unitStr = ""
            } else if let compound = UnitStore.commonCompoundUnits[dimensions] {
                unitStr = formatUnitSymbol(compound)
            } else if let bestUnit = findBestUnitFor(dimensions: dimensions) {
                // Here, we have a raw value in SI units. We need to convert it for display.
                let convertedValue = coeffAbsValue / bestUnit.conversionFactor
                _ = formatScalar(convertedValue, settings: settings)
                
                // Since we're overriding the coefficient display, we need to handle it carefully.
                // This logic becomes complex. Let's simplify the approach by just formatting the unit.
                unitStr = formatUnitSymbol(bestUnit.symbol)
            } else {
                unitStr = formatDimensions(dimensions)
            }
            
            let needsCoeff = abs(coeffAbsValue - 1.0) > 1e-9 || i == 0
            let hasUnit = !unitStr.isEmpty
            
            if needsCoeff {
                result += formattedCoeff
                if hasUnit && i > 0 { result += "\\," }
            }
            
            if hasUnit {
                if unitStr.contains("/") || unitStr.contains("cdot") || unitStr.contains(" ") {
                    result += "\\left( \(unitStr) \\right)"
                } else {
                    result += unitStr
                }
            }
            
            if i > 0 {
                if needsCoeff || hasUnit { result += "\\," }
                result += "x"
            }
            if i > 1 { result += "^{\(i)}" }
        }
        return result.replacingOccurrences(of: "y =  + ", with: "y = ")
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

