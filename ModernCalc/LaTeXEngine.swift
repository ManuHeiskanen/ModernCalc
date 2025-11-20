//
//  LaTeXEngine.swift
//  ModernCalc
//
//  Created by Manu Heiskanen on 1.9.2025.
//

import Foundation

// A dedicated engine for converting calculations into LaTeX strings.
struct LaTeXEngine {

    private static let maxRowsForPreview = 15

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
                // Avoid adding ^1 for clarity
                if expLaTeX != "1" { latex += "^{\(expLaTeX)}" }
            }
            return latex

        case let constantNode as ConstantNode:
            return formatIdentifier(constantNode.name)

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
            // For right-associative power, allow equal precedence without parens on the right
            if let rightBinary = binaryNode.right as? BinaryOpNode,
               operatorPrecedence[rightBinary.op.rawValue, default: 0] < operatorPrecedence[binaryNode.op.rawValue, default: 0] ||
                (operatorPrecedence[rightBinary.op.rawValue, default: 0] == operatorPrecedence[binaryNode.op.rawValue, default: 0] && binaryNode.op.rawValue != "^") {
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
                let leftIsParen = (binaryNode.left as? ConstantNode)?.name.hasSuffix(")") ?? false // Crude check
                let rightIsIdentifier = binaryNode.right is ConstantNode && !(binaryNode.right as! ConstantNode).name.starts(with: ".") // Avoid implicit mult with units
                let rightIsFunc = binaryNode.right is FunctionCallNode
                let rightIsParenExpr = binaryNode.right is BinaryOpNode // Check if right side starts with '(' essentially

                if (leftIsNumber || leftIsParen) && (rightIsIdentifier || rightIsFunc || rightIsParenExpr) {
                    // Check if left is just a number and right starts with a sign, avoid merging e.g. 2 * -3 -> 2-3
                    if binaryNode.left is NumberNode,
                       let unaryNode = binaryNode.right as? UnaryOpNode,
                       unaryNode.op.rawValue == "-" {
                         return "\(left) \\cdot \(right)"
                    }
                    return "\(left)\(right)" // Implicit multiplication
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
                 // Use dagger for conjugate transpose
                return "{\(args)}^{\\dagger}"
            }

            if let latexFunc = functionMap[functionCallNode.name] {
                return "\(latexFunc){\(args)}"
            }
            // Use \text for user-defined or unknown functions.
            return "\\text{\(functionCallNode.name.replacingOccurrences(of: "_", with: "\\_"))}\\left(\(args)\\right)"

        case let assignmentNode as AssignmentNode:
            let expr = formatNode(assignmentNode.expression, evaluator: evaluator, settings: settings)
            // FIX Issue 3: Format identifier properly to handle subscripts (C_outmin -> C_{outmin})
            return "\(formatIdentifier(assignmentNode.name)) := \(expr)"

        case let funcDefNode as FunctionDefinitionNode:
            let params = funcDefNode.parameterNames.joined(separator: ", ")
            let body = formatNode(funcDefNode.body, evaluator: evaluator, settings: settings)
            return "\\text{\(funcDefNode.name)}\\left(\(params)\\right) := \(body)"

        case let vectorNode as VectorNode:
            if vectorNode.elements.count > Self.maxRowsForPreview {
                return "\\text{\(vectorNode.elements.count)-element Vector}"
            }
            let elements = vectorNode.elements.map { formatNode($0, evaluator: evaluator, settings: settings) }.joined(separator: " \\\\ ")
            return "\\begin{bmatrix} \(elements) \\end{bmatrix}"

        case let matrixNode as MatrixNode:
            if matrixNode.rows.count > Self.maxRowsForPreview {
                let cols = matrixNode.rows.first?.count ?? 0
                return "\\text{\(matrixNode.rows.count)x\(cols) Matrix}"
            }
            let rows = matrixNode.rows.map { $0.map { formatNode($0, evaluator: evaluator, settings: settings) }.joined(separator: " & ") }.joined(separator: " \\\\ ")
            return "\\begin{bmatrix} \(rows) \\end{bmatrix}"

        case let postfixNode as PostfixOpNode:
            let child = formatNode(postfixNode.child, evaluator: evaluator, settings: settings)
            switch postfixNode.op.rawValue {
            case "'": return "{\(child)}^T" // Transpose
            case "!": return needsParenthesesForFactorial(postfixNode.child) ? "\(wrapInParentheses(child))!" : "\(child)!"
            default: return "\(child)\(postfixNode.op.rawValue)"
            }

        case let cVectorNode as ComplexVectorNode:
            if cVectorNode.elements.count > Self.maxRowsForPreview {
                return "\\text{\(cVectorNode.elements.count)-element Complex Vector}"
            }
            let elements = cVectorNode.elements.map { formatNode($0, evaluator: evaluator, settings: settings) }.joined(separator: " \\\\ ")
            return "\\begin{bmatrix} \(elements) \\end{bmatrix}"

        case let cMatrixNode as ComplexMatrixNode:
            if cMatrixNode.rows.count > Self.maxRowsForPreview {
                let cols = cMatrixNode.rows.first?.count ?? 0
                return "\\text{\(cMatrixNode.rows.count)x\(cols) Complex Matrix}"
            }
            let rows = cMatrixNode.rows.map { $0.map { formatNode($0, evaluator: evaluator, settings: settings) }.joined(separator: " & ") }.joined(separator: " \\\\ ")
            return "\\begin{bmatrix} \(rows) \\end{bmatrix}"

        case let derivativeNode as DerivativeNode:
            let body = formatNode(derivativeNode.body, evaluator: evaluator, settings: settings)
            let variable = derivativeNode.variable?.name ?? "x" // Assuming 'x' if variable not specified
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

        case let stringNode as StringNode:
            return "\\text{``\(stringNode.value)''}" // Use LaTeX quotes

        case is ImportCSVNode:
             return "\\text{importcsv()}"

        // Add cases for other node types if needed
        default:
            // Fallback for unhandled nodes - try basic string formatting
            return fallbackFormatExpression(node.description)
        }
    }

    // MARK: - MathValue Formatting

    // --- NEW: Helper function ---
    /// Checks if all dimension exponents are effectively integers within a small tolerance.
    private static func allDimensionsAreIntegers(_ dims: UnitDimension) -> Bool {
        // Allow empty dimensions
        if dims.isEmpty { return true }
        // Check if all non-zero exponents are close to an integer
        return dims.values.filter { abs($0) > 1e-15 }.allSatisfy { abs($0 - round($0)) < 1e-9 }
    }


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
            // --- MODIFIED: Use the integer check before looking up preferred units ---
            if allDimensionsAreIntegers(u.dimensions) {
                if let preferredUnitSymbol = u.preferredDisplayUnit,
                   let preferredUnitDef = UnitStore.units[preferredUnitSymbol],
                   preferredUnitDef.dimensions == u.dimensions { // Check exact match
                    let convertedValue = u.value / preferredUnitDef.conversionFactor
                    return "\(formatScalar(convertedValue, settings: settings)) \\, \(formatUnitSymbol(preferredUnitSymbol))"
                }

                if u.dimensions.isEmpty { return formatScalar(u.value, settings: settings) }

                if let compoundUnitString = UnitStore.commonCompoundUnits[u.dimensions] {
                    return "\(formatScalar(u.value, settings: settings)) \\, \(formatUnitSymbol(compoundUnitString))"
                }

                if let bestUnit = findBestUnitFor(dimensions: u.dimensions) {
                    let convertedValue = u.value / bestUnit.conversionFactor
                    return "\(formatScalar(convertedValue, settings: settings)) \\, \(formatUnitSymbol(bestUnit.symbol))"
                }
            } else if u.dimensions.isEmpty {
                 return formatScalar(u.value, settings: settings)
            }
            // --- END MODIFICATION ---

            // Fallback for fractional exponents or no common unit found
            let valStr = formatScalar(u.value, settings: settings)
            let unitStr = formatDimensions(u.dimensions) // Updated in Phase 3
            return "\(valStr) \\, \(unitStr)"

        case .complex(let c):
            return formatComplex(c, settings: settings)
        case .vector(let v):
            if v.dimension > Self.maxRowsForPreview {
                return "\\text{\(v.dimension)-element Vector}"
            }
            // --- ADD CHECK ---
            if allDimensionsAreIntegers(v.dimensions) {
                if let compoundUnitString = UnitStore.commonCompoundUnits[v.dimensions] {
                    let elements = v.values.map { formatScalar($0, settings: settings) }.joined(separator: " \\\\ ")
                    let matrix = "\\begin{bmatrix} \(elements) \\end{bmatrix}"
                    return "\(matrix) \\, \(formatUnitSymbol(compoundUnitString))"
                }
                if let bestUnit = findBestUnitFor(dimensions: v.dimensions) {
                    let convertedValues = v.values.map { $0 / bestUnit.conversionFactor }
                    let elements = convertedValues.map { formatScalar($0, settings: settings) }.joined(separator: " \\\\ ")
                    let matrix = "\\begin{bmatrix} \(elements) \\end{bmatrix}"
                    // Use \text{} for unit symbols
                    return "\(matrix) \\, \(formatUnitSymbol(bestUnit.symbol))"
                }
            }
            // --- END CHECK ---

            let elements = v.values.map { formatScalar($0, settings: settings) }.joined(separator: " \\\\ ")
            let matrix = "\\begin{bmatrix} \(elements) \\end{bmatrix}"
            let unitStr = formatDimensions(v.dimensions) // Updated in Phase 3
            return unitStr.isEmpty ? matrix : "\(matrix) \\, \(unitStr)"
        case .matrix(let m):
            if m.rows > Self.maxRowsForPreview {
                return "\\text{\(m.rows)x\(m.columns) Matrix}"
            }
             // --- ADD CHECK ---
            if allDimensionsAreIntegers(m.dimensions) {
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
                     // Use \text{} for unit symbols
                    return "\(matrix) \\, \(formatUnitSymbol(bestUnit.symbol))"
                }
            }
            // --- END CHECK ---

            let rows = (0..<m.rows).map { r in
                (0..<m.columns).map { c in formatScalar(m[r, c], settings: settings) }.joined(separator: " & ")
            }.joined(separator: " \\\\ ")
            let matrix = "\\begin{bmatrix} \(rows) \\end{bmatrix}"
            let unitStr = formatDimensions(m.dimensions) // Updated in Phase 3
            return unitStr.isEmpty ? matrix : "\(matrix) \\, \(unitStr)"
        case .tuple(let values):
            return values.map { formatMathValue($0, angleMode: angleMode, settings: settings, expression: expression) }.joined(separator: " \\text{ or } ")
        case .polar(let c):
            let mag = formatScalar(c.abs(), settings: settings)
            let angle = c.argument()
            let angleVal = angleMode == .degrees ? angle * (180.0 / .pi) : angle
            let angleUnit = angleMode == .degrees ? "^{\\circ}" : "\\text{ rad}" // Clarify radians
            return "\(mag) \\angle \(formatScalar(angleVal, settings: settings))\(angleUnit)"
        case .functionDefinition(let name):
            return "\\text{Defined: } \(name)"
        case .complexVector(let cv):
            if cv.dimension > Self.maxRowsForPreview {
                return "\\text{\(cv.dimension)-element Complex Vector}"
            }
            let elements = cv.values.map { formatComplex($0, settings: settings) }.joined(separator: " \\\\ ")
            let matrix = "\\begin{bmatrix} \(elements) \\end{bmatrix}"
            let unitStr = formatDimensions(cv.dimensions) // Updated in Phase 3
            return unitStr.isEmpty ? matrix : "\(matrix) \\, \(unitStr)"
        case .complexMatrix(let cm):
            if cm.rows > Self.maxRowsForPreview {
                return "\\text{\(cm.rows)x\(cm.columns) Complex Matrix}"
            }
            let rows = (0..<cm.rows).map { r in
                (0..<cm.columns).map { c in formatComplex(cm[r, c], settings: settings) }.joined(separator: " & ")
            }.joined(separator: " \\\\ ")
            let matrix = "\\begin{bmatrix} \(rows) \\end{bmatrix}"
            let unitStr = formatDimensions(cm.dimensions) // Updated in Phase 3
            return unitStr.isEmpty ? matrix : "\(matrix) \\, \(unitStr)"
        case .complexUnitValue(let cu):
            let complexValue = cu.value
            let dimensions = cu.dimensions

            if dimensions.isEmpty {
                return formatComplex(complexValue, settings: settings)
            }

            let unitStr: String
            let complexStrToFormat: String

            // --- ADD CHECK ---
            if allDimensionsAreIntegers(dimensions) {
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
                    unitStr = formatDimensions(dimensions) // Updated in Phase 3
                }
            } else {
                 complexStrToFormat = formatComplex(complexValue, settings: settings) // Already in base SI
                 unitStr = formatDimensions(dimensions) // Updated in Phase 3
            }
             // --- END CHECK ---

            return "\\left( \(complexStrToFormat) \\right) \\, \(unitStr)"

        case .regressionResult(let slope, let intercept):
            let slopeLatex = formatMathValue(.unitValue(slope), angleMode: angleMode, settings: settings)
            let interceptLatex = formatMathValue(.unitValue(intercept), angleMode: angleMode, settings: settings)
            return "\\begin{cases} \\text{Slope} = \(slopeLatex) \\\\ \\text{Intercept} = \(interceptLatex) \\end{cases}"
        case .polynomialFit(let polyCoeffs):
            return formatPolyFitWithUnits(polyCoeffs, angleMode: angleMode, settings: settings)
        case .plot(let data):
            return "\\text{Plot: \(fallbackFormatExpression(data.expression))}" // Use fallback for plot expression
        case .uncertain(let u):
            let baseString = "\(formatScalar(u.value, settings: settings)) \\pm \(formatScalar(u.totalUncertainty, settings: settings))"
            if u.dimensions.isEmpty {
                return baseString
            }

            let unitValueForFormatting = UnitValue(value: 1.0, dimensions: u.dimensions)
            // Recursive call's unitStr formatting is now updated (Phase 3)
            let unitStr = formatMathValue(.unitValue(unitValueForFormatting), angleMode: angleMode, settings: settings).trimmingCharacters(in: .whitespacesAndNewlines).replacingOccurrences(of: "1 ", with: "")

            return "\\left( \(baseString) \\right) \\, \(unitStr)"
        case .roots(let roots):
            if roots.isEmpty {
                return "\\text{No real roots found}"
            }

            var variableName = "x" // Default variable name
            if let expr = expression {
                // Regex to capture the variable name after the equation and comma
                let pattern = #"(?:solve|nsolve)\s*\([^,]+,\s*([a-zA-Z_][a-zA-Z0-9_]*)"#
                do {
                    let regex = try NSRegularExpression(pattern: pattern)
                    if let match = regex.firstMatch(in: expr, range: NSRange(expr.startIndex..., in: expr)),
                       let range = Range(match.range(at: 1), in: expr) {
                        variableName = String(expr[range])
                    }
                } catch {
                    // Handle regex error if necessary, though unlikely here
                    print("Regex error: \(error)")
                }
            }


            let rootsString = roots.map { rootValue -> String in
                let tolerance = 1e-7
                var valueToFormat = rootValue

                // Attempt to round near-integer results for cleaner display
                if let unitVal = rootValue.asUnitValue() {
                   let scalar = unitVal.value
                   let roundedScalar = round(scalar)
                   if abs(scalar - roundedScalar) < tolerance {
                       var newUnitValue = unitVal
                       newUnitValue.value = roundedScalar
                       valueToFormat = .unitValue(newUnitValue)
                   }
                }
                // Format the potentially rounded value
                return formatMathValue(valueToFormat, angleMode: angleMode, settings: settings, expression: nil)
            }.joined(separator: ", ")


            if roots.count > 1 {
                 // Use \dots for more than 4 roots
                if roots.count > 4 {
                    let firstFour = roots.prefix(4).map { formatMathValue($0, angleMode: angleMode, settings: settings, expression: nil) }.joined(separator: ", ")
                    return "\(variableName) \\approx \\{ \(firstFour), \\dots \\}"
                }
                return "\(variableName) \\approx \\{ \(rootsString) \\}"
            } else {
                return "\(variableName) \\approx \(rootsString)"
            }
        case .eigenDecomposition(let eigenvectors, let eigenvalues):
            let vMatrix = formatMathValue(.matrix(eigenvectors), angleMode: angleMode, settings: settings, expression: nil)
            let dMatrix = formatMathValue(.matrix(eigenvalues), angleMode: angleMode, settings: settings, expression: nil)
            // Use bold for matrices
            return "\\begin{cases} \\mathbf{V} = \(vMatrix) \\\\ \\mathbf{D} = \(dMatrix) \\end{cases}"
        case .odeSolution(let time, let states):
            // Display dimensions rather than full vectors/matrices for brevity
            let tDim = time.dimension
            let sRows = states.rows
            let sCols = states.columns
            return "\\begin{cases} \\mathbf{T}: \\text{\(tDim)-element Vector} \\\\ \\mathbf{Y}: \\text{\(sRows)x\(sCols) Matrix} \\end{cases}"


        default:
             // Should not be reached if all MathValue cases are handled
            return "\\text{Error: Unknown Result Type}"
        }
    }

    // MARK: - Private Helpers & Mappings

    private static let operatorPrecedence: [String: Int] = [
        "||": 1, "&&": 2, ">": 3, "<": 3, ">=": 3, "<=": 3, "==": 3, "!=": 3, "in": 3, // 'in' has comparison precedence
        "±": 4, // Treat like addition
        "+": 4, "-": 4,
        "*": 5, "/": 5, ".*": 5, "./": 5, "∠": 5, // Angle has multiplicative precedence
        "^": 6, // Power is right-associative
        "~": 7 // Unary negation (logical NOT)
        // Note: Implicit multiplication has precedence 5 as well
    ]


    private static let operatorMap: [String: String] = [
        "±": "\\pm", "==": "=", "!=": "\\neq", ">=": "\\geq", "<=": "\\leq",
        "&&": "\\land", "||": "\\lor", "in": "\\rightarrow" // Use arrow for 'in' conversion
    ]

    private static let functionMap: [String: String] = [
        "sin": "\\sin", "cos": "\\cos", "tan": "\\tan", "sec": "\\sec", "csc": "\\csc", "cot": "\\cot",
        "asin": "\\arcsin", "acos": "\\arccos", "atan": "\\arctan", "asec": "\\arcsec", "acsc": "\\arccsc", "acot": "\\arccot",
        "sinh": "\\sinh", "cosh": "\\cosh", "tanh": "\\tanh", "coth": "\\coth", "sech": "\\operatorname{sech}", "csch": "\\operatorname{csch}",
        "asinh": "\\operatorname{arsinh}", "acosh": "\\operatorname{arcosh}", "atanh": "\\operatorname{artanh}",
        "asech": "\\operatorname{arsech}", "acsch": "\\operatorname{arcsch}", "acoth": "\\operatorname{arcoth}",
        "log": "\\log", "lg": "\\lg", "ln": "\\ln", "det": "\\det", "sqrt": "\\sqrt", "abs": "\\left|", // Use | for abs
        "rank": "\\operatorname{rank}", "trace": "\\operatorname{Tr}"
    ]


    private static func isBoolean(expression: String) -> Bool {
        // Simple check, might need refinement
        let boolOperators = ["==", "!=", ">", "<", ">=", "<=", "&&", "||", "~"]
        return boolOperators.contains { expression.contains($0) }
    }

    private static func findBestUnitFor(dimensions: UnitDimension) -> UnitDefinition? {
        // Ensure dimensions are integers before searching
        guard allDimensionsAreIntegers(dimensions) else { return nil }

        if dimensions.isEmpty {
            return nil
        }
        
        if dimensions == [.meter: 2.0] || dimensions == [.meter: 3.0] {
             return nil
        }

        // Avoid suggesting m^2 or m^3 directly if other units exist
        if dimensions == [.meter: 2.0] || dimensions == [.meter: 3.0] {
             // Check if a specific area/volume unit matches EXACTLY first
             if let exactMatch = UnitStore.units.first(where: { $0.value.dimensions == dimensions })?.value {
                 return exactMatch
             }
             return nil // Otherwise, don't suggest anything, let it format as m^2 or m^3
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

        // Prioritize preferred symbols
        for symbol in preferredSymbols {
            if let match = potentialMatches.first(where: { $0.symbol == symbol }) {
                return match
            }
        }

        // Fallback: return the first match if no preferred symbol found
        return potentialMatches.first
    }

    // --- UPDATED: formatDimensions to handle Double exponents ---
    private static func formatDimensions(_ dimensions: UnitDimension) -> String {
        let positive = dimensions.filter { $0.value > 1e-15 }.sorted { $0.key.rawValue < $1.key.rawValue }
        let negative = dimensions.filter { $0.value < -1e-15 }.sorted { $0.key.rawValue < $1.key.rawValue }

        let formatPart = { (dims: [(key: BaseUnit, value: Double)]) -> String in
            dims.map { (unit, exp) -> String in
                let symbolText = "\\text{\(UnitStore.baseUnitSymbols[unit] ?? unit.rawValue)}"
                let absExp = abs(exp)

                // Special formatting for common fractional exponents
                if abs(absExp - 0.5) < 1e-9 {
                    return "\\sqrt{\(symbolText)}"
                }
                // Check if exponent is effectively an integer
                else if abs(absExp - round(absExp)) < 1e-9 {
                    let intExp = Int(round(absExp))
                    return intExp == 1 ? symbolText : "\(symbolText)^{\(intExp)}"
                }
                // Format other fractional exponents with limited precision
                else {
                    let formattedExp = String(format: "%.2g", absExp).replacingOccurrences(of: "-", with: "") // Avoid "--"
                    return "\(symbolText)^{\(formattedExp)}"
                }
            }.joined(separator: " \\cdot ")
        }

        let num = formatPart(positive)
        let den = formatPart(negative)

        if den.isEmpty {
            return num // Return "1" if dimensionless
        }
        if num.isEmpty {
            // If denominator has only one term with sqrt, format nicely
             if negative.count == 1 && abs(abs(negative[0].value) - 0.5) < 1e-9 {
                 return "\\frac{1}{\(den)}" // den will be \sqrt{...}
             }
            return "\\frac{1}{\(den)}"
        }
        return "\\frac{\(num)}{\(den)}"
    }


    private static func formatUnitSymbol(_ symbol: String) -> String {
        // Handle compound units like N/m or Pa·s first
         if symbol.contains("/") {
            let parts = symbol.split(separator: "/")
            let num = formatUnitSymbol(String(parts[0])) // Recursive call
            let den = formatUnitSymbol(String(parts[1])) // Recursive call
            return "\\frac{\(num)}{\(den)}"
        }
        if symbol.contains("·") {
            let parts = symbol.split(separator: "·")
            let latexParts = parts.map { formatUnitSymbol(String($0)) } // Recursive call
            return latexParts.joined(separator: " \\cdot ")
        }
         // Handle exponents like m^2
        if let caretIndex = symbol.firstIndex(of: "^") {
            let base = symbol[..<caretIndex]
            let exponent = symbol[symbol.index(after: caretIndex)...]
             // Ensure base is wrapped in \text if it's not a special symbol
            let baseFormatted = (base == "Ω") ? "\\Omega" : "\\text{\(base)}"
            return "\(baseFormatted)^{\(exponent)}"
        }
        // Handle special symbols
        if symbol == "Ω" { return "\\Omega" }
        // Default: wrap in \text
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
                if absValue > 0 && (absValue < 1e-2 || absValue >= 1e15) {
                    return formatScientificNotation(fromString: String(format: "%.4g", value), using: settings)
                }
                // Use settings to control max precision, remove trailing zeros
                let maxPrecision = settings.livePreviewDecimalPlaces > 0 ? settings.livePreviewDecimalPlaces : 10
                let tempFormatted = String(format: "%.\(maxPrecision)f", value)
                // Regex to remove trailing zeros after decimal point
                if let regex = try? NSRegularExpression(pattern: "\\.?0+$") {
                    let nsString = tempFormatted as NSString
                    let range = NSRange(location: 0, length: nsString.length)
                    let modString = regex.stringByReplacingMatches(in: tempFormatted, options: [], range: range, withTemplate: "")
                    let finalString = modString.isEmpty ? "0" : modString
                    // Remove trailing decimal point if it exists
                    formattedString = finalString.hasSuffix(".") ? String(finalString.dropLast()) : finalString
                } else {
                    formattedString = tempFormatted // Fallback if regex fails
                }
            }
        case .scientific:
            return formatScientificNotation(fromString: String(format: "%.*e", settings.fixedDecimalPlaces, value), using: settings)
        case .fixed:
            formattedString = String(format: "%.\(settings.fixedDecimalPlaces)f", value)
        }
        // Handle decimal separator
        return settings.decimalSeparator == .comma ? formattedString.replacingOccurrences(of: ".", with: "{,}") : formattedString
    }

    private static func formatComplex(_ c: Complex, settings: UserSettings) -> String {
        // Use tolerance for zero checks
        let realIsZero = abs(c.real) < 1e-12
        let imagIsZero = abs(c.imaginary) < 1e-12

        if realIsZero && imagIsZero { return "0" }
        if imagIsZero { return formatScalar(c.real, settings: settings) }
        if realIsZero {
            if abs(c.imaginary - 1.0) < 1e-12 { return "i" }
            if abs(c.imaginary - -1.0) < 1e-12 { return "-i" }
            return "\(formatScalar(c.imaginary, settings: settings))i"
        }
        let sign = c.imaginary < 0 ? "-" : "+"
        // Format absolute value of imaginary part
        let imagScalarRaw = abs(c.imaginary)
        // Check if imaginary part is effectively 1
        let imagIsOne = abs(imagScalarRaw - 1.0) < 1e-12
        let imagPart = imagIsOne ? "i" : "\(formatScalar(imagScalarRaw, settings: settings))i"

        return "\(formatScalar(c.real, settings: settings)) \(sign) \(imagPart)"
    }


    private static func formatPolyFitWithUnits(_ polyCoeffs: PolynomialCoefficients, angleMode: AngleMode, settings: UserSettings) -> String {
        var result = "y = "
        let coefficients = polyCoeffs.coefficients
        var isFirstTerm = true

        for (i, coeffUnitValue) in coefficients.enumerated().reversed() {
            // Skip zero coefficients unless it's the only term (constant 0)
            if abs(coeffUnitValue.value) < 1e-9 && coefficients.count > 1 { continue }

            let coeffValue = coeffUnitValue.value

            // Handle sign for non-first terms
            if !isFirstTerm {
                result += (coeffValue < 0) ? " - " : " + "
            } else if coeffValue < 0 {
                 // Handle negative sign for the very first term
                result += "- "
            } else if result == "y = " {
                 // Remove initial space if first term is positive
                result = "y="
            }
            isFirstTerm = false

            let coeffAbsValue = abs(coeffValue)
            // Format coefficient, potentially converting units
            let formattedCoeffWithValue = formatMathValue(.unitValue(UnitValue(value: coeffAbsValue, dimensions: coeffUnitValue.dimensions)), angleMode: angleMode, settings: settings)

            // Extract formatted value and unit string
            let parts = formattedCoeffWithValue.split(separator: "\\,").map { $0.trimmingCharacters(in: .whitespaces) }
            let formattedCoeff = parts[0]
            let unitStr = parts.count > 1 ? parts[1] : ""

            let needsCoeffValue = abs(coeffAbsValue - 1.0) > 1e-9 || i == 0
            let hasUnit = !unitStr.isEmpty

            // Add coefficient value if needed (not 1 unless it's the constant term)
            if needsCoeffValue {
                result += formattedCoeff
                // Add space if unit or x term follows
                if hasUnit || i > 0 { result += "\\," }
            } else if hasUnit && i == 0 {
                 // If coeff is 1 and it's the constant term, still need to show 1 if there's a unit
                 result += formattedCoeff
                 if hasUnit { result += "\\," }
            }

            // Add unit string
            if hasUnit {
                // Wrap complex units in parentheses
                if unitStr.contains("/") || unitStr.contains("cdot") || unitStr.contains(" ") || unitStr.contains("sqrt") || unitStr.contains("{") {
                    result += "\\left( \(unitStr) \\right)"
                } else {
                    result += unitStr
                }
                 // Add space if x term follows
                if i > 0 { result += "\\," }
            }

            // Add x term and exponent
            if i > 0 {
                result += "x"
            }
            if i > 1 {
                result += "^{\(i)}"
            }
             // Handle case where coefficient is exactly 1 or -1 and there's no unit, but it's not the constant term
             else if !needsCoeffValue && !hasUnit && i > 0 && abs(coeffAbsValue - 1.0) < 1e-9 {
                  // The 'x' or 'x^n' is already added, nothing more needed.
             }
             // Handle constant term 0
             else if needsCoeffValue && abs(coeffAbsValue) < 1e-9 && i == 0 && coefficients.count == 1 {
                 result += formattedCoeff // Ensures "y=0" is displayed
             }

        }
         // Clean up potential leading "+ " if the first term was skipped but others exist
        if result.hasPrefix("y =  + ") {
            result = String(result.dropFirst(7))
            result = "y=" + result
        } else if result == "y = " {
             // Handle case where all coefficients were zero
             result = "y = 0"
        }
        return result
    }


    private static func formatScientificNotation(fromString scientificString: String, using settings: UserSettings) -> String {
        let parts = scientificString.lowercased().split(separator: "e")
        guard parts.count == 2, let exponent = Int(parts[1]) else { return scientificString }
        var mantissa = String(parts[0])
        // Ensure decimal separator is correct *before* potential comma insertion
        if settings.decimalSeparator == .comma { mantissa = mantissa.replacingOccurrences(of: ".", with: ",") }
        else { mantissa = mantissa.replacingOccurrences(of: ",", with: ".") } // Ensure internal representation uses period

        // Format mantissa with fixed decimals if needed for consistency, respecting separator
        if let mantissaValue = Double(mantissa.replacingOccurrences(of: ",", with: ".")) { // Convert back to Double for formatting
            let fixedMantissa = String(format: "%.\(settings.fixedDecimalPlaces)f", mantissaValue)
            mantissa = settings.decimalSeparator == .comma ? fixedMantissa.replacingOccurrences(of: ".", with: "{,}") : fixedMantissa
        }

        return "\(mantissa) \\times 10^{\(exponent)}"
    }


    private static func fallbackFormatExpression(_ expression: String) -> String {
        // More robust fallback replacements
        return expression
            .replacingOccurrences(of: "*", with: " \\cdot ")
            .replacingOccurrences(of: "/", with: " / ") // Keep / as / for fallback
            .replacingOccurrences(of: "pi", with: "\\pi")
            .replacingOccurrences(of: "==", with: "=")
            .replacingOccurrences(of: "!=", with: "\\neq")
            .replacingOccurrences(of: ">=", with: "\\geq")
            .replacingOccurrences(of: "<=", with: "\\leq")
            .replacingOccurrences(of: "&&", with: "\\land")
            .replacingOccurrences(of: "||", with: "\\lor")
            .replacingOccurrences(of: "~", with: "\\neg")
            .replacingOccurrences(of: "->", with: "\\rightarrow") // For 'in'
             // Basic unit formatting attempt
            .replacingOccurrences(of: #"\.([a-zA-ZΩµ]+)"#, with: "\\text{$1}", options: .regularExpression)
    }

    private static func wrapInParentheses(_ text: String) -> String {
        // Avoid double parentheses
        if text.hasPrefix("\\left(") && text.hasSuffix("\\right)") {
            return text
        }
        return "\\left( \(text) \\right)"
    }
    
    // FIX Issue 3: Added helper method to correctly format identifiers, handling subscripts (e.g. C_outmin -> C_{outmin})
    private static func formatIdentifier(_ name: String) -> String {
        if name == "pi" { return "\\pi" }

        if let underscoreIndex = name.firstIndex(of: "_") {
            let base = name[..<underscoreIndex]
            let subscriptPart = name[name.index(after: underscoreIndex)...]

            let formattedBase = String(base).replacingOccurrences(of: "π", with: "\\pi")
            let formattedSubscript = String(subscriptPart).replacingOccurrences(of: "π", with: "\\pi")
            
            // If the subscript part is more than one character, wrap it in braces for correct LaTeX rendering
            // Even for single chars, wrapping is safe, but logic here prioritizes consistency.
            return "\(formattedBase)_{\(formattedSubscript)}"
        }

        return name.replacingOccurrences(of: "π", with: "\\pi")
    }

    private static func needsParenthesesForFactorial(_ node: ExpressionNode) -> Bool {
        // Factorial needs parentheses for binary ops, unary ops (except simple negation of number/constant), and function calls.
        if node is BinaryOpNode || node is FunctionCallNode { return true }
        if let unary = node as? UnaryOpNode {
            // Don't add parens for just "-5!" or "-x!"
            return !(unary.op.rawValue == "-" && (unary.child is NumberNode || unary.child is ConstantNode))
        }
        return false
    }
}
