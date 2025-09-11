//
//  LaTeXEngine.swift
//  ModernCalc
//
//  Created by Manu Heiskanen on 1.9.2025.
//

import Foundation

// A dedicated engine for converting calculations into LaTeX strings.
struct LaTeXEngine {
    
    // The main public method now accepts a settings object.
    static func format(calculation: Calculation, evaluator: Evaluator, angleMode: AngleMode, settings: UserSettings) -> String {
        let expressionLaTeX = formatExpression(calculation.expression, evaluator: evaluator, settings: settings)
        
        switch calculation.type {
        case .evaluation, .variableAssignment:
            let resultLaTeX = formatMathValue(calculation.result, angleMode: angleMode, settings: settings)
            return "\(expressionLaTeX) = \(resultLaTeX)"
        case .functionDefinition:
            return expressionLaTeX
        case .plot:
            // For a plot, we just show the expression that was plotted.
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
    
    // --- AST-Based Formatting ---
    
    // Made this function public so it can be accessed from the ViewModel
    static func formatNode(_ node: ExpressionNode, evaluator: Evaluator, settings: UserSettings) -> String {
        switch node {
        case let numberNode as NumberNode:
            return formatScalar(numberNode.value, settings: settings)
            
        case let constantNode as ConstantNode:
            if constantNode.name == "pi" { return "\\pi" }
            return constantNode.name.replacingOccurrences(of: "π", with: "\\pi")
            
        case let unaryNode as UnaryOpNode:
            let childLaTeX = formatNode(unaryNode.child, evaluator: evaluator, settings: settings)
            if unaryNode.child is BinaryOpNode {
                return "\(unaryNode.op.rawValue)\\left( \(childLaTeX) \\right)"
            }
            return "\(unaryNode.op.rawValue)\(childLaTeX)"

        case let binaryNode as BinaryOpNode:
            
            if let indexedOpNode = binaryNode.right as? IndexedOperationNode {
                let targetLaTeX = formatNode(binaryNode.left, evaluator: evaluator, settings: settings)
                let indexLaTeX = formatNode(indexedOpNode.index, evaluator: evaluator, settings: settings)
                let scalarLaTeX = formatNode(indexedOpNode.scalar, evaluator: evaluator, settings: settings)
                let opString = binaryNode.op.rawValue.dropLast() // remove the @
                return "\(targetLaTeX)_{ \(indexLaTeX) } \(opString) \(scalarLaTeX)"
            }
            
            var leftLaTeX = formatNode(binaryNode.left, evaluator: evaluator, settings: settings)
            var rightLaTeX = formatNode(binaryNode.right, evaluator: evaluator, settings: settings)

            if let leftBinary = binaryNode.left as? BinaryOpNode {
                if operatorPrecedence(for: leftBinary.op.rawValue) < operatorPrecedence(for: binaryNode.op.rawValue) {
                    leftLaTeX = "\\left( \(leftLaTeX) \\right)"
                }
            }
            if let rightBinary = binaryNode.right as? BinaryOpNode {
                let currentPrecedence = operatorPrecedence(for: binaryNode.op.rawValue)
                let rightPrecedence = operatorPrecedence(for: rightBinary.op.rawValue)
                if rightPrecedence < currentPrecedence || (currentPrecedence == rightPrecedence && ["-", "/"].contains(binaryNode.op.rawValue)) {
                     rightLaTeX = "\\left( \(rightLaTeX) \\right)"
                }
            }
            
            switch binaryNode.op.rawValue {
            case "/", "÷":
                return "\\frac{\(leftLaTeX)}{\(rightLaTeX)}"
            case "*", "×":
                // Heuristic for rendering implicit multiplication without a dot.
                if binaryNode.op.rawValue == "*" {
                    let leftIsNumber = binaryNode.left is NumberNode
                    let leftIsConstant = binaryNode.left is ConstantNode
                    
                    let rightIsConstant = binaryNode.right is ConstantNode
                    let rightIsFuncCall = binaryNode.right is FunctionCallNode
                    let rightIsVectorOrMatrix = binaryNode.right is VectorNode || binaryNode.right is MatrixNode || binaryNode.right is ComplexVectorNode || binaryNode.right is ComplexMatrixNode
                    
                    // e.g., "2x", "ax", "2sin(x)", "2[1;2]"
                    if (leftIsNumber || leftIsConstant) && (rightIsConstant || rightIsFuncCall || rightIsVectorOrMatrix) {
                        return "\(leftLaTeX)\(rightLaTeX)"
                    }
                    
                    let rightIsNumber = binaryNode.right is NumberNode
                    // e.g., "x2"
                    if leftIsConstant && rightIsNumber {
                        return "\(leftLaTeX)\(rightLaTeX)"
                    }
                }
                return "\(leftLaTeX) \\cdot \(rightLaTeX)"
            case "^":
                return "{\(leftLaTeX)}^{\(rightLaTeX)}"
            case "±":
                return "\(leftLaTeX) \\pm \(rightLaTeX)"
            case "∠":
                return "\(leftLaTeX) \\angle \(rightLaTeX)"
            default:
                return "\(leftLaTeX) \(binaryNode.op.rawValue) \(rightLaTeX)"
            }

        case let functionCallNode as FunctionCallNode:
            if functionCallNode.name == "grad" && functionCallNode.arguments.count == 2 {
                let funcName = formatNode(functionCallNode.arguments[0], evaluator: evaluator, settings: settings)
                
                if let pointVectorNode = functionCallNode.arguments[1] as? VectorNode {
                    let pointElements = pointVectorNode.elements.map { formatNode($0, evaluator: evaluator, settings: settings) }.joined(separator: ", ")
                    return "\\nabla \\text{\(funcName)}(\(pointElements))"
                } else {
                    let point = formatNode(functionCallNode.arguments[1], evaluator: evaluator, settings: settings)
                    return "\\nabla \\text{\(funcName)}(\(point))"
                }
            }
            
            let args = functionCallNode.arguments.map { formatNode($0, evaluator: evaluator, settings: settings) }.joined(separator: ", ")
            let knownFuncs = ["sin", "cos", "tan", "log", "ln", "det", "exp", "min", "max", "lim"]
            if functionCallNode.name == "sqrt" {
                return "\\sqrt{\(args)}"
            } else if knownFuncs.contains(functionCallNode.name) {
                return "\\\(functionCallNode.name)(\(args))"
            }
            return "\\text{\(functionCallNode.name.replacingOccurrences(of: "_", with: "\\_"))}(\(args))"

        case let assignmentNode as AssignmentNode:
            return "\(assignmentNode.name) := \(formatNode(assignmentNode.expression, evaluator: evaluator, settings: settings))"
            
        case let funcDefNode as FunctionDefinitionNode:
            let params = funcDefNode.parameterNames.joined(separator: ", ")
            return "\(funcDefNode.name)(\(params)) := \(formatNode(funcDefNode.body, evaluator: evaluator, settings: settings))"

        case let vectorNode as VectorNode:
            let elements = vectorNode.elements.map { formatNode($0, evaluator: evaluator, settings: settings) }.joined(separator: " \\\\ ")
            return "\\begin{bmatrix} \(elements) \\end{bmatrix}"

        case let matrixNode as MatrixNode:
            let rows = matrixNode.rows.map { row in
                row.map { formatNode($0, evaluator: evaluator, settings: settings) }.joined(separator: " & ")
            }.joined(separator: " \\\\ ")
            return "\\begin{bmatrix} \(rows) \\end{bmatrix}"
        
        case let postfixNode as PostfixOpNode:
            let childLaTeX = formatNode(postfixNode.child, evaluator: evaluator, settings: settings)

            if postfixNode.op.rawValue == "'" {
                if let cVectorNode = postfixNode.child as? ComplexVectorNode {
                    let elements = cVectorNode.elements.map { formatNode($0, evaluator: evaluator, settings: settings) }.joined(separator: " \\\\ ")
                    return "\\left(\\begin{bmatrix} \(elements) \\end{bmatrix}\\right)^\\dagger"
                }
                return "{\(childLaTeX)}^T"
            }
            
            if postfixNode.op.rawValue == "!" {
                if postfixNode.child is BinaryOpNode || postfixNode.child is UnaryOpNode {
                    return "\\left( \(childLaTeX) \\right)!"
                }
                return "\(childLaTeX)!"
            }
            
            return "\(childLaTeX)\(postfixNode.op.rawValue)"

        case let cVectorNode as ComplexVectorNode:
            let elements = cVectorNode.elements.map { formatNode($0, evaluator: evaluator, settings: settings) }.joined(separator: " \\\\ ")
            return "\\begin{bmatrix} \(elements) \\end{bmatrix}"

        case let cMatrixNode as ComplexMatrixNode:
            let rows = cMatrixNode.rows.map { row in
                row.map { formatNode($0, evaluator: evaluator, settings: settings) }.joined(separator: " & ")
            }.joined(separator: " \\\\ ")
            return "\\begin{bmatrix} \(rows) \\end{bmatrix}"
        
        case let derivativeNode as DerivativeNode:
            if let variableNode = derivativeNode.variable {
                // Standard derivative(body, var, point) form
                let body = formatNode(derivativeNode.body, evaluator: evaluator, settings: settings)
                let variable = variableNode.name
                let point = formatNode(derivativeNode.point, evaluator: evaluator, settings: settings)
                let orderLaTeX = formatNode(derivativeNode.order, evaluator: evaluator, settings: settings)

                if let orderNode = derivativeNode.order as? NumberNode, orderNode.value == 1.0 {
                    return "\\frac{d}{d\(variable)}{\\left(\(body)\\right)}\\Big|_{\(variable)=\(point)}"
                } else {
                    return "\\frac{d^{\(orderLaTeX)}}{d\(variable)^{\(orderLaTeX)}}{\\left(\(body)\\right)}\\Big|_{\(variable)=\(point)}"
                }
            } else {
                // Shorthand derivative(f, point) form
                // Here, derivativeNode.body is expected to be a ConstantNode with the function name
                let functionName = (derivativeNode.body as? ConstantNode)?.name ?? "f"
                let point = formatNode(derivativeNode.point, evaluator: evaluator, settings: settings)
                
                // Note: The parser for this form hardcodes the order to 1, so we only need to handle f'
                return "\\text{\(functionName.replacingOccurrences(of: "_", with: "\\_"))}'(\(point))"
            }

        case let integralNode as IntegralNode:
            let body = formatNode(integralNode.body, evaluator: evaluator, settings: settings)
            let variable = integralNode.variable.name
            let lower = formatNode(integralNode.lowerBound, evaluator: evaluator, settings: settings)
            let upper = formatNode(integralNode.upperBound, evaluator: evaluator, settings: settings)
            return "\\int_{\(lower)}^{\(upper)} \(body) \\,d\(variable)"

        case let primeNode as PrimeDerivativeNode:
            let argument = formatNode(primeNode.argument, evaluator: evaluator, settings: settings)
            return "\\text{\(primeNode.functionName)}'(\(argument))"

        case let plotNode as PlotNode:
            let expressions = plotNode.expressions.map { formatNode($0, evaluator: evaluator, settings: settings) }.joined(separator: ", ")
            return "\\text{plot}(\(expressions))"
            
        case is ImportCSVNode:
             return "\\text{importcsv()}"

        case is TupleNode:
            return ""
            
        case is IndexedOperationNode: // Should be handled by the BinaryOpNode case
            return ""

        default:
            return node.description
        }
    }
    
    static func formatMathValue(_ value: MathValue, angleMode: AngleMode, settings: UserSettings) -> String {
        switch value {
        case .scalar(let doubleValue):
            return formatScalar(doubleValue, settings: settings)
        case .complex(let complexValue):
            let realPart = formatScalar(complexValue.real, settings: settings)
            let imagPart = formatScalar(abs(complexValue.imaginary), settings: settings)
            if complexValue.real != 0 && complexValue.imaginary != 0 {
                return "\(realPart) \(complexValue.imaginary < 0 ? "-" : "+") \(imagPart)i"
            } else if complexValue.real != 0 { return realPart }
            else if complexValue.imaginary != 0 { return "\(formatScalar(complexValue.imaginary, settings: settings))i" }
            else { return "0" }
        case .vector(let vector):
            let elements = vector.values.map { formatScalar($0, settings: settings) }.joined(separator: " \\\\ ")
            return "\\begin{bmatrix} \(elements) \\end{bmatrix}"
        case .matrix(let matrix):
            let rows = (0..<matrix.rows).map { r in
                (0..<matrix.columns).map { c in
                    formatScalar(matrix[r, c], settings: settings)
                }.joined(separator: " & ")
            }.joined(separator: " \\\\ ")
            return "\\begin{bmatrix} \(rows) \\end{bmatrix}"
        case .tuple(let values):
            return values.map { formatMathValue($0, angleMode: angleMode, settings: settings) }.joined(separator: " \\text{ or } ")
        case .polar(let complexValue):
            let magnitude = formatScalar(complexValue.abs(), settings: settings)
            let angle = complexValue.argument()
            let angleString = angleMode == .degrees ? "\(formatScalar(angle * (180.0 / .pi), settings: settings))^{\\circ}" : formatScalar(angle, settings: settings)
            return "\(magnitude) \\angle \(angleString)"
        case .functionDefinition(let name):
            return "\\text{Function defined: } \(name)"
        case .complexVector(let cVector):
            let elements = cVector.values.map { formatMathValue(.complex($0), angleMode: angleMode, settings: settings) }.joined(separator: " \\\\ ")
            return "\\begin{bmatrix} \(elements) \\end{bmatrix}"
        case .complexMatrix(let cMatrix):
            let rows = (0..<cMatrix.rows).map { r in
                (0..<cMatrix.columns).map { c in
                    formatMathValue(.complex(cMatrix[r, c]), angleMode: angleMode, settings: settings)
                }.joined(separator: " & ")
            }.joined(separator: " \\\\ ")
            return "\\begin{bmatrix} \(rows) \\end{bmatrix}"
        case .regressionResult(let slope, let intercept):
            let m = formatScalar(slope, settings: settings)
            let b = formatScalar(intercept, settings: settings)
            return "\\text{m = } \(m), \\text{ b = } \(b)"
        case .polynomialFit(let coeffs):
            return formatPolyFitForLaTeX(coeffs, settings: settings)
        case .plot(let plotData):
            let expression = plotData.expression.replacingOccurrences(of: "*", with: "\\cdot")
            return "\\text{Plot: \(expression)}"
        case .triggerCSVImport:
            return "\\text{...}"
        }
    }

    private static func operatorPrecedence(for op: String) -> Int {
        switch op {
        case "±": return 1
        case "+", "-": return 2
        case "*", "/", "×", "÷": return 3
        case ".=@", ".+@", ".-@", ".*@", "./@": return 4
        case "^": return 5
        default: return 0
        }
    }

    private static func formatScalar(_ value: Double, settings: UserSettings) -> String {
        let formattedString: String
        switch settings.displayMode {
        case .auto:
            if value.truncatingRemainder(dividingBy: 1) == 0 {
                formattedString = String(format: "%.0f", value)
            } else {
                let absValue = abs(value)
                if absValue > 0 && (absValue < 1e-4 || absValue >= 1e15) {
                    let sciString = String(format: "%.4g", value)
                    if sciString.contains("e") {
                        return formatScientificNotation(fromString: sciString, using: settings)
                    }
                    formattedString = sciString
                } else {
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
        case .scientific:
            let sciString = String(format: "%.*e", settings.fixedDecimalPlaces, value)
            return formatScientificNotation(fromString: sciString, using: settings)
        case .fixed:
            formattedString = String(format: "%.\(settings.fixedDecimalPlaces)f", value)
        }
        
        if settings.decimalSeparator == .comma {
            return formattedString.replacingOccurrences(of: ".", with: "{,}")
        }
        return formattedString
    }
    
    private static func formatPolyFitForLaTeX(_ coeffs: Vector, settings: UserSettings) -> String {
        var result = "y = "
        for (i, coeff) in coeffs.values.enumerated().reversed() {
            if abs(coeff) < 1e-9 && coeffs.dimension > 1 { continue }

            let isFirstTerm = (result == "y = ")
            let formattedCoeff = formatScalar(abs(coeff), settings: settings)
            
            // Sign
            if !isFirstTerm {
                result += (coeff < 0) ? " - " : " + "
            } else if coeff < 0 {
                result += "- "
            }
            
            // Coefficient
            if abs(abs(coeff) - 1.0) > 1e-9 || i == 0 {
                result += formattedCoeff
            }
            
            // Variable and power
            if i > 0 {
                result += "x"
                if i > 1 {
                    result += "^{\(i)}"
                }
            }
        }
        return result
    }

    private static func formatScientificNotation(fromString scientificString: String, using settings: UserSettings) -> String {
        let parts = scientificString.lowercased().split(separator: "e")
        guard parts.count == 2, let exponent = Int(parts[1]) else {
            return scientificString // Fallback
        }

        var mantissaString = String(parts[0])
        
        if settings.decimalSeparator == .comma {
            mantissaString = mantissaString.replacingOccurrences(of: ".", with: "{,}")
        }

        return "\(mantissaString) \\times 10^{\(exponent)}"
    }

    private static func fallbackFormatExpression(_ expression: String) -> String {
        var latex = expression
        latex = latex.replacingOccurrences(of: "*", with: " \\cdot ")
        latex = latex.replacingOccurrences(of: "pi", with: "\\pi")
        return latex
    }
}
