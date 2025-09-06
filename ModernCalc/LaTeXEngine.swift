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
        let resultLaTeX = formatMathValue(calculation.result, angleMode: angleMode, settings: settings)
        
        switch calculation.type {
        case .evaluation, .variableAssignment:
            return "\(expressionLaTeX) = \(resultLaTeX)"
        case .functionDefinition:
            return expressionLaTeX
        }
    }
    
    private static func formatExpression(_ expression: String, evaluator: Evaluator, settings: UserSettings) -> String {
        do {
            let lexer = Lexer(input: expression, decimalSeparator: settings.decimalSeparator)
            let tokens = lexer.tokenize()
            let parser = Parser(tokens: tokens)
            let node = try parser.parse()
            return formatNode(node, evaluator: evaluator, settings: settings)
        } catch {
            print("LaTeX generation failed to parse, falling back to string replacement. Error: \(error)")
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
                return "\(unaryNode.op.rawValue)(\\left. \(childLaTeX) \\right.)"
            }
            return "\(unaryNode.op.rawValue)\(childLaTeX)"

        case let binaryNode as BinaryOpNode:
            var leftLaTeX = formatNode(binaryNode.left, evaluator: evaluator, settings: settings)
            var rightLaTeX = formatNode(binaryNode.right, evaluator: evaluator, settings: settings)

            if let leftBinary = binaryNode.left as? BinaryOpNode {
                if operatorPrecedence(for: leftBinary.op.rawValue) < operatorPrecedence(for: binaryNode.op.rawValue) {
                    leftLaTeX = "(\\left. \(leftLaTeX) \\right.)"
                }
            }
            if let rightBinary = binaryNode.right as? BinaryOpNode {
                let currentPrecedence = operatorPrecedence(for: binaryNode.op.rawValue)
                let rightPrecedence = operatorPrecedence(for: rightBinary.op.rawValue)
                if rightPrecedence < currentPrecedence || (currentPrecedence == rightPrecedence && ["-", "/"].contains(binaryNode.op.rawValue)) {
                     rightLaTeX = "(\\left. \(rightLaTeX) \\right.)"
                }
            }
            
            switch binaryNode.op.rawValue {
            case "/", "÷":
                return "\\frac{\(leftLaTeX)}{\(rightLaTeX)}"
            case "*", "×":
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
            let args = functionCallNode.arguments.map { formatNode($0, evaluator: evaluator, settings: settings) }.joined(separator: ", ")
            let knownFuncs = ["sin", "cos", "tan", "log", "ln", "det"]
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
        
        // --- ADDED: LaTeX formatting for PostfixOpNode on ComplexVectorNode ---
        case let postfixNode as PostfixOpNode:
            if postfixNode.op.rawValue == "'", let cVectorNode = postfixNode.child as? ComplexVectorNode {
                // For a complex vector transpose, output a column vector with a dagger
                let elements = cVectorNode.elements.map { formatNode($0, evaluator: evaluator, settings: settings) }.joined(separator: " \\\\ ")
                return "\\left(\\begin{bmatrix} \(elements) \\end{bmatrix}\\right)^\\dagger"
            }
            let childLaTeX = formatNode(postfixNode.child, evaluator: evaluator, settings: settings)
            if postfixNode.op.rawValue == "'" {
                // For a real matrix transpose, use a simple T
                return "{\(childLaTeX)}^T"
            }
            return "\(childLaTeX)\(postfixNode.op.rawValue)"

        case let cVectorNode as ComplexVectorNode:
            let elements = cVectorNode.elements.map { formatNode($0, evaluator: evaluator, settings: settings) }.joined(separator: " \\\\ ")
            return "\\begin{bmatrix} \(elements) \\end{bmatrix}"

        case let cMatrixNode as ComplexMatrixNode:
            // This is the problematic part. We need to format the expression nodes, not a MathValue.
            let rows = cMatrixNode.rows.map { row in
                row.map { formatNode($0, evaluator: evaluator, settings: settings) }.joined(separator: " & ")
            }.joined(separator: " \\\\ ")
            return "\\begin{bmatrix} \(rows) \\end{bmatrix}"
        
        case is TupleNode:
            return ""

        default:
            return node.description
        }
    }
    
    // --- Value Formatting ---
    
    // --- CHANGE: Removed 'private' to make it accessible from the ViewModel ---
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
            let angleString = angleMode == .degrees ? "\(String(format: "%.2f", angle * (180.0 / .pi)))^{circ}" : formatScalar(angle, settings: settings)
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
        }
    }

    // --- Helper Methods ---

    private static func operatorPrecedence(for op: String) -> Int {
        switch op {
        case "±": return 1
        case "+", "-": return 2
        case "*", "/", "×", "÷": return 3
        case "^": return 4
        default: return 0
        }
    }

    private static func formatScalar(_ value: Double, settings: UserSettings) -> String {
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
                        formattedString = regex.stringByReplacingMatches(in: tempFormatted, options: [], range: NSRange(location: 0, length: tempFormatted.utf16.count), withTemplate: "")
                    } else { formattedString = tempFormatted }
                }
            }
        case .scientific:
            formattedString = String(format: "%.*e", settings.fixedDecimalPlaces, value)
        case .fixed:
            formattedString = String(format: "%.\(settings.fixedDecimalPlaces)f", value)
        }
        
        // Use {,} for LaTeX commas to avoid ambiguity.
        if settings.decimalSeparator == .comma {
            return formattedString.replacingOccurrences(of: ".", with: "{,}")
        }
        return formattedString
    }

    private static func fallbackFormatExpression(_ expression: String) -> String {
        var latex = expression
        latex = latex.replacingOccurrences(of: "*", with: " \\cdot ")
        latex = latex.replacingOccurrences(of: "pi", with: "\\pi")
        return latex
    }
}
