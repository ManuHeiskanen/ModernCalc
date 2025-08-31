//
//  LaTeXEngine.swift
//  ModernCalc
//
//  Created by Manu Heiskanen on 31.8.2025.
//

import Foundation

struct LaTeXEngine {
    
    /// The main entry point for formatting a calculation into a LaTeX string.
    static func format(calculation: Calculation, evaluator: Evaluator, angleMode: AngleMode) -> String {
        let expressionLaTeX = formatExpressionForLaTeX(calculation.expression, evaluator: evaluator)
        let resultLaTeX = formatMathValueForLaTeX(calculation.result, angleMode: angleMode)
        
        switch calculation.type {
        case .evaluation, .variableAssignment:
            return "\(expressionLaTeX) = \(resultLaTeX)"
        case .functionDefinition:
            return expressionLaTeX
        }
    }
    
    // MARK: - Private Expression Formatting
    
    private static func formatExpressionForLaTeX(_ expression: String, evaluator: Evaluator) -> String {
        do {
            let lexer = Lexer(input: expression)
            let tokens = lexer.tokenize()
            let parser = Parser(tokens: tokens)
            let node = try parser.parse()
            return formatNodeAsLaTeX(node, evaluator: evaluator)
        } catch {
            print("LaTeX generation failed to parse, falling back to string replacement. Error: \(error)")
            // Fallback for expressions that fail to parse, though this should be rare.
            return fallbackFormatExpressionForLaTeX(expression)
        }
    }
    
    private static func formatNodeAsLaTeX(_ node: ExpressionNode, evaluator: Evaluator) -> String {
        switch node {
        case let numberNode as NumberNode:
            return formatScalarForDisplay(numberNode.value)
            
        case let constantNode as ConstantNode:
            if constantNode.name == "pi" { return "\\pi" }
            return constantNode.name.replacingOccurrences(of: "π", with: "\\pi")
            
        case let unaryNode as UnaryOpNode:
            let childLaTeX = formatNodeAsLaTeX(unaryNode.child, evaluator: evaluator)
            // Add parentheses if the child is a binary operation to avoid ambiguity (e.g., -(2+3))
            if unaryNode.child is BinaryOpNode {
                return "\(unaryNode.op.rawValue)(\(childLaTeX))"
            }
            return "\(unaryNode.op.rawValue)\(childLaTeX)"

        case let binaryNode as BinaryOpNode:
            var leftLaTeX = formatNodeAsLaTeX(binaryNode.left, evaluator: evaluator)
            var rightLaTeX = formatNodeAsLaTeX(binaryNode.right, evaluator: evaluator)

            // Add parentheses based on operator precedence
            if let leftBinary = binaryNode.left as? BinaryOpNode {
                if latexOperatorPrecedence(for: leftBinary.op.rawValue) < latexOperatorPrecedence(for: binaryNode.op.rawValue) {
                    leftLaTeX = "(\(leftLaTeX))"
                }
            }
            if let rightBinary = binaryNode.right as? BinaryOpNode {
                let currentPrecedence = latexOperatorPrecedence(for: binaryNode.op.rawValue)
                let rightPrecedence = latexOperatorPrecedence(for: rightBinary.op.rawValue)
                if rightPrecedence < currentPrecedence {
                     rightLaTeX = "(\(rightLaTeX))"
                } else if currentPrecedence == rightPrecedence && (binaryNode.op.rawValue == "-" || binaryNode.op.rawValue == "/") {
                     rightLaTeX = "(\(rightLaTeX))"
                }
            }
            
            switch binaryNode.op.rawValue {
            case "/", "÷":
                return "\\frac{\(leftLaTeX)}{\(rightLaTeX)}"
            case "*", "×":
                 return "\(leftLaTeX) \\cdot \(rightLaTeX)"
            case "^":
                // Use braces for powers with complex bases
                if binaryNode.left is BinaryOpNode || (binaryNode.left as? NumberNode)?.value ?? 0 < 0 {
                    return "{\(leftLaTeX)}^{\(rightLaTeX)}"
                }
                return "\(leftLaTeX)^{\(rightLaTeX)}"
            case "±":
                return "\(leftLaTeX) \\pm \(rightLaTeX)"
            case "∠":
                return "\(leftLaTeX) \\angle \(rightLaTeX)"
            default:
                return "\(leftLaTeX) \(binaryNode.op.rawValue) \(rightLaTeX)"
            }

        case let functionCallNode as FunctionCallNode:
            let args = functionCallNode.arguments.map { formatNodeAsLaTeX($0, evaluator: evaluator) }.joined(separator: ", ")
            let knownFuncs = ["sin", "cos", "tan", "log", "ln", "det"]
            if functionCallNode.name == "sqrt" {
                return "\\sqrt{\(args)}"
            } else if knownFuncs.contains(functionCallNode.name) {
                return "\\\(functionCallNode.name)(\(args))"
            }
            // Escape underscores for function names like area_rect
            return "\(functionCallNode.name.replacingOccurrences(of: "_", with: "\\_"))(\(args))"

        case let assignmentNode as AssignmentNode:
            return "\(assignmentNode.name) := \(formatNodeAsLaTeX(assignmentNode.expression, evaluator: evaluator))"
            
        case let funcDefNode as FunctionDefinitionNode:
            let params = funcDefNode.parameterNames.joined(separator: ", ")
            return "\(funcDefNode.name)(\(params)) := \(formatNodeAsLaTeX(funcDefNode.body, evaluator: evaluator))"

        // For matrix/vector definitions, evaluate them to get a MathValue, then format that
        case is VectorNode, is MatrixNode:
            do {
                var emptyVars = [String: MathValue]()
                var emptyFuncs = [String: FunctionDefinitionNode]()
                let (value, _) = try evaluator.evaluate(node: node, variables: &emptyVars, functions: &emptyFuncs, angleMode: .radians)
                return formatMathValueForLaTeX(value, angleMode: .radians) // Angle mode doesn't matter here
            } catch {
                return node.description
            }
        
        default:
            return node.description
        }
    }
    
    // MARK: - Private Value Formatting
    
    private static func formatMathValueForLaTeX(_ value: MathValue, angleMode: AngleMode) -> String {
        switch value {
        case .scalar(let doubleValue):
            return formatScalarForDisplay(doubleValue)
        case .complex(let complexValue):
            return formatComplexForDisplay(complexValue)
        case .vector(let vector):
            let elements = vector.values.map { formatScalarForDisplay($0) }.joined(separator: " \\\\ ")
            return "\\begin{pmatrix} \(elements) \\end{pmatrix}"
        case .matrix(let matrix):
            let rows = (0..<matrix.rows).map { r in
                (0..<matrix.columns).map { c in
                    formatScalarForDisplay(matrix[r, c])
                }.joined(separator: " & ")
            }.joined(separator: " \\\\ ")
            return "\\begin{pmatrix} \(rows) \\end{pmatrix}"
        case .tuple(let values):
            return values.map { formatMathValueForLaTeX($0, angleMode: angleMode) }.joined(separator: " \\text{ or } ")
        case .polar(let complexValue):
            return formatPolarForDisplay(complexValue, angleMode: angleMode)
        case .functionDefinition(let name):
            return "\\text{Function defined: } \(name)"
        case .complexVector(let cVector):
            let elements = cVector.values.map { formatComplexForDisplay($0) }.joined(separator: " \\\\ ")
            return "\\begin{pmatrix} \(elements) \\end{pmatrix}"
        case .complexMatrix(let cMatrix):
            let rows = (0..<cMatrix.rows).map { r in
                (0..<cMatrix.columns).map { c in
                    formatComplexForDisplay(cMatrix[r, c])
                }.joined(separator: " & ")
            }.joined(separator: " \\\\ ")
            return "\\begin{pmatrix} \(rows) \\end{pmatrix}"
        }
    }
    
    // MARK: - Private Helpers (from ViewModel)
    
    private static func formatScalarForDisplay(_ value: Double) -> String {
        if value.truncatingRemainder(dividingBy: 1) == 0 {
            return String(format: "%.0f", value)
        }
        
        let absValue = abs(value)
        if absValue > 0 && (absValue < 1e-4 || absValue >= 1e15) {
            return String(format: "%.4g", value)
        } else {
            let formatted = String(format: "%.4f", value)
            if let regex = try? NSRegularExpression(pattern: "\\.?0+$", options: .caseInsensitive) {
                return regex.stringByReplacingMatches(in: formatted, options: [], range: NSRange(location: 0, length: formatted.utf16.count), withTemplate: "")
            }
            return formatted
        }
    }
    
    private static func formatComplexForDisplay(_ value: Complex) -> String {
        if value.real != 0 && value.imaginary != 0 {
            return "\(formatScalarForDisplay(value.real)) \(value.imaginary < 0 ? "-" : "+") \(formatScalarForDisplay(abs(value.imaginary)))i"
        } else if value.real != 0 {
            return formatScalarForDisplay(value.real)
        } else if value.imaginary != 0 {
            return "\(formatScalarForDisplay(value.imaginary))i"
        } else {
            return "0"
        }
    }
    
    private static func formatPolarForDisplay(_ value: Complex, angleMode: AngleMode) -> String {
        let magnitude = formatScalarForDisplay(value.abs())
        let angle = value.argument()
        let angleString: String
        if angleMode == .degrees {
            let angleDegrees = angle * (180.0 / .pi)
            angleString = "\(String(format: "%.2f", angleDegrees))^\\circ"
        } else {
            angleString = formatScalarForDisplay(angle)
        }
        return "\(magnitude) \\angle \(angleString)"
    }
    
    private static func latexOperatorPrecedence(for op: String) -> Int {
        switch op {
        case "±": return 1
        case "+", "-": return 2
        case "*", "/", "×", "÷": return 3
        case "^": return 4
        default: return 0
        }
    }
    
    // MARK: - Fallback
    
    private static func fallbackFormatExpressionForLaTeX(_ expression: String) -> String {
        var latex = expression
        if let regex = try? NSRegularExpression(pattern: "(?<=[0-9\\)])(?=[a-zA-Z\\(])") {
            latex = regex.stringByReplacingMatches(in: latex, options: [], range: NSRange(location: 0, length: latex.utf16.count), withTemplate: " \\cdot ")
        }
        if let regex = try? NSRegularExpression(pattern: "\\b(sqrt|sin|cos|tan|log|ln)\\((.*?)\\)") {
            latex = regex.stringByReplacingMatches(in: latex, options: [], range: NSRange(location: 0, length: latex.utf16.count), withTemplate: "\\\\$1{$2}")
        }
        latex = latex.replacingOccurrences(of: "*", with: " \\cdot ")
        latex = latex.replacingOccurrences(of: "pi", with: "\\pi")
        return latex
    }
}
