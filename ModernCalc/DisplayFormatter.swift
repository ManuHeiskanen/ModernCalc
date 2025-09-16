import Foundation

/// A utility struct for formatting mathematical expressions and values for display.
struct DisplayFormatter {

    // MARK: - Operator Precedence

    private static let infixPrecedence: [String: Int] = [
        "||": 1, "&&": 2,
        "==": 3, "!=": 3, ">": 3, "<": 3, ">=": 3, "<=": 3,
        "in": 4, "±": 4,
        "+": 5, "-": 5,
        "*": 6, "/": 6, "∠": 6, ".*": 6, "./": 6, ":": 6, // Treat ':' like division
        ".=@": 7, ".+@": 7, ".-@": 7, ".*@": 7, "./@": 7,
        "^": 8
    ]

    private static let unaryPrecedence = 9

    // MARK: - Public Formatting Functions
    
    /// Formats a double into a string with a limited number of significant figures.
    static func formatScalar(_ value: Double) -> String {
        // Use up to 4 significant digits for a cleaner look in the equation.
        let tempFormatted = String(format: "%.4g", value)
        
        // This regex helps remove trailing zeros (e.g., "2.500" -> "2.5").
        if let regex = try? NSRegularExpression(pattern: "\\.?0+$") {
            let nsString = tempFormatted as NSString
            let range = NSRange(location: 0, length: nsString.length)
            let modString = regex.stringByReplacingMatches(in: tempFormatted, options: [], range: range, withTemplate: "")
            if modString.hasSuffix(".") {
                return String(modString.dropLast())
            }
            return modString.isEmpty ? "0" : modString
        }
        return tempFormatted
    }

    /// Formats an abstract syntax tree node into a human-readable string, suitable for legends.
    /// This method aims to reduce unnecessary parentheses and simplify unit notation.
    static func formatNodeForLegend(node: ExpressionNode) -> String {
        return _formatNode(node, parentPrecedence: 0)
    }

    /// Formats the coefficients of a polynomial into a readable equation string.
    static func formatPolynomialEquation(coeffs: Vector) -> String {
        var parts: [String] = []
        for i in (0..<coeffs.dimension).reversed() {
            let coeff = coeffs[i]
            if abs(coeff) < 1e-9 { continue }

            let sign = (coeff < 0 && !parts.isEmpty) ? " - " : (parts.isEmpty ? "" : " + ")
            let absCoeff = abs(coeff)
            
            var coeffStr = ""
            if abs(absCoeff - 1.0) > 1e-9 || i == 0 {
                coeffStr = formatScalar(absCoeff)
            }

            var term = ""
            if i > 0 {
                term = (i == 1) ? "x" : "x^\(i)"
            }
            
            if !coeffStr.isEmpty && !term.isEmpty {
                parts.append("\(sign)\(coeffStr)·\(term)")
            } else if !term.isEmpty {
                 parts.append("\(sign)\(term)")
            } else {
                 parts.append("\(sign)\(coeffStr)")
            }
        }
        if parts.isEmpty { return "y = 0" }
        // Handle the leading sign correctly
        let firstPart = parts.first!
        if firstPart.hasPrefix(" + ") {
            parts[0] = String(firstPart.dropFirst(3))
        } else if firstPart.hasPrefix(" - ") {
            parts[0] = "-" + String(firstPart.dropFirst(3))
        }
        
        return "y = \(parts.joined())"
    }

    // MARK: - Private Recursive Formatter

    private static func _formatNode(_ node: ExpressionNode, parentPrecedence: Int) -> String {
        switch node {
        case let numberNode as NumberNode:
            return formatScalar(numberNode.value)

        case let unitNode as UnitAndExponentNode:
            if let exp = unitNode.exponent {
                // Ensure exponent doesn't get unnecessary parentheses if it's just a number
                let expStr = (exp is NumberNode) ? _formatNode(exp, parentPrecedence: 100) : "(\(_formatNode(exp, parentPrecedence: 0)))"
                return "\(unitNode.unitSymbol)^\(expStr)"
            }
            return unitNode.unitSymbol
            
        case let constantNode as ConstantNode:
            return constantNode.name
            
        case let funcCall as FunctionCallNode where funcCall.name == "if" && funcCall.arguments.count == 3:
             let cond = _formatNode(funcCall.arguments[0], parentPrecedence: 0)
             let trueVal = _formatNode(funcCall.arguments[1], parentPrecedence: 0)
             let falseVal = _formatNode(funcCall.arguments[2], parentPrecedence: 0)
             
             let maxLegendPartLength = 25
             let abbreviatedTrueVal = trueVal.count > maxLegendPartLength ? String(trueVal.prefix(maxLegendPartLength - 3)) + "..." : trueVal
             let abbreviatedFalseVal = falseVal.count > maxLegendPartLength ? String(falseVal.prefix(maxLegendPartLength - 3)) + "..." : falseVal

             return "if(\(cond), \(abbreviatedTrueVal), \(abbreviatedFalseVal))"

        case let funcCall as FunctionCallNode:
            let args = funcCall.arguments.map { _formatNode($0, parentPrecedence: 0) }.joined(separator: ", ")
            return "\(funcCall.name)(\(args))"

        case let unaryNode as UnaryOpNode:
            let childStr = _formatNode(unaryNode.child, parentPrecedence: unaryPrecedence)
            let result = "\(unaryNode.op.rawValue)\(childStr)"
            return result
            
        case let binaryNode as BinaryOpNode:
            // Special case for implicit multiplication with a unit, format as "value unit"
            if binaryNode.op.rawValue == "*" && (binaryNode.right is UnitAndExponentNode) {
                 let leftStr = _formatNode(binaryNode.left, parentPrecedence: infixPrecedence["*"]!)
                 let rightStr = _formatNode(binaryNode.right, parentPrecedence: 100)
                 return "\(leftStr) \(rightStr)"
            }
            
            let op = binaryNode.op.rawValue
            // Replace ':' with '/' for display
            let displayOp = op == ":" ? "/" : op
            
            let myPrecedence = infixPrecedence[op] ?? 0
            
            // Adjust precedence for left-associativity. For right-associative ops like '^', the right child can have same precedence.
            let rightPrecedence = myPrecedence + (op == "^" ? 0 : 1)
            let leftStr = _formatNode(binaryNode.left, parentPrecedence: myPrecedence)
            let rightStr = _formatNode(binaryNode.right, parentPrecedence: rightPrecedence)
            
            var result = "\(leftStr) \(displayOp) \(rightStr)"
            
            if myPrecedence < parentPrecedence {
                result = "(\(result))"
            }
            return result
            
        default:
            // Fallback for any other node type not explicitly handled, with basic cleanup
            return node.description
                .replacingOccurrences(of: ".", with: "")
                .replacingOccurrences(of: " : ", with: " / ")
        }
    }
}

