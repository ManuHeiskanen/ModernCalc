import Foundation

// --- ABSTRACT SYNTAX TREE NODES ---
protocol ExpressionNode {
    var description: String { get }
}
struct FunctionDefinitionNode: ExpressionNode {
    let name: String, parameterNames: [String], body: ExpressionNode
    var description: String { "\(name)(\(parameterNames.joined(separator: ", "))) := \(body.description)" }
}
struct TupleNode: ExpressionNode {
    let elements: [ExpressionNode]
    var description: String { "(\(elements.map { $0.description }.joined(separator: " OR ")))" }
}
struct AssignmentNode: ExpressionNode {
    let name: String, expression: ExpressionNode
    var description: String { "\(name) := \(expression.description)" }
}
struct NumberNode: ExpressionNode {
    let value: Double
    var description: String { "\(value)" }
}
// NEW: An AST node representing a unit with an optional exponent, like .m or .m^2.
// This node does NOT contain the numeric value, only the unit information.
struct UnitAndExponentNode: ExpressionNode {
    let unitSymbol: String
    let exponent: ExpressionNode?
    var description: String {
        if let exp = exponent {
            return ".\(unitSymbol)^\(exp.description)"
        }
        return ".\(unitSymbol)"
    }
}
struct StringNode: ExpressionNode {
    let value: String
    var description: String { "\"\(value)\"" }
}
struct ConstantNode: ExpressionNode {
    let name: String
    var description: String { name }
}
struct FunctionCallNode: ExpressionNode {
    let name: String, arguments: [ExpressionNode]
    var description: String { "\(name)(\(arguments.map { $0.description }.joined(separator: ", ")))" }
}
struct UncertaintyNode: ExpressionNode {
    let value: ExpressionNode
    let namedArgs: [String: ExpressionNode]
    var description: String { "uncert(\(value.description), \(namedArgs.map { "\($0.key): \($0.value.description)" }.joined(separator: ", ")))" }
}
struct VectorNode: ExpressionNode {
    let elements: [ExpressionNode]
    var description: String { "vector(\(elements.map { $0.description }.joined(separator: "; ")))" }
}
struct MatrixNode: ExpressionNode {
    let rows: [[ExpressionNode]]
    var description: String { "matrix(\(rows.map { "[" + $0.map({ $0.description }).joined(separator: ", ") + "]" }.joined(separator: "; ")))" }
}
struct ComplexVectorNode: ExpressionNode {
    let elements: [ExpressionNode]
    var description: String { "cvector(\(elements.map { $0.description }.joined(separator: "; ")))" }
}
struct ComplexMatrixNode: ExpressionNode {
    let rows: [[ExpressionNode]]
    var description: String { "cmatrix(\(rows.map { "[" + $0.map({ $0.description }).joined(separator: ", ") + "]" }.joined(separator: "; ")))" }
}
struct BinaryOpNode: ExpressionNode {
    let op: Token, left: ExpressionNode, right: ExpressionNode
    var description: String { "(\(left.description) \(op.rawValue) \(right.description))" }
}
struct UnaryOpNode: ExpressionNode {
    let op: Token, child: ExpressionNode
    var description: String { "(\(op.rawValue)\(child.description))" }
}
struct PostfixOpNode: ExpressionNode {
    let op: Token, child: ExpressionNode
    var description: String { "(\(child.description)\(op.rawValue))" }
}
struct DerivativeNode: ExpressionNode {
    let body: ExpressionNode, variable: ConstantNode?, point: ExpressionNode, order: ExpressionNode
    var description: String { "derivative(\(body.description), at: \(variable?.description ?? "auto")=\(point.description), order: \(order.description))" }
}
struct IntegralNode: ExpressionNode {
    let body: ExpressionNode, variable: ConstantNode, lowerBound: ExpressionNode, upperBound: ExpressionNode
    var description: String { "integral(\(body.description) d\(variable.description) from \(lowerBound.description) to \(upperBound.description))" }
}
struct PrimeDerivativeNode: ExpressionNode {
    let functionName: String, argument: ExpressionNode
    var description: String { "\(functionName)'(\(argument.description))" }
}
struct IndexedOperationNode: ExpressionNode {
    let index: ExpressionNode
    let scalar: ExpressionNode
    var description: String { "@(\(index.description), \(scalar.description))" }
}
struct AutoplotNode: ExpressionNode {
    let expressions: [ExpressionNode]
    var description: String { "autoplot(\(expressions.map { $0.description }.joined(separator: ", ")))" }
}

struct PlotNode: ExpressionNode {
    let expressions: [ExpressionNode]
    let variable: ConstantNode
    let xRange: (ExpressionNode, ExpressionNode)
    let yRange: (ExpressionNode, ExpressionNode)?
    var description: String { "plot(\(expressions.map { $0.description }.joined(separator: ", ")), for: \(variable.description) in [\(xRange.0.description), \(xRange.1.description)])" }
}

struct ScatterplotNode: ExpressionNode {
    let arguments: [ExpressionNode]
    var description: String { "scatterplot(\(arguments.map { $0.description }.joined(separator: ", ")))" }
}

struct ImportCSVNode: ExpressionNode {
    var description: String { "importcsv()" }
}

enum ParserError: Error, CustomStringConvertible {
    case unexpectedToken(token: Token?, expected: String)
    case unexpectedEndOfInput(expected: String)
    case matrixRowMismatch(expected: Int, found: Int)
    case invalidParameterSyntax
    case invalidAssignmentTarget
    case incorrectArgumentCount(function: String, expected: String, found: Int)
    case invalidNamedArgument(function: String, argument: String)
    case duplicateNamedArgument(function: String, argument: String)

    var description: String {
        switch self {
        case .unexpectedToken(let token, let expected):
            let tokenStr = token?.rawValue ?? "end of input"
            return "Error: Unexpected token '\(tokenStr)'. Expected \(expected)."
        case .unexpectedEndOfInput(let expected):
            return "Error: Unexpected end of input. Expected \(expected)."
        case .matrixRowMismatch(let expected, let found):
            return "Error: Matrix rows must have same number of columns. Expected \(expected), found \(found)."
        case .invalidParameterSyntax:
            return "Error: Function parameters must be simple identifiers."
        case .invalidAssignmentTarget:
            return "Error: Invalid target for assignment."
        case .incorrectArgumentCount(let function, let expected, let found):
            return "Error: Function '\(function)' expects \(expected) arguments, got \(found)."
        case .invalidNamedArgument(let function, let argument):
            return "Error: Invalid named argument '\(argument)' for function '\(function)'."
        case .duplicateNamedArgument(let function, let argument):
            return "Error: Duplicate named argument '\(argument)' for function '\(function)'."
        }
    }
}

class Parser {
    private let tokens: [Token]
    private var currentIndex = 0
    private var lastTokenType: TokenType?

    init(tokens: [Token]) {
        self.tokens = tokens
    }

    func parse() throws -> ExpressionNode {
        guard !isAtEnd() else { return NumberNode(value: 0) }
        let expression = try parseAssignment()
        guard isAtEnd() else { throw ParserError.unexpectedToken(token: peek(), expected: "end of expression") }
        return expression
    }
    
    private func parseAssignment() throws -> ExpressionNode {
        let left = try parseExpression()
        if let nextToken = peek(), case .assignment = nextToken.type {
            try advance()
            if let identifier = left as? ConstantNode {
                let value = try parseExpression()
                return AssignmentNode(name: identifier.name, expression: value)
            } else if let funcCall = left as? FunctionCallNode {
                let paramNames = try funcCall.arguments.map { arg -> String in
                    guard let param = arg as? ConstantNode else { throw ParserError.invalidParameterSyntax }
                    return param.name
                }
                let body = try parseExpression()
                return FunctionDefinitionNode(name: funcCall.name, parameterNames: paramNames, body: body)
            }
            throw ParserError.invalidAssignmentTarget
        }
        return left
    }

    private func parseExpression(currentPrecedence: Int = 0) throws -> ExpressionNode {
        var left = try parsePrimary()
        
        while !isAtEnd() {
            var operatorPrecedence: Int?
            var opToken: Token?
            var isImplicit = false

            if let currentToken = peek(), let precedence = infixOperatorPrecedence(for: currentToken) {
                operatorPrecedence = precedence
                opToken = currentToken
            }
            else if let nextToken = peek(), shouldImplicitlyMultiply(after: lastTokenType, next: nextToken.type) {
                operatorPrecedence = implicitMultiplicationPrecedence()
                opToken = Token(type: .op("*"), rawValue: "*")
                isImplicit = true
            }

            if let precedence = operatorPrecedence, let op = opToken, precedence > currentPrecedence {
                if !isImplicit {
                    try advance()
                }
                
                if op.rawValue.hasSuffix("@") {
                     try consume(.paren("("), orThrow: .unexpectedToken(token: peek(), expected: "'(' for indexed operation"))
                     let index = try parseExpression()
                     try consumeArgumentSeparator(orThrow: .unexpectedToken(token: peek(), expected: "argument separator"))
                     let scalar = try parseExpression()
                     try consume(.paren(")"), orThrow: .unexpectedToken(token: peek(), expected: "')'"))
                     left = BinaryOpNode(op: op, left: left, right: IndexedOperationNode(index: index, scalar: scalar))
                } else {
                    let nextPrecedence = (op.rawValue == "^") ? precedence - 1 : precedence
                    let right = try parseExpression(currentPrecedence: nextPrecedence)
                    left = BinaryOpNode(op: op, left: left, right: right)
                }
            } else {
                break
            }
        }
        return left
    }

    private func parsePrimary() throws -> ExpressionNode {
        var node = try parsePrefix()
        
        // FIX: The logic for unit attachment has been completely reworked.
        // Instead of binding the number and unit together here, we now treat the unit
        // as a separate entity that is implicitly multiplied by the number. This
        // correctly handles operator precedence for exponents.
        if let token = peek(), case .unit(let unitSymbol) = token.type {
            try advance()

            var exponentNode: ExpressionNode? = nil
            // Check for an exponent immediately following the unit symbol.
            if let opToken = peek(), opToken.type == .op("^") {
                try advance() // consume '^'
                // The exponent has a higher precedence than multiplication, so we parse it first.
                exponentNode = try parseExpression(currentPrecedence: 5)
            }

            // Create a node for just the unit part (e.g., .m^2).
            let unitPart = UnitAndExponentNode(unitSymbol: unitSymbol, exponent: exponentNode)
            
            // Create a multiplication node to combine the number and the unit part.
            node = BinaryOpNode(
                op: Token(type: .op("*"), rawValue: "*"),
                left: node,
                right: unitPart
            )
        }
        
        return try parsePostfix(left: node)
    }

    private func parsePostfix(left: ExpressionNode) throws -> ExpressionNode {
        var result = left
        while let token = peek(), case .op(let opString) = token.type, ["'", "!"].contains(opString) {
            try advance()
            result = PostfixOpNode(op: token, child: result)
        }
        return result
    }

    private func parsePrefix() throws -> ExpressionNode {
        // FIX: If the next token is a unit, implicitly prepend a "1" to allow expressions like "/ .s^2".
        if let token = peek(), case .unit = token.type {
            return NumberNode(value: 1.0)
        }
        
        let token = try advance()
        switch token.type {
        case .number(let value): return NumberNode(value: value)
        case .string(let value): return StringNode(value: value)
        case .unitVector(let char): return ConstantNode(name: "\(char)'")
        case .identifier(let name):
            if let nextToken = peek(), case .paren("(") = nextToken.type {
                switch name {
                case "matrix": return try parseMatrix()
                case "vector": return try parseVector()
                case "cmatrix": return try parseComplexMatrix()
                case "cvector": return try parseComplexVector()
                case "derivative": return try parseDerivative()
                case "integral": return try parseIntegral()
                case "autoplot": return try parseAutoplot()
                case "plot": return try parsePlot()
                case "scatterplot": return try parseScatterplot()
                case "importcsv": return try parseImportCSV()
                case "uncert": return try parseUncert()
                default: return try parseFunctionCall(name: name)
                }
            } else if let primeToken = peek(), case .op("'") = primeToken.type, let parenToken = peek(offset: 1), case .paren("(") = parenToken.type {
                try advance()
                try advance()
                
                let argument = try parseExpression()
                try consume(.paren(")"), orThrow: .unexpectedToken(token: peek(), expected: "closing ')' for derivative call"))
                return PrimeDerivativeNode(functionName: name, argument: argument)
            } else {
                return ConstantNode(name: name)
            }
        case .op(let opString) where opString == "-" || opString == "+":
            let child = try parseExpression(currentPrecedence: unaryOperatorPrecedence())
            return UnaryOpNode(op: token, child: child)
        case .paren("("):
            let expression = try parseExpression()
            try consume(.paren(")"), orThrow: .unexpectedToken(token: peek(), expected: "closing ')'"))
            return expression
        default: throw ParserError.unexpectedToken(token: token, expected: "a number, identifier, or unary operator")
        }
    }
    
    private func parseFunctionCall(name: String) throws -> ExpressionNode {
        try consume(.paren("("), orThrow: .unexpectedToken(token: peek(), expected: "'(' for function call"))
        var arguments: [ExpressionNode] = []
        if let nextToken = peek(), case .paren(")") = nextToken.type {
            try advance()
            return FunctionCallNode(name: name, arguments: arguments)
        }
        
        repeat {
            arguments.append(try parseExpression())
            if let nextToken = peek(), case .separator(let char) = nextToken.type, char != ";" {
                try advance()
            } else { break }
        } while true
        
        try consume(.paren(")"), orThrow: .unexpectedToken(token: peek(), expected: "argument separator or ')' for function call"))
        return FunctionCallNode(name: name, arguments: arguments)
    }
    
    private func parseUncert() throws -> ExpressionNode {
        try consume(.paren("("), orThrow: .unexpectedToken(token: peek(), expected: "'(' for uncert call"))

        let valueNode = try parseExpression()
        
        var namedArgs: [String: ExpressionNode] = [:]
        
        while let separator = peek(), case .separator(let char) = separator.type, char == "," {
            try advance()
            
            if let closing = peek(), case .paren(")") = closing.type {
                break
            }
            
            let nameToken = try advance()
            guard case .identifier(let name) = nameToken.type else {
                throw ParserError.unexpectedToken(token: nameToken, expected: "argument name (e.g., 'random')")
            }
            
            let colonToken = try advance()
            guard case .assignment = colonToken.type else {
                throw ParserError.unexpectedToken(token: colonToken, expected: "':' after argument name")
            }
            
            let exprNode = try parseExpression()
            
            if namedArgs[name] != nil {
                throw ParserError.duplicateNamedArgument(function: "uncert", argument: name)
            }
            namedArgs[name] = exprNode
        }
        
        try consume(.paren(")"), orThrow: .unexpectedToken(token: peek(), expected: "')' or ',' for uncert call"))
        
        return UncertaintyNode(value: valueNode, namedArgs: namedArgs)
    }
    
    private func parseAutoplot() throws -> ExpressionNode {
        try consume(.paren("("), orThrow: .unexpectedToken(token: peek(), expected: "'(' for autoplot call"))
        var expressions: [ExpressionNode] = []
        if let nextToken = peek(), case .paren(")") = nextToken.type {
            try advance()
            throw ParserError.incorrectArgumentCount(function: "autoplot", expected: "at least 1", found: 0)
        }
        
        repeat {
            expressions.append(try parseExpression())
            if let nextToken = peek(), case .separator(let char) = nextToken.type, char != ";" {
                try advance()
            } else { break }
        } while true
        
        try consume(.paren(")"), orThrow: .unexpectedToken(token: peek(), expected: "argument separator or ')' for autoplot call"))
        return AutoplotNode(expressions: expressions)
    }

    private func parsePlot() throws -> ExpressionNode {
        try consume(.paren("("), orThrow: .unexpectedToken(token: peek(), expected: "'(' for plot call"))
        var allArguments: [ExpressionNode] = []
        
        if let nextToken = peek(), case .paren(")") = nextToken.type {
            try advance()
            throw ParserError.incorrectArgumentCount(function: "plot", expected: "at least 3", found: 0)
        }
        
        repeat {
            allArguments.append(try parseExpression())
            if let nextToken = peek(), case .separator(let char) = nextToken.type, char != ";" {
                try advance()
            } else { break }
        } while true
        
        try consume(.paren(")"), orThrow: .unexpectedToken(token: peek(), expected: "argument separator or ')' for plot call"))
        
        var yRange: (ExpressionNode, ExpressionNode)? = nil
        let xRange: (ExpressionNode, ExpressionNode)
        let variableNode: ConstantNode
        let expressions: [ExpressionNode]
        
        if allArguments.count >= 5 && allArguments[allArguments.count - 4] is ConstantNode {
            let yMax = allArguments.removeLast()
            let yMin = allArguments.removeLast()
            yRange = (yMin, yMax)
        }

        guard allArguments.count >= 3 else {
            throw ParserError.incorrectArgumentCount(function: "plot", expected: "at least 3 (expr, var, range)", found: allArguments.count)
        }
        
        let xMax = allArguments.removeLast()
        let xMin = allArguments.removeLast()
        guard let variable = allArguments.removeLast() as? ConstantNode else {
            throw ParserError.unexpectedToken(token: nil, expected: "an identifier for the plot variable (e.g., 'x' or 't')")
        }
        
        xRange = (xMin, xMax)
        variableNode = variable
        expressions = allArguments
        
        if expressions.isEmpty {
             throw ParserError.incorrectArgumentCount(function: "plot", expected: "at least one expression to plot", found: 0)
        }

        return PlotNode(expressions: expressions, variable: variableNode, xRange: xRange, yRange: yRange)
    }
    
    private func parseScatterplot() throws -> ExpressionNode {
        try consume(.paren("("), orThrow: .unexpectedToken(token: peek(), expected: "'(' for scatterplot call"))
        var arguments: [ExpressionNode] = []
        if let nextToken = peek(), case .paren(")") = nextToken.type {
            try advance()
             throw ParserError.incorrectArgumentCount(function: "scatterplot", expected: "1, 2, or 3", found: 0)
        }
        
        repeat {
            arguments.append(try parseExpression())
            if let nextToken = peek(), case .separator(let char) = nextToken.type, char != ";" {
                try advance()
            } else { break }
        } while true
        
        try consume(.paren(")"), orThrow: .unexpectedToken(token: peek(), expected: "argument separator or ')' for scatterplot call"))
        
        guard !arguments.isEmpty && arguments.count <= 3 else {
            throw ParserError.incorrectArgumentCount(function: "scatterplot", expected: "1 (Matrix), 2 (Vectors), or 3 (Vectors, Degree)", found: arguments.count)
        }
        
        return ScatterplotNode(arguments: arguments)
    }

    private func parseImportCSV() throws -> ExpressionNode {
        try consume(.paren("("), orThrow: .unexpectedToken(token: peek(), expected: "'(' for importcsv call"))
        
        if let nextToken = peek(), nextToken.type != .paren(")") {
             throw ParserError.incorrectArgumentCount(function: "importcsv", expected: "0", found: 1)
        }
        
        try consume(.paren(")"), orThrow: .unexpectedToken(token: peek(), expected: "')' for importcsv call"))
        return ImportCSVNode()
    }

    private func parseDerivative() throws -> ExpressionNode {
        try consume(.paren("("), orThrow: .unexpectedToken(token: peek(), expected: "'(' for derivative"))
        
        let arg1 = try parseExpression()
        try consumeArgumentSeparator(orThrow: .unexpectedToken(token: peek(), expected: "argument separator after first argument"))
        let arg2 = try parseExpression()

        var order: ExpressionNode = NumberNode(value: 1)

        if let nextToken = peek(), case .separator(let char) = nextToken.type, char != ";" {
            try advance()
            let arg3 = try parseExpression()

            guard let varNode = arg2 as? ConstantNode else {
                throw ParserError.unexpectedToken(token: nil, expected: "a variable name for the second argument (e.g., 'x')")
            }

            if let anotherSeparator = peek(), case .separator(let char) = anotherSeparator.type, char != ";" {
                try advance()
                order = try parseExpression()
            }
            
            try consume(.paren(")"), orThrow: .unexpectedToken(token: peek(), expected: "')' to close derivative call"))
            return DerivativeNode(body: arg1, variable: varNode, point: arg3, order: order)

        } else {
            try consume(.paren(")"), orThrow: .unexpectedToken(token: peek(), expected: "')' to close derivative call"))
            return DerivativeNode(body: arg1, variable: nil, point: arg2, order: order)
        }
    }

    private func parseIntegral() throws -> ExpressionNode {
        try consume(.paren("("), orThrow: .unexpectedToken(token: peek(), expected: "'(' for integral"))
        
        let body = try parseExpression()
        try consumeArgumentSeparator(orThrow: .unexpectedToken(token: peek(), expected: "argument separator after expression"))

        let variableToken = try advance()
        guard case .identifier(let varName) = variableToken.type else {
            throw ParserError.unexpectedToken(token: variableToken, expected: "variable name")
        }
        let variableNode = ConstantNode(name: varName)
        
        try consumeArgumentSeparator(orThrow: .unexpectedToken(token: peek(), expected: "argument separator after variable"))
        let lowerBound = try parseExpression()
        
        try consumeArgumentSeparator(orThrow: .unexpectedToken(token: peek(), expected: "argument separator after lower bound"))
        let upperBound = try parseExpression()
        
        try consume(.paren(")"), orThrow: .unexpectedToken(token: peek(), expected: "')'"))
        return IntegralNode(body: body, variable: variableNode, lowerBound: lowerBound, upperBound: upperBound)
    }
    
    private func parseVector() throws -> ExpressionNode {
        try consume(.paren("("), orThrow: .unexpectedToken(token: peek(), expected: "'(' for vector"))
        var elements: [ExpressionNode] = []
        if let nextToken = peek(), case .paren(")") = nextToken.type {
            try advance()
            return VectorNode(elements: [])
        }
        
        repeat {
            elements.append(try parseExpression())
            if let nextToken = peek(), case .separator(";") = nextToken.type {
                try advance()
            } else { break }
        } while true
        
        try consume(.paren(")"), orThrow: .unexpectedToken(token: peek(), expected: "';' or ')' for vector"))
        return VectorNode(elements: elements)
    }
    
    private func parseMatrix() throws -> ExpressionNode {
        try consume(.paren("("), orThrow: .unexpectedToken(token: peek(), expected: "'(' for matrix"))
        var rows: [[ExpressionNode]] = []
        if let nextToken = peek(), case .paren(")") = nextToken.type {
            try advance()
            return MatrixNode(rows: [])
        }
        
        repeat {
            var row: [ExpressionNode] = []
            repeat {
                row.append(try parseExpression())
                if let nextToken = peek(), case .separator(let char) = nextToken.type, char != ";" {
                    try advance()
                } else { break }
            } while true
            rows.append(row)
            if let nextToken = peek(), case .separator(";") = nextToken.type {
                try advance()
            } else { break }
        } while true

        try consume(.paren(")"), orThrow: .unexpectedToken(token: peek(), expected: "';' or ')' for matrix"))
        
        let firstRowColumnCount = rows.first?.count ?? 0
        guard rows.allSatisfy({ $0.count == firstRowColumnCount }) else {
            throw ParserError.matrixRowMismatch(expected: firstRowColumnCount, found: rows.first(where: { $0.count != firstRowColumnCount })!.count)
        }
        return MatrixNode(rows: rows)
    }

    private func parseComplexVector() throws -> ExpressionNode {
        try consume(.paren("("), orThrow: .unexpectedToken(token: peek(), expected: "'(' for cvector"))
        var elements: [ExpressionNode] = []
        if let nextToken = peek(), case .paren(")") = nextToken.type {
            try advance()
            return ComplexVectorNode(elements: [])
        }
        
        repeat {
            elements.append(try parseExpression())
            if let nextToken = peek(), case .separator(";") = nextToken.type {
                try advance()
            } else { break }
        } while true
        
        try consume(.paren(")"), orThrow: .unexpectedToken(token: peek(), expected: "';' or ')' for cvector"))
        return ComplexVectorNode(elements: elements)
    }
    
    private func parseComplexMatrix() throws -> ExpressionNode {
        try consume(.paren("("), orThrow: .unexpectedToken(token: peek(), expected: "'(' for cmatrix"))
        var rows: [[ExpressionNode]] = []
        if let nextToken = peek(), case .paren(")") = nextToken.type {
            try advance()
            return ComplexMatrixNode(rows: [])
        }
        
        repeat {
            var row: [ExpressionNode] = []
            repeat {
                row.append(try parseExpression())
                if let nextToken = peek(), case .separator(let char) = nextToken.type, char != ";" {
                    try advance()
                } else { break }
            } while true
            rows.append(row)
            if let nextToken = peek(), case .separator(";") = nextToken.type {
                try advance()
            } else { break }
        } while true

        try consume(.paren(")"), orThrow: .unexpectedToken(token: peek(), expected: "';' or ')' for cmatrix"))
        
        let firstRowColumnCount = rows.first?.count ?? 0
        guard rows.allSatisfy({ $0.count == firstRowColumnCount }) else {
            throw ParserError.matrixRowMismatch(expected: firstRowColumnCount, found: rows.first(where: { $0.count != firstRowColumnCount })!.count)
        }
        return ComplexMatrixNode(rows: rows)
    }
    
    private func consumeArgumentSeparator(orThrow error: @autoclosure () -> ParserError) throws {
        guard let token = peek(), case .separator(let char) = token.type, char != ";" else {
            throw error()
        }
        try advance()
    }

    private func unaryOperatorPrecedence() -> Int { return 6 }
    private func implicitMultiplicationPrecedence() -> Int { return 4 }
    private func infixOperatorPrecedence(for token: Token) -> Int? {
        if case .op(let opString) = token.type {
            switch opString {
            case "±": return 1
            case "+", "-": return 2
            case "*", "/", "∠", ".*", "./": return 3
            case ".=@", ".+@", ".-@", ".*@", "./@": return 4
            case "^": return 5
            default: return nil
            }
        }
        return nil
    }
    
    private func shouldImplicitlyMultiply(after lastType: TokenType?, next nextType: TokenType) -> Bool {
        guard let lastType = lastType else { return false }

        let wasValue = {
            switch lastType {
            case .number, .identifier, .unitVector, .paren(")"), .op("'"), .op("!"), .unit: return true
            default: return false
            }
        }()

        let isValue = {
            switch nextType {
            case .number, .identifier, .unitVector, .paren("("), .string: return true
            default: return false
            }
        }()

        return wasValue && isValue
    }
    
    private func isAtEnd() -> Bool { return currentIndex >= tokens.count }
    private func peek(offset: Int = 0) -> Token? {
        guard currentIndex + offset < tokens.count else { return nil }
        return tokens[currentIndex + offset]
    }
    
    @discardableResult private func advance() throws -> Token {
        guard !isAtEnd() else { throw ParserError.unexpectedEndOfInput(expected: "more tokens") }
        let currentToken = tokens[currentIndex]
        self.lastTokenType = currentToken.type
        currentIndex += 1
        return currentToken
    }
    
    private func consume(_ type: TokenType, orThrow error: @autoclosure () -> ParserError) throws {
        guard let token = peek(), token.type == type else { throw error() }
        try advance()
    }
}
