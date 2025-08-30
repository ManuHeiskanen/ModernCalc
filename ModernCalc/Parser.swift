import Foundation

// --- ABSTRACT SYNTAX TREE NODES ---
protocol ExpressionNode {
    var description: String { get }
}
struct ComplexNode: ExpressionNode {
    let real: ExpressionNode, imaginary: ExpressionNode
    var description: String { "(\(real.description) + \(imaginary.description)i)" }
}
struct FunctionDefinitionNode: ExpressionNode {
    let name: String, parameterNames: [String], body: ExpressionNode
    var description: String { "\(name)(\(parameterNames.joined(separator: ", "))) := \(body.description)" }
}
struct TupleNode: ExpressionNode {
    let elements: [ExpressionNode]
    var description: String { "(\(elements.map { $0.description }.joined(separator: " ± ")))" }
}
struct AssignmentNode: ExpressionNode {
    let name: String, expression: ExpressionNode
    var description: String { "\(name) := \(expression.description)" }
}
struct NumberNode: ExpressionNode {
    let value: Double
    var description: String { "\(value)" }
}
struct ConstantNode: ExpressionNode {
    let name: String
    var description: String { name }
}
struct FunctionCallNode: ExpressionNode {
    let name: String, arguments: [ExpressionNode]
    var description: String { "\(name)(\(arguments.map { $0.description }.joined(separator: ", ")))" }
}
struct VectorNode: ExpressionNode {
    let elements: [ExpressionNode]
    var description: String { "vector(\(elements.map { $0.description }.joined(separator: "; ")))" }
}
struct MatrixNode: ExpressionNode {
    let rows: [[ExpressionNode]]
    var description: String { "matrix(\(rows.map { "[" + $0.map({ $0.description }).joined(separator: ", ") + "]" }.joined(separator: "; ")))" }
}
// NEW: Nodes for complex vectors and matrices
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


// --- PARSER ---
enum ParserError: Error, CustomStringConvertible {
    case unexpectedToken(token: Token?, expected: String)
    case unexpectedEndOfInput(expected: String)
    case matrixRowMismatch(expected: Int, found: Int)
    case invalidParameterSyntax
    case invalidAssignmentTarget

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
        }
    }
}

class Parser {
    private let tokens: [Token]
    private var currentIndex = 0

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
        var left = try parsePrefix()
        
        while !isAtEnd(), let currentToken = peek(), let infixPrecedence = infixOperatorPrecedence(for: currentToken), infixPrecedence > currentPrecedence {
            let opToken = try advance()
            
            if opToken.rawValue == "±" {
                let right = try parseExpression(currentPrecedence: infixPrecedence)
                let plusNode = BinaryOpNode(op: Token(type: .op("+"), rawValue: "+"), left: left, right: right)
                let minusNode = BinaryOpNode(op: Token(type: .op("-"), rawValue: "-"), left: left, right: right)
                return TupleNode(elements: [plusNode, minusNode])
            }
            
            let nextPrecedence = (opToken.rawValue == "^") ? infixPrecedence - 1 : infixPrecedence
            let right = try parseExpression(currentPrecedence: nextPrecedence)
            left = BinaryOpNode(op: opToken, left: left, right: right)
        }
        return left
    }

    private func parsePrefix() throws -> ExpressionNode {
        let token = try advance()
        switch token.type {
        case .number(let value): return NumberNode(value: value)
        case .complexLiteral(let value): return ComplexNode(real: NumberNode(value: 0), imaginary: NumberNode(value: value))
        case .unitVector(let char): return ConstantNode(name: "\(char)'")
        case .identifier(let name):
            if let nextToken = peek(), case .paren("(") = nextToken.type {
                // MODIFIED: Handle cvector and cmatrix
                switch name {
                case "matrix": return try parseMatrix()
                case "vector": return try parseVector()
                case "cmatrix": return try parseComplexMatrix()
                case "cvector": return try parseComplexVector()
                default: return try parseFunctionCall(name: name)
                }
            } else { return ConstantNode(name: name) }
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
            if let nextToken = peek(), case .separator(",") = nextToken.type {
                try advance()
            } else { break }
        } while true
        
        try consume(.paren(")"), orThrow: .unexpectedToken(token: peek(), expected: "',' or ')' for function call"))
        return FunctionCallNode(name: name, arguments: arguments)
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
                if let nextToken = peek(), case .separator(",") = nextToken.type {
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

    // NEW: Function to parse a complex vector
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
    
    // NEW: Function to parse a complex matrix
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
                if let nextToken = peek(), case .separator(",") = nextToken.type {
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

    private func unaryOperatorPrecedence() -> Int { return 6 }
    private func infixOperatorPrecedence(for token: Token) -> Int? {
        if case .op(let opString) = token.type {
            switch opString {
            case "±": return 1
            case "+", "-": return 2
            case "*", "/": return 3
            case "%": return 4
            case "^": return 5
            default: return nil
            }
        }
        return nil
    }
    
    private func isAtEnd() -> Bool { return currentIndex >= tokens.count }
    private func peek(offset: Int = 0) -> Token? {
        guard currentIndex + offset < tokens.count else { return nil }
        return tokens[currentIndex + offset]
    }
    @discardableResult private func advance() throws -> Token {
        guard !isAtEnd() else { throw ParserError.unexpectedEndOfInput(expected: "more tokens") }
        let currentToken = tokens[currentIndex]
        currentIndex += 1
        return currentToken
    }
    private func consume(_ type: TokenType, orThrow error: @autoclosure () -> ParserError) throws {
        guard let token = peek(), token.type == type else { throw error() }
        try advance()
    }
}

