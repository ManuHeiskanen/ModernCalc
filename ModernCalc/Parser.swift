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
// --- NEW NODES FOR CALCULUS ---
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
// --- NEW NODES FOR PLOTTING ---
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



// --- PARSER ---
enum ParserError: Error, CustomStringConvertible {
    case unexpectedToken(token: Token?, expected: String)
    case unexpectedEndOfInput(expected: String)
    case matrixRowMismatch(expected: Int, found: Int)
    case invalidParameterSyntax
    case invalidAssignmentTarget
    case incorrectArgumentCount(function: String, expected: String, found: Int)

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
                     try consume(.separator(","), orThrow: .unexpectedToken(token: peek(), expected: "','"))
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
        let prefixNode = try parsePrefix()
        return try parsePostfix(left: prefixNode)
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
        let token = try advance()
        switch token.type {
        case .number(let value): return NumberNode(value: value)
        case .unitVector(let char): return ConstantNode(name: "\(char)'")
        case .identifier(let name):
            if let nextToken = peek(), case .paren("(") = nextToken.type {
                // This is a standard function call
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
                default: return try parseFunctionCall(name: name)
                }
            } else if let primeToken = peek(), case .op("'") = primeToken.type, let parenToken = peek(offset: 1), case .paren("(") = parenToken.type {
                // This is an f'(x) style call
                try advance() // consume '
                try advance() // consume (
                
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
            if let nextToken = peek(), case .separator(",") = nextToken.type {
                try advance()
            } else { break }
        } while true
        
        try consume(.paren(")"), orThrow: .unexpectedToken(token: peek(), expected: "',' or ')' for function call"))
        return FunctionCallNode(name: name, arguments: arguments)
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
            if let nextToken = peek(), case .separator(",") = nextToken.type {
                try advance()
            } else { break }
        } while true
        
        try consume(.paren(")"), orThrow: .unexpectedToken(token: peek(), expected: "',' or ')' for autoplot call"))
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
            if let nextToken = peek(), case .separator(",") = nextToken.type {
                try advance()
            } else { break }
        } while true
        
        try consume(.paren(")"), orThrow: .unexpectedToken(token: peek(), expected: "',' or ')' for plot call"))
        
        // --- Argument Interpretation Logic ---
        var yRange: (ExpressionNode, ExpressionNode)? = nil
        let xRange: (ExpressionNode, ExpressionNode)
        let variableNode: ConstantNode
        let expressions: [ExpressionNode]
        
        // Check for optional y-range: plot(..., var, x_min, x_max, y_min, y_max)
        if allArguments.count >= 5 && allArguments[allArguments.count - 4] is ConstantNode {
            let yMax = allArguments.removeLast()
            let yMin = allArguments.removeLast()
            yRange = (yMin, yMax)
        }

        guard allArguments.count >= 3 else {
            throw ParserError.incorrectArgumentCount(function: "plot", expected: "at least 3 (expr, var, range)", found: allArguments.count)
        }
        
        // The last three arguments must be x_max, x_min, and the variable
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
            if let nextToken = peek(), case .separator(",") = nextToken.type {
                try advance()
            } else { break }
        } while true
        
        try consume(.paren(")"), orThrow: .unexpectedToken(token: peek(), expected: "',' or ')' for scatterplot call"))
        
        guard !arguments.isEmpty && arguments.count <= 3 else {
            throw ParserError.incorrectArgumentCount(function: "scatterplot", expected: "1 (Matrix), 2 (Vectors), or 3 (Vectors, Degree)", found: arguments.count)
        }
        
        return ScatterplotNode(arguments: arguments)
    }

    private func parseDerivative() throws -> ExpressionNode {
        try consume(.paren("("), orThrow: .unexpectedToken(token: peek(), expected: "'(' for derivative"))
        
        let arg1 = try parseExpression() // This can be the body expression OR a function name
        try consume(.separator(","), orThrow: .unexpectedToken(token: peek(), expected: "',' after first argument"))
        let arg2 = try parseExpression() // This can be the variable OR the point

        var order: ExpressionNode = NumberNode(value: 1)

        // Check if there is a third argument, which clarifies the function signature
        if let nextToken = peek(), case .separator(",") = nextToken.type {
            // This is a 3 or 4-argument call: derivative(body, var, point, [order])
            try advance() // Consume the comma
            let arg3 = try parseExpression() // This is the point

            guard let varNode = arg2 as? ConstantNode else {
                throw ParserError.unexpectedToken(token: nil, expected: "a variable name for the second argument (e.g., 'x')")
            }

            // Check for optional 4th argument (order)
            if let anotherComma = peek(), case .separator(",") = anotherComma.type {
                try advance()
                order = try parseExpression()
            }
            
            try consume(.paren(")"), orThrow: .unexpectedToken(token: peek(), expected: "')' to close derivative call"))
            return DerivativeNode(body: arg1, variable: varNode, point: arg3, order: order)

        } else {
            // This is a 2-argument call: derivative(body, point)
            try consume(.paren(")"), orThrow: .unexpectedToken(token: peek(), expected: "')' to close derivative call"))
            // The variable is left as nil, to be inferred by the Evaluator
            return DerivativeNode(body: arg1, variable: nil, point: arg2, order: order)
        }
    }

    private func parseIntegral() throws -> ExpressionNode {
        try consume(.paren("("), orThrow: .unexpectedToken(token: peek(), expected: "'(' for integral"))
        
        let body = try parseExpression()
        try consume(.separator(","), orThrow: .unexpectedToken(token: peek(), expected: "',' after expression"))

        let variableToken = try advance()
        guard case .identifier(let varName) = variableToken.type else {
            throw ParserError.unexpectedToken(token: variableToken, expected: "variable name")
        }
        let variableNode = ConstantNode(name: varName)
        
        try consume(.separator(","), orThrow: .unexpectedToken(token: peek(), expected: "',' after variable"))
        let lowerBound = try parseExpression()
        
        try consume(.separator(","), orThrow: .unexpectedToken(token: peek(), expected: "',' after lower bound"))
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
            case .number, .identifier, .unitVector, .paren(")"), .op("'"), .op("!"): return true // Add !
            default: return false
            }
        }()

        let isValue = {
            switch nextType {
            case .number, .identifier, .unitVector, .paren("("): return true
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
