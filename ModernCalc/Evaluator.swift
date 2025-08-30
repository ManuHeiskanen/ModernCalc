import Foundation

// Define the possible mathematical errors.
enum MathError: Error, CustomStringConvertible {
    case divisionByZero
    case unknownOperator(op: String)
    case unknownConstant(name: String)
    case unknownFunction(name: String)
    case invalidNode
    case typeMismatch(expected: String, found: String)
    case unsupportedOperation(op: String, typeA: String, typeB: String?)
    case dimensionMismatch(reason: String)
    case incorrectArgumentCount(function: String, expected: Int, found: Int)

    var description: String {
        switch self {
        case .divisionByZero:
            return "Error: Division by zero."
        case .unknownOperator(let op):
            return "Error: Unknown operator '\(op)'."
        case .unknownConstant(let name):
            return "Error: Unknown variable or constant '\(name)'."
        case .unknownFunction(let name):
            return "Error: Unknown function '\(name)'."
        case .invalidNode:
            return "Error: The expression tree contains an invalid node."
        case .typeMismatch(let expected, let found):
            return "Error: Type mismatch. Expected \(expected), but found \(found)."
        case .unsupportedOperation(let op, let typeA, let typeB):
            if let typeB = typeB {
                return "Error: Operator '\(op)' is not supported between \(typeA) and \(typeB)."
            }
            return "Error: Operator '\(op)' is not supported for \(typeA)."
        case .dimensionMismatch(let reason):
            return "Error: Dimension mismatch. \(reason)."
        case .incorrectArgumentCount(let function, let expected, let found):
            return "Error: Function '\(function)' expects \(expected) argument(s), but received \(found)."
        }
    }
}

// The Evaluator component.
struct Evaluator {

    private let constants: [String: Double] = [ "pi": Double.pi, "e": M_E ]
    private let scalarFunctions: [String: (Double) -> Double] = [
        "sqrt": sqrt, "sin": sin, "cos": cos, "tan": tan,
        "asin": asin, "acos": acos, "atan": atan,
        "log": log10, "ln": log
    ]
    
    func evaluate(node: ExpressionNode, variables: inout [String: MathValue], functions: inout [String: FunctionDefinitionNode]) throws -> MathValue {
        switch node {
        case let numberNode as NumberNode:
            return .scalar(numberNode.value)
        
        case let complexNode as ComplexNode:
            let realPart = try evaluate(node: complexNode.real, variables: &variables, functions: &functions)
            let imagPart = try evaluate(node: complexNode.imaginary, variables: &variables, functions: &functions)
            guard case .scalar(let r) = realPart, case .scalar(let i) = imagPart else {
                throw MathError.typeMismatch(expected: "Scalar", found: "Non-scalar in complex definition")
            }
            return .complex(Complex(real: r, imaginary: i))

        case let constantNode as ConstantNode:
            if let value = variables[constantNode.name] {
                return value
            } else if let value = constants[constantNode.name] {
                return .scalar(value)
            } else {
                throw MathError.unknownConstant(name: constantNode.name)
            }
            
        case let assignmentNode as AssignmentNode:
            let value = try evaluate(node: assignmentNode.expression, variables: &variables, functions: &functions)
            variables[assignmentNode.name] = value
            return value

        case let funcDefNode as FunctionDefinitionNode:
            functions[funcDefNode.name] = funcDefNode
            return .functionDefinition(funcDefNode.name)
            
        case let functionNode as FunctionCallNode:
            return try evaluateFunctionCall(functionNode, variables: &variables, functions: &functions)
            
        case let vectorNode as VectorNode:
            var elements: [Double] = []
            for elementNode in vectorNode.elements {
                let evaluatedElement = try evaluate(node: elementNode, variables: &variables, functions: &functions)
                guard case .scalar(let scalarElement) = evaluatedElement else {
                    throw MathError.typeMismatch(expected: "Scalar", found: "Non-scalar in vector definition")
                }
                elements.append(scalarElement)
            }
            return .vector(Vector(values: elements))
            
        case let matrixNode as MatrixNode:
            var values: [Double] = []
            let rows = matrixNode.rows.count
            let columns = matrixNode.rows.first?.count ?? 0
            for row in matrixNode.rows {
                for elementNode in row {
                    let evaluatedElement = try evaluate(node: elementNode, variables: &variables, functions: &functions)
                    guard case .scalar(let scalarElement) = evaluatedElement else {
                        throw MathError.typeMismatch(expected: "Scalar", found: "Non-scalar in matrix definition")
                    }
                    values.append(scalarElement)
                }
            }
            return .matrix(Matrix(values: values, rows: rows, columns: columns))

        // NEW: Handle evaluation for complex vectors
        case let cVectorNode as ComplexVectorNode:
            var elements: [Complex] = []
            for elementNode in cVectorNode.elements {
                let evaluatedElement = try evaluate(node: elementNode, variables: &variables, functions: &functions)
                switch evaluatedElement {
                case .complex(let c):
                    elements.append(c)
                case .scalar(let s):
                    elements.append(Complex(real: s, imaginary: 0))
                default:
                    throw MathError.typeMismatch(expected: "Complex or Scalar", found: evaluatedElement.typeName)
                }
            }
            return .complexVector(ComplexVector(values: elements))

        // NEW: Handle evaluation for complex matrices
        case let cMatrixNode as ComplexMatrixNode:
            var values: [Complex] = []
            let rows = cMatrixNode.rows.count
            let columns = cMatrixNode.rows.first?.count ?? 0
            for row in cMatrixNode.rows {
                for elementNode in row {
                    let evaluatedElement = try evaluate(node: elementNode, variables: &variables, functions: &functions)
                    switch evaluatedElement {
                    case .complex(let c):
                        values.append(c)
                    case .scalar(let s):
                        values.append(Complex(real: s, imaginary: 0))
                    default:
                        throw MathError.typeMismatch(expected: "Complex or Scalar", found: evaluatedElement.typeName)
                    }
                }
            }
            return .complexMatrix(ComplexMatrix(values: values, rows: rows, columns: columns))

        case let unaryNode as UnaryOpNode:
            let childValue = try evaluate(node: unaryNode.child, variables: &variables, functions: &functions)
            return try evaluateUnaryOperation(op: unaryNode.op, value: childValue)

        case let binaryNode as BinaryOpNode:
            let leftValue = try evaluate(node: binaryNode.left, variables: &variables, functions: &functions)
            let rightValue = try evaluate(node: binaryNode.right, variables: &variables, functions: &functions)
            return try evaluateBinaryOperation(op: binaryNode.op, left: leftValue, right: rightValue)

        default:
            throw MathError.invalidNode
        }
    }

    // --- EVALUATION HELPERS ---

    private func evaluateFunctionCall(_ node: FunctionCallNode, variables: inout [String: MathValue], functions: inout [String: FunctionDefinitionNode]) throws -> MathValue {
        if let function = scalarFunctions[node.name] {
            guard node.arguments.count == 1 else {
                throw MathError.incorrectArgumentCount(function: node.name, expected: 1, found: node.arguments.count)
            }
            let argumentValue = try evaluate(node: node.arguments[0], variables: &variables, functions: &functions)
            
            if case .scalar(let scalarArg) = argumentValue {
                 if node.name == "sqrt" && scalarArg < 0 {
                     return .complex(Complex(real: scalarArg, imaginary: 0).sqrt())
                 }
                 return .scalar(function(scalarArg))
            } else if case .complex(let complexArg) = argumentValue, node.name == "sqrt" {
                 return .complex(complexArg.sqrt())
            } else {
                 throw MathError.typeMismatch(expected: "Scalar", found: argumentValue.typeName)
            }
        }
        
        switch node.name {
        case "transpose":
            guard node.arguments.count == 1 else {
                throw MathError.incorrectArgumentCount(function: node.name, expected: 1, found: node.arguments.count)
            }
            let argumentValue = try evaluate(node: node.arguments[0], variables: &variables, functions: &functions)
            guard case .matrix(let matrix) = argumentValue else {
                throw MathError.typeMismatch(expected: "Matrix", found: argumentValue.typeName)
            }
            return .matrix(transpose(matrix))
        default:
            if let userFunction = functions[node.name] {
                guard node.arguments.count == userFunction.parameterNames.count else {
                    throw MathError.incorrectArgumentCount(function: node.name, expected: userFunction.parameterNames.count, found: node.arguments.count)
                }
                
                var localVariables = variables
                for (paramName, argNode) in zip(userFunction.parameterNames, node.arguments) {
                    localVariables[paramName] = try evaluate(node: argNode, variables: &variables, functions: &functions)
                }
                
                return try evaluate(node: userFunction.body, variables: &localVariables, functions: &functions)
            }
            throw MathError.unknownFunction(name: node.name)
        }
    }

    private func evaluateUnaryOperation(op: Token, value: MathValue) throws -> MathValue {
        guard op.rawValue == "-" || op.rawValue == "+" else {
            throw MathError.unknownOperator(op: op.rawValue)
        }
        
        let opSign = (op.rawValue == "-") ? -1.0 : 1.0
        
        switch value {
        case .scalar(let s):
            return .scalar(s * opSign)
        case .complex(let c):
            return .complex(c * opSign)
        case .vector(let v):
            let newValues = v.values.map { $0 * opSign }
            return .vector(Vector(values: newValues))
        case .matrix(let m):
            let newValues = m.values.map { $0 * opSign }
            return .matrix(Matrix(values: newValues, rows: m.rows, columns: m.columns))
        // NEW: Handle unary operations for complex vectors/matrices
        case .complexVector(let cv):
            let newValues = cv.values.map { $0 * opSign }
            return .complexVector(ComplexVector(values: newValues))
        case .complexMatrix(let cm):
            let newValues = cm.values.map { $0 * opSign }
            return .complexMatrix(ComplexMatrix(values: newValues, rows: cm.rows, columns: cm.columns))
        default:
            throw MathError.unsupportedOperation(op: op.rawValue, typeA: value.typeName, typeB: nil)
        }
    }

    private func evaluateBinaryOperation(op: Token, left: MathValue, right: MathValue) throws -> MathValue {
        switch (left, right) {
        // --- Scalar, Scalar ---
        case (.scalar(let l), .scalar(let r)):
            return .scalar(try performScalarScalarOp(op.rawValue, l, r))
            
        // --- Complex & Scalar/Complex Operations ---
        case (.complex(let l), .complex(let r)):
            return .complex(try performComplexComplexOp(op.rawValue, l, r))
        case (.complex(let l), .scalar(let r)):
            return .complex(try performComplexComplexOp(op.rawValue, l, Complex(real: r, imaginary: 0)))
        case (.scalar(let l), .complex(let r)):
            return .complex(try performComplexComplexOp(op.rawValue, Complex(real: l, imaginary: 0), r))

        // --- Matrix & Scalar/Matrix Operations ---
        case (.matrix(let m), .scalar(let s)):
            return .matrix(try performMatrixScalarOp(op.rawValue, m, s))
        case (.scalar(let s), .matrix(let m)):
            return .matrix(try performMatrixScalarOp(op.rawValue, m, s, reversed: true))
        case (.matrix(let l), .matrix(let r)):
            return .matrix(try performMatrixMatrixOp(op.rawValue, l, r))
            
        // --- Promote Real Matrix/Vector to Complex for operations ---
        case (.matrix(let l), .complex(let r)):
            let promotedMatrix = ComplexMatrix(from: l)
            return .complexMatrix(try performComplexMatrixComplexOp(op.rawValue, promotedMatrix, r))
        case (.complex(let l), .matrix(let r)):
            let promotedMatrix = ComplexMatrix(from: r)
            return .complexMatrix(try performComplexMatrixComplexOp(op.rawValue, promotedMatrix, l, reversed: true))
        case (.vector(let l), .complex(let r)):
            let promotedVector = ComplexVector(from: l)
            return .complexVector(try performComplexVectorComplexOp(op.rawValue, promotedVector, r))
        case (.complex(let l), .vector(let r)):
            let promotedVector = ComplexVector(from: r)
            return .complexVector(try performComplexVectorComplexOp(op.rawValue, promotedVector, l, reversed: true))
            
        // NEW: Handle Complex Matrix/Vector with Scalar
        case (.complexMatrix(let l), .scalar(let r)):
            let complexScalar = Complex(real: r, imaginary: 0)
            return .complexMatrix(try performComplexMatrixComplexOp(op.rawValue, l, complexScalar))
        case (.scalar(let l), .complexMatrix(let r)):
            let complexScalar = Complex(real: l, imaginary: 0)
            return .complexMatrix(try performComplexMatrixComplexOp(op.rawValue, r, complexScalar, reversed: true))
        case (.complexVector(let l), .scalar(let r)):
            let complexScalar = Complex(real: r, imaginary: 0)
            return .complexVector(try performComplexVectorComplexOp(op.rawValue, l, complexScalar))
        case (.scalar(let l), .complexVector(let r)):
            let complexScalar = Complex(real: l, imaginary: 0)
            return .complexVector(try performComplexVectorComplexOp(op.rawValue, r, complexScalar, reversed: true))

        default:
            throw MathError.unsupportedOperation(op: op.rawValue, typeA: left.typeName, typeB: right.typeName)
        }
    }
    
    // --- OPERATOR IMPLEMENTATIONS ---
    
    private func performScalarScalarOp(_ op: String, _ l: Double, _ r: Double) throws -> Double {
        switch op {
        case "+": return l + r
        case "-": return l - r
        case "*": return l * r
        case "/":
            guard r != 0 else { throw MathError.divisionByZero }
            return l / r
        case "%":
            guard r != 0 else { throw MathError.divisionByZero }
            return l.truncatingRemainder(dividingBy: r)
        case "^": return pow(l, r)
        default: throw MathError.unknownOperator(op: op)
        }
    }

    private func performComplexComplexOp(_ op: String, _ l: Complex, _ r: Complex) throws -> Complex {
        switch op {
        case "+": return l + r
        case "-": return l - r
        case "*": return l * r
        case "/": return try l / r
        case "^": return try l.pow(r)
        default: throw MathError.unknownOperator(op: op)
        }
    }
    
    private func performMatrixScalarOp(_ op: String, _ m: Matrix, _ s: Double, reversed: Bool = false) throws -> Matrix {
        let newValues: [Double]
        switch op {
        case "+": newValues = m.values.map { $0 + s }
        case "*": newValues = m.values.map { $0 * s }
        case "-": newValues = reversed ? m.values.map { s - $0 } : m.values.map { $0 - s }
        case "/":
            if reversed {
                throw MathError.unsupportedOperation(op: op, typeA: "Scalar", typeB: "Matrix")
            }
            guard s != 0 else { throw MathError.divisionByZero }
            newValues = m.values.map { $0 / s }
        default: throw MathError.unsupportedOperation(op: op, typeA: "Matrix", typeB: "Scalar")
        }
        return Matrix(values: newValues, rows: m.rows, columns: m.columns)
    }
    
    private func performMatrixMatrixOp(_ op: String, _ l: Matrix, _ r: Matrix) throws -> Matrix {
        switch op {
        case "+", "-":
            guard l.rows == r.rows && l.columns == r.columns else {
                throw MathError.dimensionMismatch(reason: "Matrices must have same dimensions for +/-.")
            }
            var newValues = [Double](repeating: 0, count: l.values.count)
            for i in 0..<l.values.count {
                newValues[i] = (op == "+") ? l.values[i] + r.values[i] : l.values[i] - r.values[i]
            }
            return Matrix(values: newValues, rows: l.rows, columns: l.columns)

        case "*": // Dot Product
            guard l.columns == r.rows else {
                throw MathError.dimensionMismatch(reason: "For A*B, columns of A must equal rows of B.")
            }
            var newValues = [Double](repeating: 0, count: l.rows * r.columns)
            for i in 0..<l.rows {
                for j in 0..<r.columns {
                    var sum = 0.0
                    for k in 0..<l.columns {
                        sum += l[i, k] * r[k, j]
                    }
                    newValues[i * r.columns + j] = sum
                }
            }
            return Matrix(values: newValues, rows: l.rows, columns: r.columns)
        default:
            throw MathError.unsupportedOperation(op: op, typeA: "Matrix", typeB: "Matrix")
        }
    }
    
    private func performComplexVectorComplexOp(_ op: String, _ v: ComplexVector, _ c: Complex, reversed: Bool = false) throws -> ComplexVector {
        if reversed {
            switch op {
            case "+": return ComplexVector(values: v.values.map { c + $0 })
            case "*": return ComplexVector(values: v.values.map { c * $0 })
            case "-": return ComplexVector(values: v.values.map { c - $0 })
            case "/": throw MathError.unsupportedOperation(op: op, typeA: "Complex", typeB: "ComplexVector")
            default: throw MathError.unsupportedOperation(op: op, typeA: "ComplexVector", typeB: "Complex")
            }
        } else {
            switch op {
            case "+": return v + c
            case "*": return v * c
            case "-": return v - c
            case "/": return try v / c
            default: throw MathError.unsupportedOperation(op: op, typeA: "ComplexVector", typeB: "Complex")
            }
        }
    }
    
    private func performComplexMatrixComplexOp(_ op: String, _ m: ComplexMatrix, _ c: Complex, reversed: Bool = false) throws -> ComplexMatrix {
         if reversed {
            switch op {
            case "+": return ComplexMatrix(values: m.values.map { c + $0 }, rows: m.rows, columns: m.columns)
            case "*": return ComplexMatrix(values: m.values.map { c * $0 }, rows: m.rows, columns: m.columns)
            case "-": return ComplexMatrix(values: m.values.map { c - $0 }, rows: m.rows, columns: m.columns)
            case "/": throw MathError.unsupportedOperation(op: op, typeA: "Complex", typeB: "ComplexMatrix")
            default: throw MathError.unsupportedOperation(op: op, typeA: "ComplexMatrix", typeB: "Complex")
            }
        } else {
            switch op {
            case "+": return m + c
            case "*": return m * c
            case "-": return m - c
            case "/": return try m / c
            default: throw MathError.unsupportedOperation(op: op, typeA: "ComplexMatrix", typeB: "Complex")
            }
        }
    }
    
    private func transpose(_ m: Matrix) -> Matrix {
        var newValues = [Double](repeating: 0, count: m.values.count)
        for r in 0..<m.rows {
            for c in 0..<m.columns {
                newValues[c * m.rows + r] = m[r, c]
            }
        }
        return Matrix(values: newValues, rows: m.columns, columns: m.rows)
    }
}

