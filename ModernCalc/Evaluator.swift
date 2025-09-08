import Foundation

enum MathError: Error, CustomStringConvertible {
    case divisionByZero
    case unknownOperator(op: String)
    case unknownConstant(name: String)
    case unknownFunction(name: String)
    case invalidNode
    case typeMismatch(expected: String, found: String)
    case unsupportedOperation(op: String, typeA: String, typeB: String?)
    case dimensionMismatch(reason: String)
    case incorrectArgumentCount(function: String, expected: String, found: Int)
    case requiresAtLeastOneArgument(function: String)
    case plotError(reason: String)

    var description: String {
        switch self {
        case .divisionByZero: return "Error: Division by zero."
        case .unknownOperator(let op): return "Error: Unknown operator '\(op)'."
        case .unknownConstant(let name): return "Error: Unknown variable or constant '\(name)'."
        case .unknownFunction(let name): return "Error: Unknown function '\(name)'."
        case .invalidNode: return "Error: The expression tree contains an invalid node."
        case .typeMismatch(let expected, let found): return "Error: Type mismatch. Expected \(expected), but found \(found)."
        case .unsupportedOperation(let op, let typeA, let typeB):
            if let typeB = typeB {
                if typeA == "Tuple" && typeB == "Tuple" {
                    return "Error: Operations between two multi-valued results are not supported."
                }
                return "Error: Operator '\(op)' is not supported between \(typeA) and \(typeB)."
            }
            if typeA == "hyp < side" { return "Error: Hypotenuse must be greater than or equal to the side." }
            if typeA == "Singular Matrix" { return "Error: Matrix is singular and cannot be inverted."}
            return "Error: Operator '\(op)' is not supported for \(typeA)."
        case .dimensionMismatch(let reason): return "Error: Dimension mismatch. \(reason)."
        case .incorrectArgumentCount(let function, let expected, let found): return "Error: Function '\(function)' expects \(expected) argument(s), but received \(found)."
        case .requiresAtLeastOneArgument(let function): return "Error: Function '\(function)' requires at least one argument."
        case .plotError(let reason): return "Plot Error: \(reason)."
        }
    }
}

struct Evaluator {

    private let h = 1e-5 // A small step size for numerical differentiation

    private let constants: [String: Double] = [
        "pi": Double.pi, "e": M_E, "c": 299792458, "μ0": 1.25663706212e-6, "ε0": 8.8541878128e-12,
        "g": 9.80665, "G": 6.67430e-11, "h": 6.62607015e-34, "ħ": 1.054571817e-34, "me": 9.1093837015e-31,
        "mp": 1.67262192369e-27, "mn": 1.67492749804e-27, "e0": 1.602176634e-19, "NA": 6.02214076e23,
        "R": 8.314462618, "kB": 1.380649e-23, "F": 96485.33212, "Rinf": 10973731.568160, "σ": 5.670374419e-8,
        "b": 2.897771955e-3, "atm": 101325, "Vm": 22.41396954e-3
    ]
    
    private let siPrefixes: [String: Double] = [
        "yotta": 1e24, "zetta": 1e21, "exa": 1e18, "peta": 1e15, "tera": 1e12, "giga": 1e9, "mega": 1e6,
        "kilo": 1e3, "hecto": 1e2, "deca": 1e1, "deci": 1e-1, "centi": 1e-2, "milli": 1e-3, "micro": 1e-6,
        "nano": 1e-9, "pico": 1e-12, "femto": 1e-15, "atto": 1e-18, "zepto": 1e-21, "yocto": 1e-24
    ]
    
    private let scalarFunctions: [String: (Double) -> Double] = ["ln": log, "lg": log10, "log": log10]
    
    private let variadicFunctions: [String: ([MathValue]) throws -> MathValue] = [
        "sum": { args in try performStatisticalOperation(args: args, on: { $0.sum() }) },
        "avg": { args in try performStatisticalOperation(args: args, on: { $0.average() }) },
        "average": { args in try performStatisticalOperation(args: args, on: { $0.average() }) },
        "min": { args in try performStatisticalOperation(args: args, on: { $0.min() }) },
        "max": { args in try performStatisticalOperation(args: args, on: { $0.max() }) },
        "median": { args in try performStatisticalOperation(args: args, on: { $0.median() }) },
        "stddev": { args in try performStatisticalOperation(args: args, on: { $0.stddev() }) },
        "variance": { args in try performStatisticalOperation(args: args, on: { $0.variance() }) },
        "stddevp": { args in try performStatisticalOperation(args: args, on: { $0.stddevp() }) }
    ]
    
    private let multiArgumentFunctions: [String: ([MathValue]) throws -> MathValue] = [
        "range": { args in
            guard args.count == 2 || args.count == 3 else {
                throw MathError.incorrectArgumentCount(function: "range", expected: "2 or 3", found: args.count)
            }
            let start = try args[0].asScalar()
            let end = try args[1].asScalar()
            let step = try args.count == 3 ? args[2].asScalar() : 1.0
            
            guard step != 0 else { throw MathError.unsupportedOperation(op: "range", typeA: "step cannot be zero", typeB: nil) }
            
            var values: [Double] = []
            var current = start
            
            if step > 0 {
                while current <= end {
                    values.append(current)
                    current += step
                }
            } else { // step < 0
                while current >= end {
                    values.append(current)
                    current += step
                }
            }
            return .vector(Vector(values: values))
        },
        "linspace": { args in
            guard args.count == 3 else {
                throw MathError.incorrectArgumentCount(function: "linspace", expected: "3", found: args.count)
            }
            let start = try args[0].asScalar()
            let end = try args[1].asScalar()
            let countScalar = try args[2].asScalar()
            
            guard countScalar >= 2, countScalar.truncatingRemainder(dividingBy: 1) == 0 else {
                throw MathError.unsupportedOperation(op: "linspace", typeA: "count must be an integer >= 2", typeB: nil)
            }
            let count = Int(countScalar)

            var values: [Double] = []
            if count == 0 { return .vector(Vector(values: [])) }
            if count == 1 { return .vector(Vector(values: [start]))}
            
            let step = (end - start) / Double(count - 1)
            for i in 0..<count {
                values.append(start + step * Double(i))
            }
            return .vector(Vector(values: values))
        },
        "random": { args in
            switch args.count {
            case 0:
                return .scalar(Double.random(in: 0...1))
            case 1:
                let max = try args[0].asScalar()
                guard max >= 1, max.truncatingRemainder(dividingBy: 1) == 0 else { throw MathError.unsupportedOperation(op: "random", typeA: "max must be an integer >= 1", typeB: nil)}
                return .scalar(Double(Int.random(in: 1...Int(max))))
            case 2:
                let min = try args[0].asScalar()
                let max = try args[1].asScalar()
                guard min <= max, min.truncatingRemainder(dividingBy: 1) == 0, max.truncatingRemainder(dividingBy: 1) == 0 else { throw MathError.unsupportedOperation(op: "random", typeA: "min and max must be integers where min <= max", typeB: nil)}
                return .scalar(Double(Int.random(in: Int(min)...Int(max))))
            case 3:
                let min = try args[0].asScalar()
                let max = try args[1].asScalar()
                let count = try args[2].asScalar()
                guard min <= max, min.truncatingRemainder(dividingBy: 1) == 0, max.truncatingRemainder(dividingBy: 1) == 0 else { throw MathError.unsupportedOperation(op: "random", typeA: "min and max must be integers where min <= max", typeB: nil)}
                guard count >= 1, count.truncatingRemainder(dividingBy: 1) == 0 else { throw MathError.unsupportedOperation(op: "random", typeA: "count must be an integer >= 1", typeB: nil)}
                let values = (0..<Int(count)).map { _ in Double(Int.random(in: Int(min)...Int(max))) }
                return .vector(Vector(values: values))
            default:
                throw MathError.incorrectArgumentCount(function: "random", expected: "0, 1, 2, or 3", found: args.count)
            }
        }
    ]

    private let singleArgumentFunctions: [String: (MathValue) throws -> MathValue] = [
        "abs": { arg in
            switch arg {
            case .scalar(let s): return .scalar(abs(s))
            case .complex(let c): return .scalar(c.abs())
            case .vector(let v): return .scalar(v.magnitude())
            default: throw MathError.typeMismatch(expected: "Scalar, Complex, or Vector", found: arg.typeName)
            }
        },
        "polar": { arg in
            guard case .complex(let c) = arg else { throw MathError.typeMismatch(expected: "Complex", found: arg.typeName) }
            return .polar(c)
        },
        "sqrt": { arg in
            if case .scalar(let s) = arg {
                if s < 0 { return .complex(Complex(real: s, imaginary: 0).sqrt()) }
                return .scalar(sqrt(s))
            } else if case .complex(let c) = arg {
                return .complex(c.sqrt())
            } else { throw MathError.typeMismatch(expected: "Scalar or Complex", found: arg.typeName) }
        },
        "round": { arg in
            guard case .scalar(let s) = arg else { throw MathError.typeMismatch(expected: "Scalar", found: arg.typeName) }
            return .scalar(round(s))
        },
        "floor": { arg in
            guard case .scalar(let s) = arg else { throw MathError.typeMismatch(expected: "Scalar", found: arg.typeName) }
            return .scalar(floor(s))
        },
        "ceil": { arg in
            guard case .scalar(let s) = arg else { throw MathError.typeMismatch(expected: "Scalar", found: arg.typeName) }
            return .scalar(ceil(s))
        },
        "fact": { arg in
            guard case .scalar(let s) = arg else { throw MathError.typeMismatch(expected: "Scalar", found: arg.typeName) }
            return .scalar(try factorial(s))
        },
        "det": { arg in
            switch arg {
            case .matrix(let m): return .scalar(try m.determinant())
            case .complexMatrix(let cm): return .complex(try cm.determinant())
            default: throw MathError.typeMismatch(expected: "Matrix or ComplexMatrix", found: arg.typeName)
            }
        },
        "inv": { arg in
            switch arg {
            case .matrix(let m): return .matrix(try m.inverse())
            case .complexMatrix(let cm): return .complexMatrix(try cm.inverse())
            default: throw MathError.typeMismatch(expected: "Matrix or ComplexMatrix", found: arg.typeName)
            }
        },
        "real": { arg in
            guard case .complex(let c) = arg else { throw MathError.typeMismatch(expected: "Complex", found: arg.typeName) }
            return .scalar(c.real)
        },
        "imag": { arg in
            guard case .complex(let c) = arg else { throw MathError.typeMismatch(expected: "Complex", found: arg.typeName) }
            return .scalar(c.imaginary)
        },
        "conj": { arg in
            guard case .complex(let c) = arg else { throw MathError.typeMismatch(expected: "Complex", found: arg.typeName) }
            return .complex(c.conjugate())
        },
        "area_circle": { arg in
            guard case .scalar(let r) = arg else { throw MathError.typeMismatch(expected: "Scalar", found: arg.typeName) }
            return .scalar(Double.pi * r * r)
        },
        "circum_circle": { arg in
            guard case .scalar(let r) = arg else { throw MathError.typeMismatch(expected: "Scalar", found: arg.typeName) }
            return .scalar(2 * Double.pi * r)
        },
        "vol_sphere": { arg in
            guard case .scalar(let r) = arg else { throw MathError.typeMismatch(expected: "Scalar", found: arg.typeName) }
            return .scalar((4.0/3.0) * Double.pi * pow(r, 3))
        },
        "vol_cube": { arg in
            guard case .scalar(let s) = arg else { throw MathError.typeMismatch(expected: "Scalar", found: arg.typeName) }
            return .scalar(pow(s, 3))
        },
        "unit": { arg in
            guard case .vector(let v) = arg else { throw MathError.typeMismatch(expected: "Vector", found: arg.typeName) }
            return .vector(v.unit())
        },
        "transpose": { arg in
            switch arg {
            case .matrix(let m): return .matrix(m.transpose())
            case .complexMatrix(let cm): return .complexMatrix(cm.transpose())
            default: throw MathError.typeMismatch(expected: "Matrix", found: arg.typeName)
            }
        },
        "trace": { arg in
            switch arg {
            case .matrix(let m): return .scalar(try m.trace())
            case .complexMatrix(let cm): return .complex(try cm.trace())
            default: throw MathError.typeMismatch(expected: "Matrix or ComplexMatrix", found: arg.typeName)
            }
        }
    ]
    
    private let angleAwareFunctions: [String: (MathValue, AngleMode) throws -> MathValue] = [
        "sin": { arg, mode in
            guard case .scalar(let s) = arg else { throw MathError.typeMismatch(expected: "Scalar", found: arg.typeName) }
            return .scalar(sin(mode == .degrees ? s * .pi / 180 : s))
        },
        "cos": { arg, mode in
            guard case .scalar(let s) = arg else { throw MathError.typeMismatch(expected: "Scalar", found: arg.typeName) }
            return .scalar(cos(mode == .degrees ? s * .pi / 180 : s))
        },
        "tan": { arg, mode in
            guard case .scalar(let s) = arg else { throw MathError.typeMismatch(expected: "Scalar", found: arg.typeName) }
            return .scalar(tan(mode == .degrees ? s * .pi / 180 : s))
        },
        "asin": { arg, mode in
            guard case .scalar(let s) = arg else { throw MathError.typeMismatch(expected: "Scalar", found: arg.typeName) }
            let angle = asin(s)
            return .scalar(mode == .degrees ? angle * 180 / .pi : angle)
        },
        "acos": { arg, mode in
            guard case .scalar(let s) = arg else { throw MathError.typeMismatch(expected: "Scalar", found: arg.typeName) }
            let angle = acos(s)
            return .scalar(mode == .degrees ? angle * 180 / .pi : angle)
        },
        "atan": { arg, mode in
            guard case .scalar(let s) = arg else { throw MathError.typeMismatch(expected: "Scalar", found: arg.typeName) }
            let angle = atan(s)
            return .scalar(mode == .degrees ? angle * 180 / .pi : angle)
        },
        "arg": { arg, mode in
            guard case .complex(let c) = arg else { throw MathError.typeMismatch(expected: "Complex", found: arg.typeName) }
            let angle = c.argument()
            return .scalar(mode == .degrees ? angle * 180 / .pi : angle)
        },
        "angle": { arg, mode in
             guard case .tuple(let values) = arg, values.count == 2,
                  case .vector(let v1) = values[0], case .vector(let v2) = values[1] else {
                throw MathError.typeMismatch(expected: "Two Vectors", found: "other")
            }
            let angleRad = try v1.angle(with: v2)
            return .scalar(mode == .degrees ? angleRad * 180 / .pi : angleRad)
        }
    ]
    
    private let twoArgumentFunctions: [String: (MathValue, MathValue) throws -> MathValue] = [
        "linreg": { a, b in
            guard case .vector(let xVec) = a, case .vector(let yVec) = b else {
                throw MathError.typeMismatch(expected: "Two Vectors", found: "\(a.typeName), \(b.typeName)")
            }
            guard xVec.dimension == yVec.dimension, xVec.dimension >= 2 else {
                throw MathError.dimensionMismatch(reason: "Vectors must have the same number of elements (at least 2) for linear regression.")
            }
            let n = Double(xVec.dimension)
            let sumX = xVec.sum()
            let sumY = yVec.sum()
            let sumXY = try xVec.hadamard(with: yVec).sum()
            let sumX2 = xVec.values.map { $0 * $0 }.reduce(0, +)

            let denominator = (n * sumX2 - sumX * sumX)
            guard denominator != 0 else { throw MathError.unsupportedOperation(op: "linreg", typeA: "Cannot perform regression on vertical line (undefined slope)", typeB: nil) }
            
            let slope = (n * sumXY - sumX * sumY) / denominator
            let intercept = (sumY - slope * sumX) / n

            return .regressionResult(slope: slope, intercept: intercept)
        },
        "dot": { a, b in
            if case .vector(let v1) = a, case .vector(let v2) = b {
                return .scalar(try v1.dot(with: v2))
            } else if case .complexVector(let v1) = a, case .complexVector(let v2) = b {
                return .complex(try v1.dot(with: v2))
            }
            throw MathError.typeMismatch(expected: "Two Vectors or Two ComplexVectors", found: "\(a.typeName), \(b.typeName)")
        },
        "cross": { a, b in
            guard case .vector(let v1) = a, case .vector(let v2) = b else { throw MathError.typeMismatch(expected: "Two 3D Vectors", found: "\(a.typeName), \(b.typeName)") }
            return .vector(try v1.cross(with: v2))
        },
        "nPr": { a, b in
            guard case .scalar(let n) = a, case .scalar(let k) = b else { throw MathError.typeMismatch(expected: "Two Scalars", found: "\(a.typeName), \(b.typeName)") }
            return .scalar(try permutations(n: n, k: k))
        },
        "nCr": { a, b in
            guard case .scalar(let n) = a, case .scalar(let k) = b else { throw MathError.typeMismatch(expected: "Two Scalars", found: "\(a.typeName), \(b.typeName)") }
            return .scalar(try combinations(n: n, k: k))
        },
        "hypot": { a, b in
            guard case .scalar(let s1) = a, case .scalar(let s2) = b else { throw MathError.typeMismatch(expected: "Two Scalars", found: "\(a.typeName), \(b.typeName)") }
            return .scalar(Foundation.sqrt(s1*s1 + s2*s2))
        },
        "side": { a, b in
            guard case .scalar(let c) = a, case .scalar(let s) = b else { throw MathError.typeMismatch(expected: "Two Scalars (hyp, side)", found: "\(a.typeName), \(b.typeName)") }
            guard c >= s else { throw MathError.unsupportedOperation(op: "side", typeA: "hyp < side", typeB: nil) }
            return .scalar(Foundation.sqrt(c*c - s*s))
        },
        "area_rect": { a, b in
            guard case .scalar(let w) = a, case .scalar(let h) = b else { throw MathError.typeMismatch(expected: "Two Scalars (width, height)", found: "\(a.typeName), \(b.typeName)") }
            return .scalar(w * h)
        },
        "area_tri": { a, b in
            guard case .scalar(let base) = a, case .scalar(let h) = b else { throw MathError.typeMismatch(expected: "Two Scalars (base, height)", found: "\(a.typeName), \(b.typeName)") }
            return .scalar(0.5 * base * h)
        },
        "vol_cylinder": { a, b in
            guard case .scalar(let r) = a, case .scalar(let h) = b else { throw MathError.typeMismatch(expected: "Two Scalars (radius, height)", found: "\(a.typeName), \(b.typeName)") }
            return .scalar(Double.pi * r * r * h)
        },
        "vol_cone": { a, b in
            guard case .scalar(let r) = a, case .scalar(let h) = b else { throw MathError.typeMismatch(expected: "Two Scalars (radius, height)", found: "\(a.typeName), \(b.typeName)") }
            return .scalar((1.0/3.0) * Double.pi * r * r * h)
        },
        "root": { a, b in
                guard case .scalar(let x) = a, case .scalar(let n) = b else {
                    throw MathError.typeMismatch(expected: "Two Scalars", found: "\(a.typeName), \(b.typeName)")
                }
                if x < 0 && n.truncatingRemainder(dividingBy: 2) == 0 {
                    let real = pow(abs(x), 1/n)
                    return .complex(Complex(real: 0, imaginary: real))
                }
                return .scalar(pow(x, 1/n))
            }
    ]

    private func evaluateWithTempVar(node: ExpressionNode, varName: String, varValue: Double, variables: inout [String: MathValue], functions: inout [String: FunctionDefinitionNode], angleMode: AngleMode) throws -> MathValue {
        var tempVars = variables
        tempVars[varName] = .scalar(varValue)
        let (result, _) = try _evaluateSingle(node: node, variables: &tempVars, functions: &functions, angleMode: angleMode)
        return result
    }
    
    func evaluate(node: ExpressionNode, variables: inout [String: MathValue], functions: inout [String: FunctionDefinitionNode], angleMode: AngleMode) throws -> (result: MathValue, usedAngle: Bool) {
        let (results, usedAngle) = try evaluateAndExpand(node: node, variables: &variables, functions: &functions, angleMode: angleMode)
        if results.count == 1 {
            return (results[0], usedAngle)
        } else {
            return (.tuple(results), usedAngle)
        }
    }
    
    private func evaluateAndExpand(node: ExpressionNode, variables: inout [String: MathValue], functions: inout [String: FunctionDefinitionNode], angleMode: AngleMode) throws -> (results: [MathValue], usedAngle: Bool) {
        if let binaryNode = node as? BinaryOpNode, binaryNode.op.rawValue == "±" {
            let (leftResults, leftUsedAngle) = try evaluateAndExpand(node: binaryNode.left, variables: &variables, functions: &functions, angleMode: angleMode)
            let (rightResults, rightUsedAngle) = try evaluateAndExpand(node: binaryNode.right, variables: &variables, functions: &functions, angleMode: angleMode)
            
            var combinedResults: [MathValue] = []
            for l in leftResults {
                for r in rightResults {
                    combinedResults.append(try evaluateBinaryOperation(op: Token(type: .op("+"), rawValue: "+"), left: l, right: r))
                    combinedResults.append(try evaluateBinaryOperation(op: Token(type: .op("-"), rawValue: "-"), left: l, right: r))
                }
            }
            return (combinedResults, leftUsedAngle || rightUsedAngle)
        }
        
        let (result, nodeUsedAngle) = try _evaluateSingle(node: node, variables: &variables, functions: &functions, angleMode: angleMode)
        return ([result], nodeUsedAngle)
    }

    private func _evaluateSingle(node: ExpressionNode, variables: inout [String: MathValue], functions: inout [String: FunctionDefinitionNode], angleMode: AngleMode) throws -> (result: MathValue, usedAngle: Bool) {
        var usedAngle = false
        
        switch node {
        case let numberNode as NumberNode:
            return (.scalar(numberNode.value), usedAngle)
        
        case let constantNode as ConstantNode:
            if constantNode.name == "i" { return (.complex(Complex.i), usedAngle) }
            if let value = variables[constantNode.name] { return (value, usedAngle) }
            else if let value = siPrefixes[constantNode.name] { return (.scalar(value), usedAngle) }
            else if let value = constants[constantNode.name] { return (.scalar(value), usedAngle) }
            else { throw MathError.unknownConstant(name: constantNode.name) }
            
        case let assignmentNode as AssignmentNode:
            let (value, valueUsedAngle) = try evaluate(node: assignmentNode.expression, variables: &variables, functions: &functions, angleMode: angleMode)
            variables[assignmentNode.name] = value
            return (value, valueUsedAngle)

        case let funcDefNode as FunctionDefinitionNode:
            functions[funcDefNode.name] = funcDefNode
            return (.functionDefinition(funcDefNode.name), usedAngle)
            
        case let functionNode as FunctionCallNode:
            return try evaluateFunctionCall(functionNode, variables: &variables, functions: &functions, angleMode: angleMode)
            
        case let vectorNode as VectorNode:
            var elements: [Double] = []
            for elementNode in vectorNode.elements {
                let (evaluatedElement, elementUsedAngle) = try _evaluateSingle(node: elementNode, variables: &variables, functions: &functions, angleMode: angleMode)
                usedAngle = usedAngle || elementUsedAngle
                guard case .scalar(let scalarElement) = evaluatedElement else { throw MathError.typeMismatch(expected: "Scalar", found: "Non-scalar in vector definition") }
                elements.append(scalarElement)
            }
            return (.vector(Vector(values: elements)), usedAngle)
            
        case let matrixNode as MatrixNode:
            var values: [Double] = []
            let rows = matrixNode.rows.count; let columns = matrixNode.rows.first?.count ?? 0
            for row in matrixNode.rows {
                for elementNode in row {
                    let (evaluatedElement, elementUsedAngle) = try _evaluateSingle(node: elementNode, variables: &variables, functions: &functions, angleMode: angleMode)
                    usedAngle = usedAngle || elementUsedAngle
                    guard case .scalar(let scalarElement) = evaluatedElement else { throw MathError.typeMismatch(expected: "Scalar", found: "Non-scalar in matrix definition") }
                    values.append(scalarElement)
                }
            }
            return (.matrix(Matrix(values: values, rows: rows, columns: columns)), usedAngle)

        case let cVectorNode as ComplexVectorNode:
            var elements: [Complex] = []
            for elementNode in cVectorNode.elements {
                let (evaluatedElement, elementUsedAngle) = try _evaluateSingle(node: elementNode, variables: &variables, functions: &functions, angleMode: angleMode)
                usedAngle = usedAngle || elementUsedAngle
                switch evaluatedElement {
                case .complex(let c): elements.append(c)
                case .scalar(let s): elements.append(Complex(real: s, imaginary: 0))
                default: throw MathError.typeMismatch(expected: "Complex or Scalar", found: evaluatedElement.typeName)
                }
            }
            return (.complexVector(ComplexVector(values: elements)), usedAngle)

        case let cMatrixNode as ComplexMatrixNode:
            var values: [Complex] = []; let rows = cMatrixNode.rows.count; let columns = cMatrixNode.rows.first?.count ?? 0
            for row in cMatrixNode.rows {
                for elementNode in row {
                    let (evaluatedElement, elementUsedAngle) = try _evaluateSingle(node: elementNode, variables: &variables, functions: &functions, angleMode: angleMode)
                    usedAngle = usedAngle || elementUsedAngle
                    switch evaluatedElement {
                    case .complex(let c): values.append(c)
                    case .scalar(let s): values.append(Complex(real: s, imaginary: 0))
                    default: throw MathError.typeMismatch(expected: "Complex or Scalar", found: evaluatedElement.typeName)
                    }
                }
            }
            return (.complexMatrix(ComplexMatrix(values: values, rows: rows, columns: columns)), usedAngle)

        case let unaryNode as UnaryOpNode:
            let (childValue, childUsedAngle) = try _evaluateSingle(node: unaryNode.child, variables: &variables, functions: &functions, angleMode: angleMode)
            let result = try evaluateUnaryOperation(op: unaryNode.op, value: childValue)
            return (result, childUsedAngle)
            
        case let postfixNode as PostfixOpNode:
             let (childValue, childUsedAngle) = try _evaluateSingle(node: postfixNode.child, variables: &variables, functions: &functions, angleMode: angleMode)
             let result = try evaluateUnaryOperation(op: postfixNode.op, value: childValue)
             return (result, childUsedAngle)

        case let binaryNode as BinaryOpNode:
            if binaryNode.op.rawValue == "∠" {
                let (rValue, rUsedAngle) = try _evaluateSingle(node: binaryNode.left, variables: &variables, functions: &functions, angleMode: angleMode)
                let (thetaValue, thetaUsedAngle) = try _evaluateSingle(node: binaryNode.right, variables: &variables, functions: &functions, angleMode: angleMode)
                usedAngle = rUsedAngle || thetaUsedAngle
                guard case .scalar(let r) = rValue, case .scalar(let theta) = thetaValue else { throw MathError.typeMismatch(expected: "Scalar ∠ Scalar", found: "\(rValue.typeName) ∠ \(thetaValue.typeName)") }
                let thetaRad = angleMode == .degrees ? theta * .pi / 180.0 : theta
                return (.complex(Complex(real: r * cos(thetaRad), imaginary: r * sin(thetaRad))), true)
            }
            
            let (leftValue, leftUsedAngle) = try _evaluateSingle(node: binaryNode.left, variables: &variables, functions: &functions, angleMode: angleMode)
            
            if let indexedOpNode = binaryNode.right as? IndexedOperationNode {
                return try evaluateIndexedAssignment(op: binaryNode.op, target: leftValue, indexedOp: indexedOpNode, variables: &variables, functions: &functions, angleMode: angleMode)
            }

            let (rightValue, rightUsedAngle) = try _evaluateSingle(node: binaryNode.right, variables: &variables, functions: &functions, angleMode: angleMode)
            let result = try evaluateBinaryOperation(op: binaryNode.op, left: leftValue, right: rightValue)
            return (result, leftUsedAngle || rightUsedAngle)

        case let derivativeNode as DerivativeNode:
            let (bodyNode, varName): (ExpressionNode, String)
            let pointNode = derivativeNode.point

            if let variableNode = derivativeNode.variable {
                // This handles the 3 and 4-argument calls, e.g., derivative(x^2, x, 2)
                bodyNode = derivativeNode.body
                varName = variableNode.name
            } else {
                // This handles the 2-argument calls, e.g., derivative(f, 2) or derivative(f(x), 2)
                let funcName: String
                
                // Determine the function name from the body node
                if let funcNameNode = derivativeNode.body as? ConstantNode {
                    funcName = funcNameNode.name
                } else if let funcCallNode = derivativeNode.body as? FunctionCallNode {
                    // If the user writes derivative(f(x), 2), we use 'f' as the function name.
                    funcName = funcCallNode.name
                } else {
                    // If it's a raw expression like derivative(x^2, 2), we can't infer the variable.
                    throw MathError.incorrectArgumentCount(function: "derivative", expected: "3 arguments (expression, variable, point) for this type of expression", found: 2)
                }
                
                guard let userFunction = functions[funcName] else {
                    throw MathError.unknownFunction(name: funcName)
                }
                guard userFunction.parameterNames.count == 1 else {
                    throw MathError.unsupportedOperation(op: "derivative", typeA: "function with \(userFunction.parameterNames.count) variables in 2-argument form", typeB: nil)
                }
                bodyNode = userFunction.body
                varName = userFunction.parameterNames[0]
            }

            let (pointValue, pointUsedAngle) = try _evaluateSingle(node: pointNode, variables: &variables, functions: &functions, angleMode: angleMode)
            let (orderValue, _) = try _evaluateSingle(node: derivativeNode.order, variables: &variables, functions: &functions, angleMode: angleMode)

            guard case .scalar(let point) = pointValue else {
                throw MathError.typeMismatch(expected: "Scalar for differentiation point", found: pointValue.typeName)
            }
            guard case .scalar(let orderScalar) = orderValue, orderScalar >= 1, orderScalar.truncatingRemainder(dividingBy: 1) == 0 else {
                throw MathError.typeMismatch(expected: "Positive integer for derivative order", found: orderValue.typeName)
            }
            let order = Int(orderScalar)

            var tempVarsForDryRun = variables
            tempVarsForDryRun[varName] = .scalar(0)
            let bodyUsedAngle = (try? _evaluateSingle(node: bodyNode, variables: &tempVarsForDryRun, functions: &functions, angleMode: angleMode))?.usedAngle ?? false

            func calculateNthDerivative(node: ExpressionNode, varName: String, at a: Double, order n: Int) throws -> Double {
                if n == 1 {
                    let valPlus = try evaluateWithTempVar(node: node, varName: varName, varValue: a + h, variables: &variables, functions: &functions, angleMode: angleMode)
                    let valMinus = try evaluateWithTempVar(node: node, varName: varName, varValue: a - h, variables: &variables, functions: &functions, angleMode: angleMode)
                    guard case .scalar(let scalarPlus) = valPlus, case .scalar(let scalarMinus) = valMinus else {
                        throw MathError.typeMismatch(expected: "Scalar expression for differentiation", found: "Non-scalar")
                    }
                    return (scalarPlus - scalarMinus) / (2 * h)
                }
                
                let f_prime_at_a_plus_h = try calculateNthDerivative(node: node, varName: varName, at: a + h, order: n - 1)
                let f_prime_at_a_minus_h = try calculateNthDerivative(node: node, varName: varName, at: a - h, order: n - 1)
                
                return (f_prime_at_a_plus_h - f_prime_at_a_minus_h) / (2 * h)
            }
            
            let result = try calculateNthDerivative(node: bodyNode, varName: varName, at: point, order: order)
            return (.scalar(result), pointUsedAngle || bodyUsedAngle)

        case let integralNode as IntegralNode:
            let (lowerValue, lowerUsedAngle) = try _evaluateSingle(node: integralNode.lowerBound, variables: &variables, functions: &functions, angleMode: angleMode)
            let (upperValue, upperUsedAngle) = try _evaluateSingle(node: integralNode.upperBound, variables: &variables, functions: &functions, angleMode: angleMode)

            guard case .scalar(let a) = lowerValue, case .scalar(let b) = upperValue else {
                throw MathError.typeMismatch(expected: "Scalar for integration bounds", found: "Non-scalar")
            }

            var tempVarsForDryRun = variables
            tempVarsForDryRun[integralNode.variable.name] = .scalar(0)
            let bodyUsedAngle = (try? _evaluateSingle(node: integralNode.body, variables: &tempVarsForDryRun, functions: &functions, angleMode: angleMode))?.usedAngle ?? false

            let capturedVariables = variables
            let capturedFunctions = functions

            let f: (Double) throws -> Double = { x in
                var localVars = capturedVariables
                var localFuncs = capturedFunctions
                let value = try self.evaluateWithTempVar(
                    node: integralNode.body,
                    varName: integralNode.variable.name,
                    varValue: x,
                    variables: &localVars,
                    functions: &localFuncs,
                    angleMode: angleMode
                )
                guard case .scalar(let scalarValue) = value else {
                    throw MathError.typeMismatch(expected: "Scalar expression for integration", found: value.typeName)
                }
                return scalarValue
            }
                
            let tolerance = 1e-7
            let result = try adaptiveSimpson(f: f, a: a, b: b, tolerance: tolerance)
                    
            return (.scalar(result), lowerUsedAngle || upperUsedAngle || bodyUsedAngle)


        case let primeNode as PrimeDerivativeNode:
            guard let userFunction = functions[primeNode.functionName] else {
                throw MathError.unknownFunction(name: primeNode.functionName)
            }
            guard userFunction.parameterNames.count == 1 else {
                throw MathError.incorrectArgumentCount(function: "\(primeNode.functionName)'", expected: "1", found: userFunction.parameterNames.count)
            }
            let varName = userFunction.parameterNames[0]

            let (pointValue, pointUsedAngle) = try _evaluateSingle(node: primeNode.argument, variables: &variables, functions: &functions, angleMode: angleMode)
            guard case .scalar(let point) = pointValue else {
                throw MathError.typeMismatch(expected: "Scalar for differentiation point", found: pointValue.typeName)
            }

            var tempVars = variables
            tempVars[varName] = .scalar(point)
            let bodyUsedAngle = (try? _evaluateSingle(node: userFunction.body, variables: &tempVars, functions: &functions, angleMode: angleMode))?.usedAngle ?? false

            let valPlus = try evaluateWithTempVar(node: userFunction.body, varName: varName, varValue: point + h, variables: &variables, functions: &functions, angleMode: angleMode)
            let valMinus = try evaluateWithTempVar(node: userFunction.body, varName: varName, varValue: point - h, variables: &variables, functions: &functions, angleMode: angleMode)
            
            guard case .scalar(let scalarPlus) = valPlus, case .scalar(let scalarMinus) = valMinus else {
                throw MathError.typeMismatch(expected: "Scalar function for differentiation", found: "Non-scalar")
            }

            let derivative = (scalarPlus - scalarMinus) / (2 * h)
            return (.scalar(derivative), pointUsedAngle || bodyUsedAngle)
            
        case let plotNode as PlotNode:
            return try (evaluatePlot(plotNode, variables: &variables, functions: &functions, angleMode: angleMode), false)


        default:
            throw MathError.invalidNode
        }
    }

    private func evaluateFunctionCall(_ node: FunctionCallNode, variables: inout [String: MathValue], functions: inout [String: FunctionDefinitionNode], angleMode: AngleMode) throws -> (result: MathValue, usedAngle: Bool) {
        var usedAngle = false
        
        if node.name == "grad" {
            return try evaluateGradFunction(node, variables: &variables, functions: &functions, angleMode: angleMode)
        }
        
        if node.name == "angle" {
            guard node.arguments.count == 2 else {
                throw MathError.incorrectArgumentCount(function: "angle", expected: "2", found: node.arguments.count)
            }
            let (arg1, arg1UsedAngle) = try _evaluateSingle(node: node.arguments[0], variables: &variables, functions: &functions, angleMode: angleMode)
            let (arg2, arg2UsedAngle) = try _evaluateSingle(node: node.arguments[1], variables: &variables, functions: &functions, angleMode: angleMode)
            let result = try angleAwareFunctions["angle"]?(.tuple([arg1, arg2]), angleMode)
            return (result!, true || arg1UsedAngle || arg2UsedAngle)
        }

        if let variadicFunc = variadicFunctions[node.name] {
            var args: [MathValue] = []; for argNode in node.arguments {
                let (arg, argUsedAngle) = try _evaluateSingle(node: argNode, variables: &variables, functions: &functions, angleMode: angleMode)
                usedAngle = usedAngle || argUsedAngle; args.append(arg)
            }
            return (try variadicFunc(args), usedAngle)
        }
        
        if let multiArgFunc = multiArgumentFunctions[node.name] {
            var args: [MathValue] = []; for argNode in node.arguments {
                let (arg, argUsedAngle) = try _evaluateSingle(node: argNode, variables: &variables, functions: &functions, angleMode: angleMode)
                usedAngle = usedAngle || argUsedAngle; args.append(arg)
            }
            return (try multiArgFunc(args), usedAngle)
        }
        
        if let singleArgFunc = singleArgumentFunctions[node.name] {
            guard node.arguments.count == 1 else { throw MathError.incorrectArgumentCount(function: node.name, expected: "1", found: node.arguments.count) }
            let (arg, argUsedAngle) = try _evaluateSingle(node: node.arguments[0], variables: &variables, functions: &functions, angleMode: angleMode)
            return (try singleArgFunc(arg), argUsedAngle)
        }
        
        if let twoArgFunc = twoArgumentFunctions[node.name] {
            guard node.arguments.count == 2 else { throw MathError.incorrectArgumentCount(function: node.name, expected: "2", found: node.arguments.count) }
            let (arg1, arg1UsedAngle) = try _evaluateSingle(node: node.arguments[0], variables: &variables, functions: &functions, angleMode: angleMode)
            let (arg2, arg2UsedAngle) = try _evaluateSingle(node: node.arguments[1], variables: &variables, functions: &functions, angleMode: angleMode)
            return (try twoArgFunc(arg1, arg2), arg1UsedAngle || arg2UsedAngle)
        }
        
        if let angleFunc = angleAwareFunctions[node.name] {
            guard node.arguments.count == 1 else { throw MathError.incorrectArgumentCount(function: node.name, expected: "1", found: node.arguments.count) }
            let (arg, argUsedAngle) = try _evaluateSingle(node: node.arguments[0], variables: &variables, functions: &functions, angleMode: angleMode)
            return (try angleFunc(arg, angleMode), true || argUsedAngle)
        }

        if let scalarFunc = scalarFunctions[node.name] {
            guard node.arguments.count == 1 else { throw MathError.incorrectArgumentCount(function: node.name, expected: "1", found: node.arguments.count) }
            let (arg, argUsedAngle) = try _evaluateSingle(node: node.arguments[0], variables: &variables, functions: &functions, angleMode: angleMode)
            guard case .scalar(let s) = arg else { throw MathError.typeMismatch(expected: "Scalar", found: arg.typeName) }
            return (.scalar(scalarFunc(s)), argUsedAngle)
        }
        
        if let userFunction = functions[node.name] {
            // Check for vectorization: single-parameter function called with a single vector argument
            if userFunction.parameterNames.count == 1 && node.arguments.count == 1 {
                let (argValue, argUsedAngle) = try evaluate(node: node.arguments[0], variables: &variables, functions: &functions, angleMode: angleMode)
                if case .vector(let v) = argValue {
                    var resultValues: [Double] = []
                    var overallUsedAngle = argUsedAngle
                    for element in v.values {
                        var localVariables = variables
                        localVariables[userFunction.parameterNames[0]] = .scalar(element)
                        let (result, elementUsedAngle) = try evaluate(node: userFunction.body, variables: &localVariables, functions: &functions, angleMode: angleMode)
                        guard case .scalar(let scalarResult) = result else {
                            throw MathError.typeMismatch(expected: "Scalar result from vectorized function", found: result.typeName)
                        }
                        resultValues.append(scalarResult)
                        overallUsedAngle = overallUsedAngle || elementUsedAngle
                    }
                    return (.vector(Vector(values: resultValues)), overallUsedAngle)
                }
            }
            
            // Standard function call
            guard node.arguments.count == userFunction.parameterNames.count else { throw MathError.incorrectArgumentCount(function: node.name, expected: "\(userFunction.parameterNames.count)", found: node.arguments.count) }
            var localVariables = variables
            for (paramName, argNode) in zip(userFunction.parameterNames, node.arguments) {
                let (argValue, _) = try evaluate(node: argNode, variables: &variables, functions: &functions, angleMode: angleMode)
                localVariables[paramName] = argValue
            }
            return try evaluate(node: userFunction.body, variables: &localVariables, functions: &functions, angleMode: angleMode)
        }
        
        throw MathError.unknownFunction(name: node.name)
    }
    
    private func evaluateGradFunction(_ node: FunctionCallNode, variables: inout [String: MathValue], functions: inout [String: FunctionDefinitionNode], angleMode: AngleMode) throws -> (result: MathValue, usedAngle: Bool) {
        guard node.arguments.count == 2 else {
            throw MathError.incorrectArgumentCount(function: "grad", expected: "2", found: node.arguments.count)
        }

        // First argument must be a function name, not evaluated.
        guard let funcNameNode = node.arguments[0] as? ConstantNode else {
            throw MathError.typeMismatch(expected: "User function name for gradient", found: node.arguments[0].description)
        }
        let funcName = funcNameNode.name

        // Look up the function definition.
        guard let userFunction = functions[funcName] else {
            throw MathError.unknownFunction(name: funcName)
        }
        let varNames = userFunction.parameterNames
        guard !varNames.isEmpty else {
            throw MathError.unsupportedOperation(op: "grad", typeA: "function with no parameters", typeB: nil)
        }

        // Second argument is the point vector, which needs evaluation.
        let (pointValue, pointUsedAngle) = try _evaluateSingle(node: node.arguments[1], variables: &variables, functions: &functions, angleMode: angleMode)
        guard case .vector(let pointVector) = pointValue else {
            throw MathError.typeMismatch(expected: "Vector for gradient point", found: pointValue.typeName)
        }
        guard pointVector.dimension == varNames.count else {
            throw MathError.dimensionMismatch(reason: "Point dimension (\(pointVector.dimension)) must match number of function variables (\(varNames.count)).")
        }

        // Create non-inout copies to be captured by the closure.
        let capturedVariables = variables
        let capturedFunctions = functions
        
        var partialDerivatives: [Double] = []
        
        for (i, _) in varNames.enumerated() {
            // To calculate the partial derivative with respect to varName, all other variables
            // must be fixed to their values from the pointVector.
            let pointForThisVar = pointVector.values[i]
            
            // This closure will evaluate the function with one variable perturbed.
            let f: (Double) throws -> Double = { x_perturbed in
                var tempVars = capturedVariables
                for (j, otherVarName) in varNames.enumerated() {
                    let value = (j == i) ? x_perturbed : pointVector.values[j]
                    tempVars[otherVarName] = .scalar(value)
                }
                var tempFuncs = capturedFunctions
                let result = try self._evaluateSingle(node: userFunction.body, variables: &tempVars, functions: &tempFuncs, angleMode: angleMode).result
                guard case .scalar(let scalarResult) = result else {
                    throw MathError.typeMismatch(expected: "Scalar expression for gradient calculation", found: result.typeName)
                }
                return scalarResult
            }
            
            // Central difference method
            let valPlus = try f(pointForThisVar + h)
            let valMinus = try f(pointForThisVar - h)
            
            let derivative = (valPlus - valMinus) / (2 * h)
            partialDerivatives.append(derivative)
        }
        
        return (.vector(Vector(values: partialDerivatives)), pointUsedAngle)
    }

    private func evaluateUnaryOperation(op: Token, value: MathValue) throws -> MathValue {
        switch op.rawValue {
        case "+": return value
        case "-":
            switch value {
            case .scalar(let s): return .scalar(-s)
            case .complex(let c): return .complex(c * -1.0)
            case .vector(let v): return .vector(Vector(values: v.values.map { -$0 }))
            case .matrix(let m): return .matrix(Matrix(values: m.values.map { -$0 }, rows: m.rows, columns: m.columns))
            case .complexVector(let cv): return .complexVector(ComplexVector(values: cv.values.map { $0 * -1.0 }))
            case .complexMatrix(let cm): return .complexMatrix(ComplexMatrix(values: cm.values.map { $0 * -1.0 }, rows: cm.rows, columns: cm.columns))
            default: throw MathError.unsupportedOperation(op: op.rawValue, typeA: value.typeName, typeB: nil)
            }
        case "'":
            switch value {
            case .matrix(let m): return .matrix(m.transpose())
            case .complexMatrix(let cm): return .complexMatrix(cm.conjugateTranspose())
            case .complexVector(let cv): return .complexMatrix(cv.conjugateTranspose())
            default: throw MathError.unsupportedOperation(op: "'", typeA: value.typeName, typeB: nil)
            }
        case "!":
            guard case .scalar(let s) = value else { throw MathError.typeMismatch(expected: "Scalar for factorial", found: value.typeName) }
            return .scalar(try factorial(s))
        default: throw MathError.unknownOperator(op: op.rawValue)
        }
    }

    private func evaluateBinaryOperation(op: Token, left: MathValue, right: MathValue) throws -> MathValue {
        if case .tuple = left { throw MathError.unsupportedOperation(op: op.rawValue, typeA: left.typeName, typeB: right.typeName) }
        if case .tuple = right { throw MathError.unsupportedOperation(op: op.rawValue, typeA: left.typeName, typeB: right.typeName) }
        
        switch (left, right) {
        case (.scalar(let l), .scalar(let r)): return .scalar(try performScalarScalarOp(op.rawValue, l, r))
        case (.complex(let l), .complex(let r)): return .complex(try performComplexComplexOp(op.rawValue, l, r))
        case (.complex(let l), .scalar(let r)): return .complex(try performComplexComplexOp(op.rawValue, l, Complex(real: r, imaginary: 0)))
        case (.scalar(let l), .complex(let r)): return .complex(try performComplexComplexOp(op.rawValue, Complex(real: l, imaginary: 0), r))
        case (.vector(let v), .scalar(let s)): return .vector(try performVectorScalarOp(op.rawValue, v, s))
        case (.scalar(let s), .vector(let v)): return .vector(try performVectorScalarOp(op.rawValue, v, s, reversed: true))
        case (.vector(let l), .vector(let r)): return .vector(try performVectorVectorOp(op.rawValue, l, r))
        case (.matrix(let m), .scalar(let s)): return .matrix(try performMatrixScalarOp(op.rawValue, m, s))
        case (.scalar(let s), .matrix(let m)): return .matrix(try performMatrixScalarOp(op.rawValue, m, s, reversed: true))
        case (.matrix(let l), .matrix(let r)): return .matrix(try performMatrixMatrixOp(op.rawValue, l, r))
        case (.matrix(let m), .vector(let v)):
             if op.rawValue == "*" { return .vector(try m * v) }
             else { throw MathError.unsupportedOperation(op: op.rawValue, typeA: left.typeName, typeB: right.typeName) }
        case (.complexVector(let cv), .complex(let c)): return .complexVector(try performComplexVectorComplexOp(op.rawValue, cv, c))
        case (.complex(let c), .complexVector(let cv)): return .complexVector(try performComplexVectorComplexOp(op.rawValue, cv, c, reversed: true))
        case (.complexVector(let l), .complexVector(let r)): return .complexVector(try performCVectorCVectorOp(op.rawValue, l, r))
        case (.complexMatrix(let cm), .complex(let c)): return .complexMatrix(try performComplexMatrixComplexOp(op.rawValue, cm, c))
        case (.complex(let c), .complexMatrix(let cm)): return .complexMatrix(try performComplexMatrixComplexOp(op.rawValue, cm, c, reversed: true))
        case (.complexMatrix(let l), .complexMatrix(let r)): return .complexMatrix(try performCMatrixCMatrixOp(op.rawValue, l, r))
        case (.complexMatrix(let m), .complexVector(let v)):
            if op.rawValue == "*" { return .complexVector(try m * v) }
            else { throw MathError.unsupportedOperation(op: op.rawValue, typeA: left.typeName, typeB: right.typeName) }
        case (.matrix, .complex), (.complex, .matrix), (.vector, .complex), (.complex, .vector), (.complexMatrix, .scalar), (.scalar, .complexMatrix), (.complexVector, .scalar), (.scalar, .complexVector):
            let (promotedL, promotedR) = try promote(left, right)
            return try evaluateBinaryOperation(op: op, left: promotedL, right: promotedR)
            
        default: throw MathError.unsupportedOperation(op: op.rawValue, typeA: left.typeName, typeB: right.typeName)
        }
    }
    
    private func evaluateIndexedAssignment(op: Token, target: MathValue, indexedOp: IndexedOperationNode, variables: inout [String: MathValue], functions: inout [String: FunctionDefinitionNode], angleMode: AngleMode) throws -> (MathValue, Bool) {
        
        let (indexValue, indexUsedAngle) = try _evaluateSingle(node: indexedOp.index, variables: &variables, functions: &functions, angleMode: angleMode)
        let (scalarValue, scalarUsedAngle) = try _evaluateSingle(node: indexedOp.scalar, variables: &variables, functions: &functions, angleMode: angleMode)
        
        guard case .scalar(let indexScalar) = indexValue, indexScalar.truncatingRemainder(dividingBy: 1) == 0 else {
            throw MathError.typeMismatch(expected: "Integer for index", found: indexValue.typeName)
        }
        let oneBasedIndex = Int(indexScalar)
        let zeroBasedIndex = oneBasedIndex - 1

        let result: MathValue
        switch target {
        case .vector(let v):
            guard case .scalar(let s) = scalarValue else { throw MathError.typeMismatch(expected: "Scalar", found: scalarValue.typeName) }
            let opFunction: (Double, Double) -> Double
            switch op.rawValue {
            case ".=@": opFunction = { _, s in s }
            case ".+@": opFunction = (+)
            case ".-@": opFunction = (-)
            case ".*@": opFunction = (*)
            case "./@":
                opFunction = { v, s in
                    guard s != 0 else { return .nan } // Will be caught by the check below
                    return v / s
                }
            default: throw MathError.unknownOperator(op: op.rawValue)
            }
            let modifiedVector = try v.modifying(at: zeroBasedIndex, with: s, operation: opFunction)
            if modifiedVector.values.contains(where: { $0.isNaN }) { throw MathError.divisionByZero }
            result = .vector(modifiedVector)
            
        case .complexVector(let cv):
            let complexScalar: Complex
            if case .scalar(let s) = scalarValue { complexScalar = Complex(real: s, imaginary: 0) }
            else if case .complex(let c) = scalarValue { complexScalar = c }
            else { throw MathError.typeMismatch(expected: "Scalar or Complex", found: scalarValue.typeName) }
            
            let opFunction: (Complex, Complex) throws -> Complex
            switch op.rawValue {
            case ".=@": opFunction = { _, s in s }
            case ".+@": opFunction = (+)
            case ".-@": opFunction = (-)
            case ".*@": opFunction = (*)
            case "./@": opFunction = { v, s in try v / s }
            default: throw MathError.unknownOperator(op: op.rawValue)
            }
            result = .complexVector(try cv.modifying(at: zeroBasedIndex, with: complexScalar, operation: opFunction))
            
        default:
            throw MathError.typeMismatch(expected: "Vector or ComplexVector", found: target.typeName)
        }
        
        return (result, indexUsedAngle || scalarUsedAngle)
    }

    private func performScalarScalarOp(_ op: String, _ l: Double, _ r: Double) throws -> Double {
        switch op {
        case "+": return l + r; case "-": return l - r; case "*": return l * r
        case "/": guard r != 0 else { throw MathError.divisionByZero }; return l / r
        case "%": guard r != 0 else { throw MathError.divisionByZero }; return l.truncatingRemainder(dividingBy: r)
        case "^": return pow(l, r)
        default: throw MathError.unknownOperator(op: op)
        }
    }
    private func performComplexComplexOp(_ op: String, _ l: Complex, _ r: Complex) throws -> Complex {
        switch op {
        case "+": return l + r; case "-": return l - r; case "*": return l * r
        case "/": return try l / r; case "^": return try l.pow(r)
        default: throw MathError.unknownOperator(op: op)
        }
    }
    private func performVectorScalarOp(_ op: String, _ v: Vector, _ s: Double, reversed: Bool = false) throws -> Vector {
        if reversed {
            switch op { case "+": return s + v; case "*": return s * v; case "-": return s - v
            case "/": guard !v.values.contains(0) else { throw MathError.divisionByZero }; return s / v
            case "^": throw MathError.unsupportedOperation(op: op, typeA: "Scalar", typeB: "Vector")
            default: throw MathError.unsupportedOperation(op: op, typeA: "Scalar", typeB: "Vector")
            }
        } else {
            switch op { case "+": return v + s; case "*": return v * s; case "-": return v - s
            case "/": guard s != 0 else { throw MathError.divisionByZero }; return v / s
            case "^": return Vector(values: v.values.map { pow($0, s) })
            default: throw MathError.unsupportedOperation(op: op, typeA: "Vector", typeB: "Scalar")
            }
        }
    }
    private func performVectorVectorOp(_ op: String, _ l: Vector, _ r: Vector) throws -> Vector {
        switch op {
        case "+": return try l + r; case "-": return try l - r
        case ".*": return try l.hadamard(with: r); case "./": return try l.hadamardDivision(with: r)
        case "^": return Vector(values: zip(l.values, r.values).map(pow))
        default: throw MathError.unsupportedOperation(op: op, typeA: "Vector", typeB: "Vector")
        }
    }
    private func performMatrixScalarOp(_ op: String, _ m: Matrix, _ s: Double, reversed: Bool = false) throws -> Matrix {
        let newValues: [Double]; switch op {
        case "+": newValues = m.values.map { $0 + s }; case "*": newValues = m.values.map { $0 * s }
        case "-": newValues = reversed ? m.values.map { s - $0 } : m.values.map { $0 - s }
        case "/":
            if reversed { throw MathError.unsupportedOperation(op: op, typeA: "Scalar", typeB: "Matrix") }
            guard s != 0 else { throw MathError.divisionByZero }; newValues = m.values.map { $0 / s }
        default: throw MathError.unsupportedOperation(op: op, typeA: "Matrix", typeB: "Scalar")
        }
        return Matrix(values: newValues, rows: m.rows, columns: m.columns)
    }
    private func performMatrixMatrixOp(_ op: String, _ l: Matrix, _ r: Matrix) throws -> Matrix {
        switch op {
        case "+": return try l + r; case "-": return try l - r; case "*": return try l * r
        case ".*": return try l.hadamard(with: r); case "./": return try l.hadamardDivision(with: r)
        default: throw MathError.unsupportedOperation(op: op, typeA: "Matrix", typeB: "Matrix")
        }
    }
    private func performComplexVectorComplexOp(_ op: String, _ v: ComplexVector, _ c: Complex, reversed: Bool = false) throws -> ComplexVector {
        if reversed {
            switch op {
            case "+": return ComplexVector(values: v.values.map { c + $0 }); case "*": return ComplexVector(values: v.values.map { c * $0 })
            case "-": return ComplexVector(values: v.values.map { c - $0 }); case "/": throw MathError.unsupportedOperation(op: op, typeA: "Complex", typeB: "ComplexVector")
            default: throw MathError.unsupportedOperation(op: op, typeA: "ComplexVector", typeB: "Complex")
            }
        } else {
            switch op { case "+": return v + c; case "*": return v * c; case "-": return v - c; case "/": return try v / c
            default: throw MathError.unsupportedOperation(op: op, typeA: "ComplexVector", typeB: "Complex")
            }
        }
    }
    private func performCVectorCVectorOp(_ op: String, _ l: ComplexVector, _ r: ComplexVector) throws -> ComplexVector {
        switch op { case "+": return try l + r; case "-": return try l - r
        default: throw MathError.unsupportedOperation(op: op, typeA: "ComplexVector", typeB: "ComplexVector")
        }
    }
    private func performComplexMatrixComplexOp(_ op: String, _ m: ComplexMatrix, _ c: Complex, reversed: Bool = false) throws -> ComplexMatrix {
         if reversed {
            switch op { case "+": return ComplexMatrix(values: m.values.map { c + $0 }, rows: m.rows, columns: m.columns)
            case "*": return ComplexMatrix(values: m.values.map { c * $0 }, rows: m.rows, columns: m.columns)
            case "-": return ComplexMatrix(values: m.values.map { c - $0 }, rows: m.rows, columns: m.columns)
            case "/": throw MathError.unsupportedOperation(op: op, typeA: "Complex", typeB: "ComplexMatrix")
            default: throw MathError.unsupportedOperation(op: op, typeA: "ComplexMatrix", typeB: "Complex")
            }
        } else {
            switch op { case "+": return m + c; case "*": return m * c; case "-": return m - c; case "/": return try m / c
            default: throw MathError.unsupportedOperation(op: op, typeA: "ComplexMatrix", typeB: "Complex")
            }
        }
    }
    private func performCMatrixCMatrixOp(_ op: String, _ l: ComplexMatrix, _ r: ComplexMatrix) throws -> ComplexMatrix {
        switch op { case "+": return try l + r; case "-": return try l - r; case "*": return try l * r
        default: throw MathError.unsupportedOperation(op: op, typeA: "ComplexMatrix", typeB: "ComplexMatrix")
        }
    }
    private func promote(_ left: MathValue, _ right: MathValue) throws -> (MathValue, MathValue) {
        switch (left, right) {
        case (.matrix(let m), .complex(let c)): return (.complexMatrix(ComplexMatrix(from: m)), .complex(c))
        case (.complex(let c), .matrix(let m)): return (.complex(c), .complexMatrix(ComplexMatrix(from: m)))
        case (.vector(let v), .complex(let c)): return (.complexVector(ComplexVector(from: v)), .complex(c))
        case (.complex(let c), .vector(let v)): return (.complex(c), .complexVector(ComplexVector(from: v)))
        case (.complexMatrix(let cm), .scalar(let s)): return (.complexMatrix(cm), .complex(Complex(real: s, imaginary: 0)))
        case (.scalar(let s), .complexMatrix(let cm)): return (.complex(Complex(real: s, imaginary: 0)), .complexMatrix(cm))
        case (.complexVector(let cv), .scalar(let s)): return (.complexVector(cv), .complex(Complex(real: s, imaginary: 0)))
        case (.scalar(let s), .complexVector(let cv)): return (.complex(Complex(real: s, imaginary: 0)), .complexVector(cv))
        default: return (left, right)
        }
    }
    
    private func adaptiveSimpson(f: (Double) throws -> Double, a: Double, b: Double, tolerance: Double) throws -> Double {
            let c = (a + b) / 2.0
            let h = b - a
            let fa = try f(a)
            let fb = try f(b)
            let fc = try f(c)
            let s = (h / 6.0) * (fa + 4.0 * fc + fb)
            return try _adaptiveSimpsonRecursive(f: f, a: a, b: b, fa: fa, fb: fb, fc: fc, whole: s, tolerance: tolerance)
        }

        private func _adaptiveSimpsonRecursive(f: (Double) throws -> Double, a: Double, b: Double, fa: Double, fb: Double, fc: Double, whole: Double, tolerance: Double) throws -> Double {
            let c = (a + b) / 2.0
            let d = (a + c) / 2.0
            let e = (c + b) / 2.0
            let fd = try f(d)
            let fe = try f(e)
            let left = (c - a) / 6.0 * (fa + 4.0 * fd + fc)
            let right = (b - c) / 6.0 * (fc + 4.0 * fe + fb)

            if abs(left + right - whole) <= 15.0 * tolerance {
                return left + right + (left + right - whole) / 15.0
            }

            let leftHalf = try _adaptiveSimpsonRecursive(f: f, a: a, b: c, fa: fa, fb: fc, fc: fd, whole: left, tolerance: tolerance / 2.0)
            let rightHalf = try _adaptiveSimpsonRecursive(f: f, a: c, b: b, fa: fc, fb: fb, fc: fe, whole: right, tolerance: tolerance / 2.0)
            
            return leftHalf + rightHalf
        }
    
    private func evaluatePlot(_ node: PlotNode, variables: inout [String: MathValue], functions: inout [String: FunctionDefinitionNode], angleMode: AngleMode) throws -> MathValue {
        
        let numPoints = 200
        let defaultRange = -10.0...10.0
        
        // TODO: Parse 'from', 'to', 'variable' arguments later.
        
        switch node.expressions.count {
        case 1: // Standard plot: plot(y(x))
            let body = node.expressions[0]
            let varName = "x" // Default variable
            
            var dataPoints: [DataPoint] = []
            let step = (defaultRange.upperBound - defaultRange.lowerBound) / Double(numPoints - 1)
            
            for i in 0..<numPoints {
                let x = defaultRange.lowerBound + Double(i) * step
                do {
                    let yValue = try evaluateWithTempVar(node: body, varName: varName, varValue: x, variables: &variables, functions: &functions, angleMode: angleMode)
                    if case .scalar(let y) = yValue {
                        dataPoints.append(DataPoint(x: x, y: y))
                    }
                } catch {
                    // Skip points where the function is undefined (e.g., division by zero)
                }
            }
            
            if dataPoints.isEmpty { throw MathError.plotError(reason: "Could not generate any data points for the expression.")}
            
            let plotData = PlotData(expression: node.description, dataPoints: dataPoints, plotType: .line)
            return .plot(plotData)
            
        case 2: // Parametric plot: plot(x(t), y(t))
            let xBody = node.expressions[0]
            let yBody = node.expressions[1]
            let varName = "t" // Default parameter
            
            var dataPoints: [DataPoint] = []
            let step = (defaultRange.upperBound - defaultRange.lowerBound) / Double(numPoints - 1)
            
            for i in 0..<numPoints {
                let t = defaultRange.lowerBound + Double(i) * step
                do {
                    let xValue = try evaluateWithTempVar(node: xBody, varName: varName, varValue: t, variables: &variables, functions: &functions, angleMode: angleMode)
                    let yValue = try evaluateWithTempVar(node: yBody, varName: varName, varValue: t, variables: &variables, functions: &functions, angleMode: angleMode)
                    
                    if case .scalar(let x) = xValue, case .scalar(let y) = yValue {
                        dataPoints.append(DataPoint(x: x, y: y))
                    }
                } catch {
                    // Skip points
                }
            }
            
            if dataPoints.isEmpty { throw MathError.plotError(reason: "Could not generate any data points for the parametric expressions.")}
            
            let plotData = PlotData(expression: node.description, dataPoints: dataPoints, plotType: .parametric)
            return .plot(plotData)

        default:
            throw MathError.incorrectArgumentCount(function: "plot", expected: "1 or 2", found: node.expressions.count)
        }
    }
}

private func performStatisticalOperation(args: [MathValue], on operation: (Vector) -> Double?) throws -> MathValue {
    if args.count == 1, case .vector(let v) = args[0] {
        guard let result = operation(v) else { throw MathError.unsupportedOperation(op: "Statistical", typeA: "Vector with zero or one elements", typeB: nil) }
        return .scalar(result)
    } else if args.count == 1, case .matrix(let m) = args[0] {
        let v = Vector(values: m.values)
        guard let result = operation(v) else { throw MathError.unsupportedOperation(op: "Statistical", typeA: "Matrix with zero or one elements", typeB: nil) }
        return .scalar(result)
    } else {
        var scalars: [Double] = []
        for arg in args {
            guard case .scalar(let s) = arg else { throw MathError.typeMismatch(expected: "Scalar arguments or a single Vector/Matrix", found: arg.typeName) }
            scalars.append(s)
        }
        guard !scalars.isEmpty else { throw MathError.requiresAtLeastOneArgument(function: "Statistical function") }
        let v = Vector(values: scalars)
        guard let result = operation(v) else { throw MathError.unsupportedOperation(op: "Statistical", typeA: "Scalar list", typeB: nil) }
        return .scalar(result)
    }
}
