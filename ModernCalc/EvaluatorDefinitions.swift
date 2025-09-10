import Foundation

/// This extension contains all the built-in constant and function definitions,
/// as well as the logic for handling operators and function calls.
extension Evaluator {
    
    // MARK: - Constants and Prefixes
    
    static let constants: [String: Double] = [
        "pi": Double.pi, "e": M_E, "c": 299792458, "μ0": 1.25663706212e-6, "ε0": 8.8541878128e-12,
        "g": 9.80665, "G": 6.67430e-11, "h": 6.62607015e-34, "ħ": 1.054571817e-34, "me": 9.1093837015e-31,
        "mp": 1.67262192369e-27, "mn": 1.67492749804e-27, "e0": 1.602176634e-19, "NA": 6.02214076e23,
        "R": 8.314462618, "kB": 1.380649e-23, "F": 96485.33212, "Rinf": 10973731.568160, "σ": 5.670374419e-8,
        "b": 2.897771955e-3, "atm": 101325, "Vm": 22.41396954e-3
    ]
    
    static let siPrefixes: [String: Double] = [
        "yotta": 1e24, "zetta": 1e21, "exa": 1e18, "peta": 1e15, "tera": 1e12, "giga": 1e9, "mega": 1e6,
        "kilo": 1e3, "hecto": 1e2, "deca": 1e1, "deci": 1e-1, "centi": 1e-2, "milli": 1e-3, "micro": 1e-6,
        "nano": 1e-9, "pico": 1e-12, "femto": 1e-15, "atto": 1e-18, "zepto": 1e-21, "yocto": 1e-24
    ]
    
    // MARK: - Function Dictionaries
    
    static let scalarFunctions: [String: (Double) -> Double] = [
        "ln": log, "lg": log10, "log": log10,
        "sinh": sinh, "cosh": cosh, "tanh": tanh,
        "asinh": asinh, "acosh": acosh, "atanh": atanh
    ]
    
    static let variadicFunctions: [String: ([MathValue]) throws -> MathValue] = [
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
    
    static let multiArgumentFunctions: [String: ([MathValue]) throws -> MathValue] = [
        "range": { args in
            guard args.count == 2 || args.count == 3 else { throw MathError.incorrectArgumentCount(function: "range", expected: "2 or 3", found: args.count) }
            let start = try args[0].asScalar()
            let end = try args[1].asScalar()
            let step = try args.count == 3 ? args[2].asScalar() : 1.0
            guard step != 0 else { throw MathError.unsupportedOperation(op: "range", typeA: "step cannot be zero", typeB: nil) }
            var values: [Double] = []; var current = start
            if step > 0 { while current <= end { values.append(current); current += step } }
            else { while current >= end { values.append(current); current += step } }
            return .vector(Vector(values: values))
        },
        "linspace": { args in
            guard args.count == 3 else { throw MathError.incorrectArgumentCount(function: "linspace", expected: "3", found: args.count) }
            let start = try args[0].asScalar()
            let end = try args[1].asScalar()
            let countScalar = try args[2].asScalar()
            guard countScalar >= 2, countScalar.truncatingRemainder(dividingBy: 1) == 0 else { throw MathError.unsupportedOperation(op: "linspace", typeA: "count must be an integer >= 2", typeB: nil) }
            let count = Int(countScalar)
            if count == 0 { return .vector(Vector(values: [])) }
            if count == 1 { return .vector(Vector(values: [start]))}
            var values: [Double] = []
            let step = (end - start) / Double(count - 1)
            for i in 0..<count { values.append(start + step * Double(i)) }
            return .vector(Vector(values: values))
        },
        "random": { args in
            switch args.count {
            case 0: return .scalar(Double.random(in: 0...1))
            case 1:
                let max = try args[0].asScalar()
                guard max >= 1, max.truncatingRemainder(dividingBy: 1) == 0 else { throw MathError.unsupportedOperation(op: "random", typeA: "max must be an integer >= 1", typeB: nil)}
                return .scalar(Double(Int.random(in: 1...Int(max))))
            case 2:
                let min = try args[0].asScalar(); let max = try args[1].asScalar()
                guard min <= max, min.truncatingRemainder(dividingBy: 1) == 0, max.truncatingRemainder(dividingBy: 1) == 0 else { throw MathError.unsupportedOperation(op: "random", typeA: "min and max must be integers where min <= max", typeB: nil)}
                return .scalar(Double(Int.random(in: Int(min)...Int(max))))
            case 3:
                let min = try args[0].asScalar(); let max = try args[1].asScalar(); let count = try args[2].asScalar()
                guard min <= max, min.truncatingRemainder(dividingBy: 1) == 0, max.truncatingRemainder(dividingBy: 1) == 0 else { throw MathError.unsupportedOperation(op: "random", typeA: "min and max must be integers where min <= max", typeB: nil)}
                guard count >= 1, count.truncatingRemainder(dividingBy: 1) == 0 else { throw MathError.unsupportedOperation(op: "random", typeA: "count must be an integer >= 1", typeB: nil)}
                let values = (0..<Int(count)).map { _ in Double(Int.random(in: Int(min)...Int(max))) }
                return .vector(Vector(values: values))
            default: throw MathError.incorrectArgumentCount(function: "random", expected: "0, 1, 2, or 3", found: args.count)
            }
        }
    ]

    static let singleArgumentFunctions: [String: (MathValue) throws -> MathValue] = [
        "abs": { arg in
            switch arg {
            case .scalar(let s): return .scalar(abs(s))
            case .complex(let c): return .scalar(c.abs())
            case .vector(let v): return .scalar(v.magnitude())
            default: throw MathError.typeMismatch(expected: "Scalar, Complex, or Vector", found: arg.typeName)
            }
        },
        "polar": { arg in guard case .complex(let c) = arg else { throw MathError.typeMismatch(expected: "Complex", found: arg.typeName) }; return .polar(c) },
        "sqrt": { arg in
            if case .scalar(let s) = arg { return s < 0 ? .complex(Complex(real: s, imaginary: 0).sqrt()) : .scalar(sqrt(s)) }
            else if case .complex(let c) = arg { return .complex(c.sqrt()) }
            else { throw MathError.typeMismatch(expected: "Scalar or Complex", found: arg.typeName) }
        },
        "round": { arg in guard case .scalar(let s) = arg else { throw MathError.typeMismatch(expected: "Scalar", found: arg.typeName) }; return .scalar(round(s)) },
        "floor": { arg in guard case .scalar(let s) = arg else { throw MathError.typeMismatch(expected: "Scalar", found: arg.typeName) }; return .scalar(floor(s)) },
        "ceil": { arg in guard case .scalar(let s) = arg else { throw MathError.typeMismatch(expected: "Scalar", found: arg.typeName) }; return .scalar(ceil(s)) },
        "fact": { arg in guard case .scalar(let s) = arg else { throw MathError.typeMismatch(expected: "Scalar", found: arg.typeName) }; return .scalar(try factorial(s)) },
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
        "real": { arg in guard case .complex(let c) = arg else { throw MathError.typeMismatch(expected: "Complex", found: arg.typeName) }; return .scalar(c.real) },
        "imag": { arg in guard case .complex(let c) = arg else { throw MathError.typeMismatch(expected: "Complex", found: arg.typeName) }; return .scalar(c.imaginary) },
        "conj": { arg in guard case .complex(let c) = arg else { throw MathError.typeMismatch(expected: "Complex", found: arg.typeName) }; return .complex(c.conjugate()) },
        "area_circle": { arg in guard case .scalar(let r) = arg else { throw MathError.typeMismatch(expected: "Scalar", found: arg.typeName) }; return .scalar(Double.pi * r * r) },
        "circum_circle": { arg in guard case .scalar(let r) = arg else { throw MathError.typeMismatch(expected: "Scalar", found: arg.typeName) }; return .scalar(2 * Double.pi * r) },
        "vol_sphere": { arg in guard case .scalar(let r) = arg else { throw MathError.typeMismatch(expected: "Scalar", found: arg.typeName) }; return .scalar((4.0/3.0) * Double.pi * pow(r, 3)) },
        "vol_cube": { arg in guard case .scalar(let s) = arg else { throw MathError.typeMismatch(expected: "Scalar", found: arg.typeName) }; return .scalar(pow(s, 3)) },
        "unit": { arg in guard case .vector(let v) = arg else { throw MathError.typeMismatch(expected: "Vector", found: arg.typeName) }; return .vector(v.unit()) },
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
        },
        "randv": { arg in
            guard case .scalar(let size_s) = arg else { throw MathError.typeMismatch(expected: "Scalar for size", found: arg.typeName) }
            guard size_s > 0, size_s.truncatingRemainder(dividingBy: 1) == 0 else { throw MathError.unsupportedOperation(op: "randv", typeA: "size must be a positive integer", typeB: nil) }
            let size = Int(size_s)
            let values = (0..<size).map { _ in Double.random(in: 0...1) }
            return .vector(Vector(values: values))
        }
    ]
    
    static let angleAwareFunctions: [String: ([MathValue], AngleMode) throws -> MathValue] = [
        "sin": { args, mode in
            guard args.count == 1, case .scalar(let s) = args[0] else { throw MathError.typeMismatch(expected: "Scalar", found: args.first?.typeName ?? "none") }
            return .scalar(sin(mode == .degrees ? s * .pi / 180 : s))
        },
        "cos": { args, mode in
            guard args.count == 1, case .scalar(let s) = args[0] else { throw MathError.typeMismatch(expected: "Scalar", found: args.first?.typeName ?? "none") }
            return .scalar(cos(mode == .degrees ? s * .pi / 180 : s))
        },
        "tan": { args, mode in
            guard args.count == 1, case .scalar(let s) = args[0] else { throw MathError.typeMismatch(expected: "Scalar", found: args.first?.typeName ?? "none") }
            return .scalar(tan(mode == .degrees ? s * .pi / 180 : s))
        },
        "asin": { args, mode in
            guard args.count == 1, case .scalar(let s) = args[0] else { throw MathError.typeMismatch(expected: "Scalar", found: args.first?.typeName ?? "none") }
            let a = asin(s); return .scalar(mode == .degrees ? a * 180 / .pi : a)
        },
        "acos": { args, mode in
            guard args.count == 1, case .scalar(let s) = args[0] else { throw MathError.typeMismatch(expected: "Scalar", found: args.first?.typeName ?? "none") }
            let a = acos(s); return .scalar(mode == .degrees ? a * 180 / .pi : a)
        },
        "atan": { args, mode in
            guard args.count == 1, case .scalar(let s) = args[0] else { throw MathError.typeMismatch(expected: "Scalar", found: args.first?.typeName ?? "none") }
            let a = atan(s); return .scalar(mode == .degrees ? a * 180 / .pi : a)
        },
        "atan2": { args, mode in
            guard args.count == 2, case .scalar(let y) = args[0], case .scalar(let x) = args[1] else { throw MathError.typeMismatch(expected: "Two Scalars (y, x)", found: "other") }
            let a = Foundation.atan2(y, x); return .scalar(mode == .degrees ? a * 180 / .pi : a)
        },
        "arg": { args, mode in
            guard args.count == 1, case .complex(let c) = args[0] else { throw MathError.typeMismatch(expected: "Complex", found: args.first?.typeName ?? "none") }
            let a = c.argument(); return .scalar(mode == .degrees ? a * 180 / .pi : a)
        },
        "angle": { args, mode in
            guard args.count == 2, case .vector(let v1) = args[0], case .vector(let v2) = args[1] else { throw MathError.typeMismatch(expected: "Two Vectors", found: "other") }
            let angleRad = try v1.angle(with: v2)
            return .scalar(mode == .degrees ? angleRad * 180 / .pi : angleRad)
        }
    ]
    
    static let twoArgumentFunctions: [String: (MathValue, MathValue) throws -> MathValue] = [
        "linreg": { a, b in
            guard case .vector(let xVec) = a, case .vector(let yVec) = b else { throw MathError.typeMismatch(expected: "Two Vectors", found: "\(a.typeName), \(b.typeName)") }
            guard xVec.dimension == yVec.dimension, xVec.dimension >= 2 else { throw MathError.dimensionMismatch(reason: "Vectors must have the same number of elements (at least 2) for linear regression.") }
            let n = Double(xVec.dimension); let sumX = xVec.sum(); let sumY = yVec.sum(); let sumXY = try xVec.hadamard(with: yVec).sum(); let sumX2 = xVec.values.map { $0 * $0 }.reduce(0, +)
            let denominator = (n * sumX2 - sumX * sumX)
            guard denominator != 0 else { throw MathError.unsupportedOperation(op: "linreg", typeA: "Cannot perform regression on vertical line (undefined slope)", typeB: nil) }
            let slope = (n * sumXY - sumX * sumY) / denominator
            let intercept = (sumY - slope * sumX) / n
            return .regressionResult(slope: slope, intercept: intercept)
        },
        "dot": { a, b in
            if case .vector(let v1) = a, case .vector(let v2) = b { return .scalar(try v1.dot(with: v2)) }
            else if case .complexVector(let v1) = a, case .complexVector(let v2) = b { return .complex(try v1.dot(with: v2)) }
            throw MathError.typeMismatch(expected: "Two Vectors or Two ComplexVectors", found: "\(a.typeName), \(b.typeName)")
        },
        "cross": { a, b in guard case .vector(let v1) = a, case .vector(let v2) = b else { throw MathError.typeMismatch(expected: "Two 3D Vectors", found: "\(a.typeName), \(b.typeName)") }; return .vector(try v1.cross(with: v2)) },
        "nPr": { a, b in guard case .scalar(let n) = a, case .scalar(let k) = b else { throw MathError.typeMismatch(expected: "Two Scalars", found: "\(a.typeName), \(b.typeName)") }; return .scalar(try permutations(n: n, k: k)) },
        "nCr": { a, b in guard case .scalar(let n) = a, case .scalar(let k) = b else { throw MathError.typeMismatch(expected: "Two Scalars", found: "\(a.typeName), \(b.typeName)") }; return .scalar(try combinations(n: n, k: k)) },
        "hypot": { a, b in guard case .scalar(let s1) = a, case .scalar(let s2) = b else { throw MathError.typeMismatch(expected: "Two Scalars", found: "\(a.typeName), \(b.typeName)") }; return .scalar(Foundation.sqrt(s1*s1 + s2*s2)) },
        "side": { a, b in guard case .scalar(let c) = a, case .scalar(let s) = b else { throw MathError.typeMismatch(expected: "Two Scalars (hyp, side)", found: "\(a.typeName), \(b.typeName)") }; guard c >= s else { throw MathError.unsupportedOperation(op: "side", typeA: "hyp < side", typeB: nil) }; return .scalar(Foundation.sqrt(c*c - s*s)) },
        "area_rect": { a, b in guard case .scalar(let w) = a, case .scalar(let h) = b else { throw MathError.typeMismatch(expected: "Two Scalars (width, height)", found: "\(a.typeName), \(b.typeName)") }; return .scalar(w * h) },
        "area_tri": { a, b in guard case .scalar(let base) = a, case .scalar(let h) = b else { throw MathError.typeMismatch(expected: "Two Scalars (base, height)", found: "\(a.typeName), \(b.typeName)") }; return .scalar(0.5 * base * h) },
        "vol_cylinder": { a, b in guard case .scalar(let r) = a, case .scalar(let h) = b else { throw MathError.typeMismatch(expected: "Two Scalars (radius, height)", found: "\(a.typeName), \(b.typeName)") }; return .scalar(Double.pi * r * r * h) },
        "vol_cone": { a, b in guard case .scalar(let r) = a, case .scalar(let h) = b else { throw MathError.typeMismatch(expected: "Two Scalars (radius, height)", found: "\(a.typeName), \(b.typeName)") }; return .scalar((1.0/3.0) * Double.pi * r * r * h) },
        "root": { a, b in
                guard case .scalar(let x) = a, case .scalar(let n) = b else { throw MathError.typeMismatch(expected: "Two Scalars", found: "\(a.typeName), \(b.typeName)") }
                if x < 0 && n.truncatingRemainder(dividingBy: 2) == 0 { return .complex(Complex(real: 0, imaginary: pow(abs(x), 1/n))) }
                return .scalar(pow(x, 1/n))
        },
        "randm": { a, b in
            guard case .scalar(let rows_s) = a, case .scalar(let cols_s) = b else { throw MathError.typeMismatch(expected: "Two Scalars for dimensions", found: "\(a.typeName), \(b.typeName)") }
            guard rows_s > 0, cols_s > 0, rows_s.truncatingRemainder(dividingBy: 1) == 0, cols_s.truncatingRemainder(dividingBy: 1) == 0 else { throw MathError.unsupportedOperation(op: "randm", typeA: "dimensions must be positive integers", typeB: nil) }
            let rows = Int(rows_s); let cols = Int(cols_s)
            let values = (0..<(rows * cols)).map { _ in Double.random(in: 0...1) }
            return .matrix(Matrix(values: values, rows: rows, columns: cols))
        },
        "mod": { a, b in
            guard case .scalar(let n1) = a, case .scalar(let n2) = b else { throw MathError.typeMismatch(expected: "Two Scalars", found: "\(a.typeName), \(b.typeName)") }
            guard n2 != 0 else { throw MathError.divisionByZero }
            return .scalar(n1 - n2 * floor(n1 / n2))
        },
        "gcd": { a, b in
            guard case .scalar(let n1) = a, case .scalar(let n2) = b, n1.truncatingRemainder(dividingBy: 1) == 0, n2.truncatingRemainder(dividingBy: 1) == 0 else {
                throw MathError.unsupportedOperation(op: "gcd", typeA: "arguments must be integers", typeB: nil)
            }
            return .scalar(performGcd(abs(n1), abs(n2)))
        },
        "lcm": { a, b in
            guard case .scalar(let n1) = a, case .scalar(let n2) = b, n1.truncatingRemainder(dividingBy: 1) == 0, n2.truncatingRemainder(dividingBy: 1) == 0 else {
                throw MathError.unsupportedOperation(op: "lcm", typeA: "arguments must be integers", typeB: nil)
            }
            if n1 == 0 || n2 == 0 { return .scalar(0) }
            return .scalar(abs(n1 * n2) / performGcd(abs(n1), abs(n2)))
        }
    ]

    // MARK: - Function & Operator Evaluation
    
    func evaluateFunctionCall(_ node: FunctionCallNode, variables: inout [String: MathValue], functions: inout [String: FunctionDefinitionNode], angleMode: AngleMode) throws -> (result: MathValue, usedAngle: Bool) {
        var usedAngle = false
        if node.name == "grad" { return try evaluateGradFunction(node, variables: &variables, functions: &functions, angleMode: angleMode) }
        
        if let angleFunc = Evaluator.angleAwareFunctions[node.name] {
            var args: [MathValue] = []
            var argsUsedAngle = false
            for argNode in node.arguments {
                let (arg, argUsedAngle) = try _evaluateSingle(node: argNode, variables: &variables, functions: &functions, angleMode: angleMode)
                args.append(arg)
                argsUsedAngle = argsUsedAngle || argUsedAngle
            }
            return (try angleFunc(args, angleMode), true || argsUsedAngle)
        }

        if let variadicFunc = Evaluator.variadicFunctions[node.name] {
            var args: [MathValue] = []; for argNode in node.arguments {
                let (arg, argUsedAngle) = try _evaluateSingle(node: argNode, variables: &variables, functions: &functions, angleMode: angleMode)
                usedAngle = usedAngle || argUsedAngle; args.append(arg)
            }
            return (try variadicFunc(args), usedAngle)
        }
        
        if let multiArgFunc = Evaluator.multiArgumentFunctions[node.name] {
            var args: [MathValue] = []; for argNode in node.arguments {
                let (arg, argUsedAngle) = try _evaluateSingle(node: argNode, variables: &variables, functions: &functions, angleMode: angleMode)
                usedAngle = usedAngle || argUsedAngle; args.append(arg)
            }
            return (try multiArgFunc(args), usedAngle)
        }
        
        if let singleArgFunc = Evaluator.singleArgumentFunctions[node.name] {
            guard node.arguments.count == 1 else { throw MathError.incorrectArgumentCount(function: node.name, expected: "1", found: node.arguments.count) }
            let (arg, argUsedAngle) = try _evaluateSingle(node: node.arguments[0], variables: &variables, functions: &functions, angleMode: angleMode)
            return (try singleArgFunc(arg), argUsedAngle)
        }
        
        if let twoArgFunc = Evaluator.twoArgumentFunctions[node.name] {
            guard node.arguments.count == 2 else { throw MathError.incorrectArgumentCount(function: node.name, expected: "2", found: node.arguments.count) }
            let (arg1, arg1UsedAngle) = try _evaluateSingle(node: node.arguments[0], variables: &variables, functions: &functions, angleMode: angleMode)
            let (arg2, arg2UsedAngle) = try _evaluateSingle(node: node.arguments[1], variables: &variables, functions: &functions, angleMode: angleMode)
            return (try twoArgFunc(arg1, arg2), arg1UsedAngle || arg2UsedAngle)
        }
        
        if let scalarFunc = Evaluator.scalarFunctions[node.name] {
            guard node.arguments.count == 1 else { throw MathError.incorrectArgumentCount(function: node.name, expected: "1", found: node.arguments.count) }
            let (arg, argUsedAngle) = try _evaluateSingle(node: node.arguments[0], variables: &variables, functions: &functions, angleMode: angleMode)
            guard case .scalar(let s) = arg else { throw MathError.typeMismatch(expected: "Scalar", found: arg.typeName) }
            return (.scalar(scalarFunc(s)), argUsedAngle)
        }
        
        if let userFunction = functions[node.name] {
            if userFunction.parameterNames.count == 1 && node.arguments.count == 1 {
                let (argValue, argUsedAngle) = try evaluate(node: node.arguments[0], variables: &variables, functions: &functions, angleMode: angleMode)
                if case .vector(let v) = argValue {
                    var resultValues: [Double] = []; var overallUsedAngle = argUsedAngle
                    for element in v.values {
                        var localVariables = variables
                        localVariables[userFunction.parameterNames[0]] = .scalar(element)
                        let (result, elementUsedAngle) = try evaluate(node: userFunction.body, variables: &localVariables, functions: &functions, angleMode: angleMode)
                        guard case .scalar(let scalarResult) = result else { throw MathError.typeMismatch(expected: "Scalar result from vectorized function", found: result.typeName) }
                        resultValues.append(scalarResult)
                        overallUsedAngle = overallUsedAngle || elementUsedAngle
                    }
                    return (.vector(Vector(values: resultValues)), overallUsedAngle)
                }
            }
            
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
    
    func evaluateUnaryOperation(op: Token, value: MathValue) throws -> MathValue {
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

    func evaluateBinaryOperation(op: Token, left: MathValue, right: MathValue) throws -> MathValue {
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
    
    func evaluateIndexedAssignment(op: Token, target: MathValue, indexedOp: IndexedOperationNode, variables: inout [String: MathValue], functions: inout [String: FunctionDefinitionNode], angleMode: AngleMode) throws -> (MathValue, Bool) {
        let (indexValue, indexUsedAngle) = try _evaluateSingle(node: indexedOp.index, variables: &variables, functions: &functions, angleMode: angleMode)
        let (scalarValue, scalarUsedAngle) = try _evaluateSingle(node: indexedOp.scalar, variables: &variables, functions: &functions, angleMode: angleMode)
        
        guard case .scalar(let indexScalar) = indexValue, indexScalar.truncatingRemainder(dividingBy: 1) == 0 else { throw MathError.typeMismatch(expected: "Integer for index", found: indexValue.typeName) }
        let oneBasedIndex = Int(indexScalar); let zeroBasedIndex = oneBasedIndex - 1

        let result: MathValue
        switch target {
        case .vector(let v):
            guard case .scalar(let s) = scalarValue else { throw MathError.typeMismatch(expected: "Scalar", found: scalarValue.typeName) }
            let opFunction: (Double, Double) -> Double
            switch op.rawValue {
            case ".=@": opFunction = { _, s in s }; case ".+@": opFunction = (+); case ".-@": opFunction = (-); case ".*@": opFunction = (*)
            case "./@": opFunction = { v, s in guard s != 0 else { return .nan }; return v / s }
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
            case ".=@": opFunction = { _, s in s }; case ".+@": opFunction = (+); case ".-@": opFunction = (-); case ".*@": opFunction = (*); case "./@": opFunction = { v, s in try v / s }
            default: throw MathError.unknownOperator(op: op.rawValue)
            }
            result = .complexVector(try cv.modifying(at: zeroBasedIndex, with: complexScalar, operation: opFunction))
        default: throw MathError.typeMismatch(expected: "Vector or ComplexVector", found: target.typeName)
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
        switch op { case "+": return l + r; case "-": return l - r; case "*": return l * r; case "/": return try l / r; case "^": return try l.pow(r); default: throw MathError.unknownOperator(op: op) }
    }
    private func performVectorScalarOp(_ op: String, _ v: Vector, _ s: Double, reversed: Bool = false) throws -> Vector {
        if reversed {
            switch op { case "+": return s + v; case "*": return s * v; case "-": return s - v
            case "/": guard !v.values.contains(0) else { throw MathError.divisionByZero }; return s / v
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
        switch op { case "+": return try l + r; case "-": return try l - r; case ".*": return try l.hadamard(with: r); case "./": return try l.hadamardDivision(with: r)
        case "^": return Vector(values: zip(l.values, r.values).map(pow)); default: throw MathError.unsupportedOperation(op: op, typeA: "Vector", typeB: "Vector")
        }
    }
    private func performMatrixScalarOp(_ op: String, _ m: Matrix, _ s: Double, reversed: Bool = false) throws -> Matrix {
        let newValues: [Double]; switch op {
        case "+": newValues = m.values.map { $0 + s }; case "*": newValues = m.values.map { $0 * s }
        case "-": newValues = reversed ? m.values.map { s - $0 } : m.values.map { $0 - s }
        case "/": if reversed { throw MathError.unsupportedOperation(op: op, typeA: "Scalar", typeB: "Matrix") }; guard s != 0 else { throw MathError.divisionByZero }; newValues = m.values.map { $0 / s }
        default: throw MathError.unsupportedOperation(op: op, typeA: "Matrix", typeB: "Scalar")
        }
        return Matrix(values: newValues, rows: m.rows, columns: m.columns)
    }
    private func performMatrixMatrixOp(_ op: String, _ l: Matrix, _ r: Matrix) throws -> Matrix {
        switch op { case "+": return try l + r; case "-": return try l - r; case "*": return try l * r; case ".*": return try l.hadamard(with: r); case "./": return try l.hadamardDivision(with: r); default: throw MathError.unsupportedOperation(op: op, typeA: "Matrix", typeB: "Matrix") }
    }
    private func performComplexVectorComplexOp(_ op: String, _ v: ComplexVector, _ c: Complex, reversed: Bool = false) throws -> ComplexVector {
         if reversed {
            switch op { case "+": return ComplexVector(values: v.values.map { c + $0 }); case "*": return ComplexVector(values: v.values.map { c * $0 }); case "-": return ComplexVector(values: v.values.map { c - $0 }); default: throw MathError.unsupportedOperation(op: op, typeA: "ComplexVector", typeB: "Complex") }
        } else {
            switch op { case "+": return v + c; case "*": return v * c; case "-": return v - c; case "/": return try v / c; default: throw MathError.unsupportedOperation(op: op, typeA: "ComplexVector", typeB: "Complex") }
        }
    }
    private func performCVectorCVectorOp(_ op: String, _ l: ComplexVector, _ r: ComplexVector) throws -> ComplexVector {
        switch op { case "+": return try l + r; case "-": return try l - r; default: throw MathError.unsupportedOperation(op: op, typeA: "ComplexVector", typeB: "ComplexVector") }
    }
    private func performComplexMatrixComplexOp(_ op: String, _ m: ComplexMatrix, _ c: Complex, reversed: Bool = false) throws -> ComplexMatrix {
         if reversed {
            switch op { case "+": return ComplexMatrix(values: m.values.map { c + $0 }, rows: m.rows, columns: m.columns); case "*": return ComplexMatrix(values: m.values.map { c * $0 }, rows: m.rows, columns: m.columns); case "-": return ComplexMatrix(values: m.values.map { c - $0 }, rows: m.rows, columns: m.columns); default: throw MathError.unsupportedOperation(op: op, typeA: "ComplexMatrix", typeB: "Complex") }
        } else {
            switch op { case "+": return m + c; case "*": return m * c; case "-": return m - c; case "/": return try m / c; default: throw MathError.unsupportedOperation(op: op, typeA: "ComplexMatrix", typeB: "Complex") }
        }
    }
    private func performCMatrixCMatrixOp(_ op: String, _ l: ComplexMatrix, _ r: ComplexMatrix) throws -> ComplexMatrix {
        switch op { case "+": return try l + r; case "-": return try l - r; case "*": return try l * r; default: throw MathError.unsupportedOperation(op: op, typeA: "ComplexMatrix", typeB: "ComplexMatrix") }
    }
    private func promote(_ left: MathValue, _ right: MathValue) throws -> (MathValue, MathValue) {
        switch (left, right) {
        case (.matrix(let m), .complex(let c)): return (.complexMatrix(ComplexMatrix(from: m)), .complex(c)); case (.complex(let c), .matrix(let m)): return (.complex(c), .complexMatrix(ComplexMatrix(from: m))); case (.vector(let v), .complex(let c)): return (.complexVector(ComplexVector(from: v)), .complex(c)); case (.complex(let c), .vector(let v)): return (.complex(c), .complexVector(ComplexVector(from: v))); case (.complexMatrix(let cm), .scalar(let s)): return (.complexMatrix(cm), .complex(Complex(real: s, imaginary: 0))); case (.scalar(let s), .complexMatrix(let cm)): return (.complex(Complex(real: s, imaginary: 0)), .complexMatrix(cm)); case (.complexVector(let cv), .scalar(let s)): return (.complexVector(cv), .complex(Complex(real: s, imaginary: 0))); case (.scalar(let s), .complexVector(let cv)): return (.complex(Complex(real: s, imaginary: 0)), .complexVector(cv)); default: return (left, right)
        }
    }
}

/// Helper function for variadic statistical functions like sum(), avg(), etc.
fileprivate func performStatisticalOperation(args: [MathValue], on operation: (Vector) -> Double?) throws -> MathValue {
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

/// Helper function for gcd, using Euclidean algorithm.
fileprivate func performGcd(_ a: Double, _ b: Double) -> Double {
    let r = a.truncatingRemainder(dividingBy: b)
    if r != 0 {
        return performGcd(b, r)
    } else {
        return abs(b)
    }
}
