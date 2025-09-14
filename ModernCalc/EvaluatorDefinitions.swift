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
        "b": 2.897771955e-3, "atm": 101325, "Vm": 22.41396954e-3,
        // New Constants
        "amu": 1.66053906660e-27, "RE": 6.371e6, "ME": 5.972e24, "c0": 343,
        "μB": 9.2740100783e-24, "μN": 5.0507837461e-27
    ]
    
    static let siPrefixes: [String: Double] = [
        "yotta": 1e24, "zetta": 1e21, "exa": 1e18, "peta": 1e15, "tera": 1e12, "giga": 1e9, "mega": 1e6,
        "kilo": 1e3, "hecto": 1e2, "deca": 1e1, "deci": 1e-1, "centi": 1e-2, "milli": 1e-3, "micro": 1e-6,
        "nano": 1e-9, "pico": 1e-12, "femto": 1e-15, "atto": 1e-18, "zepto": 1e-21, "yocto": 1e-24
    ]
    
    // MARK: - Function Dictionaries
    
    static let dimensionlessScalarFunctions: [String: (Double) -> Double] = [
        "ln": log, "lg": log10, "log": log10,
        "sinh": sinh, "cosh": cosh, "tanh": tanh,
        "asinh": asinh, "acosh": acosh, "atanh": atanh,
        // Hyperbolic Reciprocal Functions
        "sech": { 1.0 / cosh($0) },
        "csch": { 1.0 / sinh($0) },
        "coth": { 1.0 / tanh($0) },
        "asech": { acosh(1.0 / $0) },
        "acsch": { asinh(1.0 / $0) },
        "acoth": { atanh(1.0 / $0) },
    ]
    
    static let variadicFunctions: [String: ([MathValue]) throws -> MathValue] = [
        "sum": { args in try performStatisticalOperation(args: args, on: { $0.sum() }) },
        "avg": { args in try performStatisticalOperation(args: args, on: { $0.average() }) },
        "average": { args in try performStatisticalOperation(args: args, on: { $0.average() }) },
        "mean": { args in try performStatisticalOperation(args: args, on: { $0.average() }) },
        "min": { args in try performStatisticalOperation(args: args, on: { $0.min() }) },
        "max": { args in try performStatisticalOperation(args: args, on: { $0.max() }) },
        "median": { args in try performStatisticalOperation(args: args, on: { $0.median() }) },
        "stddev": { args in try performStatisticalOperation(args: args, on: { $0.stddev() }) },
        "variance": { args in try performStatisticalOperation(args: args, on: { $0.variance() }) },
        "stddevp": { args in try performStatisticalOperation(args: args, on: { $0.stddevp() }) },
        "mode": { args in
            let values = try extractDoublesFromVariadicArgs(args)
            guard !values.isEmpty else { throw MathError.requiresAtLeastOneArgument(function: "mode") }
            
            let frequencyDict = values.reduce(into: [:]) { counts, value in counts[value, default: 0] += 1 }
            
            guard let maxFrequency = frequencyDict.values.max() else { return .vector(Vector(values: [])) }
            
            let modes = frequencyDict.filter { $0.value == maxFrequency }.keys.sorted()
            
            if modes.count == 1 {
                return .dimensionless(modes[0])
            } else {
                return .vector(Vector(values: modes))
            }
        },
        "geomean": { args in
            let values = try extractDoublesFromVariadicArgs(args)
            guard !values.isEmpty else { throw MathError.requiresAtLeastOneArgument(function: "geomean") }
            guard values.allSatisfy({ $0 >= 0 }) else { throw MathError.unsupportedOperation(op: "geomean", typeA: "All values must be non-negative.", typeB: nil) }
            if values.contains(0) { return .dimensionless(0) }
            let product = values.reduce(1, *)
            return .dimensionless(pow(product, 1.0 / Double(values.count)))
        },
        "harmean": { args in
            let values = try extractDoublesFromVariadicArgs(args)
            guard !values.isEmpty else { throw MathError.requiresAtLeastOneArgument(function: "harmean") }
            guard !values.contains(0) else { throw MathError.unsupportedOperation(op: "harmean", typeA: "Values cannot be zero.", typeB: nil) }
            let sumOfReciprocals = values.map { 1.0 / $0 }.reduce(0, +)
            guard sumOfReciprocals != 0 else { throw MathError.divisionByZero }
            return .dimensionless(Double(values.count) / sumOfReciprocals)
        },
        "gcd": { args in try performAggregateIntegerOperation(args: args, initialValue: 0, operation: performGcd) },
        "lcm": { args in
            let lcmOp = { (a: Double, b: Double) -> Double in
                if a == 0 || b == 0 { return 0 }
                return abs(a * b) / performGcd(a, b)
            }
            return try performAggregateIntegerOperation(args: args, initialValue: 1, operation: lcmOp)
        }
    ]
    
    static let multiArgumentFunctions: [String: ([MathValue]) throws -> MathValue] = [
        "if": { args in
            guard args.count == 3 else { throw MathError.incorrectArgumentCount(function: "if", expected: "3", found: args.count) }
            let condition = try args[0].asScalar()
            return condition != 0 ? args[1] : args[2]
        },
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
        "polyfit": { args in
            guard args.count == 3 else { throw MathError.incorrectArgumentCount(function: "polyfit", expected: "3", found: args.count) }
            guard case .vector(let xVec) = args[0], case .vector(let yVec) = args[1] else {
                throw MathError.typeMismatch(expected: "Vector, Vector, Scalar", found: "\(args[0].typeName), \(args[1].typeName), \(args[2].typeName)")
            }
            let degree = try args[2].asScalar()
            let coeffs = try performPolynomialFit(x: xVec, y: yVec, degree: degree)
            return .polynomialFit(coefficients: coeffs)
        },
        "normdist": { args in
            guard args.count == 3 else { throw MathError.incorrectArgumentCount(function: "normdist", expected: "3", found: args.count) }
            let x = try args[0].asScalar()
            let mean = try args[1].asScalar()
            let stddev = try args[2].asScalar()
            return .dimensionless(normalDistribution(x: x, mean: mean, stddev: stddev))
        },
        "binomdist": { args in
            guard args.count == 3 else { throw MathError.incorrectArgumentCount(function: "binomdist", expected: "3", found: args.count) }
            let k = try args[0].asScalar()
            let n = try args[1].asScalar()
            let p = try args[2].asScalar()
            return .dimensionless(try binomialDistribution(k: k, n: n, p: p))
        },
        "random": { args in
            switch args.count {
            case 0: return .dimensionless(Double.random(in: 0...1))
            case 1:
                let max = try args[0].asScalar()
                guard max >= 1, max.truncatingRemainder(dividingBy: 1) == 0 else { throw MathError.unsupportedOperation(op: "random", typeA: "max must be an integer >= 1", typeB: nil)}
                return .dimensionless(Double(Int.random(in: 1...Int(max))))
            case 2:
                let min = try args[0].asScalar(); let max = try args[1].asScalar()
                guard min <= max, min.truncatingRemainder(dividingBy: 1) == 0, max.truncatingRemainder(dividingBy: 1) == 0 else { throw MathError.unsupportedOperation(op: "random", typeA: "min and max must be integers where min <= max", typeB: nil)}
                return .dimensionless(Double(Int.random(in: Int(min)...Int(max))))
            case 3:
                let min = try args[0].asScalar(); let max = try args[1].asScalar(); let count = try args[2].asScalar()
                guard min <= max, min.truncatingRemainder(dividingBy: 1) == 0, max.truncatingRemainder(dividingBy: 1) == 0 else { throw MathError.unsupportedOperation(op: "random", typeA: "min and max must be integers where min <= max", typeB: nil)}
                guard count >= 1, count.truncatingRemainder(dividingBy: 1) == 0 else { throw MathError.unsupportedOperation(op: "random", typeA: "count must be an integer >= 1", typeB: nil)}
                let values = (0..<Int(count)).map { _ in Double(Int.random(in: Int(min)...Int(max))) }
                return .vector(Vector(values: values))
            default: throw MathError.incorrectArgumentCount(function: "random", expected: "0, 1, 2, or 3", found: args.count)
            }
        },
        "sort": { args in
            guard args.count == 1 || args.count == 2 else { throw MathError.incorrectArgumentCount(function: "sort", expected: "1 or 2", found: args.count) }
            let values = try extractDoubles(from: args[0])
            var descending = false
            if args.count == 2 {
                guard case .constant(let direction) = args[1] else { throw MathError.typeMismatch(expected: "String 'asc' or 'desc'", found: args[1].typeName) }
                if direction.lowercased() == "desc" { descending = true }
                else if direction.lowercased() != "asc" { throw MathError.unsupportedOperation(op: "sort", typeA: "direction must be 'asc' or 'desc'", typeB: nil) }
            }
            let sortedValues = descending ? values.sorted(by: >) : values.sorted(by: <)
            return .vector(Vector(values: sortedValues))
        },
        "zeros": { args in
            switch args.count {
            case 1:
                let rows_s = try args[0].asScalar()
                guard rows_s > 0, rows_s.truncatingRemainder(dividingBy: 1) == 0 else { throw MathError.unsupportedOperation(op: "zeros", typeA: "dimensions must be positive integers", typeB: nil) }
                return .vector(Vector(values: [Double](repeating: 0, count: Int(rows_s))))
            case 2:
                let rows_s = try args[0].asScalar(); let cols_s = try args[1].asScalar()
                guard rows_s > 0, cols_s > 0, rows_s.truncatingRemainder(dividingBy: 1) == 0, cols_s.truncatingRemainder(dividingBy: 1) == 0 else { throw MathError.unsupportedOperation(op: "zeros", typeA: "dimensions must be positive integers", typeB: nil) }
                let rows = Int(rows_s); let cols = Int(cols_s)
                return .matrix(Matrix(values: [Double](repeating: 0, count: rows*cols), rows: rows, columns: cols))
            default:
                throw MathError.incorrectArgumentCount(function: "zeros", expected: "1 or 2", found: args.count)
            }
        },
        "ones": { args in
            switch args.count {
            case 1:
                let rows_s = try args[0].asScalar()
                guard rows_s > 0, rows_s.truncatingRemainder(dividingBy: 1) == 0 else { throw MathError.unsupportedOperation(op: "ones", typeA: "dimensions must be positive integers", typeB: nil) }
                return .vector(Vector(values: [Double](repeating: 1, count: Int(rows_s))))
            case 2:
                let rows_s = try args[0].asScalar(); let cols_s = try args[1].asScalar()
                guard rows_s > 0, cols_s > 0, rows_s.truncatingRemainder(dividingBy: 1) == 0, cols_s.truncatingRemainder(dividingBy: 1) == 0 else { throw MathError.unsupportedOperation(op: "ones", typeA: "dimensions must be positive integers", typeB: nil) }
                let rows = Int(rows_s); let cols = Int(cols_s)
                return .matrix(Matrix(values: [Double](repeating: 1, count: rows*cols), rows: rows, columns: cols))
            default:
                throw MathError.incorrectArgumentCount(function: "ones", expected: "1 or 2", found: args.count)
            }
        },
    ]

    static let singleArgumentFunctions: [String: (MathValue) throws -> MathValue] = [
        "abs": { arg in
            switch arg {
            case .dimensionless(let s): return .dimensionless(abs(s))
            case .unitValue(let u): return .unitValue(UnitValue(value: abs(u.value), dimensions: u.dimensions))
            case .complex(let c): return .dimensionless(c.abs())
            case .vector(let v): return .dimensionless(v.magnitude())
            case .uncertain(let u): return .dimensionless(abs(u.value))
            default: throw MathError.typeMismatch(expected: "Scalar, Complex, Vector, or UncertainValue", found: arg.typeName)
            }
        },
        "norm": { arg in
             switch arg {
             case .vector(let v): return .dimensionless(v.magnitude())
             case .matrix(let m): return .dimensionless(m.frobeniusNorm())
             default: throw MathError.typeMismatch(expected: "Vector or Matrix", found: arg.typeName)
             }
        },
        "rank": { arg in
            guard case .matrix(let m) = arg else { throw MathError.typeMismatch(expected: "Matrix", found: arg.typeName) }
            return .dimensionless(Double(m.rank()))
        },
        "eye": { arg in
            let n_s = try arg.asScalar()
            guard n_s > 0, n_s.truncatingRemainder(dividingBy: 1) == 0 else { throw MathError.unsupportedOperation(op: "eye", typeA: "size must be a positive integer", typeB: nil) }
            let n = Int(n_s)
            var values = [Double](repeating: 0, count: n*n)
            for i in 0..<n { values[i*n + i] = 1 }
            return .matrix(Matrix(values: values, rows: n, columns: n))
        },
        "iqr": { arg in
            let values = try extractDoubles(from: arg).sorted()
            guard !values.isEmpty else { throw MathError.requiresAtLeastOneArgument(function: "iqr") }
            
            let q1 = performPercentile(values: values, p: 25)
            let q3 = performPercentile(values: values, p: 75)
            
            return .dimensionless(q3 - q1)
        },
        "sign": { arg in
            let s = try arg.asScalar()
            if s > 0 { return .dimensionless(1) }
            else if s < 0 { return .dimensionless(-1) }
            else { return .dimensionless(0) }
        },
        "polar": { arg in guard case .complex(let c) = arg else { throw MathError.typeMismatch(expected: "Complex", found: arg.typeName) }; return .polar(c) },
        "sqrt": { arg in
            if case .dimensionless(let s) = arg { return s < 0 ? .complex(Complex(real: s, imaginary: 0).sqrt()) : .dimensionless(sqrt(s)) }
            if case .unitValue(let u) = arg { return .unitValue(u.pow(0.5)) }
            else if case .complex(let c) = arg { return .complex(c.sqrt()) }
            else if case .uncertain(let u) = arg { return .uncertain(u.pow(0.5)) }
            else { throw MathError.typeMismatch(expected: "Scalar, Complex or UncertainValue", found: arg.typeName) }
        },
        "round": { arg in let s = try arg.asScalar(); return .dimensionless(round(s)) },
        "floor": { arg in let s = try arg.asScalar(); return .dimensionless(floor(s)) },
        "ceil": { arg in let s = try arg.asScalar(); return .dimensionless(ceil(s)) },
        "fact": { arg in let s = try arg.asScalar(); return .dimensionless(try factorial(s)) },
        "det": { arg in
            switch arg {
            case .matrix(let m): return .dimensionless(try m.determinant())
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
        "real": { arg in guard case .complex(let c) = arg else { throw MathError.typeMismatch(expected: "Complex", found: arg.typeName) }; return .dimensionless(c.real) },
        "imag": { arg in guard case .complex(let c) = arg else { throw MathError.typeMismatch(expected: "Complex", found: arg.typeName) }; return .dimensionless(c.imaginary) },
        "conj": { arg in guard case .complex(let c) = arg else { throw MathError.typeMismatch(expected: "Complex", found: arg.typeName) }; return .complex(c.conjugate()) },
        "area_circle": { arg in let r = try arg.asScalar(); return .dimensionless(Double.pi * r * r) },
        "circum_circle": { arg in let r = try arg.asScalar(); return .dimensionless(2 * Double.pi * r) },
        "vol_sphere": { arg in let r = try arg.asScalar(); return .dimensionless((4.0/3.0) * Double.pi * pow(r, 3)) },
        "vol_cube": { arg in let s = try arg.asScalar(); return .dimensionless(pow(s, 3)) },
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
            case .matrix(let m): return .dimensionless(try m.trace())
            case .complexMatrix(let cm): return .complex(try cm.trace())
            default: throw MathError.typeMismatch(expected: "Matrix or ComplexMatrix", found: arg.typeName)
            }
        },
        "randv": { arg in
            let size_s = try arg.asScalar()
            guard size_s > 0, size_s.truncatingRemainder(dividingBy: 1) == 0 else { throw MathError.unsupportedOperation(op: "randv", typeA: "size must be a positive integer", typeB: nil) }
            let size = Int(size_s)
            let values = (0..<size).map { _ in Double.random(in: 0...1) }
            return .vector(Vector(values: values))
        },
        "isprime": { arg in let n = try arg.asScalar(); return .dimensionless(try performIsPrime(n) ? 1.0 : 0.0) },
        "factor": { arg in let n = try arg.asScalar(); return .vector(Vector(values: try performFactor(n))) },
        "unique": { arg in
            let values = try extractDoubles(from: arg)
            let uniqueValues = Array(Set(values)).sorted()
            return .vector(Vector(values: uniqueValues))
        },
        "deg2rad": { arg in let s = try arg.asScalar(); return .dimensionless(s * .pi / 180.0) },
        "rad2deg": { arg in let s = try arg.asScalar(); return .dimensionless(s * 180.0 / .pi) }
    ]
    
    static let angleAwareFunctions: [String: ([MathValue], AngleMode) throws -> MathValue] = [
        "sin": { args, mode in
            guard args.count == 1 else { throw MathError.typeMismatch(expected: "Scalar or UncertainValue", found: "multiple arguments") }
            if case .uncertain(let u) = args[0] {
                let valRad = mode == .degrees ? u.value * .pi / 180 : u.value
                let uncTotalRad = mode == .degrees ? u.totalUncertainty * .pi / 180 : u.totalUncertainty
                
                let u_rad_total = UncertainValue(value: valRad, randomUncertainty: uncTotalRad, systematicUncertainty: 0)
                let propagated_u = u_rad_total.propagate(derivative: cos(valRad))

                let randomRatio = u.totalUncertainty > 0 ? u.randomUncertainty / u.totalUncertainty : 0
                
                return .uncertain(UncertainValue(value: sin(valRad),
                                                 randomUncertainty: propagated_u.randomUncertainty * randomRatio,
                                                 systematicUncertainty: propagated_u.randomUncertainty * (1 - randomRatio)))
            }
            let s = try args[0].asScalar(); let valRad = mode == .degrees ? s * .pi / 180 : s; return .dimensionless(sin(valRad))
        },
        "cos": { args, mode in
            guard args.count == 1 else { throw MathError.typeMismatch(expected: "Scalar or UncertainValue", found: "multiple arguments") }
            if case .uncertain(let u) = args[0] {
                let valRad = mode == .degrees ? u.value * .pi / 180 : u.value
                let uncTotalRad = mode == .degrees ? u.totalUncertainty * .pi / 180 : u.totalUncertainty
                
                let u_rad_total = UncertainValue(value: valRad, randomUncertainty: uncTotalRad, systematicUncertainty: 0)
                let propagated_u = u_rad_total.propagate(derivative: -sin(valRad))
                
                let randomRatio = u.totalUncertainty > 0 ? u.randomUncertainty / u.totalUncertainty : 0

                return .uncertain(UncertainValue(value: cos(valRad),
                                                 randomUncertainty: propagated_u.randomUncertainty * randomRatio,
                                                 systematicUncertainty: propagated_u.randomUncertainty * (1 - randomRatio)))
            }
            let s = try args[0].asScalar(); let valRad = mode == .degrees ? s * .pi / 180 : s; return .dimensionless(cos(valRad))
        },
        "tan": { args, mode in
            guard args.count == 1 else { throw MathError.typeMismatch(expected: "Scalar or UncertainValue", found: "multiple arguments") }
            if case .uncertain(let u) = args[0] {
                let valRad = mode == .degrees ? u.value * .pi / 180 : u.value
                let uncTotalRad = mode == .degrees ? u.totalUncertainty * .pi / 180 : u.totalUncertainty
                
                let u_rad_total = UncertainValue(value: valRad, randomUncertainty: uncTotalRad, systematicUncertainty: 0)
                let propagated_u = u_rad_total.propagate(derivative: 1.0 / pow(cos(valRad), 2))
                
                let randomRatio = u.totalUncertainty > 0 ? u.randomUncertainty / u.totalUncertainty : 0
                
                return .uncertain(UncertainValue(value: tan(valRad),
                                                 randomUncertainty: propagated_u.randomUncertainty * randomRatio,
                                                 systematicUncertainty: propagated_u.randomUncertainty * (1 - randomRatio)))
            }
            let s = try args[0].asScalar(); let valRad = mode == .degrees ? s * .pi / 180 : s; return .dimensionless(tan(valRad))
        },
        "sec": { args, mode in let s = try args[0].asScalar(); let valRad = mode == .degrees ? s * .pi / 180 : s; return .dimensionless(1.0 / cos(valRad)) },
        "csc": { args, mode in let s = try args[0].asScalar(); let valRad = mode == .degrees ? s * .pi / 180 : s; return .dimensionless(1.0 / sin(valRad)) },
        "cot": { args, mode in let s = try args[0].asScalar(); let valRad = mode == .degrees ? s * .pi / 180 : s; return .dimensionless(1.0 / tan(valRad)) },
        "asin": { args, mode in let a = asin(try args[0].asScalar()); return .dimensionless(mode == .degrees ? a * 180 / .pi : a) },
        "acos": { args, mode in let a = acos(try args[0].asScalar()); return .dimensionless(mode == .degrees ? a * 180 / .pi : a) },
        "atan": { args, mode in let a = atan(try args[0].asScalar()); return .dimensionless(mode == .degrees ? a * 180 / .pi : a) },
        "asec": { args, mode in let a = acos(1.0 / (try args[0].asScalar())); return .dimensionless(mode == .degrees ? a * 180 / .pi : a) },
        "acsc": { args, mode in let a = asin(1.0 / (try args[0].asScalar())); return .dimensionless(mode == .degrees ? a * 180 / .pi : a) },
        "acot": { args, mode in let a = atan(1.0 / (try args[0].asScalar())); return .dimensionless(mode == .degrees ? a * 180 / .pi : a) },
        "atan2": { args, mode in let a = Foundation.atan2(try args[0].asScalar(), try args[1].asScalar()); return .dimensionless(mode == .degrees ? a * 180 / .pi : a) },
        "arg": { args, mode in
            guard args.count == 1, case .complex(let c) = args[0] else { throw MathError.typeMismatch(expected: "Complex", found: args.first?.typeName ?? "none") }
            let a = c.argument(); return .dimensionless(mode == .degrees ? a * 180 / .pi : a)
        },
        "angle": { args, mode in
            guard args.count == 2, case .vector(let v1) = args[0], case .vector(let v2) = args[1] else { throw MathError.typeMismatch(expected: "Two Vectors", found: "other") }
            let angleRad = try v1.angle(with: v2)
            return .dimensionless(mode == .degrees ? angleRad * 180 / .pi : angleRad)
        }
    ]
    
    static let twoArgumentFunctions: [String: (MathValue, MathValue) throws -> MathValue] = [
        "quartile": { data, q_val in
            let values = try extractDoubles(from: data).sorted()
            guard !values.isEmpty else { throw MathError.requiresAtLeastOneArgument(function: "quartile") }
            let q = try q_val.asScalar()
            guard [1, 2, 3].contains(q) && q.truncatingRemainder(dividingBy: 1) == 0 else {
                throw MathError.unsupportedOperation(op: "quartile", typeA: "q must be 1, 2, or 3", typeB: nil)
            }
            let p = q * 25.0
            return .dimensionless(performPercentile(values: values, p: p))
        },
        "rmse": { a, b in
            guard case .vector(let v1) = a, case .vector(let v2) = b else { throw MathError.typeMismatch(expected: "Two Vectors", found: "\(a.typeName), \(b.typeName)") }
            guard v1.dimension == v2.dimension else { throw MathError.dimensionMismatch(reason: "Vectors must have the same dimension for RMSE.") }
            guard v1.dimension > 0 else { return .dimensionless(0) }
            let squaredErrors = zip(v1.values, v2.values).map { pow($0 - $1, 2) }
            let meanSquaredError = squaredErrors.reduce(0, +) / Double(v1.dimension)
            return .dimensionless(sqrt(meanSquaredError))
        },
        "rmsd": { a, b in // alias for rmse
            guard case .vector(let v1) = a, case .vector(let v2) = b else { throw MathError.typeMismatch(expected: "Two Vectors", found: "\(a.typeName), \(b.typeName)") }
            guard v1.dimension == v2.dimension else { throw MathError.dimensionMismatch(reason: "Vectors must have the same dimension for RMSD.") }
            guard v1.dimension > 0 else { return .dimensionless(0) }
            let squaredErrors = zip(v1.values, v2.values).map { pow($0 - $1, 2) }
            let meanSquaredError = squaredErrors.reduce(0, +) / Double(v1.dimension)
            return .dimensionless(sqrt(meanSquaredError))
        },
        "log": { a, b in
            let base = try a.asScalar()
            let number = try b.asScalar()
            guard base > 0, base != 1, number > 0 else { throw MathError.unsupportedOperation(op: "log", typeA: "Logarithm base must be > 0 and != 1, and the number must be > 0.", typeB: nil) }
            return .dimensionless(Foundation.log(number) / Foundation.log(base))
        },
        "cov": { a, b in
            guard case .vector(let xVec) = a, case .vector(let yVec) = b else { throw MathError.typeMismatch(expected: "Two Vectors", found: "\(a.typeName), \(b.typeName)") }
            guard xVec.dimension == yVec.dimension, xVec.dimension >= 2 else { throw MathError.dimensionMismatch(reason: "Vectors must have the same number of elements (at least 2) for covariance.") }
            
            let n = Double(xVec.dimension)
            let meanX = xVec.average()
            let meanY = yVec.average()
            
            let sumOfProducts = zip(xVec.values, yVec.values).map { ($0 - meanX) * ($1 - meanY) }.reduce(0, +)
            
            return .dimensionless(sumOfProducts / (n - 1)) // Sample covariance
        },
        "corr": { a, b in
            guard case .vector(let xVec) = a, case .vector(let yVec) = b else { throw MathError.typeMismatch(expected: "Two Vectors", found: "\(a.typeName), \(b.typeName)") }
            guard xVec.dimension == yVec.dimension, xVec.dimension >= 2 else { throw MathError.dimensionMismatch(reason: "Vectors must have the same number of elements (at least 2) for correlation.") }
            let n = Double(xVec.dimension)
            let sumX = xVec.sum(); let sumY = yVec.sum()
            let sumXY = try xVec.hadamard(with: yVec).sum()
            let sumX2 = xVec.values.map { $0 * $0 }.reduce(0, +)
            let sumY2 = yVec.values.map { $0 * $0 }.reduce(0, +)
            
            let numerator = n * sumXY - sumX * sumY
            let denominator = sqrt((n * sumX2 - sumX * sumX) * (n * sumY2 - sumY * sumY))
            
            guard denominator != 0 else { throw MathError.unsupportedOperation(op: "corr", typeA: "Cannot calculate correlation, denominator is zero.", typeB: nil) }
            return .dimensionless(numerator / denominator)
        },
        "count": { data, value in
            let values = try extractDoubles(from: data)
            let target = try value.asScalar()
            return .dimensionless(Double(values.filter { $0 == target }.count))
        },
        "countabove": { data, threshold in
            let values = try extractDoubles(from: data)
            let target = try threshold.asScalar()
            return .dimensionless(Double(values.filter { $0 > target }.count))
        },
        "countbelow": { data, threshold in
            let values = try extractDoubles(from: data)
            let target = try threshold.asScalar()
            return .dimensionless(Double(values.filter { $0 < target }.count))
        },
        "find": { data, value in
            let values = try extractDoubles(from: data)
            let target = try value.asScalar()
            let indices = values.indices.filter { values[$0] == target }.map { Double($0 + 1) }
            return .vector(Vector(values: indices))
        },
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
        "linsolve": { a, b in
            guard case .matrix(let matrixA) = a, case .vector(let vectorB) = b else {
                throw MathError.typeMismatch(expected: "Matrix, Vector", found: "\(a.typeName), \(b.typeName)")
            }
            return .vector(try solveLinearSystem(A: matrixA, b: vectorB))
        },
        "dot": { a, b in
            if case .vector(let v1) = a, case .vector(let v2) = b { return .dimensionless(try v1.dot(with: v2)) }
            else if case .complexVector(let v1) = a, case .complexVector(let v2) = b { return .complex(try v1.dot(with: v2)) }
            throw MathError.typeMismatch(expected: "Two Vectors or Two ComplexVectors", found: "\(a.typeName), \(b.typeName)")
        },
        "cross": { a, b in guard case .vector(let v1) = a, case .vector(let v2) = b else { throw MathError.typeMismatch(expected: "Two 3D Vectors", found: "\(a.typeName), \(b.typeName)") }; return .vector(try v1.cross(with: v2)) },
        "getcolumn": { a, b in
            guard case .matrix(let matrix) = a else { throw MathError.typeMismatch(expected: "Matrix", found: a.typeName) }
            let indexScalar = try b.asScalar()
            guard indexScalar.truncatingRemainder(dividingBy: 1) == 0 else { throw MathError.typeMismatch(expected: "Integer for column index", found: "Non-integer scalar") }
            let index = Int(indexScalar)
            return .vector(try matrix.getcolumn(index: index))
        },
        "getrow": { a, b in
            guard case .matrix(let matrix) = a else { throw MathError.typeMismatch(expected: "Matrix", found: a.typeName) }
            let indexScalar = try b.asScalar()
            guard indexScalar.truncatingRemainder(dividingBy: 1) == 0 else { throw MathError.typeMismatch(expected: "Integer for row index", found: "Non-integer scalar") }
            let index = Int(indexScalar)
            return .vector(try matrix.getrow(index: index))
        },
        "nPr": { a, b in let n = try a.asScalar(); let k = try b.asScalar(); return .dimensionless(try permutations(n: n, k: k)) },
        "nCr": { a, b in let n = try a.asScalar(); let k = try b.asScalar(); return .dimensionless(try combinations(n: n, k: k)) },
        "hypot": { a, b in let s1 = try a.asScalar(); let s2 = try b.asScalar(); return .dimensionless(Foundation.sqrt(s1*s1 + s2*s2)) },
        "side": { a, b in let c = try a.asScalar(); let s = try b.asScalar(); guard c >= s else { throw MathError.unsupportedOperation(op: "side", typeA: "hyp < side", typeB: nil) }; return .dimensionless(Foundation.sqrt(c*c - s*s)) },
        "area_rect": { a, b in let w = try a.asScalar(); let h = try b.asScalar(); return .dimensionless(w * h) },
        "area_tri": { a, b in let base = try a.asScalar(); let h = try b.asScalar(); return .dimensionless(0.5 * base * h) },
        "vol_cylinder": { a, b in let r = try a.asScalar(); let h = try b.asScalar(); return .dimensionless(Double.pi * r * r * h) },
        "vol_cone": { a, b in let r = try a.asScalar(); let h = try b.asScalar(); return .dimensionless((1.0/3.0) * Double.pi * r * r * h) },
        "root": { a, b in
                let x = try a.asScalar(); let n = try b.asScalar()
                if x < 0 && n.truncatingRemainder(dividingBy: 2) == 0 { return .complex(Complex(real: 0, imaginary: pow(abs(x), 1/n))) }
                return .dimensionless(pow(x, 1/n))
        },
        "randm": { a, b in
            let rows_s = try a.asScalar(); let cols_s = try b.asScalar()
            guard rows_s > 0, cols_s > 0, rows_s.truncatingRemainder(dividingBy: 1) == 0, cols_s.truncatingRemainder(dividingBy: 1) == 0 else { throw MathError.unsupportedOperation(op: "randm", typeA: "dimensions must be positive integers", typeB: nil) }
            let rows = Int(rows_s); let cols = Int(cols_s)
            let values = (0..<(rows * cols)).map { _ in Double.random(in: 0...1) }
            return .matrix(Matrix(values: values, rows: rows, columns: cols))
        },
        "mod": { a, b in
            let n1 = try a.asScalar(); let n2 = try b.asScalar()
            guard n2 != 0 else { throw MathError.divisionByZero }
            return .dimensionless(n1 - n2 * floor(n1 / n2))
        },
        "percentile": { data, p_val in
            let values = try extractDoubles(from: data).sorted()
            guard !values.isEmpty else { throw MathError.requiresAtLeastOneArgument(function: "percentile") }
            let p = try p_val.asScalar()
            guard p >= 0 && p <= 100 else { throw MathError.unsupportedOperation(op: "percentile", typeA: "p must be between 0 and 100", typeB: nil) }
            return .dimensionless(performPercentile(values: values, p: p))
        },
        "gcd": { a, b in
            return try performElementWiseIntegerOp(a, b, opName: "gcd", operation: performGcd)
        },
        "lcm": { a, b in
            let lcmOp = { (n1: Double, n2: Double) -> Double in
                if n1 == 0 || n2 == 0 { return 0 }
                return abs(n1 * n2) / performGcd(abs(n1), abs(n2))
            }
            return try performElementWiseIntegerOp(a, b, opName: "lcm", operation: lcmOp)
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
            var args: [MathValue] = []
            if node.name == "sort" && node.arguments.count == 2 {
                let (arg1, arg1UsedAngle) = try _evaluateSingle(node: node.arguments[0], variables: &variables, functions: &functions, angleMode: angleMode)
                usedAngle = usedAngle || arg1UsedAngle
                args.append(arg1)
                if let constNode = node.arguments[1] as? ConstantNode {
                    args.append(.constant(constNode.name))
                } else {
                    throw MathError.typeMismatch(expected: "String literal 'asc' or 'desc'", found: "expression")
                }
            } else {
                for argNode in node.arguments {
                    let (arg, argUsedAngle) = try _evaluateSingle(node: argNode, variables: &variables, functions: &functions, angleMode: angleMode)
                    usedAngle = usedAngle || argUsedAngle; args.append(arg)
                }
            }
            return (try multiArgFunc(args), usedAngle)
        }
        
        if let singleArgFunc = Evaluator.singleArgumentFunctions[node.name] {
            guard node.arguments.count == 1 else { throw MathError.incorrectArgumentCount(function: node.name, expected: "1", found: node.arguments.count) }
            let (arg, argUsedAngle) = try _evaluateSingle(node: node.arguments[0], variables: &variables, functions: &functions, angleMode: angleMode)
            return (try singleArgFunc(arg), argUsedAngle)
        }
        
        if let twoArgFunc = Evaluator.twoArgumentFunctions[node.name] {
            guard node.arguments.count == 2 else {
                // Special case for log(number) with an implicit base of 10
                if node.name == "log" && node.arguments.count == 1 {
                    let (arg, argUsedAngle) = try _evaluateSingle(node: node.arguments[0], variables: &variables, functions: &functions, angleMode: angleMode)
                    if case .dimensionless(let s) = arg {
                        return (.dimensionless(log10(s)), argUsedAngle)
                    } else {
                        throw MathError.typeMismatch(expected: "Dimensionless", found: arg.typeName)
                    }
                }
                throw MathError.incorrectArgumentCount(function: node.name, expected: "2", found: node.arguments.count)
            }
            let (arg1, arg1UsedAngle) = try _evaluateSingle(node: node.arguments[0], variables: &variables, functions: &functions, angleMode: angleMode)
            let (arg2, arg2UsedAngle) = try _evaluateSingle(node: node.arguments[1], variables: &variables, functions: &functions, angleMode: angleMode)
            return (try twoArgFunc(arg1, arg2), arg1UsedAngle || arg2UsedAngle)
        }
        
        if let scalarFunc = Evaluator.dimensionlessScalarFunctions[node.name] {
            guard node.arguments.count == 1 else { throw MathError.incorrectArgumentCount(function: node.name, expected: "1", found: node.arguments.count) }
            let (arg, argUsedAngle) = try _evaluateSingle(node: node.arguments[0], variables: &variables, functions: &functions, angleMode: angleMode)
            
            if case .uncertain(let u) = arg {
                let val = u.value
                let resultVal = scalarFunc(val)
                var derivative: Double
                switch node.name {
                case "ln", "log", "lg": derivative = 1 / (val * (node.name == "ln" ? 1 : log(10)))
                case "sinh": derivative = cosh(val); case "cosh": derivative = sinh(val); case "tanh": derivative = 1 - pow(tanh(val), 2)
                case "asinh": derivative = 1 / sqrt(pow(val, 2) + 1); case "acosh": derivative = 1 / sqrt(pow(val, 2) - 1); case "atanh": derivative = 1 / (1 - pow(val, 2))
                default: derivative = 0
                }
                let propagated = u.propagate(derivative: derivative)
                return (.uncertain(UncertainValue(value: resultVal, randomUncertainty: propagated.randomUncertainty, systematicUncertainty: propagated.systematicUncertainty)), argUsedAngle)
            }
            
            guard case .dimensionless(let s) = arg else { throw MathError.typeMismatch(expected: "Dimensionless or UncertainValue", found: arg.typeName) }
            return (.dimensionless(scalarFunc(s)), argUsedAngle)
        }
        
        if let userFunction = functions[node.name] {
            if userFunction.parameterNames.count == 1 && node.arguments.count == 1 {
                let (argValue, argUsedAngle) = try evaluate(node: node.arguments[0], variables: &variables, functions: &functions, angleMode: angleMode)
                if case .vector(let v) = argValue {
                    var resultValues: [Double] = []; var overallUsedAngle = argUsedAngle
                    for element in v.values {
                        var localVariables = variables
                        localVariables[userFunction.parameterNames[0]] = .dimensionless(element)
                        let (result, elementUsedAngle) = try evaluate(node: userFunction.body, variables: &localVariables, functions: &functions, angleMode: angleMode)
                        let scalarResult = try result.asScalar()
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
            case .dimensionless(let s): return .dimensionless(-s)
            case .unitValue(let u): return .unitValue(UnitValue(value: -u.value, dimensions: u.dimensions))
            case .complex(let c): return .complex(c * -1.0)
            case .vector(let v): return .vector(Vector(values: v.values.map { -$0 }))
            case .matrix(let m): return .matrix(Matrix(values: m.values.map { -$0 }, rows: m.rows, columns: m.columns))
            case .complexVector(let cv): return .complexVector(ComplexVector(values: cv.values.map { $0 * -1.0 }))
            case .complexMatrix(let cm): return .complexMatrix(ComplexMatrix(values: cm.values.map { $0 * -1.0 }, rows: cm.rows, columns: cm.columns))
            case .uncertain(let u): return .uncertain(-u)
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
            let s = try value.asScalar()
            return .dimensionless(try factorial(s))
        default: throw MathError.unknownOperator(op: op.rawValue)
        }
    }

    func evaluateBinaryOperation(op: Token, left: MathValue, right: MathValue) throws -> MathValue {
        if case .tuple = left { throw MathError.unsupportedOperation(op: op.rawValue, typeA: left.typeName, typeB: right.typeName) }
        if case .tuple = right { throw MathError.unsupportedOperation(op: op.rawValue, typeA: left.typeName, typeB: right.typeName) }
        
        if op.rawValue == "in" {
            guard case .unitValue(let sourceValue) = left else {
                throw MathError.typeMismatch(expected: "Value with units", found: left.typeName)
            }
            guard case .unitValue(let targetUnit) = right else {
                throw MathError.typeMismatch(expected: "A target unit (e.g., .cm)", found: right.typeName)
            }

            guard let targetSymbol = targetUnit.preferredDisplayUnit, let targetDef = UnitStore.units[targetSymbol] else {
                throw MathError.unknownConstant(name: "target unit for conversion")
            }
            
            guard sourceValue.dimensions == targetDef.dimensions else {
                throw MathError.dimensionMismatch(reason: "Cannot convert between incompatible units.")
            }
            
            let convertedValue = sourceValue.value / targetDef.conversionFactor
            var result = UnitValue(value: convertedValue, dimensions: sourceValue.dimensions)
            result.preferredDisplayUnit = targetSymbol
            return .unitValue(result)
        }
        
        switch (left, right) {
        // --- Unit & Dimensionless Operations ---
        case (.unitValue(let l), .unitValue(let r)): return .unitValue(try performUnitUnitOp(op.rawValue, l, r))
        case (.unitValue(let l), .dimensionless(let r)): return .unitValue(try performUnitUnitOp(op.rawValue, l, .dimensionless(r)))
        case (.dimensionless(let l), .unitValue(let r)): return .unitValue(try performUnitUnitOp(op.rawValue, .dimensionless(l), r))
        case (.dimensionless(let l), .dimensionless(let r)): return .dimensionless(try performDimensionlessOp(op.rawValue, l, r))

        // --- Uncertainty propagation rules ---
        case (.uncertain(let l), .uncertain(let r)): return .uncertain(try performUncertainUncertainOp(op.rawValue, l, r))
        case (.uncertain(let l), .dimensionless(let r)): return .uncertain(try performUncertainUncertainOp(op.rawValue, l, UncertainValue(value: r, randomUncertainty: 0, systematicUncertainty: 0)))
        case (.dimensionless(let l), .uncertain(let r)): return .uncertain(try performUncertainUncertainOp(op.rawValue, UncertainValue(value: l, randomUncertainty: 0, systematicUncertainty: 0), r))
            
        // --- Complex Number Operations ---
        case (.complex(let l), .complex(let r)): return .complex(try performComplexComplexOp(op.rawValue, l, r))
        case (.complex(let l), .dimensionless(let r)): return .complex(try performComplexComplexOp(op.rawValue, l, Complex(real: r, imaginary: 0)))
        case (.dimensionless(let l), .complex(let r)): return .complex(try performComplexComplexOp(op.rawValue, Complex(real: l, imaginary: 0), r))
        
        // --- Vector Operations ---
        case (.vector(let v), .dimensionless(let s)): return .vector(try performVectorScalarOp(op.rawValue, v, s))
        case (.dimensionless(let s), .vector(let v)): return .vector(try performVectorScalarOp(op.rawValue, v, s, reversed: true))
        case (.vector(let l), .vector(let r)): return .vector(try performVectorVectorOp(op.rawValue, l, r))
            
        // --- Matrix Operations ---
        case (.matrix(let m), .dimensionless(let s)): return .matrix(try performMatrixScalarOp(op.rawValue, m, s))
        case (.dimensionless(let s), .matrix(let m)): return .matrix(try performMatrixScalarOp(op.rawValue, m, s, reversed: true))
        case (.matrix(let l), .matrix(let r)): return .matrix(try performMatrixMatrixOp(op.rawValue, l, r))
        case (.matrix(let m), .vector(let v)):
             if op.rawValue == "*" { return .vector(try m * v) }
             else { throw MathError.unsupportedOperation(op: op.rawValue, typeA: left.typeName, typeB: right.typeName) }
        
        // --- Complex Vector/Matrix Operations ---
        case (.complexVector(let cv), .complex(let c)): return .complexVector(try performComplexVectorComplexOp(op.rawValue, cv, c))
        case (.complex(let c), .complexVector(let cv)): return .complexVector(try performComplexVectorComplexOp(op.rawValue, cv, c, reversed: true))
        case (.complexVector(let l), .complexVector(let r)): return .complexVector(try performCVectorCVectorOp(op.rawValue, l, r))
        case (.complexMatrix(let cm), .complex(let c)): return .complexMatrix(try performComplexMatrixComplexOp(op.rawValue, cm, c))
        case (.complex(let c), .complexMatrix(let cm)): return .complexMatrix(try performComplexMatrixComplexOp(op.rawValue, cm, c, reversed: true))
        case (.complexMatrix(let l), .complexMatrix(let r)): return .complexMatrix(try performCMatrixCMatrixOp(op.rawValue, l, r))
        case (.complexMatrix(let m), .complexVector(let v)):
            if op.rawValue == "*" { return .complexVector(try m * v) }
            else { throw MathError.unsupportedOperation(op: op.rawValue, typeA: left.typeName, typeB: right.typeName) }
        
        // --- Promotion cases for mixed types ---
        case (.matrix, .complex), (.complex, .matrix), (.vector, .complex), (.complex, .vector), (.complexMatrix, .dimensionless), (.dimensionless, .complexMatrix), (.complexVector, .dimensionless), (.dimensionless, .complexVector):
            let (promotedL, promotedR) = try promote(left, right)
            return try evaluateBinaryOperation(op: op, left: promotedL, right: promotedR)
        default: throw MathError.unsupportedOperation(op: op.rawValue, typeA: left.typeName, typeB: right.typeName)
        }
    }
    
    func evaluateIndexedAssignment(op: Token, target: MathValue, indexedOp: IndexedOperationNode, variables: inout [String: MathValue], functions: inout [String: FunctionDefinitionNode], angleMode: AngleMode) throws -> (MathValue, Bool) {
        let (indexValue, indexUsedAngle) = try _evaluateSingle(node: indexedOp.index, variables: &variables, functions: &functions, angleMode: angleMode)
        let (scalarValue, scalarUsedAngle) = try _evaluateSingle(node: indexedOp.scalar, variables: &variables, functions: &functions, angleMode: angleMode)
        
        let indexScalar = try indexValue.asScalar()
        guard indexScalar.truncatingRemainder(dividingBy: 1) == 0 else { throw MathError.typeMismatch(expected: "Integer for index", found: indexValue.typeName) }
        let oneBasedIndex = Int(indexScalar); let zeroBasedIndex = oneBasedIndex - 1

        let result: MathValue
        switch target {
        case .vector(let v):
            let s = try scalarValue.asScalar()
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
            if case .dimensionless(let s) = scalarValue { complexScalar = Complex(real: s, imaginary: 0) }
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

    private func performUncertainUncertainOp(_ op: String, _ l: UncertainValue, _ r: UncertainValue) throws -> UncertainValue {
        switch op {
        case "+": return l + r; case "-": return l - r; case "*": return l * r
        case "/": return try l / r
        case "^": return l.pow(r.value)
        default: throw MathError.unknownOperator(op: op)
        }
    }
    private func performDimensionlessOp(_ op: String, _ l: Double, _ r: Double) throws -> Double {
        switch op {
        case "+": return l + r; case "-": return l - r; case "*": return l * r
        case "/": guard r != 0 else { throw MathError.divisionByZero }; return l / r
        case "%": guard r != 0 else { throw MathError.divisionByZero }; return l.truncatingRemainder(dividingBy: r)
        case "^": return pow(l, r)
        // New comparison operators
        case ">": return l > r ? 1.0 : 0.0
        case "<": return l < r ? 1.0 : 0.0
        case ">=": return l >= r ? 1.0 : 0.0
        case "<=": return l <= r ? 1.0 : 0.0
        case "==": return l == r ? 1.0 : 0.0
        case "!=": return l != r ? 1.0 : 0.0
        default: throw MathError.unknownOperator(op: op)
        }
    }
    private func performUnitUnitOp(_ op: String, _ l: UnitValue, _ r: UnitValue) throws -> UnitValue {
        switch op {
        case "+": return try l + r
        case "-": return try l - r
        case "*": return l * r
        case "/": return try l / r
        case "^":
            guard r.dimensions.isEmpty else { throw MathError.typeMismatch(expected: "Dimensionless exponent", found: "Value with units") }
            return l.pow(r.value)
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
        case (.matrix(let m), .complex(let c)): return (.complexMatrix(ComplexMatrix(from: m)), .complex(c)); case (.complex(let c), .matrix(let m)): return (.complex(c), .complexMatrix(ComplexMatrix(from: m))); case (.vector(let v), .complex(let c)): return (.complexVector(ComplexVector(from: v)), .complex(c)); case (.complex(let c), .vector(let v)): return (.complex(c), .complexVector(ComplexVector(from: v))); case (.complexMatrix(let cm), .dimensionless(let s)): return (.complexMatrix(cm), .complex(Complex(real: s, imaginary: 0))); case (.dimensionless(let s), .complexMatrix(let cm)): return (.complex(Complex(real: s, imaginary: 0)), .complexMatrix(cm)); case (.complexVector(let cv), .dimensionless(let s)): return (.complexVector(cv), .complex(Complex(real: s, imaginary: 0))); case (.dimensionless(let s), .complexVector(let cv)): return (.complex(Complex(real: s, imaginary: 0)), .complexVector(cv)); default: return (left, right)
        }
    }
}

/// Helper function to extract all scalar values from a MathValue.
fileprivate func extractDoubles(from data: MathValue) throws -> [Double] {
    switch data {
    case .dimensionless(let s):
        return [s]
    case .vector(let v):
        return v.values
    case .matrix(let m):
        return m.values
    case .uncertain(let u):
        return [u.value]
    default:
        throw MathError.typeMismatch(expected: "Vector, Matrix, or Dimensionless value", found: data.typeName)
    }
}

/// Helper for variadic functions that can accept a list of scalars or a single collection.
fileprivate func extractDoublesFromVariadicArgs(_ args: [MathValue]) throws -> [Double] {
    if args.count == 1 {
        return try extractDoubles(from: args[0])
    } else {
        var scalars: [Double] = []
        for arg in args {
            scalars.append(try arg.asScalar())
        }
        return scalars
    }
}

/// Helper function for variadic statistical functions like sum(), avg(), etc.
fileprivate func performStatisticalOperation(args: [MathValue], on operation: (Vector) -> Double?) throws -> MathValue {
    // This is a new, unit-aware implementation
    let (values, finalDimension) = try extractAndConvertUnitValues(from: args)
    guard !values.isEmpty else { throw MathError.requiresAtLeastOneArgument(function: "Statistical function") }
    let v = Vector(values: values)
    guard let resultValue = operation(v) else { throw MathError.unsupportedOperation(op: "Statistical", typeA: "Operation failed (e.g., stddev on single element)", typeB: nil) }

    if finalDimension.isEmpty {
        return .dimensionless(resultValue)
    } else {
        return .unitValue(UnitValue(value: resultValue, dimensions: finalDimension))
    }
}

/// Extracts and validates values for statistical functions, ensuring unit consistency.
fileprivate func extractAndConvertUnitValues(from args: [MathValue]) throws -> (values: [Double], dimensions: UnitDimension) {
    var allValues: [Double] = []
    var commonDimension: UnitDimension? = nil

    // Inner function to process a single MathValue, checking for unit consistency.
    func processValue(_ val: MathValue) throws {
        switch val {
        case .dimensionless(let d):
            if commonDimension == nil { commonDimension = [:] }
            if commonDimension != [:] { throw MathError.dimensionMismatch(reason: "Cannot mix units and dimensionless numbers.") }
            allValues.append(d)
        case .unitValue(let u):
            if commonDimension == nil { commonDimension = u.dimensions }
            if commonDimension != u.dimensions { throw MathError.dimensionMismatch(reason: "All values must have the same units.") }
            allValues.append(u.value)
        case .vector(let v): // Vectors are currently always dimensionless
            if commonDimension == nil { commonDimension = [:] }
            if commonDimension != [:] { throw MathError.dimensionMismatch(reason: "Cannot mix units and dimensionless numbers.") }
            allValues.append(contentsOf: v.values)
        case .matrix(let m): // Matrices are currently always dimensionless
            if commonDimension == nil { commonDimension = [:] }
            if commonDimension != [:] { throw MathError.dimensionMismatch(reason: "Cannot mix units and dimensionless numbers.") }
            allValues.append(contentsOf: m.values)
        default:
            throw MathError.typeMismatch(expected: "Numeric value", found: val.typeName)
        }
    }

    if args.count == 1 {
        // If there's only one argument, it could be a vector or matrix itself.
        try processValue(args[0])
    } else {
        // Otherwise, process each argument as a separate value.
        for arg in args {
            try processValue(arg)
        }
    }

    return (allValues, commonDimension ?? [:])
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

fileprivate func performAggregateIntegerOperation(args: [MathValue], initialValue: Double, operation: (Double, Double) -> Double) throws -> MathValue {
    let extractIntegers: ([MathValue]) throws -> [Double] = { values in
        var integers: [Double] = []
        for value in values {
            switch value {
            case .dimensionless, .uncertain:
                let val = try value.asScalar()
                guard val.truncatingRemainder(dividingBy: 1) == 0 else { throw MathError.unsupportedOperation(op: "Integer", typeA: "arguments must be integers", typeB: nil) }
                integers.append(val)
            case .vector(let v):
                guard v.values.allSatisfy({ $0.truncatingRemainder(dividingBy: 1) == 0 }) else { throw MathError.unsupportedOperation(op: "Integer", typeA: "vector elements must be integers", typeB: nil) }
                integers.append(contentsOf: v.values)
            case .matrix(let m):
                guard m.values.allSatisfy({ $0.truncatingRemainder(dividingBy: 1) == 0 }) else { throw MathError.unsupportedOperation(op: "Integer", typeA: "matrix elements must be integers", typeB: nil) }
                integers.append(contentsOf: m.values)
            default:
                throw MathError.typeMismatch(expected: "Scalars, a Vector, or a Matrix", found: value.typeName)
            }
        }
        return integers
    }

    let integers = try extractIntegers(args)
    guard !integers.isEmpty else { throw MathError.requiresAtLeastOneArgument(function: "Integer operation") }

    let result = integers.reduce(initialValue, operation)
    return .dimensionless(result)
}


fileprivate func performElementWiseIntegerOp(_ a: MathValue, _ b: MathValue, opName: String, operation: (Double, Double) -> Double) throws -> MathValue {
    let checkInt: (Double) throws -> Double = { val in
        guard val.truncatingRemainder(dividingBy: 1) == 0 else { throw MathError.unsupportedOperation(op: opName, typeA: "arguments must be integers", typeB: nil) }
        return val
    }
    
    switch (a, b) {
    case (.dimensionless(let n1), .dimensionless(let n2)):
        return .dimensionless(operation(try checkInt(n1), try checkInt(n2)))
        
    case (.vector(let v), .dimensionless(let s)):
        let checkedS = try checkInt(s)
        let results = try v.values.map { operation(try checkInt($0), checkedS) }
        return .vector(Vector(values: results))
        
    case (.dimensionless(let s), .vector(let v)):
        let checkedS = try checkInt(s)
        let results = try v.values.map { operation(checkedS, try checkInt($0)) }
        return .vector(Vector(values: results))
        
    case (.vector(let v1), .vector(let v2)):
        guard v1.dimension == v2.dimension else { throw MathError.dimensionMismatch(reason: "Vectors must have the same dimension for element-wise \(opName).") }
        let results = try zip(v1.values, v2.values).map { operation(try checkInt($0), try checkInt($1)) }
        return .vector(Vector(values: results))

    default:
        throw MathError.typeMismatch(expected: "Scalars or Vectors of integers", found: "\(a.typeName), \(b.typeName)")
    }
}

// MARK: - New Algorithms

/// Calculates the probability density function (PDF) for the normal distribution.
fileprivate func normalDistribution(x: Double, mean: Double, stddev: Double) -> Double {
    guard stddev > 0 else { return Double.nan } // Or throw an error
    let variance = pow(stddev, 2)
    let coefficient = 1.0 / sqrt(2.0 * .pi * variance)
    let exponent = -pow(x - mean, 2) / (2.0 * variance)
    return coefficient * exp(exponent)
}

/// Calculates the probability mass function (PMF) for the binomial distribution.
fileprivate func binomialDistribution(k: Double, n: Double, p: Double) throws -> Double {
    guard k >= 0, n >= 0, p >= 0, p <= 1,
          k.truncatingRemainder(dividingBy: 1) == 0,
          n.truncatingRemainder(dividingBy: 1) == 0,
          k <= n else {
        throw MathError.unsupportedOperation(op: "binomdist", typeA: "Invalid arguments", typeB: nil)
    }
    let combinations_nk = try combinations(n: n, k: k)
    return combinations_nk * pow(p, k) * pow(1 - p, n - k)
}

/// Checks if an integer is prime using optimized trial division.
fileprivate func performIsPrime(_ n: Double) throws -> Bool {
    guard n.truncatingRemainder(dividingBy: 1) == 0 else {
        throw MathError.unsupportedOperation(op: "isprime", typeA: "argument must be an integer", typeB: nil)
    }
    let num = Int(n)
    guard num >= 2 else { return false }
    if num == 2 || num == 3 { return true }
    if num % 2 == 0 || num % 3 == 0 { return false }
    
    var i = 5
    while i * i <= num {
        if num % i == 0 || num % (i + 2) == 0 {
            return false
        }
        i += 6
    }
    return true
}

/// Returns a vector of prime factors of an integer.
fileprivate func performFactor(_ n: Double) throws -> [Double] {
    guard n.truncatingRemainder(dividingBy: 1) == 0 else {
        throw MathError.unsupportedOperation(op: "factor", typeA: "argument must be an integer", typeB: nil)
    }
    var num = Int(abs(n))
    guard num >= 2 else { return [n] }
    
    var factors: [Double] = []
    
    while num % 2 == 0 {
        factors.append(2)
        num /= 2
    }
    
    var i = 3
    while i * i <= num {
        while num % i == 0 {
            factors.append(Double(i))
            num /= i
        }
        i += 2
    }
    
    if num > 1 {
        factors.append(Double(num))
    }
    
    return factors
}

/// Helper for percentile and quartile functions
fileprivate func performPercentile(values: [Double], p: Double) -> Double {
    if p == 100 { return values.last! }
    
    let rank = (p / 100.0) * Double(values.count - 1)
    let lowerIndex = Int(floor(rank))
    let upperIndex = Int(ceil(rank))
    
    if lowerIndex == upperIndex {
        return values[lowerIndex]
    } else {
        let lowerValue = values[lowerIndex]
        let upperValue = values[upperIndex]
        return lowerValue + (rank - Double(lowerIndex)) * (upperValue - lowerValue)
    }
}
