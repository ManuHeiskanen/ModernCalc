//
//  EvaluatorDefinitions.swift
//  ModernCalc
//
//  Created by Manu Heiskanen on 16.9.2025.
//

import Foundation
import Accelerate

/// This extension contains all the built-in constant and function definitions,
/// as well as the logic for handling function calls.
extension Evaluator {
    
    // MARK: - Constants and Prefixes
    
    static let constants: [String: Double] = [
        "pi": Double.pi, "e": M_E, "c": 299792458, "μ0": 1.25663706212e-6, "ε0": 8.8541878128e-12,
        "g": 9.80665, "G": 6.67430e-11, "h": 6.62607015e-34, "ħ": 1.054571817e-34, "me": 9.1093837015e-31,
        "mp": 1.67262192369e-27, "mn": 1.67492749804e-27, "e0": 1.602176634e-19, "NA": 6.02214076e23,
        "R": 8.314462618, "kB": 1.380649e-23, "F": 96485.33212, "Rinf": 10973731.568160, "σ": 5.670374419e-8,
        "b": 2.897771955e-3, "atm": 101325, "Vm": 22.41396954e-3,
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
        "sech": { 1.0 / cosh($0) },
        "csch": { 1.0 / sinh($0) },
        "coth": { 1.0 / tanh($0) },
        "asech": { acosh(1.0 / $0) },
        "acsch": { asinh(1.0 / $0) },
        "acoth": { atanh(1.0 / $0) },
    ]
    
    static let variadicFunctions: [String: ([MathValue]) throws -> MathValue] = [
        "sum": { args in try performStatisticalOperation(args: args, on: { .unitValue($0.sum()) }) },
        "avg": { args in try performStatisticalOperation(args: args, on: { .unitValue($0.average()) }) },
        "average": { args in try performStatisticalOperation(args: args, on: { .unitValue($0.average()) }) },
        "mean": { args in try performStatisticalOperation(args: args, on: { .unitValue($0.average()) }) },
        "min": { args in try performStatisticalOperation(args: args, on: { $0.min().map { .unitValue($0) } }) },
        "max": { args in try performStatisticalOperation(args: args, on: { $0.max().map { .unitValue($0) } }) },
        "median": { args in try performStatisticalOperation(args: args, on: { $0.median().map { .unitValue($0) } }) },
        "stddev": { args in try performStatisticalOperation(args: args, on: { $0.stddev().map { .unitValue($0) } }) },
        "variance": { args in
            try performStatisticalOperation(args: args, on: { $0.variance().map { .unitValue($0) } })
        },
        "stddevp": { args in try performStatisticalOperation(args: args, on: { $0.stddevp().map { .unitValue($0) } }) },
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
            guard args.count == 3 else {
                throw MathError.incorrectArgumentCount(function: "if", expected: "3", found: args.count)
            }
            
            let trueValue = args[1]
            let falseValue = args[2]

            // Helper to create a "signature" for a value to check for compatibility.
            // This signature includes both its general type and its specific unit dimension.
            let getCompatibilitySignature = { (value: MathValue) -> (String, UnitDimension) in
                switch value {
                case .dimensionless: return ("numeric", [:])
                case .unitValue(let u): return ("numeric", u.dimensions)
                case .uncertain(let u): return ("numeric", u.dimensions) // MODIFIED
                case .vector(let v): return ("Vector", v.dimensions)
                case .matrix(let m): return ("Matrix", m.dimensions)
                case .complexVector(let cv): return ("ComplexVector", cv.dimensions)
                case .complexMatrix(let cm): return ("ComplexMatrix", cm.dimensions)
                default: return (value.typeName, [:])
                }
            }

            let trueSignature = getCompatibilitySignature(trueValue)
            let falseSignature = getCompatibilitySignature(falseValue)

            // Compare the signatures. If they don't match, throw a detailed error.
            if trueSignature != falseSignature {
                if trueSignature.0 != falseSignature.0 { // E.g., comparing a number to a vector
                    throw MathError.dimensionMismatch(reason: "The 'true' and 'false' results of an if statement must be the same type (e.g., both numbers, or both vectors). Found \(trueSignature.0) and \(falseSignature.0).")
                } else { // E.g., comparing meters to seconds
                     throw MathError.dimensionMismatch(reason: "The 'true' and 'false' results of an if statement must have compatible units.")
                }
            }
            
            // If the checks pass, proceed with the original logic.
            let condition = try args[0].asScalar()
            return condition != 0 ? trueValue : falseValue
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
            let xVec = try args[0].asVector(for: "polyfit")
            let yVec = try args[1].asVector(for: "polyfit")
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
        "powerspectrum": { args in
            guard args.count == 2 else { throw MathError.incorrectArgumentCount(function: "powerspectrum", expected: "2 (data, sampling_rate)", found: args.count) }
            
            let data = args[0]
            let samplingRateValue = try args[1].asScalar()
            
            guard samplingRateValue > 0 else { throw MathError.unsupportedOperation(op: "powerspectrum", typeA: "Sampling rate must be positive.", typeB: nil) }

            let inputVector: ComplexVector
            switch data {
            case .vector(let v):
                inputVector = ComplexVector(from: v)
            case .matrix(let m) where m.rows == 1 || m.columns == 1:
                let complexValues = m.values.map { Complex(real: $0, imaginary: 0) }
                inputVector = ComplexVector(values: complexValues, dimensions: m.dimensions)
            default:
                throw MathError.typeMismatch(expected: "Vector or Matrix for data", found: data.typeName)
            }
            
            // Call the static helper function to perform the FFT
            let fftResult = try Evaluator.performFFT(inputVector: inputVector, direction: FFTDirection(kFFTDirection_Forward))
            
            let n = fftResult.dimension
            
            // Calculate magnitudes. For real inputs, the spectrum is symmetric, so we only need the first half.
            let magnitudes = fftResult.values.prefix(n / 2).map { $0.abs() }
            
            // Create the corresponding frequency axis using the sampling rate
            let frequencyStep = samplingRateValue / Double(n)
            let frequencies = (0..<n/2).map { Double($0) * frequencyStep }
            
            let dataPoints = zip(frequencies, magnitudes).map { DataPoint(x: $0, y: $1) }
            
            let series = PlotSeries(name: "Power Spectrum", dataPoints: dataPoints)
            let plotData = PlotData(expression: "powerspectrum(...)", series: [series], plotType: .line, explicitYRange: nil, generationTime: 0, xAxisLabel: "Frequency (Hz)", yAxisLabel: "Magnitude")
            
            return .plot(plotData)
        },
    ]
    
    static let singleArgumentFunctions: [String: (MathValue) throws -> MathValue] = [
        "abs": { arg in
            switch arg {
            case .dimensionless(let s): return .dimensionless(abs(s))
            case .unitValue(let u): return .unitValue(UnitValue(value: abs(u.value), dimensions: u.dimensions))
            case .complex(let c): return .dimensionless(c.abs())
            case .vector(let v): return .unitValue(v.magnitude())
            case .uncertain(let u): return .uncertain(UncertainValue(value: abs(u.value), randomUncertainty: u.randomUncertainty, systematicUncertainty: u.systematicUncertainty, dimensions: u.dimensions))
            default: throw MathError.typeMismatch(expected: "Scalar, Complex, Vector, or UncertainValue", found: arg.typeName)
            }
        },
        "norm": { arg in
             switch arg {
             case .vector(let v): return .unitValue(v.magnitude())
             case .matrix(let m): return .unitValue(m.frobeniusNorm())
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
            case .matrix(let m): let det = try m.determinant(); return .unitValue(det)
            case .complexMatrix(let cm): let (det, dims) = try cm.determinant(); if dims.isEmpty { return .complex(det) } else { throw MathError.unsupportedOperation(op: "det", typeA: "ComplexMatrix with units", typeB: nil) }
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
        "unit": { arg in return .vector(try arg.asVector(for: "unit").unit()) },
        "transpose": { arg in
            switch arg {
            case .matrix(let m): return .matrix(m.transpose())
            case .complexMatrix(let cm): return .complexMatrix(cm.transpose())
            default: throw MathError.typeMismatch(expected: "Matrix", found: arg.typeName)
            }
        },
        "ctranspose": { arg in
            switch arg {
            case .complexMatrix(let cm):
                return .complexMatrix(cm.conjugateTranspose())
            case .complexVector(let cv):
                return .complexMatrix(cv.conjugateTranspose())
            default:
                throw MathError.typeMismatch(expected: "ComplexMatrix or ComplexVector", found: arg.typeName)
            }
        },
        "trace": { arg in
            switch arg {
            case .matrix(let m): return .unitValue(try m.trace())
            case .complexMatrix(let cm): let (trace, dims) = try cm.trace(); if dims.isEmpty { return .complex(trace) } else { throw MathError.unsupportedOperation(op: "trace", typeA: "ComplexMatrix with units", typeB: nil) }
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
            guard args.count == 1 else { throw MathError.incorrectArgumentCount(function: "sin", expected: "1", found: args.count) }
            let arg = args[0]
            
            // FIX: Add element-wise operation for vectors and matrices
            switch arg {
            case .vector(let v):
                let newValues = v.values.map {
                    let valRad = mode == .degrees ? $0 * .pi / 180 : $0
                    return sin(valRad)
                }
                return .vector(Vector(values: newValues)) // Result is dimensionless
            case .matrix(let m) where m.rows == 1 || m.columns == 1:
                let newValues = m.values.map {
                    let valRad = mode == .degrees ? $0 * .pi / 180 : $0
                    return sin(valRad)
                }
                return .vector(Vector(values: newValues))
            case .uncertain(let u):
                guard u.dimensions.isEmpty else { throw MathError.typeMismatch(expected: "Dimensionless value for sin", found: "Uncertain value with units") }
                
                let valRad = mode == .degrees ? u.value * .pi / 180 : u.value
                let randRad = mode == .degrees ? u.randomUncertainty * .pi / 180 : u.randomUncertainty
                let sysRad = mode == .degrees ? u.systematicUncertainty * .pi / 180 : u.systematicUncertainty
                
                let u_rad = UncertainValue(value: valRad, randomUncertainty: randRad, systematicUncertainty: sysRad, dimensions: [:])
                let propagated = u_rad.propagate(derivative: cos(valRad))

                return .uncertain(UncertainValue(value: sin(valRad),
                                                 randomUncertainty: propagated.randomUncertainty,
                                                 systematicUncertainty: propagated.systematicUncertainty,
                                                 dimensions: [:]))
            default:
                 let s = try arg.asScalar(); let valRad = mode == .degrees ? s * .pi / 180 : s; return .dimensionless(sin(valRad))
            }
        },
        "cos": { args, mode in
            guard args.count == 1 else { throw MathError.incorrectArgumentCount(function: "cos", expected: "1", found: args.count) }
            let arg = args[0]
            
            // FIX: Add element-wise operation for vectors and matrices
            switch arg {
            case .vector(let v):
                let newValues = v.values.map {
                    let valRad = mode == .degrees ? $0 * .pi / 180 : $0
                    return cos(valRad)
                }
                return .vector(Vector(values: newValues))
            case .matrix(let m) where m.rows == 1 || m.columns == 1:
                let newValues = m.values.map {
                    let valRad = mode == .degrees ? $0 * .pi / 180 : $0
                    return cos(valRad)
                }
                return .vector(Vector(values: newValues))
            case .uncertain(let u):
                guard u.dimensions.isEmpty else { throw MathError.typeMismatch(expected: "Dimensionless value for cos", found: "Uncertain value with units") }

                let valRad = mode == .degrees ? u.value * .pi / 180 : u.value
                let randRad = mode == .degrees ? u.randomUncertainty * .pi / 180 : u.randomUncertainty
                let sysRad = mode == .degrees ? u.systematicUncertainty * .pi / 180 : u.systematicUncertainty

                let u_rad = UncertainValue(value: valRad, randomUncertainty: randRad, systematicUncertainty: sysRad, dimensions: [:])
                let propagated = u_rad.propagate(derivative: -sin(valRad))

                return .uncertain(UncertainValue(value: cos(valRad),
                                                 randomUncertainty: propagated.randomUncertainty,
                                                 systematicUncertainty: propagated.systematicUncertainty,
                                                 dimensions: [:]))
            default:
                let s = try arg.asScalar(); let valRad = mode == .degrees ? s * .pi / 180 : s; return .dimensionless(cos(valRad))
            }
        },
        "tan": { args, mode in
            guard args.count == 1 else { throw MathError.incorrectArgumentCount(function: "tan", expected: "1", found: args.count) }
            let arg = args[0]

            // FIX: Add element-wise operation for vectors and matrices
            switch arg {
            case .vector(let v):
                let newValues = v.values.map {
                    let valRad = mode == .degrees ? $0 * .pi / 180 : $0
                    return tan(valRad)
                }
                return .vector(Vector(values: newValues))
            case .matrix(let m) where m.rows == 1 || m.columns == 1:
                let newValues = m.values.map {
                    let valRad = mode == .degrees ? $0 * .pi / 180 : $0
                    return tan(valRad)
                }
                return .vector(Vector(values: newValues))
            case .uncertain(let u):
                guard u.dimensions.isEmpty else { throw MathError.typeMismatch(expected: "Dimensionless value for tan", found: "Uncertain value with units") }
                
                let valRad = mode == .degrees ? u.value * .pi / 180 : u.value
                let randRad = mode == .degrees ? u.randomUncertainty * .pi / 180 : u.randomUncertainty
                let sysRad = mode == .degrees ? u.systematicUncertainty * .pi / 180 : u.systematicUncertainty
                
                let u_rad = UncertainValue(value: valRad, randomUncertainty: randRad, systematicUncertainty: sysRad, dimensions: [:])
                let propagated = u_rad.propagate(derivative: 1.0 / pow(cos(valRad), 2))
                
                return .uncertain(UncertainValue(value: tan(valRad),
                                                 randomUncertainty: propagated.randomUncertainty,
                                                 systematicUncertainty: propagated.systematicUncertainty,
                                                 dimensions: [:]))
            default:
                let s = try arg.asScalar(); let valRad = mode == .degrees ? s * .pi / 180 : s; return .dimensionless(tan(valRad))
            }
        },
        "sec": { args, mode in let s = try args[0].asScalar(); let valRad = mode == .degrees ? s * .pi / 180 : s; return .dimensionless(1.0 / cos(valRad)) },
        "csc": { args, mode in let s = try args[0].asScalar(); let valRad = mode == .degrees ? s * .pi / 180 : s; return .dimensionless(1.0 / sin(valRad)) },
        "cot": { args, mode in let s = try args[0].asScalar(); let valRad = mode == .degrees ? s * .pi / 180 : s; return .dimensionless(1.0 / tan(valRad)) },
        "asin": { args, mode in
            if case .uncertain(let u) = args[0] {
                guard u.dimensions.isEmpty else { throw MathError.typeMismatch(expected: "Dimensionless value for asin", found: "Uncertain value with units") }
                let val = u.value
                let derivative = 1.0 / sqrt(1.0 - pow(val, 2))
                let propagated = u.propagate(derivative: derivative)
                
                var result = UncertainValue(value: asin(val), randomUncertainty: propagated.randomUncertainty, systematicUncertainty: propagated.systematicUncertainty, dimensions: [:])
                if mode == .degrees {
                    result.value *= 180 / .pi
                    result.randomUncertainty *= 180 / .pi
                    result.systematicUncertainty *= 180 / .pi
                }
                return .uncertain(result)
            }
            let a = asin(try args[0].asScalar()); return .dimensionless(mode == .degrees ? a * 180 / .pi : a)
        },
        "acos": { args, mode in
            if case .uncertain(let u) = args[0] {
                guard u.dimensions.isEmpty else { throw MathError.typeMismatch(expected: "Dimensionless value for acos", found: "Uncertain value with units") }
                let val = u.value
                let derivative = -1.0 / sqrt(1.0 - pow(val, 2))
                let propagated = u.propagate(derivative: derivative)
                
                var result = UncertainValue(value: acos(val), randomUncertainty: propagated.randomUncertainty, systematicUncertainty: propagated.systematicUncertainty, dimensions: [:])
                if mode == .degrees {
                    result.value *= 180 / .pi
                    result.randomUncertainty *= 180 / .pi
                    result.systematicUncertainty *= 180 / .pi
                }
                return .uncertain(result)
            }
            let a = acos(try args[0].asScalar()); return .dimensionless(mode == .degrees ? a * 180 / .pi : a)
        },
        "atan": { args, mode in
            if case .uncertain(let u) = args[0] {
                guard u.dimensions.isEmpty else { throw MathError.typeMismatch(expected: "Dimensionless value for atan", found: "Uncertain value with units") }
                let val = u.value
                let derivative = 1.0 / (1.0 + pow(val, 2))
                let propagated = u.propagate(derivative: derivative)
                
                var result = UncertainValue(value: atan(val), randomUncertainty: propagated.randomUncertainty, systematicUncertainty: propagated.systematicUncertainty, dimensions: [:])
                if mode == .degrees {
                    result.value *= 180 / .pi
                    result.randomUncertainty *= 180 / .pi
                    result.systematicUncertainty *= 180 / .pi
                }
                return .uncertain(result)
            }
            let a = atan(try args[0].asScalar()); return .dimensionless(mode == .degrees ? a * 180 / .pi : a)
        },
        "asec": { args, mode in let a = acos(1.0 / (try args[0].asScalar())); return .dimensionless(mode == .degrees ? a * 180 / .pi : a) },
        "acsc": { args, mode in let a = asin(1.0 / (try args[0].asScalar())); return .dimensionless(mode == .degrees ? a * 180 / .pi : a) },
        "acot": { args, mode in let a = atan(1.0 / (try args[0].asScalar())); return .dimensionless(mode == .degrees ? a * 180 / .pi : a) },
        "atan2": { args, mode in let a = Foundation.atan2(try args[0].asScalar(), try args[1].asScalar()); return .dimensionless(mode == .degrees ? a * 180 / .pi : a) },
        "arg": { args, mode in
            guard args.count == 1, case .complex(let c) = args[0] else { throw MathError.typeMismatch(expected: "Complex", found: args.first?.typeName ?? "none") }
            let a = c.argument(); return .dimensionless(mode == .degrees ? a * 180 / .pi : a)
        },
        "angle": { args, mode in
            guard args.count == 2 else { throw MathError.incorrectArgumentCount(function: "angle", expected: "2", found: args.count) }
            let v1 = try args[0].asVector(for: "angle")
            let v2 = try args[1].asVector(for: "angle")
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
            let v1 = try a.asVector(for: "rmse")
            let v2 = try b.asVector(for: "rmse")
            guard v1.dimension == v2.dimension else { throw MathError.dimensionMismatch(reason: "Vectors must have the same dimension for RMSE.") }
            guard v1.dimensions == v2.dimensions else { throw MathError.dimensionMismatch(reason: "Vectors must have the same units for RMSE.") }
            guard v1.dimension > 0 else { return .dimensionless(0) }
            let squaredErrors = zip(v1.values, v2.values).map { pow($0 - $1, 2) }
            let meanSquaredError = squaredErrors.reduce(0, +) / Double(v1.dimension)
            let finalDimensions = v1.dimensions
            return .unitValue(UnitValue(value: sqrt(meanSquaredError), dimensions: finalDimensions))
        },
        "rmsd": { a, b in // alias for rmse
            let v1 = try a.asVector(for: "rmsd")
            let v2 = try b.asVector(for: "rmsd")
            guard v1.dimension == v2.dimension else { throw MathError.dimensionMismatch(reason: "Vectors must have the same dimension for RMSD.") }
            guard v1.dimensions == v2.dimensions else { throw MathError.dimensionMismatch(reason: "Vectors must have the same units for RMSD.") }
            guard v1.dimension > 0 else { return .dimensionless(0) }
            let squaredErrors = zip(v1.values, v2.values).map { pow($0 - $1, 2) }
            let meanSquaredError = squaredErrors.reduce(0, +) / Double(v1.dimension)
            let finalDimensions = v1.dimensions
            return .unitValue(UnitValue(value: sqrt(meanSquaredError), dimensions: finalDimensions))
        },
        "log": { a, b in
            let base = try a.asScalar()
            let number = try b.asScalar()
            guard base > 0, base != 1, number > 0 else { throw MathError.unsupportedOperation(op: "log", typeA: "Logarithm base must be > 0 and != 1, and the number must be > 0.", typeB: nil) }
            return .dimensionless(Foundation.log(number) / Foundation.log(base))
        },
        "cov": { a, b in
            let xVec = try a.asVector(for: "cov")
            let yVec = try b.asVector(for: "cov")
            guard xVec.dimension == yVec.dimension, xVec.dimension >= 2 else { throw MathError.dimensionMismatch(reason: "Vectors must have the same number of elements (at least 2) for covariance.") }
            
            let n = Double(xVec.dimension)
            let meanX = xVec.average().value
            let meanY = yVec.average().value
            
            let sumOfProducts = zip(xVec.values, yVec.values).map { ($0 - meanX) * ($1 - meanY) }.reduce(0, +)
            
            let covValue = sumOfProducts / (n - 1)
            let newDimensions = xVec.dimensions.merging(yVec.dimensions, uniquingKeysWith: +).filter { $0.value != 0 }
            return .unitValue(UnitValue(value: covValue, dimensions: newDimensions))
        },
        "corr": { a, b in
            let xVec = try a.asVector(for: "corr")
            let yVec = try b.asVector(for: "corr")
            guard xVec.dimension == yVec.dimension, xVec.dimension >= 2 else { throw MathError.dimensionMismatch(reason: "Vectors must have the same number of elements (at least 2) for correlation.") }
            let n = Double(xVec.dimension)
            let sumX = xVec.sum().value
            let sumY = yVec.sum().value
            let sumXY = try xVec.hadamard(with: yVec).sum().value
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
            let xVec = try a.asVector(for: "linreg")
            let yVec = try b.asVector(for: "linreg")
            guard xVec.dimension == yVec.dimension, xVec.dimension >= 2 else { throw MathError.dimensionMismatch(reason: "Vectors must have the same number of elements (at least 2) for linear regression.") }
            let n = Double(xVec.dimension)
            let sumX = xVec.sum().value
            let sumY = yVec.sum().value
            let sumXY = try xVec.hadamard(with: yVec).sum().value
            let sumX2 = xVec.values.map { $0 * $0 }.reduce(0, +)
            
            let denominator = (n * sumX2 - sumX * sumX)
            guard denominator != 0 else { throw MathError.unsupportedOperation(op: "linreg", typeA: "Cannot perform regression on vertical line (undefined slope)", typeB: nil) }
            
            let slopeVal = (n * sumXY - sumX * sumY) / denominator
            let interceptVal = (sumY - slopeVal * sumX) / n
            
            let slopeDimensions = yVec.dimensions.merging(xVec.dimensions.mapValues { -$0 }, uniquingKeysWith: +).filter { $0.value != 0 }
            let interceptDimensions = yVec.dimensions
            
            let slopeUnitValue = UnitValue.create(value: slopeVal, dimensions: slopeDimensions)
            let interceptUnitValue = UnitValue.create(value: interceptVal, dimensions: interceptDimensions)
            
            return .regressionResult(slope: slopeUnitValue, intercept: interceptUnitValue)
        },
        "linsolve": { a, b in
            guard case .matrix(let matrixA) = a else {
                throw MathError.typeMismatch(expected: "Matrix", found: a.typeName)
            }
            let vectorB = try b.asVector(for: "linsolve")
            return .vector(try solveLinearSystem(A: matrixA, b: vectorB))
        },
        // FIX: Handle dot product between vectors represented as matrices
        "dot": { a, b in
            // Helper to convert a MathValue to a Vector or ComplexVector if it represents one.
            func toVector(_ value: MathValue) -> (real: Vector?, complex: ComplexVector?) {
                switch value {
                case .vector(let v): return (v, nil)
                case .matrix(let m) where m.rows == 1 || m.columns == 1: return (Vector(values: m.values, dimensions: m.dimensions), nil)
                case .complexVector(let cv): return (nil, cv)
                case .complexMatrix(let cm) where cm.rows == 1 || cm.columns == 1: return (nil, ComplexVector(values: cm.values, dimensions: cm.dimensions))
                default: return (nil, nil)
                }
            }

            let (aReal, aComplex) = toVector(a)
            let (bReal, bComplex) = toVector(b)

            switch (aComplex, bComplex) {
            case (let cv1?, let cv2?): // C.dot(C)
                let (val, dim) = try cv1.dot(with: cv2)
                if dim.isEmpty { return .complex(val) }
                throw MathError.unsupportedOperation(op: "dot", typeA: "ComplexVector with units", typeB: nil)
            case (let cv1?, nil): // C.dot(R)
                guard let v2 = bReal else { break }
                let (val, dim) = try cv1.dot(with: ComplexVector(from: v2))
                if dim.isEmpty { return .complex(val) }
                throw MathError.unsupportedOperation(op: "dot", typeA: "ComplexVector with units", typeB: nil)
            case (nil, let cv2?): // R.dot(C)
                guard let v1 = aReal else { break }
                let (val, dim) = try ComplexVector(from: v1).dot(with: cv2)
                if dim.isEmpty { return .complex(val) }
                throw MathError.unsupportedOperation(op: "dot", typeA: "ComplexVector with units", typeB: nil)
            case (nil, nil): // R.dot(R)
                if let v1 = aReal, let v2 = bReal {
                    return .unitValue(try v1.dot(with: v2))
                }
            }
            
            throw MathError.typeMismatch(expected: "Two compatible Vectors", found: "\(a.typeName), \(b.typeName)")
        },
        "cross": { a, b in
            // Helper to convert a MathValue to a Vector if it represents one.
            func to3DVector(_ value: MathValue) -> Vector? {
                switch value {
                case .vector(let v):
                    return v.dimension == 3 ? v : nil
                case .matrix(let m) where (m.rows == 3 && m.columns == 1) || (m.rows == 1 && m.columns == 3):
                    return Vector(values: m.values, dimensions: m.dimensions)
                default:
                    return nil
                }
            }
            
            guard let v1 = to3DVector(a), let v2 = to3DVector(b) else {
                throw MathError.typeMismatch(expected: "Two 3D Vectors", found: "\(a.typeName), \(b.typeName)")
            }
            return .vector(try v1.cross(with: v2))
        },
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
        },
        "impedance": { freq_val, comp_val in
            let f: Double
            switch freq_val {
            case .dimensionless(let d):
                f = d // a dimensionless frequency is assumed to be in Hz
            case .unitValue(let u):
                // Frequency is 1/s
                guard u.dimensions == [.second: -1] else {
                    throw MathError.dimensionMismatch(reason: "First argument for impedance() must be a frequency (e.g., in Hz).")
                }
                f = u.value // value is already in base SI units (Hz)
            default:
                throw MathError.typeMismatch(expected: "Frequency value", found: freq_val.typeName)
            }
            
            let omega = 2 * Double.pi * f
            
            switch comp_val {
            case .dimensionless(let r):
                // Assume dimensionless is resistance in Ohms.
                return .complex(Complex(real: r, imaginary: 0))
            case .unitValue(let u):
                // Resistance dimensions: kg*m^2*s^-3*A^-2 (Ohm)
                let resistanceDim: UnitDimension = [.kilogram: 1, .meter: 2, .second: -3, .ampere: -2]
                // Inductance dimensions: kg*m^2*s^-2*A^-2 (Henry)
                let inductanceDim: UnitDimension = [.kilogram: 1, .meter: 2, .second: -2, .ampere: -2]
                // Capacitance dimensions: kg^-1*m^-2*s^4*A^2 (Farad)
                let capacitanceDim: UnitDimension = [.kilogram: -1, .meter: -2, .second: 4, .ampere: 2]

                if u.dimensions == resistanceDim {
                    // It's a resistor
                    return .complex(Complex(real: u.value, imaginary: 0))
                } else if u.dimensions == inductanceDim {
                    // It's an inductor
                    return .complex(Complex(real: 0, imaginary: omega * u.value))
                } else if u.dimensions == capacitanceDim {
                    // It's a capacitor
                    guard omega * u.value != 0 else { throw MathError.divisionByZero }
                    return .complex(Complex(real: 0, imaginary: -1 / (omega * u.value)))
                } else {
                    throw MathError.dimensionMismatch(reason: "Second argument for impedance() must be resistance (Ω), inductance (H), or capacitance (F).")
                }
            default:
                throw MathError.typeMismatch(expected: "Resistance, Inductance, or Capacitance value", found: comp_val.typeName)
            }
        }
    ]
    
    // MARK: - Function & Operator Evaluation
    
    func evaluateFunctionCall(_ node: FunctionCallNode, variables: inout [String: MathValue], functions: inout [String: FunctionDefinitionNode], angleMode: AngleMode) throws -> (result: MathValue, usedAngle: Bool) {
        var usedAngle = false
        if node.name == "grad" { return try evaluateGradFunction(node, variables: &variables, functions: &functions, angleMode: angleMode) }
        
        if node.name == "fft" {
            return try evaluateDSPFunction(node, variables: &variables, functions: &functions, angleMode: angleMode, direction: FFTDirection(kFFTDirection_Forward))
        }
        if node.name == "ifft" {
            return try evaluateDSPFunction(node, variables: &variables, functions: &functions, angleMode: angleMode, direction: FFTDirection(kFFTDirection_Inverse))
        }
        
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
                // --- MODIFIED: Ensure uncertainty for scalar functions is dimensionless ---
                guard u.dimensions.isEmpty else { throw MathError.typeMismatch(expected: "Dimensionless value for \(node.name)", found: "Uncertain value with units") }
                let val = u.value
                let resultVal = scalarFunc(val)
                let derivative: Double
                switch node.name {
                case "ln": derivative = 1 / val
                case "lg", "log": derivative = 1 / (val * Foundation.log(10))
                case "sinh": derivative = cosh(val); case "cosh": derivative = sinh(val); case "tanh": derivative = 1 - pow(tanh(val), 2)
                case "asinh": derivative = 1 / sqrt(pow(val, 2) + 1); case "acosh": derivative = 1 / sqrt(pow(val, 2) - 1); case "atanh": derivative = 1 / (1 - pow(val, 2))
                case "sech": derivative = -tanh(val) * (1/cosh(val));
                case "csch": derivative = -(1 / tanh(val)) * (1 / sinh(val));
                case "coth": derivative = -pow(1/sinh(val), 2);
                case "asech": derivative = -1 / (val * sqrt(1 - pow(val, 2)));
                case "acsch": derivative = -1 / (abs(val) * sqrt(1 + pow(val, 2)));
                case "acoth": derivative = 1 / (1 - pow(val, 2));
                default: derivative = 0 // Should not be reached for functions in this map
                }
                let propagated = u.propagate(derivative: derivative)
                return (.uncertain(UncertainValue(value: resultVal, randomUncertainty: propagated.randomUncertainty, systematicUncertainty: propagated.systematicUncertainty, dimensions: [:])), argUsedAngle)
            }
            
            // FIX: Use asScalar() to correctly handle dimensionless UnitValues
            let scalarValue = try arg.asScalar()
            return (.dimensionless(scalarFunc(scalarValue)), argUsedAngle)
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
}

fileprivate func extractDoubles(from data: MathValue) throws -> [Double] {
    switch data {
    case .dimensionless(let s):
        return [s]
    case .vector(let v):
        // For now, statistical functions on unit vectors operate on their scalar values.
        return v.values
    case .matrix(let m):
        // For now, statistical functions on unit matrices operate on their scalar values.
        return m.values
    case .uncertain(let u):
        return [u.value]
    default:
        throw MathError.typeMismatch(expected: "Vector, Matrix, or Dimensionless value", found: data.typeName)
    }
}

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

fileprivate func performStatisticalOperation(args: [MathValue], on operation: (Vector) -> MathValue?) throws -> MathValue {
    let (values, initialDimension) = try extractAndConvertUnitValues(from: args)
    guard !values.isEmpty else { throw MathError.requiresAtLeastOneArgument(function: "Statistical function") }
    let v = Vector(values: values, dimensions: initialDimension)
    
    guard let resultValue = operation(v) else {
        throw MathError.unsupportedOperation(op: "Statistical", typeA: "Operation failed (e.g., stddev on single element)", typeB: nil)
    }
    
    return resultValue
}

fileprivate func extractAndConvertUnitValues(from args: [MathValue]) throws -> (values: [Double], dimensions: UnitDimension) {
    var allValues: [Double] = []
    var commonDimension: UnitDimension? = nil

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
        case .vector(let v):
            if commonDimension == nil { commonDimension = v.dimensions }
            if commonDimension != v.dimensions { throw MathError.dimensionMismatch(reason: "Cannot mix units and dimensionless vectors.") }
            allValues.append(contentsOf: v.values)
        case .matrix(let m):
            if commonDimension == nil { commonDimension = m.dimensions }
            if commonDimension != m.dimensions { throw MathError.dimensionMismatch(reason: "Cannot mix units and dimensionless matrices.") }
            allValues.append(contentsOf: m.values)
        default:
            throw MathError.typeMismatch(expected: "Numeric value", found: val.typeName)
        }
    }

    if args.count == 1 {
        try processValue(args[0])
    } else {
        for arg in args {
            try processValue(arg)
        }
    }

    return (allValues, commonDimension ?? [:])
}


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

fileprivate func normalDistribution(x: Double, mean: Double, stddev: Double) -> Double {
    guard stddev > 0 else { return Double.nan }
    let variance = pow(stddev, 2)
    let coefficient = 1.0 / sqrt(2.0 * .pi * variance)
    let exponent = -pow(x - mean, 2) / (2.0 * variance)
    return coefficient * exp(exponent)
}

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

