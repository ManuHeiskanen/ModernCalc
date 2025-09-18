//
//  EvaluatorOperators.swift
//  ModernCalc
//
//  Created by Manu Heiskanen on 16.9.2025.
//

import Foundation

/// This extension contains the logic for handling unary, binary, and indexed assignment operators.
extension Evaluator {
    
    func evaluateUnaryOperation(op: Token, value: MathValue) throws -> MathValue {
        switch op.rawValue {
        case "+": return value
        case "-":
            switch value {
            case .dimensionless(let s): return .dimensionless(-s)
            case .unitValue(let u): return .unitValue(UnitValue(value: -u.value, dimensions: u.dimensions))
            case .complex(let c): return .complex(c * -1.0)
            case .vector(let v): return .vector(Vector(values: v.values.map { -$0 }, dimensions: v.dimensions))
            case .matrix(let m): return .matrix(Matrix(values: m.values.map { -$0 }, rows: m.rows, columns: m.columns, dimensions: m.dimensions))
            case .complexVector(let cv): return .complexVector(ComplexVector(values: cv.values.map { $0 * -1.0 }, dimensions: cv.dimensions))
            case .complexMatrix(let cm): return .complexMatrix(ComplexMatrix(values: cm.values.map { $0 * -1.0 }, rows: cm.rows, columns: cm.columns, dimensions: cm.dimensions))
            case .uncertain(let u): return .uncertain(-u)
            default: throw MathError.unsupportedOperation(op: op.rawValue, typeA: value.typeName, typeB: nil)
            }
        case "~": // Logical NOT
            let s = try value.asScalar()
            return .dimensionless(s == 0 ? 1.0 : 0.0)
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

        // --- MODIFIED: Uncertainty propagation rules now handle units ---
        case (.uncertain(let l), .uncertain(let r)): return .uncertain(try performUncertainUncertainOp(op.rawValue, l, r))
        case (.uncertain(let l), .dimensionless(let r)): return .uncertain(try performUncertainUncertainOp(op.rawValue, l, UncertainValue(value: r, randomUncertainty: 0, systematicUncertainty: 0, dimensions: [:])))
        case (.dimensionless(let l), .uncertain(let r)): return .uncertain(try performUncertainUncertainOp(op.rawValue, UncertainValue(value: l, randomUncertainty: 0, systematicUncertainty: 0, dimensions: [:]), r))
        case (.uncertain(let l), .unitValue(let r)): return .uncertain(try performUncertainUncertainOp(op.rawValue, l, UncertainValue(value: r.value, randomUncertainty: 0, systematicUncertainty: 0, dimensions: r.dimensions)))
        case (.unitValue(let l), .uncertain(let r)): return .uncertain(try performUncertainUncertainOp(op.rawValue, UncertainValue(value: l.value, randomUncertainty: 0, systematicUncertainty: 0, dimensions: l.dimensions), r))
            
        // --- Complex Number Operations ---
        case (.complex(let l), .complex(let r)): return .complex(try performComplexComplexOp(op.rawValue, l, r))
        case (.complex(let l), .dimensionless(let r)): return .complex(try performComplexComplexOp(op.rawValue, l, Complex(real: r, imaginary: 0)))
        case (.dimensionless(let l), .complex(let r)): return .complex(try performComplexComplexOp(op.rawValue, Complex(real: l, imaginary: 0), r))
        
        // --- Vector Operations ---
        case (.vector(let v), .dimensionless(let s)): return .vector(try performVectorScalarOp(op.rawValue, v, s))
        case (.dimensionless(let s), .vector(let v)): return .vector(try performVectorScalarOp(op.rawValue, v, s, reversed: true))
        case (.vector(let v), .unitValue(let u)): return .vector(try performVectorUnitOp(op.rawValue, v, u))
        case (.unitValue(let u), .vector(let v)): return .vector(try performVectorUnitOp(op.rawValue, v, u, reversed: true))
        case (.vector(let l), .vector(let r)):
            if op.rawValue == "*" {
                return .matrix(l * r)
            }
            return .vector(try performVectorVectorOp(op.rawValue, l, r))
            
        // --- Matrix Operations ---
        case (.matrix(let m), .dimensionless(let s)): return .matrix(try performMatrixScalarOp(op.rawValue, m, s))
        case (.dimensionless(let s), .matrix(let m)): return .matrix(try performMatrixScalarOp(op.rawValue, m, s, reversed: true))
        case (.matrix(let m), .unitValue(let u)): return .matrix(try performMatrixUnitOp(op.rawValue, m, u))
        case (.unitValue(let u), .matrix(let m)): return .matrix(try performMatrixUnitOp(op.rawValue, m, u, reversed: true))
        case (.matrix(let l), .matrix(let r)): return .matrix(try performMatrixMatrixOp(op.rawValue, l, r))
        case (.matrix(let m), .vector(let v)):
             if op.rawValue == "*" { return .vector(try m * v) }
             else { throw MathError.unsupportedOperation(op: op.rawValue, typeA: left.typeName, typeB: right.typeName) }
        case (.vector(let v), .matrix(let m)):
            if op.rawValue == "*" {
                if m.rows == 1 {
                    let rowVector = Vector(values: m.values, dimensions: m.dimensions)
                    return .matrix(v * rowVector)
                }
                return .vector(try v * m)
            }
            else { throw MathError.unsupportedOperation(op: op.rawValue, typeA: left.typeName, typeB: right.typeName) }

        // --- Complex Vector/Matrix Operations ---
        case (.complexVector(let cv), .complex(let c)): return .complexVector(try performComplexVectorComplexOp(op.rawValue, cv, c))
        case (.complex(let c), .complexVector(let cv)): return .complexVector(try performComplexVectorComplexOp(op.rawValue, cv, c, reversed: true))
        case (.complexVector(let l), .complexVector(let r)):
            if op.rawValue == "*" {
                return .complexMatrix(l * r)
            }
            return .complexVector(try performCVectorCVectorOp(op.rawValue, l, r))
        case (.complexMatrix(let cm), .complex(let c)): return .complexMatrix(try performComplexMatrixComplexOp(op.rawValue, cm, c))
        case (.complex(let c), .complexMatrix(let cm)): return .complexMatrix(try performComplexMatrixComplexOp(op.rawValue, cm, c, reversed: true))
        case (.complexMatrix(let l), .complexMatrix(let r)): return .complexMatrix(try performCMatrixCMatrixOp(op.rawValue, l, r))
        case (.complexMatrix(let m), .complexVector(let v)):
            if op.rawValue == "*" { return .complexVector(try m * v) }
            else { throw MathError.unsupportedOperation(op: op.rawValue, typeA: left.typeName, typeB: right.typeName) }
        case (.complexVector(let v), .complexMatrix(let m)):
            if op.rawValue == "*" {
                if m.rows == 1 {
                    let rowVector = ComplexVector(values: m.values, dimensions: m.dimensions)
                    return .complexMatrix(v * rowVector)
                }
                return .complexVector(try v * m)
            }
            else { throw MathError.unsupportedOperation(op: op.rawValue, typeA: left.typeName, typeB: right.typeName) }

        // --- Promotion cases for mixed types ---
        case (.matrix, .complex), (.complex, .matrix),
             (.vector, .complex), (.complex, .vector),
             (.complexMatrix, .dimensionless), (.dimensionless, .complexMatrix),
             (.complexVector, .dimensionless), (.dimensionless, .complexVector),
             (.complexMatrix, .vector), (.vector, .complexMatrix),
             (.complexMatrix, .matrix), (.matrix, .complexMatrix),
             (.complexVector, .vector), (.vector, .complexVector),
             (.complexVector, .matrix), (.matrix, .complexVector):
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
        case "+": return try l + r; case "-": return try l - r; case "*": return l * r
        case "/": return try l / r
        case "^":
            guard r.dimensions.isEmpty else { throw MathError.typeMismatch(expected: "Dimensionless exponent", found: "Uncertain value with units") }
            return l.pow(r.value)
        default: throw MathError.unknownOperator(op: op)
        }
    }
    private func performDimensionlessOp(_ op: String, _ l: Double, _ r: Double) throws -> Double {
        switch op {
        case "+": return l + r; case "-": return l - r; case "*": return l * r
        case "/": guard r != 0 else { throw MathError.divisionByZero }; return l / r
        case "%": guard r != 0 else { throw MathError.divisionByZero }; return l.truncatingRemainder(dividingBy: r)
        case "^": return pow(l, r)
        // Comparison and Logical Operators
        case ">": return l > r ? 1.0 : 0.0
        case "<": return l < r ? 1.0 : 0.0
        case ">=": return l >= r ? 1.0 : 0.0
        case "<=": return l <= r ? 1.0 : 0.0
        case "==": return l == r ? 1.0 : 0.0
        case "!=": return l != r ? 1.0 : 0.0
        case "&&": return (l != 0 && r != 0) ? 1.0 : 0.0
        case "||": return (l != 0 || r != 0) ? 1.0 : 0.0
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
        // Comparison Operators
        case ">", "<", ">=", "<=", "==", "!=":
            guard l.dimensions == r.dimensions else {
                throw MathError.dimensionMismatch(reason: "Cannot compare quantities with different units.")
            }
            let result: Bool
            switch op {
            case ">": result = l.value > r.value
            case "<": result = l.value < r.value
            case ">=": result = l.value >= r.value
            case "<=": result = l.value <= r.value
            case "==": result = l.value == r.value
            case "!=": result = l.value != r.value
            default: throw MathError.unknownOperator(op: op) // Should not be reached
            }
            return UnitValue.dimensionless(result ? 1.0 : 0.0)
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
    private func performVectorUnitOp(_ op: String, _ v: Vector, _ u: UnitValue, reversed: Bool = false) throws -> Vector {
        let s = u.value
        let d = u.dimensions
        if reversed {
            switch op {
            case "*":
                let newValues = v.values.map { s * $0 }
                let newDims = d.merging(v.dimensions, uniquingKeysWith: +).filter { $0.value != 0 }
                return Vector(values: newValues, dimensions: newDims)
            default: throw MathError.unsupportedOperation(op: op, typeA: "UnitValue", typeB: "Vector")
            }
        } else {
            switch op {
            case "*":
                let newValues = v.values.map { $0 * s }
                let newDims = v.dimensions.merging(d, uniquingKeysWith: +).filter { $0.value != 0 }
                return Vector(values: newValues, dimensions: newDims)
            case "/":
                guard s != 0 else { throw MathError.divisionByZero }
                let newValues = v.values.map { $0 / s }
                let newDims = v.dimensions.merging(d.mapValues { -$0 }, uniquingKeysWith: +).filter { $0.value != 0 }
                return Vector(values: newValues, dimensions: newDims)
            default: throw MathError.unsupportedOperation(op: op, typeA: "Vector", typeB: "UnitValue")
            }
        }
    }
    private func performVectorVectorOp(_ op: String, _ l: Vector, _ r: Vector) throws -> Vector {
        switch op { case "+": return try l + r; case "-": return try l - r; case ".*": return try l.hadamard(with: r); case "./": return try l.hadamardDivision(with: r)
        case "^": throw MathError.unsupportedOperation(op: op, typeA: "Vector^Vector", typeB: nil) // a vector exponent doesn't make sense here
        default: throw MathError.unsupportedOperation(op: op, typeA: "Vector", typeB: "Vector")
        }
    }
    private func performMatrixScalarOp(_ op: String, _ m: Matrix, _ s: Double, reversed: Bool = false) throws -> Matrix {
        let newValues: [Double]; switch op {
        case "+": newValues = m.values.map { $0 + s }; case "*": newValues = m.values.map { $0 * s }
        case "-": newValues = reversed ? m.values.map { s - $0 } : m.values.map { $0 - s }
        case "/": if reversed { throw MathError.unsupportedOperation(op: op, typeA: "Scalar", typeB: "Matrix") }; guard s != 0 else { throw MathError.divisionByZero }; newValues = m.values.map { $0 / s }
        default: throw MathError.unsupportedOperation(op: op, typeA: "Matrix", typeB: "Scalar")
        }
        return Matrix(values: newValues, rows: m.rows, columns: m.columns, dimensions: m.dimensions)
    }
    private func performMatrixUnitOp(_ op: String, _ m: Matrix, _ u: UnitValue, reversed: Bool = false) throws -> Matrix {
        let s = u.value
        let d = u.dimensions
        if reversed {
            switch op {
            case "*":
                let newValues = m.values.map { s * $0 }
                let newDims = d.merging(m.dimensions, uniquingKeysWith: +).filter { $0.value != 0 }
                return Matrix(values: newValues, rows: m.rows, columns: m.columns, dimensions: newDims)
            default: throw MathError.unsupportedOperation(op: op, typeA: "UnitValue", typeB: "Matrix")
            }
        } else {
            switch op {
            case "*":
                let newValues = m.values.map { $0 * s }
                let newDims = m.dimensions.merging(d, uniquingKeysWith: +).filter { $0.value != 0 }
                return Matrix(values: newValues, rows: m.rows, columns: m.columns, dimensions: newDims)
            case "/":
                guard s != 0 else { throw MathError.divisionByZero }
                let newValues = m.values.map { $0 / s }
                let newDims = m.dimensions.merging(d.mapValues { -$0 }, uniquingKeysWith: +).filter { $0.value != 0 }
                return Matrix(values: newValues, rows: m.rows, columns: m.columns, dimensions: newDims)
            default: throw MathError.unsupportedOperation(op: op, typeA: "Matrix", typeB: "UnitValue")
            }
        }
    }
    private func performMatrixMatrixOp(_ op: String, _ l: Matrix, _ r: Matrix) throws -> Matrix {
        switch op { case "+": return try l + r; case "-": return try l - r; case "*": return try l * r; case ".*": return try l.hadamard(with: r); case "./": return try l.hadamardDivision(with: r); default: throw MathError.unsupportedOperation(op: op, typeA: "Matrix", typeB: "Matrix") }
    }
    private func performComplexVectorComplexOp(_ op: String, _ v: ComplexVector, _ c: Complex, reversed: Bool = false) throws -> ComplexVector {
         if reversed {
            switch op { case "+": return ComplexVector(values: v.values.map { c + $0 }, dimensions: v.dimensions); case "*": return ComplexVector(values: v.values.map { c * $0 }, dimensions: v.dimensions); case "-": return ComplexVector(values: v.values.map { c - $0 }, dimensions: v.dimensions); default: throw MathError.unsupportedOperation(op: op, typeA: "ComplexVector", typeB: "Complex") }
        } else {
            switch op { case "+": return v + c; case "*": return v * c; case "-": return v - c; case "/": return try v / c; default: throw MathError.unsupportedOperation(op: op, typeA: "ComplexVector", typeB: "Complex") }
        }
    }
    private func performCVectorCVectorOp(_ op: String, _ l: ComplexVector, _ r: ComplexVector) throws -> ComplexVector {
        switch op { case "+": return try l + r; case "-": return try l - r; default: throw MathError.unsupportedOperation(op: op, typeA: "ComplexVector", typeB: "ComplexVector") }
    }
    private func performComplexMatrixComplexOp(_ op: String, _ m: ComplexMatrix, _ c: Complex, reversed: Bool = false) throws -> ComplexMatrix {
         if reversed {
            switch op { case "+": return ComplexMatrix(values: m.values.map { c + $0 }, rows: m.rows, columns: m.columns, dimensions: m.dimensions); case "*": return ComplexMatrix(values: m.values.map { c * $0 }, rows: m.rows, columns: m.columns, dimensions: m.dimensions); case "-": return ComplexMatrix(values: m.values.map { c - $0 }, rows: m.rows, columns: m.columns, dimensions: m.dimensions); default: throw MathError.unsupportedOperation(op: op, typeA: "ComplexMatrix", typeB: "Complex") }
        } else {
            switch op { case "+": return m + c; case "*": return m * c; case "-": return m - c; case "/": return try m / c; default: throw MathError.unsupportedOperation(op: op, typeA: "ComplexMatrix", typeB: "Complex") }
        }
    }
    private func performCMatrixCMatrixOp(_ op: String, _ l: ComplexMatrix, _ r: ComplexMatrix) throws -> ComplexMatrix {
        switch op { case "+": return try l + r; case "-": return try l - r; case "*": return try l * r; default: throw MathError.unsupportedOperation(op: op, typeA: "ComplexMatrix", typeB: "ComplexMatrix") }
    }
    private func promote(_ left: MathValue, _ right: MathValue) throws -> (MathValue, MathValue) {
        switch (left, right) {
        // Scalar promotions
        case (.matrix(let m), .complex(let c)): return (.complexMatrix(ComplexMatrix(from: m)), .complex(c))
        case (.complex(let c), .matrix(let m)): return (.complex(c), .complexMatrix(ComplexMatrix(from: m)))
        case (.vector(let v), .complex(let c)): return (.complexVector(ComplexVector(from: v)), .complex(c))
        case (.complex(let c), .vector(let v)): return (.complex(c), .complexVector(ComplexVector(from: v)))
        case (.complexMatrix(let cm), .dimensionless(let s)): return (.complexMatrix(cm), .complex(Complex(real: s, imaginary: 0)))
        case (.dimensionless(let s), .complexMatrix(let cm)): return (.complex(Complex(real: s, imaginary: 0)), .complexMatrix(cm))
        case (.complexVector(let cv), .dimensionless(let s)): return (.complexVector(cv), .complex(Complex(real: s, imaginary: 0)))
        case (.dimensionless(let s), .complexVector(let cv)): return (.complex(Complex(real: s, imaginary: 0)), .complexVector(cv))
        
        // Matrix/Vector promotions to Complex
        case (.complexMatrix(let cm), .vector(let v)): return (.complexMatrix(cm), .complexVector(ComplexVector(from: v)))
        case (.vector(let v), .complexMatrix(let cm)): return (.complexVector(ComplexVector(from: v)), .complexMatrix(cm))
        case (.complexMatrix(let cm), .matrix(let m)): return (.complexMatrix(cm), .complexMatrix(ComplexMatrix(from: m)))
        case (.matrix(let m), .complexMatrix(let cm)): return (.complexMatrix(ComplexMatrix(from: m)), .complexMatrix(cm))
        case (.complexVector(let cv), .vector(let v)): return (.complexVector(cv), .complexVector(ComplexVector(from: v)))
        case (.vector(let v), .complexVector(let cv)): return (.complexVector(ComplexVector(from: v)), .complexVector(cv))
        case (.complexVector(let cv), .matrix(let m)): return (.complexVector(cv), .complexMatrix(ComplexMatrix(from: m)))
        case (.matrix(let m), .complexVector(let cv)): return (.complexMatrix(ComplexMatrix(from: m)), .complexVector(cv))
            
        default: return (left, right)
        }
    }
}
