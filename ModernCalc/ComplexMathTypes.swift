//
//  ComplexMathTypes.swift
//  ModernCalc
//
//  Created by Manu Heiskanen on 22.10.2025.
//

import Foundation

struct ComplexUnitValue: Equatable, Codable {
    var value: Complex
    var dimensions: UnitDimension

    static func + (lhs: ComplexUnitValue, rhs: ComplexUnitValue) throws -> ComplexUnitValue {
        guard lhs.dimensions == rhs.dimensions else {
            throw MathError.dimensionMismatch(reason: "Cannot add quantities with different units.")
        }
        return ComplexUnitValue(value: lhs.value + rhs.value, dimensions: lhs.dimensions)
    }

    static func - (lhs: ComplexUnitValue, rhs: ComplexUnitValue) throws -> ComplexUnitValue {
        guard lhs.dimensions == rhs.dimensions else {
            throw MathError.dimensionMismatch(reason: "Cannot subtract quantities with different units.")
        }
        return ComplexUnitValue(value: lhs.value - rhs.value, dimensions: lhs.dimensions)
    }

    static func * (lhs: ComplexUnitValue, rhs: ComplexUnitValue) -> ComplexUnitValue {
        let newValue = lhs.value * rhs.value
        // Use tolerance instead of != 0
        let newDimensions = lhs.dimensions.merging(rhs.dimensions, uniquingKeysWith: +).filter { abs($0.value) > 1e-15 }
        return ComplexUnitValue(value: newValue, dimensions: newDimensions)
    }

    static func / (lhs: ComplexUnitValue, rhs: ComplexUnitValue) throws -> ComplexUnitValue {
        let newValue = try lhs.value / rhs.value
        let negatedRhsDimensions = rhs.dimensions.mapValues { -$0 }
        // Use tolerance instead of != 0
        let newDimensions = lhs.dimensions.merging(negatedRhsDimensions, uniquingKeysWith: +).filter { abs($0.value) > 1e-15 }
        return ComplexUnitValue(value: newValue, dimensions: newDimensions)
    }
}

struct Complex: Equatable, Codable {
    var real: Double
    var imaginary: Double
    
    static let zero = Complex(real: 0, imaginary: 0)
    static let i = Complex(real: 0, imaginary: 1)
    
    static func + (lhs: Complex, rhs: Complex) -> Complex {
        return Complex(real: lhs.real + rhs.real, imaginary: lhs.imaginary + rhs.imaginary)
    }
    static func - (lhs: Complex, rhs: Complex) -> Complex {
        return Complex(real: lhs.real - rhs.real, imaginary: lhs.imaginary - rhs.imaginary)
    }
    static func * (lhs: Complex, rhs: Complex) -> Complex {
        let real = lhs.real * rhs.real - lhs.imaginary * rhs.imaginary
        let imaginary = lhs.real * rhs.imaginary + lhs.imaginary * rhs.real
        return Complex(real: real, imaginary: imaginary)
    }
    static func * (lhs: Complex, rhs: Double) -> Complex {
        return Complex(real: lhs.real * rhs, imaginary: lhs.imaginary * rhs)
    }
    static func / (lhs: Complex, rhs: Complex) throws -> Complex {
        let denominator = rhs.real * rhs.real + rhs.imaginary * rhs.imaginary
        guard denominator != 0 else { throw MathError.divisionByZero }
        let real = (lhs.real * rhs.real + lhs.imaginary * rhs.imaginary) / denominator
        let imaginary = (lhs.imaginary * rhs.real - lhs.real * rhs.imaginary) / denominator
        return Complex(real: real, imaginary: imaginary)
    }
    
    func abs() -> Double {
        return Foundation.sqrt(real * real + imaginary * imaginary)
    }

    func sqrt() -> Complex {
        let r = self.abs()
        let newReal = Foundation.sqrt((r + real) / 2)
        let newImag = Foundation.sqrt((r - real) / 2)
        return Complex(real: newReal, imaginary: imaginary < 0 ? -newImag : newImag)
    }

    func pow(_ exponent: Complex) throws -> Complex {
        if self == .zero && exponent == .zero { return Complex(real: 1, imaginary: 0) }
        if self == .zero { return .zero }

        let baseLogReal = log(self.abs())
        let baseLogImag = atan2(self.imaginary, self.real)
        let baseLog = Complex(real: baseLogReal, imaginary: baseLogImag)
        let product = exponent * baseLog
        let scale = exp(product.real)
        let real = scale * cos(product.imaginary)
        let imag = scale * sin(product.imaginary)
        return Complex(real: real, imaginary: imag)
    }
    
    func conjugate() -> Complex {
        return Complex(real: self.real, imaginary: -self.imaginary)
    }

    func argument() -> Double {
        return atan2(self.imaginary, self.real)
    }
}

struct ComplexVector: Equatable, Codable {
    let values: [Complex]
    let dimensions: UnitDimension
    var dimension: Int { values.count }
    
    // FIX 10: Explicitly type the default empty dictionary
    init(values: [Complex], dimensions: UnitDimension = [:] as UnitDimension) { self.values = values; self.dimensions = dimensions }
    init(from realVector: Vector) { self.values = realVector.values.map { Complex(real: $0, imaginary: 0) }; self.dimensions = realVector.dimensions }
    
    subscript(index: Int) -> Complex { return values[index] }
    
    func dot(with other: ComplexVector) throws -> (value: Complex, dimensions: UnitDimension) {
        guard self.dimension == other.dimension else { throw MathError.dimensionMismatch(reason: "Complex vectors must have same dimensions for dot product.") }
        let resultValue = zip(self.values, other.values).map { $0 * $1.conjugate() }.reduce(.zero, +)
        let newDimensions = self.dimensions.merging(other.dimensions, uniquingKeysWith: +).filter { $0.value != 0 }
        return (resultValue, newDimensions)
    }
    
    func conjugateTranspose() -> ComplexMatrix {
        let newValues = self.values.map { $0.conjugate() }
        return ComplexMatrix(values: newValues, rows: 1, columns: self.dimension, dimensions: self.dimensions)
    }
    
    func modifying(at index: Int, with scalar: Complex, operation: (Complex, Complex) throws -> Complex) throws -> ComplexVector {
        guard index >= 0 && index < self.dimension else {
            throw MathError.dimensionMismatch(reason: "Index \(index + 1) is out of bounds for vector of dimension \(self.dimension).")
        }
        var newValues = self.values
        newValues[index] = try operation(newValues[index], scalar)
        return ComplexVector(values: newValues, dimensions: self.dimensions)
    }
    
    // --- NEW: Slicing function ---
    func slice(indices: [Int]) throws -> MathValue {
        var newValues: [Complex] = []
        for index in indices {
            guard index >= 1 && index <= self.dimension else {
                throw MathError.dimensionMismatch(reason: "Index \(index) is out of bounds for vector of size \(self.dimension).")
            }
            newValues.append(self.values[index - 1])
        }
        
        if newValues.count == 1 {
            let result = newValues[0]
            if self.dimensions.isEmpty {
                return .complex(result)
            } else {
                return .complexUnitValue(ComplexUnitValue(value: result, dimensions: self.dimensions))
            }
        }
        
        return .complexVector(ComplexVector(values: newValues, dimensions: self.dimensions))
    }
    
    static func + (lhs: ComplexVector, rhs: ComplexVector) throws -> ComplexVector {
        guard lhs.dimension == rhs.dimension else { throw MathError.dimensionMismatch(reason: "Complex vectors must have same dimensions for addition.") }
        guard lhs.dimensions == rhs.dimensions else { throw MathError.dimensionMismatch(reason: "Complex vectors must have the same units for addition.") }
        return ComplexVector(values: zip(lhs.values, rhs.values).map(+), dimensions: lhs.dimensions)
    }
    static func - (lhs: ComplexVector, rhs: ComplexVector) throws -> ComplexVector {
        guard lhs.dimension == rhs.dimension else { throw MathError.dimensionMismatch(reason: "Complex vectors must have same dimensions for subtraction.") }
        guard lhs.dimensions == rhs.dimensions else { throw MathError.dimensionMismatch(reason: "Complex vectors must have the same units for subtraction.") }
        return ComplexVector(values: zip(lhs.values, rhs.values).map(-), dimensions: lhs.dimensions)
    }
    
    static func * (lhs: ComplexVector, rhs: ComplexVector) -> ComplexMatrix {
        let newValues = (0..<lhs.dimension).flatMap { i in (0..<rhs.dimension).map { j in lhs[i] * rhs[j] } }
        let newDimensions = lhs.dimensions.merging(rhs.dimensions, uniquingKeysWith: +).filter { $0.value != 0 }
        return ComplexMatrix(values: newValues, rows: lhs.dimension, columns: rhs.dimension, dimensions: newDimensions)
    }

    static func + (lhs: ComplexVector, rhs: Complex) -> ComplexVector { ComplexVector(values: lhs.values.map { $0 + rhs }, dimensions: lhs.dimensions) }
    static func - (lhs: ComplexVector, rhs: Complex) -> ComplexVector { ComplexVector(values: lhs.values.map { $0 - rhs }, dimensions: lhs.dimensions) }
    static func * (lhs: ComplexVector, rhs: Complex) -> ComplexVector { ComplexVector(values: lhs.values.map { $0 * rhs }, dimensions: lhs.dimensions) }
    static func / (lhs: ComplexVector, rhs: Complex) throws -> ComplexVector { try ComplexVector(values: lhs.values.map { try $0 / rhs }, dimensions: lhs.dimensions) }

    static func * (lhs: ComplexVector, rhs: ComplexMatrix) throws -> ComplexVector {
        guard lhs.dimension == rhs.rows else {
            throw MathError.dimensionMismatch(reason: "For v*M, dimension of v must equal rows of M.")
        }
        let newValues = (0..<rhs.columns).map { j in (0..<lhs.dimension).map { i in lhs[i] * rhs[i, j] }.reduce(.zero, +) }
        let newDimensions = lhs.dimensions.merging(rhs.dimensions, uniquingKeysWith: +).filter { $0.value != 0 }
        return ComplexVector(values: newValues, dimensions: newDimensions)
    }
}

struct ComplexMatrix: Equatable, Codable {
    let values: [Complex]
    let rows: Int
    let columns: Int
    let dimensions: UnitDimension
    
    // FIX 11: Explicitly type the default empty dictionary
    init(values: [Complex], rows: Int, columns: Int, dimensions: UnitDimension = [:] as UnitDimension) { self.values = values; self.rows = rows; self.columns = columns; self.dimensions = dimensions }
    init(from realMatrix: Matrix) { self.values = realMatrix.values.map { Complex(real: $0, imaginary: 0) }; self.rows = realMatrix.rows; self.columns = realMatrix.columns; self.dimensions = realMatrix.dimensions }
    
    subscript(row: Int, col: Int) -> Complex { return values[row * columns + col] }

    func submatrix(excludingRow: Int, excludingCol: Int) -> ComplexMatrix {
        var newValues: [Complex] = []
        for r in 0..<rows {
            guard r != excludingRow else { continue }
            for c in 0..<columns {
                guard c != excludingCol else { continue }
                newValues.append(self[r, c])
            }
        }
        return ComplexMatrix(values: newValues, rows: rows - 1, columns: columns - 1, dimensions: self.dimensions)
    }

    func determinant() throws -> (value: Complex, dimensions: UnitDimension) {
        guard rows == columns else { throw MathError.dimensionMismatch(reason: "Matrix must be square to calculate determinant.") }
        let detValue: Complex
        if rows == 1 { detValue = self[0, 0] }
        else if rows == 2 { detValue = (self[0, 0] * self[1, 1]) - (self[0, 1] * self[1, 0]) }
        else {
            detValue = try (0..<columns).map { c -> Complex in
                let sign: Double = (c % 2 == 0) ? 1.0 : -1.0
                return Complex(real: sign, imaginary: 0) * self[0, c] * (try submatrix(excludingRow: 0, excludingCol: c).determinant().value)
            }.reduce(.zero, +)
        }
        // FIX 12 (User Error 5): Cast Int to Double for multiplication with Double exponent
        let newDimensions = self.dimensions.mapValues { $0 * Double(self.rows) }.filter { $0.value != 0 }
        return (detValue, newDimensions)
    }

    func inverse() throws -> ComplexMatrix {
        let (detValue, _) = try determinant()
        guard detValue != .zero else { throw MathError.unsupportedOperation(op: "inverse", typeA: "Singular Complex Matrix", typeB: nil) }
        
        let adjugate: ComplexMatrix
        if rows == 1 {
            // FIX 13: Explicitly type empty dictionary
            adjugate = ComplexMatrix(values: [Complex(real: 1, imaginary: 0)], rows: 1, columns: 1, dimensions: [:] as UnitDimension)
        } else {
            let cofactors = try (0..<rows).flatMap { r -> [Complex] in
                try (0..<columns).map { c -> Complex in
                    let sign: Double = ((r + c) % 2 == 0) ? 1.0 : -1.0
                    return Complex(real: sign, imaginary: 0) * (try submatrix(excludingRow: r, excludingCol: c).determinant().value)
                }
            }
            // FIX 14 (User Error 6): Cast Int to Double for multiplication with Double exponent
            let adjugateDimensions = self.dimensions.mapValues { $0 * Double(self.rows - 1) }.filter { $0.value != 0 }
            adjugate = ComplexMatrix(values: cofactors, rows: rows, columns: columns, dimensions: adjugateDimensions).transpose()
        }
        
        let inverseValues = try adjugate.values.map { try $0 / detValue }
        let inverseDimensions = self.dimensions.mapValues { -$0 }.filter { $0.value != 0 }
        return ComplexMatrix(values: inverseValues, rows: rows, columns: columns, dimensions: inverseDimensions)
    }
    
    func transpose() -> ComplexMatrix {
        var newValues = [Complex](repeating: .zero, count: values.count)
        for r in 0..<rows { for c in 0..<columns { newValues[c * rows + r] = self[r, c] } }
        return ComplexMatrix(values: newValues, rows: columns, columns: rows, dimensions: self.dimensions)
    }

    func conjugateTranspose() -> ComplexMatrix {
        let transposed = self.transpose()
        let newValues = transposed.values.map { $0.conjugate() }
        return ComplexMatrix(values: newValues, rows: transposed.rows, columns: transposed.columns, dimensions: transposed.dimensions)
    }
    
    func trace() throws -> (value: Complex, dimensions: UnitDimension) {
        guard rows == columns else { throw MathError.dimensionMismatch(reason: "Matrix must be square for trace.") }
        let traceValue = (0..<rows).map { self[$0, $0] }.reduce(.zero, +)
        return (traceValue, self.dimensions)
    }
    
    // --- CORRECTED: Slicing function logic ---
    func slice(rowIndices: [Int], colIndices: [Int]) throws -> MathValue {
        var newValues: [Complex] = []
        for r in rowIndices {
            guard r >= 1 && r <= self.rows else {
                throw MathError.dimensionMismatch(reason: "Row index \(r) is out of bounds for matrix with \(self.rows) rows.")
            }
            for c in colIndices {
                guard c >= 1 && c <= self.columns else {
                     throw MathError.dimensionMismatch(reason: "Column index \(c) is out of bounds for matrix with \(self.columns) columns.")
                }
                newValues.append(self[r - 1, c - 1])
            }
        }

        if newValues.count == 1 {
            let result = newValues[0]
            if self.dimensions.isEmpty {
                return .complex(result)
            } else {
                return .complexUnitValue(ComplexUnitValue(value: result, dimensions: self.dimensions))
            }
        } else {
            return .complexMatrix(ComplexMatrix(values: newValues, rows: rowIndices.count, columns: colIndices.count, dimensions: self.dimensions))
        }
    }
    
    static func + (lhs: ComplexMatrix, rhs: ComplexMatrix) throws -> ComplexMatrix {
        guard lhs.rows == rhs.rows && lhs.columns == rhs.columns else { throw MathError.dimensionMismatch(reason: "Complex matrices must have same dimensions for addition.") }
        guard lhs.dimensions == rhs.dimensions else { throw MathError.dimensionMismatch(reason: "Complex matrices must have the same units for addition.") }
        return ComplexMatrix(values: zip(lhs.values, rhs.values).map(+), rows: lhs.rows, columns: lhs.columns, dimensions: lhs.dimensions)
    }
    static func - (lhs: ComplexMatrix, rhs: ComplexMatrix) throws -> ComplexMatrix {
        guard lhs.rows == rhs.rows && lhs.columns == rhs.columns else { throw MathError.dimensionMismatch(reason: "Complex matrices must have same dimensions for subtraction.") }
        guard lhs.dimensions == rhs.dimensions else { throw MathError.dimensionMismatch(reason: "Complex matrices must have the same units for subtraction.") }
        return ComplexMatrix(values: zip(lhs.values, rhs.values).map(-), rows: lhs.rows, columns: lhs.columns, dimensions: lhs.dimensions)
    }
    static func * (lhs: ComplexMatrix, rhs: ComplexMatrix) throws -> ComplexMatrix {
        guard lhs.columns == rhs.rows else { throw MathError.dimensionMismatch(reason: "For A*B, columns of A must equal rows of B.") }
        var newValues = [Complex](repeating: .zero, count: lhs.rows * rhs.columns)
        for i in 0..<lhs.rows { for j in 0..<rhs.columns { newValues[i * rhs.columns + j] = (0..<lhs.columns).map { k in lhs[i, k] * rhs[k, j] }.reduce(.zero, +) } }
        let newDimensions = lhs.dimensions.merging(rhs.dimensions, uniquingKeysWith: +).filter { $0.value != 0 }
        return ComplexMatrix(values: newValues, rows: lhs.rows, columns: rhs.columns, dimensions: newDimensions)
    }
    static func * (lhs: ComplexMatrix, rhs: ComplexVector) throws -> ComplexVector {
        guard lhs.columns == rhs.dimension else { throw MathError.dimensionMismatch(reason: "For M*v, columns of M must equal dimension of v.") }
        let newValues = (0..<lhs.rows).map { i in (0..<lhs.columns).map { j in lhs[i, j] * rhs[j] }.reduce(.zero, +) }
        let newDimensions = lhs.dimensions.merging(rhs.dimensions, uniquingKeysWith: +).filter { $0.value != 0 }
        return ComplexVector(values: newValues, dimensions: newDimensions)
    }
    
    static func + (lhs: ComplexMatrix, rhs: Complex) -> ComplexMatrix { ComplexMatrix(values: lhs.values.map { $0 + rhs }, rows: lhs.rows, columns: lhs.columns, dimensions: lhs.dimensions) }
    static func - (lhs: ComplexMatrix, rhs: Complex) -> ComplexMatrix { ComplexMatrix(values: lhs.values.map { $0 - rhs }, rows: lhs.rows, columns: lhs.columns, dimensions: lhs.dimensions) }
    static func * (lhs: ComplexMatrix, rhs: Complex) -> ComplexMatrix { ComplexMatrix(values: lhs.values.map { $0 * rhs }, rows: lhs.rows, columns: lhs.columns, dimensions: lhs.dimensions) }
    static func / (lhs: ComplexMatrix, rhs: Complex) throws -> ComplexMatrix { try ComplexMatrix(values: lhs.values.map { try $0 / rhs }, rows: lhs.rows, columns: lhs.columns, dimensions: lhs.dimensions) }
}
