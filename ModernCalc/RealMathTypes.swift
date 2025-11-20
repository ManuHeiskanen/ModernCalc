//
//  RealMathTypes.swift
//  ModernCalc
//
//  Created by Manu Heiskanen on 22.10.2025.
//

import Foundation
import Accelerate

// --- A struct to represent a value with its associated physical dimensions. ---
struct UnitValue: Equatable, Codable {
    var value: Double
    var dimensions: UnitDimension
    var preferredDisplayUnit: String? = nil

    static func dimensionless(_ value: Double) -> UnitValue {
        return UnitValue(value: value, dimensions: [:] as UnitDimension)
    }
    
    static func create(value: Double, dimensions: UnitDimension) -> UnitValue {
        let preferred = UnitStore.commonDerivedUnits[dimensions]
        return UnitValue(value: value, dimensions: dimensions, preferredDisplayUnit: preferred)
    }

    static func + (lhs: UnitValue, rhs: UnitValue) throws -> UnitValue {
        guard lhs.dimensions == rhs.dimensions else { throw MathError.dimensionMismatch(reason: "Cannot add quantities with different units.") }
        return UnitValue.create(value: lhs.value + rhs.value, dimensions: lhs.dimensions)
    }

    static func - (lhs: UnitValue, rhs: UnitValue) throws -> UnitValue {
        guard lhs.dimensions == rhs.dimensions else { throw MathError.dimensionMismatch(reason: "Cannot subtract quantities with different units.") }
        return UnitValue.create(value: lhs.value - rhs.value, dimensions: lhs.dimensions)
    }
    
    static func * (lhs: UnitValue, rhs: UnitValue) -> UnitValue {
        let newDimensions = lhs.dimensions.merging(rhs.dimensions, uniquingKeysWith: +).filter { $0.value != 0 }
        return UnitValue.create(value: lhs.value * rhs.value, dimensions: newDimensions)
    }
    
    static func * (lhs: UnitValue, rhs: Double) -> UnitValue {
        return UnitValue.create(value: lhs.value * rhs, dimensions: lhs.dimensions)
    }

    static func / (lhs: UnitValue, rhs: UnitValue) throws -> UnitValue {
        guard rhs.value != 0 else { throw MathError.divisionByZero }
        let newDimensions = lhs.dimensions.merging(rhs.dimensions.mapValues { -$0 }, uniquingKeysWith: +).filter { $0.value != 0 }
        return UnitValue.create(value: lhs.value / rhs.value, dimensions: newDimensions)
    }
    
    static func / (lhs: UnitValue, rhs: Double) -> UnitValue {
        return UnitValue.create(value: lhs.value / rhs, dimensions: lhs.dimensions)
    }
    
    func pow(_ exponent: Double) -> UnitValue {
        let newDimensions = self.dimensions.mapValues { $0 * exponent }.filter { $0.value != 0 }
        return UnitValue.create(value: Foundation.pow(self.value, exponent), dimensions: newDimensions)
    }
}

// --- UNCERTAINTY TYPES ---
struct UncertainValue: Equatable, Codable {
    var value: Double
    var randomUncertainty: Double
    var systematicUncertainty: Double
    var dimensions: UnitDimension

    var totalUncertainty: Double {
        return Foundation.sqrt(Foundation.pow(randomUncertainty, 2) + Foundation.pow(systematicUncertainty, 2))
    }

    static func + (lhs: UncertainValue, rhs: UncertainValue) throws -> UncertainValue {
        guard lhs.dimensions == rhs.dimensions else { throw MathError.dimensionMismatch(reason: "Cannot add uncertain values with different units.") }
        return UncertainValue(
            value: lhs.value + rhs.value,
            randomUncertainty: hypot(lhs.randomUncertainty, rhs.randomUncertainty),
            systematicUncertainty: hypot(lhs.systematicUncertainty, rhs.systematicUncertainty),
            dimensions: lhs.dimensions
        )
    }

    static func - (lhs: UncertainValue, rhs: UncertainValue) throws -> UncertainValue {
        guard lhs.dimensions == rhs.dimensions else { throw MathError.dimensionMismatch(reason: "Cannot subtract uncertain values with different units.") }
        return UncertainValue(
            value: lhs.value - rhs.value,
            randomUncertainty: hypot(lhs.randomUncertainty, rhs.randomUncertainty),
            systematicUncertainty: hypot(lhs.systematicUncertainty, rhs.systematicUncertainty),
            dimensions: lhs.dimensions
        )
    }
    
    static func * (lhs: UncertainValue, rhs: UncertainValue) -> UncertainValue {
        let newValue = lhs.value * rhs.value
        let newDimensions = lhs.dimensions.merging(rhs.dimensions, uniquingKeysWith: +).filter { $0.value != 0 }
        
        let relRandomL = lhs.value != 0 ? lhs.randomUncertainty / lhs.value : 0
        let relRandomR = rhs.value != 0 ? rhs.randomUncertainty / rhs.value : 0
        let combinedRelRandom = hypot(relRandomL, relRandomR)
        
        let relSystematicL = lhs.value != 0 ? lhs.systematicUncertainty / lhs.value : 0
        let relSystematicR = rhs.value != 0 ? rhs.systematicUncertainty / rhs.value : 0
        let combinedRelSystematic = hypot(relSystematicL, relSystematicR)

        return UncertainValue(
            value: newValue,
            randomUncertainty: abs(newValue * combinedRelRandom),
            systematicUncertainty: abs(newValue * combinedRelSystematic),
            dimensions: newDimensions
        )
    }

    static func / (lhs: UncertainValue, rhs: UncertainValue) throws -> UncertainValue {
        guard rhs.value != 0 else { throw MathError.divisionByZero }
        let newValue = lhs.value / rhs.value
        let newDimensions = lhs.dimensions.merging(rhs.dimensions.mapValues { -$0 }, uniquingKeysWith: +).filter { $0.value != 0 }

        let relRandomL = lhs.value != 0 ? lhs.randomUncertainty / lhs.value : 0
        let relRandomR = rhs.randomUncertainty / rhs.value
        let combinedRelRandom = hypot(relRandomL, relRandomR)
        
        let relSystematicL = lhs.value != 0 ? lhs.systematicUncertainty / lhs.value : 0
        let relSystematicR = rhs.systematicUncertainty / rhs.value
        let combinedRelSystematic = hypot(relSystematicL, relSystematicR)

        return UncertainValue(
            value: newValue,
            randomUncertainty: abs(newValue * combinedRelRandom),
            systematicUncertainty: abs(newValue * combinedRelSystematic),
            dimensions: newDimensions
        )
    }
    
    func pow(_ exponent: Double) -> UncertainValue {
        if self.value == 0 { return self }
        let newValue = Foundation.pow(self.value, exponent)
        let newDimensions = self.dimensions.mapValues { $0 * exponent }.filter { $0.value != 0 }
        let relativeUncertainty = self.totalUncertainty / self.value
        let newTotalUncertainty = abs(newValue * exponent * relativeUncertainty)
        let randomRatio = self.totalUncertainty > 0 ? self.randomUncertainty / self.totalUncertainty : 0
        
        return UncertainValue(
            value: newValue,
            randomUncertainty: newTotalUncertainty * randomRatio,
            systematicUncertainty: newTotalUncertainty * (1 - randomRatio),
            dimensions: newDimensions
        )
    }
    
    static prefix func - (operand: UncertainValue) -> UncertainValue {
        return UncertainValue(value: -operand.value, randomUncertainty: operand.randomUncertainty, systematicUncertainty: operand.systematicUncertainty, dimensions: operand.dimensions)
    }
    
    func propagate(derivative: Double) -> UncertainValue {
        return UncertainValue(
            value: self.value,
            randomUncertainty: abs(derivative * self.randomUncertainty),
            systematicUncertainty: abs(derivative * self.systematicUncertainty),
            dimensions: self.dimensions
        )
    }
}

// --- OPTIMIZED VECTOR (vDSP) ---
struct Vector: Equatable, Codable {
    let values: [Double]
    let dimensions: UnitDimension
    var dimension: Int { values.count }
    
    init(values: [Double], dimensions: UnitDimension = [:] as UnitDimension) {
        self.values = values
        self.dimensions = dimensions
    }
    
    subscript(index: Int) -> Double {
        return values[index]
    }
    
    // Optimized Dot Product
    func dot(with other: Vector) throws -> UnitValue {
        guard self.dimension == other.dimension else { throw MathError.dimensionMismatch(reason: "Vectors must have same dimension for dot product.") }
        
        let result = vDSP.dot(self.values, other.values)
        let newDimensions = self.dimensions.merging(other.dimensions, uniquingKeysWith: +).filter { $0.value != 0 }
        return UnitValue.create(value: result, dimensions: newDimensions)
    }
    
    func cross(with other: Vector) throws -> Vector {
        guard self.dimension == 3 && other.dimension == 3 else { throw MathError.dimensionMismatch(reason: "Cross product defined for 3D vectors only.") }
        // Cross product is small enough (3 elements) that manual calculation is faster than vDSP setup overhead.
        let u = self.values; let v = other.values
        let newValues = [
            u[1] * v[2] - u[2] * v[1],
            u[2] * v[0] - u[0] * v[2],
            u[0] * v[1] - u[1] * v[0]
        ]
        let newDimensions = self.dimensions.merging(other.dimensions, uniquingKeysWith: +).filter { $0.value != 0 }
        return Vector(values: newValues, dimensions: newDimensions)
    }
    
    func hadamard(with other: Vector) throws -> Vector {
        guard self.dimension == other.dimension else { throw MathError.dimensionMismatch(reason: "Dimension mismatch for hadamard product.") }
        let result = vDSP.multiply(self.values, other.values)
        let newDimensions = self.dimensions.merging(other.dimensions, uniquingKeysWith: +).filter { $0.value != 0 }
        return Vector(values: result, dimensions: newDimensions)
    }

    func hadamardDivision(with other: Vector) throws -> Vector {
        guard self.dimension == other.dimension else { throw MathError.dimensionMismatch(reason: "Dimension mismatch for element-wise division.") }
        if other.values.contains(0) { throw MathError.divisionByZero }
        let result = vDSP.divide(self.values, other.values)
        let newDimensions = self.dimensions.merging(other.dimensions.mapValues { -$0 }, uniquingKeysWith: +).filter { $0.value != 0 }
        return Vector(values: result, dimensions: newDimensions)
    }
    
    func unit() -> Vector {
        let mag = self.magnitude().value
        guard mag != 0 else { return self }
        let result = vDSP.divide(self.values, mag)
        return Vector(values: result, dimensions: [:])
    }

    func angle(with other: Vector) throws -> Double {
        let dotVal = try self.dot(with: other).value
        let mag1 = self.magnitude().value
        let mag2 = other.magnitude().value
        guard mag1 > 0, mag2 > 0 else { return 0 }
        return acos(Swift.min(Swift.max(dotVal / (mag1 * mag2), -1.0), 1.0))
    }
    
    func modifying(at index: Int, with scalar: Double, operation: (Double, Double) -> Double) throws -> Vector {
        guard index >= 0 && index < self.dimension else { throw MathError.dimensionMismatch(reason: "Index out of bounds.") }
        var newValues = self.values
        newValues[index] = operation(newValues[index], scalar)
        return Vector(values: newValues, dimensions: self.dimensions)
    }
    
    func slice(indices: [Int]) throws -> MathValue {
        var newValues: [Double] = []
        newValues.reserveCapacity(indices.count)
        for index in indices {
            guard index >= 1 && index <= self.dimension else { throw MathError.dimensionMismatch(reason: "Index out of bounds.") }
            newValues.append(self.values[index - 1])
        }
        if newValues.count == 1 { return .unitValue(UnitValue(value: newValues[0], dimensions: self.dimensions)) }
        return .vector(Vector(values: newValues, dimensions: self.dimensions))
    }

    // vDSP Optimizations for arithmetic
    static func + (lhs: Vector, rhs: Vector) throws -> Vector {
        guard lhs.dimension == rhs.dimension else { throw MathError.dimensionMismatch(reason: "Dimension mismatch.") }
        guard lhs.dimensions == rhs.dimensions else { throw MathError.dimensionMismatch(reason: "Unit mismatch.") }
        return Vector(values: vDSP.add(lhs.values, rhs.values), dimensions: lhs.dimensions)
    }
    static func - (lhs: Vector, rhs: Vector) throws -> Vector {
        guard lhs.dimension == rhs.dimension else { throw MathError.dimensionMismatch(reason: "Dimension mismatch.") }
        guard lhs.dimensions == rhs.dimensions else { throw MathError.dimensionMismatch(reason: "Unit mismatch.") }
        return Vector(values: vDSP.subtract(lhs.values, rhs.values), dimensions: lhs.dimensions)
    }

    static func * (lhs: Vector, rhs: Matrix) throws -> Vector {
        // v * M -> 1xN * NxM -> 1xM
        guard lhs.dimension == rhs.rows else { throw MathError.dimensionMismatch(reason: "v*M dimension mismatch.") }
        
        // REPLACEMENT FOR DEPRECATED cblas_dgemv
        // Treat vector as 1xN matrix.
        // [1xN] * [NxM] = [1xM]
        let m = vDSP_Length(1)
        let n = vDSP_Length(rhs.columns)
        let p = vDSP_Length(lhs.dimension) // common dimension
        
        var result = [Double](repeating: 0, count: Int(n))
        
        vDSP_mmulD(lhs.values, 1, rhs.values, 1, &result, 1, m, n, p)
        
        let newDims = lhs.dimensions.merging(rhs.dimensions, uniquingKeysWith: +).filter { $0.value != 0 }
        return Vector(values: result, dimensions: newDims)
    }
    
    static func * (lhs: Vector, rhs: Vector) -> Matrix {
        // Outer product: v1 (Nx1) * v2 (1xM) -> NxM
        let n = lhs.dimension
        let m = rhs.dimension
        
        // REPLACEMENT FOR DEPRECATED cblas_dger
        // We use vDSP_mmulD treating lhs as Nx1 and rhs as 1xM
        let rows = vDSP_Length(n)
        let cols = vDSP_Length(m)
        let common = vDSP_Length(1)
        
        var result = [Double](repeating: 0, count: n * m)
        
        vDSP_mmulD(lhs.values, 1, rhs.values, 1, &result, 1, rows, cols, common)
        
        let newDims = lhs.dimensions.merging(rhs.dimensions, uniquingKeysWith: +).filter { $0.value != 0 }
        return Matrix(values: result, rows: n, columns: m, dimensions: newDims)
    }
    
    static func + (lhs: Vector, rhs: Double) -> Vector { Vector(values: vDSP.add(rhs, lhs.values), dimensions: lhs.dimensions) }
    static func + (lhs: Double, rhs: Vector) -> Vector { rhs + lhs }
    static func - (lhs: Vector, rhs: Double) -> Vector { Vector(values: vDSP.add(-rhs, lhs.values), dimensions: lhs.dimensions) } // v - s = v + (-s)
    static func - (lhs: Double, rhs: Vector) -> Vector { Vector(values: vDSP.add(lhs, vDSP.multiply(-1, rhs.values)), dimensions: rhs.dimensions) }
    static func * (lhs: Vector, rhs: Double) -> Vector { Vector(values: vDSP.multiply(rhs, lhs.values), dimensions: lhs.dimensions) }
    static func * (lhs: Double, rhs: Vector) -> Vector { rhs * lhs }
    static func / (lhs: Vector, rhs: Double) -> Vector { Vector(values: vDSP.divide(lhs.values, rhs), dimensions: lhs.dimensions) }
    static func / (lhs: Double, rhs: Vector) -> Vector { Vector(values: vDSP.divide(lhs, rhs.values), dimensions: rhs.dimensions.mapValues { -$0 }) }

    // Statistics via vDSP
    func sum() -> UnitValue { UnitValue.create(value: vDSP.sum(values), dimensions: self.dimensions) }
    func average() -> UnitValue { UnitValue.create(value: vDSP.mean(values), dimensions: self.dimensions) }
    func min() -> UnitValue? { values.isEmpty ? nil : UnitValue.create(value: vDSP.minimum(values), dimensions: self.dimensions) }
    func max() -> UnitValue? { values.isEmpty ? nil : UnitValue.create(value: vDSP.maximum(values), dimensions: self.dimensions) }
    func median() -> UnitValue? {
        guard !values.isEmpty else { return nil }
        let sorted = values.sorted()
        let mid = dimension / 2
        let val = (dimension % 2 == 0) ? (sorted[mid-1] + sorted[mid])/2 : sorted[mid]
        return UnitValue.create(value: val, dimensions: self.dimensions)
    }
    func stddev() -> UnitValue? {
        guard dimension > 1 else { return nil }
        let mean = vDSP.mean(values)
        let sqDiff = vDSP.meanSquare(vDSP.add(-mean, values)) * Double(dimension) // sum of squared diffs
        return UnitValue.create(value: sqrt(sqDiff / Double(dimension - 1)), dimensions: self.dimensions)
    }
    func magnitude() -> UnitValue {
        // REPLACEMENT FOR DEPRECATED cblas_dnrm2
        // Euclidean norm (L2) = sqrt(sum(x^2))
        let sumSquares = vDSP.sumOfSquares(values)
        return UnitValue.create(value: sqrt(sumSquares), dimensions: self.dimensions)
    }
    func variance() -> UnitValue? {
        guard dimension > 1 else { return nil }
        let mean = vDSP.mean(values)
        let sqDiff = vDSP.meanSquare(vDSP.add(-mean, values)) * Double(dimension)
        let newDims = self.dimensions.mapValues { $0 * 2 }.filter { $0.value != 0 }
        return UnitValue.create(value: sqDiff / Double(dimension - 1), dimensions: newDims)
    }
    func stddevp() -> UnitValue? {
        guard dimension > 0 else { return nil }
        let mean = vDSP.mean(values)
        let sqDiff = vDSP.meanSquare(vDSP.add(-mean, values)) // this returns mean of squares, effectively variance population
        return UnitValue.create(value: sqrt(sqDiff), dimensions: self.dimensions)
    }
}

// --- OPTIMIZED MATRIX (LAPACK/BLAS) ---
struct Matrix: Equatable, Codable {
    let values: [Double]
    let rows: Int
    let columns: Int
    let dimensions: UnitDimension
    
    init(values: [Double], rows: Int, columns: Int, dimensions: UnitDimension = [:] as UnitDimension) {
        self.values = values; self.rows = rows; self.columns = columns; self.dimensions = dimensions
    }
    
    subscript(row: Int, col: Int) -> Double { values[row * columns + col] }
    
    // Helper to get submatrix (used less often now due to LAPACK)
    func submatrix(excludingRow: Int, excludingCol: Int) -> Matrix {
        var newValues = [Double]()
        newValues.reserveCapacity((rows - 1) * (columns - 1))
        for r in 0..<rows where r != excludingRow {
            for c in 0..<columns where c != excludingCol {
                newValues.append(self[r, c])
            }
        }
        return Matrix(values: newValues, rows: rows - 1, columns: columns - 1, dimensions: self.dimensions)
    }

    // Optimized Determinant using LU Decomposition (dgetrf) - O(N^3)
    func determinant() throws -> UnitValue {
        guard rows == columns else { throw MathError.dimensionMismatch(reason: "Square matrix required.") }
        
        var a = values
        // CHANGED: Int32 -> Int for ILP64
        var pivots = [Int](repeating: 0, count: rows)
        var info: Int = 0
        var m = Int(rows)
        var n = Int(columns)
        var lda = m
        
        // Perform LU decomposition (Standard LAPACK dgetrf_)
        dgetrf_(&m, &n, &a, &lda, &pivots, &info)
        
        if info < 0 { throw MathError.solverFailed(reason: "Illegal value in LAPACK dgetrf") }
        
        // Determinant is product of diagonal elements of U (stored in 'a') * (-1)^swaps
        var det = 1.0
        for i in 0..<rows { det *= a[i * rows + i] } // accessing column-major diagonal
        
        // Account for pivot swaps
        var swaps = 0
        for i in 0..<rows {
            // CHANGED: Int32 -> Int
            if pivots[i] != Int(i + 1) { swaps += 1 }
        }
        if swaps % 2 != 0 { det = -det }
        
        let newDims = self.dimensions.mapValues { $0 * Double(rows) }.filter { $0.value != 0 }
        return UnitValue.create(value: det, dimensions: newDims)
    }

    // Optimized Inverse using LU Decomposition (dgetrf + dgetri) - O(N^3)
    func inverse() throws -> Matrix {
        guard rows == columns else { throw MathError.dimensionMismatch(reason: "Square matrix required.") }
        
        var a = values // Row-Major
        // CHANGED: Int32 -> Int for ILP64
        var m = Int(rows)
        var n = Int(columns)
        var lda = m
        var pivots = [Int](repeating: 0, count: rows)
        var info: Int = 0
        
        // LU Factorization (Standard LAPACK dgetrf_)
        dgetrf_(&m, &n, &a, &lda, &pivots, &info)
        if info != 0 { throw MathError.unsupportedOperation(op: "inverse", typeA: "Singular Matrix", typeB: nil) }
        
        // Inverse calculation
        var workspace = [Double](repeating: 0, count: rows * rows)
        var lwork = Int(rows * rows)
        var n_getri = m
        var lda_getri = m
        
        // Standard LAPACK dgetri_
        dgetri_(&n_getri, &a, &lda_getri, &pivots, &workspace, &lwork, &info)
        
        if info != 0 { throw MathError.solverFailed(reason: "Inversion failed") }
        
        let newDims = self.dimensions.mapValues { -$0 }.filter { $0.value != 0 }
        return Matrix(values: a, rows: rows, columns: columns, dimensions: newDims)
    }

    func transpose() -> Matrix {
        var result = [Double](repeating: 0, count: values.count)
        vDSP_mtransD(values, 1, &result, 1, vDSP_Length(columns), vDSP_Length(rows))
        return Matrix(values: result, rows: columns, columns: rows, dimensions: self.dimensions)
    }
    
    func trace() throws -> UnitValue {
        guard rows == columns else { throw MathError.dimensionMismatch(reason: "Square matrix required.") }
        // Sum of diagonal elements
        var sum = 0.0
        for i in 0..<rows { sum += self[i, i] }
        return UnitValue.create(value: sum, dimensions: self.dimensions)
    }
    
    func frobeniusNorm() -> UnitValue {
        // REPLACEMENT FOR DEPRECATED cblas_dnrm2
        let sumSquares = vDSP.sumOfSquares(values)
        return UnitValue.create(value: sqrt(sumSquares), dimensions: self.dimensions)
    }

    func rank() -> Int {
        // Optimized rank using Singular Value Decomposition (dgesvd)
        // Rank = number of singular values > tolerance
        var a = values
        // CHANGED: Int32 -> Int for ILP64
        var m = Int(rows)
        var n = Int(columns)
        var s = [Double](repeating: 0, count: min(rows, columns))
        var u = [Double](repeating: 0, count: rows * rows)
        var vt = [Double](repeating: 0, count: columns * columns)
        var work = [Double](repeating: 0, count: max(1, 5 * min(rows, columns)))
        var lwork = Int(-1)
        var info: Int = 0
        
        var lda = m
        var ldu = m
        var ldvt = n

        // Fix for dangling pointer warnings: Explicit CChar
        var jobN: CChar = 78 // 'N'

        // Query optimal workspace size (Standard LAPACK dgesvd_)
        dgesvd_(&jobN, &jobN, &m, &n, &a, &lda, &s, &u, &ldu, &vt, &ldvt, &work, &lwork, &info)
        lwork = Int(work[0])
        work = [Double](repeating: 0, count: Int(lwork))
        
        // Compute SVD
        dgesvd_(&jobN, &jobN, &m, &n, &a, &lda, &s, &u, &ldu, &vt, &ldvt, &work, &lwork, &info)
        
        let tolerance = 1e-10
        return s.filter { $0 > tolerance }.count
    }

    func hadamard(with other: Matrix) throws -> Matrix {
        guard rows == other.rows && columns == other.columns else { throw MathError.dimensionMismatch(reason: "Size mismatch.") }
        let result = vDSP.multiply(self.values, other.values)
        let newDims = self.dimensions.merging(other.dimensions, uniquingKeysWith: +).filter { $0.value != 0 }
        return Matrix(values: result, rows: rows, columns: columns, dimensions: newDims)
    }
    
    func hadamardDivision(with other: Matrix) throws -> Matrix {
        guard rows == other.rows && columns == other.columns else { throw MathError.dimensionMismatch(reason: "Size mismatch.") }
        if other.values.contains(0) { throw MathError.divisionByZero }
        let result = vDSP.divide(self.values, other.values)
        let newDims = self.dimensions.merging(other.dimensions.mapValues { -$0 }, uniquingKeysWith: +).filter { $0.value != 0 }
        return Matrix(values: result, rows: rows, columns: columns, dimensions: newDims)
    }
    
    func getcolumn(index: Int) throws -> Vector {
        let j = index - 1
        guard j >= 0 && j < columns else { throw MathError.dimensionMismatch(reason: "Index out of bounds.") }
        
        // REPLACEMENT FOR DEPRECATED cblas_dcopy
        // We need to extract a strided column.
        // Matrix is Row-Major. Stride is 'columns'.
        var colValues = [Double](repeating: 0, count: rows)
        
        // Manual loop is clear and efficient enough here
        colValues = (0..<rows).map { r in values[r * columns + j] }
        
        return Vector(values: colValues, dimensions: self.dimensions)
    }
    
    func getrow(index: Int) throws -> Vector {
        let i = index - 1
        guard i >= 0 && i < rows else { throw MathError.dimensionMismatch(reason: "Index out of bounds.") }
        let start = i * columns
        let rowValues = Array(values[start..<(start + columns)])
        return Vector(values: rowValues, dimensions: self.dimensions)
    }
    
    func slice(rowIndices: [Int], colIndices: [Int]) throws -> MathValue {
        var newValues = [Double]()
        newValues.reserveCapacity(rowIndices.count * colIndices.count)
        for r in rowIndices {
            guard r >= 1 && r <= rows else { throw MathError.dimensionMismatch(reason: "Row index out of bounds.") }
            for c in colIndices {
                guard c >= 1 && c <= columns else { throw MathError.dimensionMismatch(reason: "Col index out of bounds.") }
                newValues.append(self[r - 1, c - 1])
            }
        }
        if newValues.count == 1 { return .unitValue(UnitValue(value: newValues[0], dimensions: self.dimensions)) }
        return .matrix(Matrix(values: newValues, rows: rowIndices.count, columns: colIndices.count, dimensions: self.dimensions))
    }
    
    // Matrix Arithmetic using vDSP
    static func + (lhs: Matrix, rhs: Matrix) throws -> Matrix {
        guard lhs.rows == rhs.rows && lhs.columns == rhs.columns else { throw MathError.dimensionMismatch(reason: "Dimension mismatch.") }
        guard lhs.dimensions == rhs.dimensions else { throw MathError.dimensionMismatch(reason: "Unit mismatch.") }
        return Matrix(values: vDSP.add(lhs.values, rhs.values), rows: lhs.rows, columns: lhs.columns, dimensions: lhs.dimensions)
    }

    static func - (lhs: Matrix, rhs: Matrix) throws -> Matrix {
        guard lhs.rows == rhs.rows && lhs.columns == rhs.columns else { throw MathError.dimensionMismatch(reason: "Dimension mismatch.") }
        guard lhs.dimensions == rhs.dimensions else { throw MathError.dimensionMismatch(reason: "Unit mismatch.") }
        return Matrix(values: vDSP.subtract(lhs.values, rhs.values), rows: lhs.rows, columns: lhs.columns, dimensions: lhs.dimensions)
    }

    static func * (lhs: Matrix, rhs: Matrix) throws -> Matrix {
        // Scalar cases
        if lhs.rows == 1 && lhs.columns == 1 {
            let s = lhs.values[0]
            let newDims = lhs.dimensions.merging(rhs.dimensions, uniquingKeysWith: +).filter { $0.value != 0 }
            return Matrix(values: vDSP.multiply(s, rhs.values), rows: rhs.rows, columns: rhs.columns, dimensions: newDims)
        }
        if rhs.rows == 1 && rhs.columns == 1 {
            let s = rhs.values[0]
            let newDims = lhs.dimensions.merging(rhs.dimensions, uniquingKeysWith: +).filter { $0.value != 0 }
            return Matrix(values: vDSP.multiply(s, lhs.values), rows: lhs.rows, columns: lhs.columns, dimensions: newDims)
        }
        
        guard lhs.columns == rhs.rows else { throw MathError.dimensionMismatch(reason: "Matrix multiplication A*B requires Cols(A) == Rows(B).") }
        
        // vDSP_mmulD performs matrix multiplication.
        // C = A * B
        // A: M x P
        // B: P x N
        // C: M x N
        let m = vDSP_Length(lhs.rows)
        let n = vDSP_Length(rhs.columns)
        let p = vDSP_Length(lhs.columns)
        
        var result = [Double](repeating: 0, count: Int(m * n))
        
        vDSP_mmulD(lhs.values, 1, rhs.values, 1, &result, 1, m, n, p)
        
        let newDims = lhs.dimensions.merging(rhs.dimensions, uniquingKeysWith: +).filter { $0.value != 0 }
        return Matrix(values: result, rows: Int(m), columns: Int(n), dimensions: newDims)
    }

    static func * (lhs: Matrix, rhs: Vector) throws -> Vector {
        // M * v -> M rows, P cols * P vector -> M vector
        guard lhs.columns == rhs.dimension else { throw MathError.dimensionMismatch(reason: "Dimension mismatch M*v.") }
        
        // REPLACEMENT FOR DEPRECATED cblas_dgemv
        // Using vDSP_mmulD treating Vector as Px1 Matrix
        let m = vDSP_Length(lhs.rows)
        let n = vDSP_Length(1) // 1 column in output vector
        let p = vDSP_Length(lhs.columns)
        
        var result = [Double](repeating: 0, count: Int(m))
        
        vDSP_mmulD(lhs.values, 1, rhs.values, 1, &result, 1, m, n, p)
        
        let newDims = lhs.dimensions.merging(rhs.dimensions, uniquingKeysWith: +).filter { $0.value != 0 }
        return Vector(values: result, dimensions: newDims)
    }
}

func factorial(_ n: Double) throws -> Double {
    guard n >= 0 && n.truncatingRemainder(dividingBy: 1) == 0 else { throw MathError.typeMismatch(expected: "non-negative integer", found: "number") }
    if n == 0 { return 1 }
    // vDSP doesn't have factorial, but tgamma(n+1) approximates it.
    // For exact integers, manual or lookup is mostly fine. For speed on large numbers, use tgamma.
    return tgamma(n + 1)
}

func permutations(n: Double, k: Double) throws -> Double {
    guard n >= k && k >= 0 else { throw MathError.unsupportedOperation(op: "nPr", typeA: "invalid args", typeB: nil) }
    return tgamma(n + 1) / tgamma(n - k + 1)
}

func combinations(n: Double, k: Double) throws -> Double {
    guard n >= k && k >= 0 else { throw MathError.unsupportedOperation(op: "nCr", typeA: "invalid args", typeB: nil) }
    return tgamma(n + 1) / (tgamma(k + 1) * tgamma(n - k + 1))
}

struct PolynomialCoefficients: Equatable, Codable {
    let coefficients: [UnitValue]
}

enum MathValue: Codable, Equatable {
    case dimensionless(Double)
    case unitValue(UnitValue)
    case complex(Complex)
    case vector(Vector)
    case matrix(Matrix)
    case tuple([MathValue])
    case functionDefinition(String)
    case complexVector(ComplexVector)
    case complexMatrix(ComplexMatrix)
    case complexUnitValue(ComplexUnitValue)
    case polar(Complex)
    case constant(String)
    case regressionResult(slope: UnitValue, intercept: UnitValue)
    case polynomialFit(coefficients: PolynomialCoefficients)
    case plot(PlotData)
    case triggerCSVImport
    case uncertain(UncertainValue)
    case roots([MathValue])
    
    // FIX: Added 'indirect' to break recursion cycle
    indirect case eigenDecomposition(eigenvectors: MathValue, eigenvalues: MathValue)
    
    case odeSolution(time: Vector, states: Matrix)

    var typeName: String {
        switch self {
        case .dimensionless: return "Dimensionless"
        case .unitValue: return "UnitValue"
        case .complex: return "Complex"
        case .vector: return "Vector"
        case .matrix: return "Matrix"
        case .tuple: return "Tuple"
        case .functionDefinition: return "FunctionDefinition"
        case .complexVector: return "ComplexVector"
        case .complexMatrix: return "ComplexMatrix"
        case .complexUnitValue: return "ComplexUnitValue"
        case .polar: return "Polar"
        case .regressionResult: return "RegressionResult"
        case .polynomialFit: return "PolynomialFit"
        case .plot: return "Plot"
        case .triggerCSVImport: return "CSVImportTrigger"
        case .constant: return "Constant"
        case .uncertain: return "UncertainValue"
        case .roots: return "Roots"
        case .eigenDecomposition: return "EigenDecomposition"
        case .odeSolution: return "ODESolution"
        }
    }
    
    func asUnitValue() throws -> UnitValue {
        switch self {
        case .dimensionless(let d): return UnitValue.dimensionless(d)
        case .unitValue(let u): return u
        case .uncertain(let u): return UnitValue(value: u.value, dimensions: u.dimensions)
        default: throw MathError.typeMismatch(expected: "Numeric value", found: self.typeName)
        }
    }
    
    func asScalar() throws -> Double {
        switch self {
        case .dimensionless(let d): return d
        case .unitValue(let u):
            guard u.dimensions.isEmpty else { throw MathError.typeMismatch(expected: "Dimensionless value", found: "Value with units") }
            return u.value
        case .uncertain(let u):
            guard u.dimensions.isEmpty else { throw MathError.typeMismatch(expected: "Dimensionless uncertain value", found: "Uncertain value with units") }
            return u.value
        case .vector(let v):
            guard v.dimensions.isEmpty && v.dimension == 1 else { throw MathError.typeMismatch(expected: "Scalar", found: "Vector") }
            return v[0]
        case .matrix(let m):
            guard m.dimensions.isEmpty && m.rows == 1 && m.columns == 1 else { throw MathError.typeMismatch(expected: "Scalar", found: "Matrix") }
            return m[0,0]
        default: throw MathError.typeMismatch(expected: "Scalar value", found: self.typeName)
        }
    }
    
    func asVector(for functionName: String) throws -> Vector {
        switch self {
        case .vector(let v): return v
        case .matrix(let m):
            if m.rows == 1 || m.columns == 1 { return Vector(values: m.values, dimensions: m.dimensions) }
            else { throw MathError.typeMismatch(expected: "Vector", found: "Matrix") }
        default: throw MathError.typeMismatch(expected: "Vector", found: self.typeName)
        }
    }
    
    // CODABLE IMPLEMENTATION
    enum CodingKeys: String, CodingKey { case type, value, slope, intercept, coefficients, plotData, eigenvectors, eigenvalues, time, states }

    func encode(to encoder: Encoder) throws {
        var container = encoder.container(keyedBy: CodingKeys.self)
        switch self {
        case .dimensionless(let d): try container.encode("dimensionless", forKey: .type); try container.encode(d, forKey: .value)
        case .unitValue(let u): try container.encode("unitValue", forKey: .type); try container.encode(u, forKey: .value)
        case .complex(let c): try container.encode("complex", forKey: .type); try container.encode(c, forKey: .value)
        case .vector(let v): try container.encode("vector", forKey: .type); try container.encode(v, forKey: .value)
        case .matrix(let m): try container.encode("matrix", forKey: .type); try container.encode(m, forKey: .value)
        case .tuple(let t): try container.encode("tuple", forKey: .type); try container.encode(t, forKey: .value)
        case .functionDefinition(let f): try container.encode("functionDefinition", forKey: .type); try container.encode(f, forKey: .value)
        case .complexVector(let cv): try container.encode("complexVector", forKey: .type); try container.encode(cv, forKey: .value)
        case .complexMatrix(let cm): try container.encode("complexMatrix", forKey: .type); try container.encode(cm, forKey: .value)
        case .complexUnitValue(let cu): try container.encode("complexUnitValue", forKey: .type); try container.encode(cu, forKey: .value)
        case .polar(let p): try container.encode("polar", forKey: .type); try container.encode(p, forKey: .value)
        case .regressionResult(let slope, let intercept):
            try container.encode("regressionResult", forKey: .type); try container.encode(slope, forKey: .slope); try container.encode(intercept, forKey: .intercept)
        case .polynomialFit(let coeffs):
            try container.encode("polynomialFit", forKey: .type); try container.encode(coeffs, forKey: .coefficients)
        case .constant(let s): try container.encode("constant", forKey: .type); try container.encode(s, forKey: .value)
        case .uncertain(let u): try container.encode("uncertain", forKey: .type); try container.encode(u, forKey: .value)
        case .roots(let r): try container.encode("roots", forKey: .type); try container.encode(r, forKey: .value)
        case .eigenDecomposition(let eigenvectors, let eigenvalues):
            try container.encode("eigenDecomposition", forKey: .type); try container.encode(eigenvectors, forKey: .eigenvectors); try container.encode(eigenvalues, forKey: .eigenvalues)
        case .odeSolution(let time, let states):
            try container.encode("odeSolution", forKey: .type); try container.encode(time, forKey: .time); try container.encode(states, forKey: .states)
        case .plot: try container.encode("plot", forKey: .type)
        case .triggerCSVImport: break
        }
    }

    init(from decoder: Decoder) throws {
        let container = try decoder.container(keyedBy: CodingKeys.self)
        let type = try container.decode(String.self, forKey: .type)
        switch type {
        case "dimensionless", "scalar": self = .dimensionless(try container.decode(Double.self, forKey: .value))
        case "unitValue": self = .unitValue(try container.decode(UnitValue.self, forKey: .value))
        case "complex": self = .complex(try container.decode(Complex.self, forKey: .value))
        case "vector": self = .vector(try container.decode(Vector.self, forKey: .value))
        case "matrix": self = .matrix(try container.decode(Matrix.self, forKey: .value))
        case "tuple": self = .tuple(try container.decode([MathValue].self, forKey: .value))
        case "functionDefinition": self = .functionDefinition(try container.decode(String.self, forKey: .value))
        case "complexVector": self = .complexVector(try container.decode(ComplexVector.self, forKey: .value))
        case "complexMatrix": self = .complexMatrix(try container.decode(ComplexMatrix.self, forKey: .value))
        case "complexUnitValue": self = .complexUnitValue(try container.decode(ComplexUnitValue.self, forKey: .value))
        case "polar": self = .polar(try container.decode(Complex.self, forKey: .value))
        case "regressionResult":
            let slope = try container.decode(UnitValue.self, forKey: .slope); let intercept = try container.decode(UnitValue.self, forKey: .intercept)
            self = .regressionResult(slope: slope, intercept: intercept)
        case "polynomialFit":
            self = .polynomialFit(coefficients: try container.decode(PolynomialCoefficients.self, forKey: .coefficients))
        case "constant": self = .constant(try container.decode(String.self, forKey: .value))
        case "uncertain": self = .uncertain(try container.decode(UncertainValue.self, forKey: .value))
        case "roots":
            if let doubleRoots = try? container.decode([Double].self, forKey: .value) { self = .roots(doubleRoots.map { .dimensionless($0) }) }
            else { self = .roots(try container.decode([MathValue].self, forKey: .value)) }
        case "eigenDecomposition":
            // FIX: Decode as MathValue.self, not Matrix.self
            let eigenvectors = try container.decode(MathValue.self, forKey: .eigenvectors)
            let eigenvalues = try container.decode(MathValue.self, forKey: .eigenvalues)
            self = .eigenDecomposition(eigenvectors: eigenvectors, eigenvalues: eigenvalues)
        case "odeSolution":
            let time = try container.decode(Vector.self, forKey: .time); let states = try container.decode(Matrix.self, forKey: .states)
            self = .odeSolution(time: time, states: states)
        case "plot": self = .plot(PlotData(expression: "Empty", series: [], plotType: .line, explicitYRange: nil))
        default: throw DecodingError.dataCorruptedError(forKey: .type, in: container, debugDescription: "Invalid MathValue type '\(type)'")
        }
    }
    
    static func == (lhs: MathValue, rhs: MathValue) -> Bool {
        // Basic equality check, largely depends on associated values being equatable
        switch (lhs, rhs) {
        case (.dimensionless(let a), .dimensionless(let b)): return a == b
        case (.unitValue(let a), .unitValue(let b)): return a == b
        case (.vector(let a), .vector(let b)): return a == b
        case (.matrix(let a), .matrix(let b)): return a == b
        case (.tuple(let a), .tuple(let b)): return a == b
        case (.functionDefinition(let a), .functionDefinition(let b)): return a == b
        case (.complexVector(let a), .complexVector(let b)): return a == b
        case (.complexMatrix(let a), .complexMatrix(let b)): return a == b
        case (.complexUnitValue(let a), .complexUnitValue(let b)): return a == b
        case (.polar(let a), .polar(let b)): return a == b
        case (.regressionResult(let s1, let i1), .regressionResult(let s2, let i2)): return s1 == s2 && i1 == i2
        case (.polynomialFit(let a), .polynomialFit(let b)): return a == b
        case (.plot(let a), .plot(let b)): return a == b
        case (.triggerCSVImport, .triggerCSVImport): return true
        case (.constant(let a), .constant(let b)): return a == b
        case (.uncertain(let a), .uncertain(let b)): return a == b
        case (.roots(let a), .roots(let b)): return a == b
        case (.eigenDecomposition(let v1, let d1), .eigenDecomposition(let v2, let d2)): return v1 == v2 && d1 == d2
        case (.odeSolution(let t1, let s1), .odeSolution(let t2, let s2)): return t1 == t2 && s1 == s2
        default: return false
        }
    }
}

extension Array {
    func chunks(ofCount chunkSize: Int) -> [[Element]] {
        return stride(from: 0, to: self.count, by: chunkSize).map {
            Array(self[$0..<Swift.min($0 + chunkSize, self.count)])
        }
    }
}
