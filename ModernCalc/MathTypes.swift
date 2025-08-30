//
//  MathTypes.swift
//  ModernCalc
//
//  Created by Manu Heiskanen on 29.8.2025.
//

import Foundation

// --- CORE DATA TYPES ---

struct Complex: Equatable {
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
}

struct Vector: Equatable {
    let values: [Double]
    var dimension: Int { values.count }
    
    init(values: [Double]) {
        self.values = values
    }
    
    subscript(index: Int) -> Double {
        return values[index]
    }
    
    // --- Vector Operations ---
    func dot(with other: Vector) throws -> Double {
        guard self.dimension == other.dimension else {
            throw MathError.dimensionMismatch(reason: "Vectors must have the same dimension for dot product.")
        }
        return zip(self.values, other.values).map(*).reduce(0, +)
    }
    
    func cross(with other: Vector) throws -> Vector {
        guard self.dimension == 3 && other.dimension == 3 else {
            throw MathError.dimensionMismatch(reason: "Cross product is only defined for 3D vectors.")
        }
        let u = self.values
        let v = other.values
        let newValues = [
            u[1] * v[2] - u[2] * v[1],
            u[2] * v[0] - u[0] * v[2],
            u[0] * v[1] - u[1] * v[0]
        ]
        return Vector(values: newValues)
    }
    
    // --- Statistical Helpers ---
    func sum() -> Double { return values.reduce(0, +) }
    func average() -> Double {
        guard !values.isEmpty else { return 0 }
        return sum() / Double(dimension)
    }
    func min() -> Double? { return values.min() }
    func max() -> Double? { return values.max() }
    func median() -> Double? {
        guard !values.isEmpty else { return nil }
        let sorted = values.sorted()
        if dimension % 2 == 0 {
            return (sorted[dimension / 2 - 1] + sorted[dimension / 2]) / 2
        } else {
            return sorted[dimension / 2]
        }
    }
    func stddev() -> Double? {
        guard dimension > 1 else { return nil }
        let mean = average()
        let sumOfSquaredDiffs = values.map { pow($0 - mean, 2.0) }.reduce(0, +)
        return Foundation.sqrt(sumOfSquaredDiffs / Double(dimension - 1))
    }
    func magnitude() -> Double {
        return Foundation.sqrt(values.map { $0 * $0 }.reduce(0, +))
    }
}

struct Matrix: Equatable {
    let values: [Double]
    let rows: Int
    let columns: Int
    
    init(values: [Double], rows: Int, columns: Int) {
        self.values = values
        self.rows = rows
        self.columns = columns
    }
    
    subscript(row: Int, col: Int) -> Double {
        return values[row * columns + col]
    }
    
    // --- Matrix Operations ---
    func submatrix(excludingRow: Int, excludingCol: Int) -> Matrix {
        var newValues: [Double] = []
        for r in 0..<rows {
            guard r != excludingRow else { continue }
            for c in 0..<columns {
                guard c != excludingCol else { continue }
                newValues.append(self[r, c])
            }
        }
        return Matrix(values: newValues, rows: rows - 1, columns: columns - 1)
    }

    func determinant() throws -> Double {
        guard rows == columns else { throw MathError.dimensionMismatch(reason: "Matrix must be square to calculate determinant.") }
        if rows == 1 { return self[0, 0] }
        if rows == 2 { return self[0, 0] * self[1, 1] - self[0, 1] * self[1, 0] }
        
        var det = 0.0
        for c in 0..<columns {
            let sign = (c % 2 == 0) ? 1.0 : -1.0
            det += sign * self[0, c] * (try submatrix(excludingRow: 0, excludingCol: c).determinant())
        }
        return det
    }

    func inverse() throws -> Matrix {
        let det = try determinant()
        guard det != 0 else { throw MathError.unsupportedOperation(op: "inverse", typeA: "Singular Matrix", typeB: nil) }
        
        if rows == 1 { return Matrix(values: [1.0 / det], rows: 1, columns: 1) }

        var cofactors: [Double] = []
        for r in 0..<rows {
            for c in 0..<columns {
                let sign = ((r + c) % 2 == 0) ? 1.0 : -1.0
                let subDet = try submatrix(excludingRow: r, excludingCol: c).determinant()
                cofactors.append(sign * subDet)
            }
        }
        
        let cofactorMatrix = Matrix(values: cofactors, rows: rows, columns: columns)
        var adjugateValues = [Double](repeating: 0.0, count: values.count)
        for r in 0..<rows {
            for c in 0..<columns {
                adjugateValues[c * rows + r] = cofactorMatrix[r, c]
            }
        }
        
        let inverseValues = adjugateValues.map { $0 / det }
        return Matrix(values: inverseValues, rows: rows, columns: columns)
    }
}

// NEW: Standalone factorial function
func factorial(_ n: Double) throws -> Double {
    guard n >= 0 && n.truncatingRemainder(dividingBy: 1) == 0 else {
        throw MathError.typeMismatch(expected: "non-negative integer", found: "number")
    }
    if n == 0 { return 1 }
    return (1...Int(n)).map(Double.init).reduce(1, *)
}


struct ComplexVector: Equatable {
    let values: [Complex]
    var dimension: Int { values.count }
    
    init(values: [Complex]) {
        self.values = values
    }
    
    init(from realVector: Vector) {
        self.values = realVector.values.map { Complex(real: $0, imaginary: 0) }
    }
    
    subscript(index: Int) -> Complex {
        return values[index]
    }
    
    static func + (lhs: ComplexVector, rhs: Complex) -> ComplexVector {
        return ComplexVector(values: lhs.values.map { $0 + rhs })
    }
    static func - (lhs: ComplexVector, rhs: Complex) -> ComplexVector {
        return ComplexVector(values: lhs.values.map { $0 - rhs })
    }
    static func * (lhs: ComplexVector, rhs: Complex) -> ComplexVector {
        return ComplexVector(values: lhs.values.map { $0 * rhs })
    }
    static func / (lhs: ComplexVector, rhs: Complex) throws -> ComplexVector {
        return try ComplexVector(values: lhs.values.map { try $0 / rhs })
    }
}

struct ComplexMatrix: Equatable {
    let values: [Complex]
    let rows: Int
    let columns: Int
    
    init(values: [Complex], rows: Int, columns: Int) {
        self.values = values
        self.rows = rows
        self.columns = columns
    }

    init(from realMatrix: Matrix) {
        self.values = realMatrix.values.map { Complex(real: $0, imaginary: 0) }
        self.rows = realMatrix.rows
        self.columns = realMatrix.columns
    }
    
    subscript(row: Int, col: Int) -> Complex {
        return values[row * columns + col]
    }
    
    static func + (lhs: ComplexMatrix, rhs: Complex) -> ComplexMatrix {
        return ComplexMatrix(values: lhs.values.map { $0 + rhs }, rows: lhs.rows, columns: lhs.columns)
    }
    static func - (lhs: ComplexMatrix, rhs: Complex) -> ComplexMatrix {
        return ComplexMatrix(values: lhs.values.map { $0 - rhs }, rows: lhs.rows, columns: lhs.columns)
    }
    static func * (lhs: ComplexMatrix, rhs: Complex) -> ComplexMatrix {
        return ComplexMatrix(values: lhs.values.map { $0 * rhs }, rows: lhs.rows, columns: lhs.columns)
    }
    static func / (lhs: ComplexMatrix, rhs: Complex) throws -> ComplexMatrix {
        return try ComplexMatrix(values: lhs.values.map { try $0 / rhs }, rows: lhs.rows, columns: lhs.columns)
    }
}

enum MathValue: Equatable {
    case scalar(Double)
    case complex(Complex)
    case vector(Vector)
    case matrix(Matrix)
    case tuple([MathValue])
    case functionDefinition(String)
    case complexVector(ComplexVector)
    case complexMatrix(ComplexMatrix)
    case polar(Complex)

    var typeName: String {
        switch self {
        case .scalar: return "Scalar"
        case .complex: return "Complex"
        case .vector: return "Vector"
        case .matrix: return "Matrix"
        case .tuple: return "Tuple"
        case .functionDefinition: return "FunctionDefinition"
        case .complexVector: return "ComplexVector"
        case .complexMatrix: return "ComplexMatrix"
        case .polar: return "Polar"
        }
    }
}

