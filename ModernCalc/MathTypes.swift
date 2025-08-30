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
    
    // NEW: Operators for element-wise operations with a Complex number
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
    
    // NEW: Operators for element-wise operations with a Complex number
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
        }
    }
}

