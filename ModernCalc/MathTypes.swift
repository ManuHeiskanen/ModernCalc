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
    
    // Standard arithmetic operators for Complex numbers
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
        guard denominator != 0 else { throw MathError.divisionByZero } // Assumes MathError is accessible
        let real = (lhs.real * rhs.real + lhs.imaginary * rhs.imaginary) / denominator
        let imaginary = (lhs.imaginary * rhs.real - lhs.real * rhs.imaginary) / denominator
        return Complex(real: real, imaginary: imaginary)
    }
    
    /// Calculates the magnitude (absolute value) of the complex number.
    func abs() -> Double {
        return Foundation.sqrt(real * real + imaginary * imaginary)
    }

    /// Calculates the principal square root of the complex number.
    func sqrt() -> Complex {
        let r = self.abs()
        let newReal = Foundation.sqrt((r + real) / 2)
        let newImag = Foundation.sqrt((r - real) / 2)
        return Complex(real: newReal, imaginary: imaginary < 0 ? -newImag : newImag)
    }

    /// Raises the complex number to a complex power.
    func pow(_ exponent: Complex) throws -> Complex {
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
    
    subscript(index: Int) -> Double {
        return values[index]
    }
}

struct Matrix: Equatable {
    let values: [Double]
    let rows: Int
    let columns: Int
    
    subscript(row: Int, col: Int) -> Double {
        return values[row * columns + col]
    }
}

struct ComplexVector: Equatable {
    let values: [Complex]
    var dimension: Int { values.count }
    
    subscript(index: Int) -> Complex {
        return values[index]
    }
}

struct ComplexMatrix: Equatable {
    let values: [Complex]
    let rows: Int
    let columns: Int
    
    subscript(row: Int, col: Int) -> Complex {
        return values[row * columns + col]
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

    /// A computed property to get a string name for the type.
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
