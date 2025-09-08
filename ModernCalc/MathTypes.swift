//
//  MathTypes.swift
//  ModernCalc
//
//  Created by Manu Heiskanen on 29.8.2025.
//

import Foundation

// --- CORE DATA TYPES ---

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

struct Vector: Equatable, Codable {
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
    
    func hadamard(with other: Vector) throws -> Vector {
        guard self.dimension == other.dimension else { throw MathError.dimensionMismatch(reason: "Vectors must have same dimensions for element-wise multiplication.") }
        return Vector(values: zip(self.values, other.values).map(*))
    }

    func hadamardDivision(with other: Vector) throws -> Vector {
        guard self.dimension == other.dimension else { throw MathError.dimensionMismatch(reason: "Vectors must have same dimensions for element-wise division.") }
        guard !other.values.contains(0) else { throw MathError.divisionByZero }
        return Vector(values: zip(self.values, other.values).map(/))
    }
    
    func unit() -> Vector {
        let mag = self.magnitude()
        guard mag != 0 else { return self }
        return self / mag
    }

    func angle(with other: Vector) throws -> Double {
        let dotProduct = try self.dot(with: other)
        let mag1 = self.magnitude()
        let mag2 = other.magnitude()
        guard mag1 != 0, mag2 != 0 else { return 0 }
        let cosTheta = dotProduct / (mag1 * mag2)
        // Clamp the value to avoid domain errors from floating point inaccuracies
        return acos(Swift.min(Swift.max(cosTheta, -1.0), 1.0))
    }
    
    func modifying(at index: Int, with scalar: Double, operation: (Double, Double) -> Double) throws -> Vector {
        guard index >= 0 && index < self.dimension else {
            throw MathError.dimensionMismatch(reason: "Index \(index + 1) is out of bounds for vector of dimension \(self.dimension).")
        }
        var newValues = self.values
        newValues[index] = operation(newValues[index], scalar)
        return Vector(values: newValues)
    }

    // --- Vector-Vector Operators ---
    static func + (lhs: Vector, rhs: Vector) throws -> Vector {
        guard lhs.dimension == rhs.dimension else { throw MathError.dimensionMismatch(reason: "Vectors must have same dimensions for addition.") }
        return Vector(values: zip(lhs.values, rhs.values).map(+))
    }
    static func - (lhs: Vector, rhs: Vector) throws -> Vector {
        guard lhs.dimension == rhs.dimension else { throw MathError.dimensionMismatch(reason: "Vectors must have same dimensions for subtraction.") }
        return Vector(values: zip(lhs.values, rhs.values).map(-))
    }

    // --- Scalar-Vector Operators ---
    static func + (lhs: Vector, rhs: Double) -> Vector { Vector(values: lhs.values.map { $0 + rhs }) }
    static func + (lhs: Double, rhs: Vector) -> Vector { rhs + lhs }
    static func - (lhs: Vector, rhs: Double) -> Vector { Vector(values: lhs.values.map { $0 - rhs }) }
    static func - (lhs: Double, rhs: Vector) -> Vector { Vector(values: rhs.values.map { lhs - $0 }) }
    static func * (lhs: Vector, rhs: Double) -> Vector { Vector(values: lhs.values.map { $0 * rhs }) }
    static func * (lhs: Double, rhs: Vector) -> Vector { rhs * lhs }
    static func / (lhs: Vector, rhs: Double) -> Vector { Vector(values: lhs.values.map { $0 / rhs }) }
    static func / (lhs: Double, rhs: Vector) -> Vector { Vector(values: rhs.values.map { lhs / $0 }) }
    
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
        if dimension % 2 == 0 { return (sorted[dimension / 2 - 1] + sorted[dimension / 2]) / 2 }
        else { return sorted[dimension / 2] }
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
    
    func variance() -> Double? {
        guard dimension > 1 else { return nil }
        let mean = average()
        let sumOfSquaredDiffs = values.map { pow($0 - mean, 2.0) }.reduce(0, +)
        return sumOfSquaredDiffs / Double(dimension - 1)
    }

    func stddevp() -> Double? {
        guard dimension > 0 else { return nil }
        let mean = average()
        let sumOfSquaredDiffs = values.map { pow($0 - mean, 2.0) }.reduce(0, +)
        return Foundation.sqrt(sumOfSquaredDiffs / Double(dimension))
    }
}

struct Matrix: Equatable, Codable {
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
        let adjugateMatrix = cofactorMatrix.transpose()
        
        let inverseValues = adjugateMatrix.values.map { $0 / det }
        return Matrix(values: inverseValues, rows: rows, columns: columns)
    }

    func transpose() -> Matrix {
        var newValues = [Double](repeating: 0, count: values.count)
        for r in 0..<rows {
            for c in 0..<columns {
                newValues[c * rows + r] = self[r, c]
            }
        }
        return Matrix(values: newValues, rows: columns, columns: rows)
    }
    
    func trace() throws -> Double {
        guard rows == columns else { throw MathError.dimensionMismatch(reason: "Matrix must be square for trace.") }
        var sum = 0.0
        for i in 0..<rows {
            sum += self[i, i]
        }
        return sum
    }
    
    func hadamard(with other: Matrix) throws -> Matrix {
        guard self.rows == other.rows && self.columns == other.columns else { throw MathError.dimensionMismatch(reason: "Matrices must have same dimensions for element-wise multiplication.") }
        return Matrix(values: zip(self.values, other.values).map(*), rows: self.rows, columns: self.columns)
    }
    
    func hadamardDivision(with other: Matrix) throws -> Matrix {
        guard self.rows == other.rows && self.columns == other.columns else { throw MathError.dimensionMismatch(reason: "Matrices must have same dimensions for element-wise division.") }
        guard !other.values.contains(0) else { throw MathError.divisionByZero }
        return Matrix(values: zip(self.values, other.values).map(/), rows: self.rows, columns: self.columns)
    }
    
    // --- Matrix-Matrix Operators ---
    static func + (lhs: Matrix, rhs: Matrix) throws -> Matrix {
        guard lhs.rows == rhs.rows && lhs.columns == rhs.columns else {
            throw MathError.dimensionMismatch(reason: "Matrices must have same dimensions for +/-.")
        }
        return Matrix(values: zip(lhs.values, rhs.values).map(+), rows: lhs.rows, columns: lhs.columns)
    }

    static func - (lhs: Matrix, rhs: Matrix) throws -> Matrix {
        guard lhs.rows == rhs.rows && lhs.columns == rhs.columns else {
            throw MathError.dimensionMismatch(reason: "Matrices must have same dimensions for +/-.")
        }
        return Matrix(values: zip(lhs.values, rhs.values).map(-), rows: lhs.rows, columns: lhs.columns)
    }

    static func * (lhs: Matrix, rhs: Matrix) throws -> Matrix {
        guard lhs.columns == rhs.rows else {
            throw MathError.dimensionMismatch(reason: "For A*B, columns of A must equal rows of B.")
        }
        var newValues = [Double](repeating: 0, count: lhs.rows * rhs.columns)
        for i in 0..<lhs.rows {
            for j in 0..<rhs.columns {
                var sum = 0.0
                for k in 0..<lhs.columns {
                    sum += lhs[i, k] * rhs[k, j]
                }
                newValues[i * rhs.columns + j] = sum
            }
        }
        return Matrix(values: newValues, rows: lhs.rows, columns: rhs.columns)
    }

    // --- Matrix-Vector Multiplication ---
    static func * (lhs: Matrix, rhs: Vector) throws -> Vector {
        guard lhs.columns == rhs.dimension else {
            throw MathError.dimensionMismatch(reason: "For M*v, columns of M must equal dimension of v.")
        }
        var newValues = [Double](repeating: 0, count: lhs.rows)
        for i in 0..<lhs.rows {
            var sum = 0.0
            for j in 0..<lhs.columns {
                sum += lhs[i, j] * rhs[j]
            }
            newValues[i] = sum
        }
        return Vector(values: newValues)
    }
}

// --- Standalone Math Functions ---
func factorial(_ n: Double) throws -> Double {
    guard n >= 0 && n.truncatingRemainder(dividingBy: 1) == 0 else { throw MathError.typeMismatch(expected: "non-negative integer", found: "number") }
    if n == 0 { return 1 }
    return (1...Int(n)).map(Double.init).reduce(1, *)
}

func permutations(n: Double, k: Double) throws -> Double {
    guard n >= k && k >= 0 else { throw MathError.unsupportedOperation(op: "nPr", typeA: "n < k or k < 0", typeB: nil) }
    return try factorial(n) / factorial(n - k)
}

func combinations(n: Double, k: Double) throws -> Double {
    guard n >= k && k >= 0 else { throw MathError.unsupportedOperation(op: "nCr", typeA: "n < k or k < 0", typeB: nil) }
    return try permutations(n: n, k: k) / factorial(k)
}

struct ComplexVector: Equatable, Codable {
    let values: [Complex]
    var dimension: Int { values.count }
    
    init(values: [Complex]) { self.values = values }
    init(from realVector: Vector) { self.values = realVector.values.map { Complex(real: $0, imaginary: 0) } }
    
    subscript(index: Int) -> Complex { return values[index] }
    
    func dot(with other: ComplexVector) throws -> Complex {
        guard self.dimension == other.dimension else { throw MathError.dimensionMismatch(reason: "Complex vectors must have same dimensions for dot product.") }
        return zip(self.values, other.values).map { $0 * $1.conjugate() }.reduce(.zero, +)
    }
    
    func conjugateTranspose() -> ComplexMatrix {
        return ComplexMatrix(values: self.values.map { $0.conjugate() }, rows: 1, columns: self.dimension)
    }
    
    func modifying(at index: Int, with scalar: Complex, operation: (Complex, Complex) throws -> Complex) throws -> ComplexVector {
        guard index >= 0 && index < self.dimension else {
            throw MathError.dimensionMismatch(reason: "Index \(index + 1) is out of bounds for vector of dimension \(self.dimension).")
        }
        var newValues = self.values
        newValues[index] = try operation(newValues[index], scalar)
        return ComplexVector(values: newValues)
    }
    
    static func + (lhs: ComplexVector, rhs: ComplexVector) throws -> ComplexVector {
        guard lhs.dimension == rhs.dimension else { throw MathError.dimensionMismatch(reason: "Complex vectors must have same dimensions for addition.") }
        return ComplexVector(values: zip(lhs.values, rhs.values).map(+))
    }
    static func - (lhs: ComplexVector, rhs: ComplexVector) throws -> ComplexVector {
        guard lhs.dimension == rhs.dimension else { throw MathError.dimensionMismatch(reason: "Complex vectors must have same dimensions for subtraction.") }
        return ComplexVector(values: zip(lhs.values, rhs.values).map(-))
    }
    
    static func + (lhs: ComplexVector, rhs: Complex) -> ComplexVector { return ComplexVector(values: lhs.values.map { $0 + rhs }) }
    static func - (lhs: ComplexVector, rhs: Complex) -> ComplexVector { return ComplexVector(values: lhs.values.map { $0 - rhs }) }
    static func * (lhs: ComplexVector, rhs: Complex) -> ComplexVector { return ComplexVector(values: lhs.values.map { $0 * rhs }) }
    static func / (lhs: ComplexVector, rhs: Complex) throws -> ComplexVector { return try ComplexVector(values: lhs.values.map { try $0 / rhs }) }
}

struct ComplexMatrix: Equatable, Codable {
    let values: [Complex]
    let rows: Int
    let columns: Int
    
    init(values: [Complex], rows: Int, columns: Int) { self.values = values; self.rows = rows; self.columns = columns }
    init(from realMatrix: Matrix) { self.values = realMatrix.values.map { Complex(real: $0, imaginary: 0) }; self.rows = realMatrix.rows; self.columns = realMatrix.columns }
    
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
        return ComplexMatrix(values: newValues, rows: rows - 1, columns: columns - 1)
    }

    func determinant() throws -> Complex {
        guard rows == columns else { throw MathError.dimensionMismatch(reason: "Matrix must be square to calculate determinant.") }
        if rows == 1 { return self[0, 0] }
        if rows == 2 { return (self[0, 0] * self[1, 1]) - (self[0, 1] * self[1, 0]) }
        
        var det = Complex.zero
        for c in 0..<columns {
            let sign: Double = (c % 2 == 0) ? 1.0 : -1.0
            det = det + (Complex(real: sign, imaginary: 0) * self[0, c] * (try submatrix(excludingRow: 0, excludingCol: c).determinant()))
        }
        return det
    }

    func inverse() throws -> ComplexMatrix {
        let det = try determinant()
        guard det != .zero else { throw MathError.unsupportedOperation(op: "inverse", typeA: "Singular Complex Matrix", typeB: nil) }
        
        if rows == 1 { return ComplexMatrix(values: [try Complex(real: 1, imaginary: 0) / det], rows: 1, columns: 1) }

        var cofactors: [Complex] = []
        for r in 0..<rows {
            for c in 0..<columns {
                let sign: Double = ((r + c) % 2 == 0) ? 1.0 : -1.0
                let subDet = try submatrix(excludingRow: r, excludingCol: c).determinant()
                cofactors.append(Complex(real: sign, imaginary: 0) * subDet)
            }
        }
        
        let cofactorMatrix = ComplexMatrix(values: cofactors, rows: rows, columns: columns)
        let adjugateMatrix = cofactorMatrix.transpose()
        
        let inverseValues = try adjugateMatrix.values.map { try $0 / det }
        return ComplexMatrix(values: inverseValues, rows: rows, columns: columns)
    }
    
    func transpose() -> ComplexMatrix {
        var newValues = [Complex](repeating: .zero, count: values.count)
        for r in 0..<rows {
            for c in 0..<columns {
                newValues[c * rows + r] = self[r, c]
            }
        }
        return ComplexMatrix(values: newValues, rows: columns, columns: rows)
    }

    func conjugateTranspose() -> ComplexMatrix {
        let transposed = self.transpose()
        return ComplexMatrix(values: transposed.values.map { $0.conjugate() }, rows: transposed.rows, columns: transposed.columns)
    }
    
    func trace() throws -> Complex {
        guard rows == columns else { throw MathError.dimensionMismatch(reason: "Matrix must be square for trace.") }
        var sum = Complex.zero
        for i in 0..<rows {
            sum = sum + self[i, i]
        }
        return sum
    }
    
    static func + (lhs: ComplexMatrix, rhs: ComplexMatrix) throws -> ComplexMatrix {
        guard lhs.rows == rhs.rows && lhs.columns == rhs.columns else { throw MathError.dimensionMismatch(reason: "Complex matrices must have same dimensions for addition.") }
        return ComplexMatrix(values: zip(lhs.values, rhs.values).map(+), rows: lhs.rows, columns: lhs.columns)
    }
    static func - (lhs: ComplexMatrix, rhs: ComplexMatrix) throws -> ComplexMatrix {
        guard lhs.rows == rhs.rows && lhs.columns == rhs.columns else { throw MathError.dimensionMismatch(reason: "Complex matrices must have same dimensions for subtraction.") }
        return ComplexMatrix(values: zip(lhs.values, rhs.values).map(-), rows: lhs.rows, columns: lhs.columns)
    }
    static func * (lhs: ComplexMatrix, rhs: ComplexMatrix) throws -> ComplexMatrix {
        guard lhs.columns == rhs.rows else { throw MathError.dimensionMismatch(reason: "For A*B, columns of A must equal rows of B.") }
        var newValues = [Complex](repeating: .zero, count: lhs.rows * rhs.columns)
        for i in 0..<lhs.rows {
            for j in 0..<rhs.columns {
                var sum = Complex.zero
                for k in 0..<lhs.columns { sum = sum + (lhs[i, k] * rhs[k, j]) }
                newValues[i * rhs.columns + j] = sum
            }
        }
        return ComplexMatrix(values: newValues, rows: lhs.rows, columns: rhs.columns)
    }
    static func * (lhs: ComplexMatrix, rhs: ComplexVector) throws -> ComplexVector {
        guard lhs.columns == rhs.dimension else { throw MathError.dimensionMismatch(reason: "For M*v, columns of M must equal dimension of v.") }
        var newValues = [Complex](repeating: .zero, count: lhs.rows)
        for i in 0..<lhs.rows {
            var sum = Complex.zero
            for j in 0..<lhs.columns { sum = sum + (lhs[i, j] * rhs[j]) }
            newValues[i] = sum
        }
        return ComplexVector(values: newValues)
    }
    
    static func + (lhs: ComplexMatrix, rhs: Complex) -> ComplexMatrix { return ComplexMatrix(values: lhs.values.map { $0 + rhs }, rows: lhs.rows, columns: lhs.columns) }
    static func - (lhs: ComplexMatrix, rhs: Complex) -> ComplexMatrix { return ComplexMatrix(values: lhs.values.map { $0 - rhs }, rows: lhs.rows, columns: lhs.columns) }
    static func * (lhs: ComplexMatrix, rhs: Complex) -> ComplexMatrix { return ComplexMatrix(values: lhs.values.map { $0 * rhs }, rows: lhs.rows, columns: lhs.columns) }
    static func / (lhs: ComplexMatrix, rhs: Complex) throws -> ComplexMatrix { return try ComplexMatrix(values: lhs.values.map { try $0 / rhs }, rows: lhs.rows, columns: lhs.columns) }
}

enum MathValue: Codable {
    case scalar(Double); case complex(Complex); case vector(Vector); case matrix(Matrix); case tuple([MathValue]); case functionDefinition(String); case complexVector(ComplexVector); case complexMatrix(ComplexMatrix); case polar(Complex)
    case regressionResult(slope: Double, intercept: Double)
    case plot(PlotData) // New case for holding plot data

    var typeName: String {
        switch self {
        case .scalar: return "Scalar"; case .complex: return "Complex"; case .vector: return "Vector"; case .matrix: return "Matrix"; case .tuple: return "Tuple"; case .functionDefinition: return "FunctionDefinition"; case .complexVector: return "ComplexVector"; case .complexMatrix: return "ComplexMatrix"; case .polar: return "Polar"; case .regressionResult: return "RegressionResult"; case .plot: return "Plot"
        }
    }
    
    enum CodingKeys: String, CodingKey { case type, value, slope, intercept, plotData }

    func encode(to encoder: Encoder) throws {
        var container = encoder.container(keyedBy: CodingKeys.self)
        switch self {
        case .scalar(let d): try container.encode("scalar", forKey: .type); try container.encode(d, forKey: .value)
        case .complex(let c): try container.encode("complex", forKey: .type); try container.encode(c, forKey: .value)
        case .vector(let v): try container.encode("vector", forKey: .type); try container.encode(v, forKey: .value)
        case .matrix(let m): try container.encode("matrix", forKey: .type); try container.encode(m, forKey: .value)
        case .tuple(let t): try container.encode("tuple", forKey: .type); try container.encode(t, forKey: .value)
        case .functionDefinition(let f): try container.encode("functionDefinition", forKey: .type); try container.encode(f, forKey: .value)
        case .complexVector(let cv): try container.encode("complexVector", forKey: .type); try container.encode(cv, forKey: .value)
        case .complexMatrix(let cm): try container.encode("complexMatrix", forKey: .type); try container.encode(cm, forKey: .value)
        case .polar(let p): try container.encode("polar", forKey: .type); try container.encode(p, forKey: .value)
        case .regressionResult(let slope, let intercept):
            try container.encode("regressionResult", forKey: .type)
            try container.encode(slope, forKey: .slope)
            try container.encode(intercept, forKey: .intercept)
        case .plot:
            // Plot data is not meant to be saved, so we encode a placeholder.
            try container.encode("plot", forKey: .type)
        }
    }

    init(from decoder: Decoder) throws {
        let container = try decoder.container(keyedBy: CodingKeys.self)
        let type = try container.decode(String.self, forKey: .type)
        switch type {
        case "scalar": self = .scalar(try container.decode(Double.self, forKey: .value))
        case "complex": self = .complex(try container.decode(Complex.self, forKey: .value))
        case "vector": self = .vector(try container.decode(Vector.self, forKey: .value))
        case "matrix": self = .matrix(try container.decode(Matrix.self, forKey: .value))
        case "tuple": self = .tuple(try container.decode([MathValue].self, forKey: .value))
        case "functionDefinition": self = .functionDefinition(try container.decode(String.self, forKey: .value))
        case "complexVector": self = .complexVector(try container.decode(ComplexVector.self, forKey: .value))
        case "complexMatrix": self = .complexMatrix(try container.decode(ComplexMatrix.self, forKey: .value))
        case "polar": self = .polar(try container.decode(Complex.self, forKey: .value))
        case "regressionResult":
            let slope = try container.decode(Double.self, forKey: .slope)
            let intercept = try container.decode(Double.self, forKey: .intercept)
            self = .regressionResult(slope: slope, intercept: intercept)
        case "plot":
            // This should not happen from saved data, but we need to handle it.
            // We'll create an empty plot as a fallback.
            self = .plot(PlotData(expression: "Empty", series: [], plotType: .line))
        default: throw DecodingError.dataCorruptedError(forKey: .type, in: container, debugDescription: "Invalid MathValue type '\(type)'")
        }
    }
    
    // Plots cannot be equated since they contain unique IDs.
    static func == (lhs: MathValue, rhs: MathValue) -> Bool {
        switch (lhs, rhs) {
        case (.scalar(let a), .scalar(let b)): return a == b
        case (.complex(let a), .complex(let b)): return a == b
        case (.vector(let a), .vector(let b)): return a == b
        // ... other cases
        case (.plot, .plot): return false // Or compare based on content if needed
        default: return false
        }
    }
}

extension MathValue {
    func asScalar() throws -> Double {
        guard case .scalar(let s) = self else {
            throw MathError.typeMismatch(expected: "Scalar", found: self.typeName)
        }
        return s
    }
}

