import Foundation

// --- NEW: A struct to represent a value with its associated physical dimensions. ---
struct UnitValue: Equatable, Codable {
    var value: Double
    var dimensions: UnitDimension
    var preferredDisplayUnit: String? = nil

    // A dimensionless value, for convenience.
    static func dimensionless(_ value: Double) -> UnitValue {
        return UnitValue(value: value, dimensions: [:])
    }

    // --- Operator Overloads for Dimensional Analysis ---

    static func + (lhs: UnitValue, rhs: UnitValue) throws -> UnitValue {
        guard lhs.dimensions == rhs.dimensions else {
            throw MathError.dimensionMismatch(reason: "Cannot add quantities with different units.")
        }
        return UnitValue(value: lhs.value + rhs.value, dimensions: lhs.dimensions)
    }

    static func - (lhs: UnitValue, rhs: UnitValue) throws -> UnitValue {
        guard lhs.dimensions == rhs.dimensions else {
            throw MathError.dimensionMismatch(reason: "Cannot subtract quantities with different units.")
        }
        return UnitValue(value: lhs.value - rhs.value, dimensions: lhs.dimensions)
    }
    
    static func * (lhs: UnitValue, rhs: UnitValue) -> UnitValue {
        let newValue = lhs.value * rhs.value
        let newDimensions = lhs.dimensions.merging(rhs.dimensions, uniquingKeysWith: +)
            .filter { $0.value != 0 } // Remove dimensions that cancel out
        return UnitValue(value: newValue, dimensions: newDimensions)
    }

    static func / (lhs: UnitValue, rhs: UnitValue) throws -> UnitValue {
        guard rhs.value != 0 else { throw MathError.divisionByZero }
        let newValue = lhs.value / rhs.value
        let negatedRhsDimensions = rhs.dimensions.mapValues { -$0 }
        let newDimensions = lhs.dimensions.merging(negatedRhsDimensions, uniquingKeysWith: +)
            .filter { $0.value != 0 }
        return UnitValue(value: newValue, dimensions: newDimensions)
    }
    
    func pow(_ exponent: Double) -> UnitValue {
        let newValue = Foundation.pow(self.value, exponent)
        let newDimensions = self.dimensions.mapValues { Int(Double($0) * exponent) }
            .filter { $0.value != 0 }
        return UnitValue(value: newValue, dimensions: newDimensions)
    }
}


// --- CORE DATA TYPES ---

// ADVANCED MODEL: A struct to represent a value with its associated uncertainties,
// separating random (statistical) and systematic components.
struct UncertainValue: Equatable, Codable {
    var value: Double
    var randomUncertainty: Double // Statistical, Type A
    var systematicUncertainty: Double // Systematic, Type B

    // A computed property for the total combined uncertainty.
    // Random and systematic uncertainties are combined in quadrature.
    var totalUncertainty: Double {
        return Foundation.sqrt(Foundation.pow(randomUncertainty, 2) + Foundation.pow(systematicUncertainty, 2))
    }

    // --- Operator Overloads with Advanced Propagation ---

    // For addition and subtraction:
    // - Random uncertainties are combined in quadrature.
    // - Systematic uncertainties are added linearly (worst-case scenario).
    static func + (lhs: UncertainValue, rhs: UncertainValue) -> UncertainValue {
        let newValue = lhs.value + rhs.value
        let newRandom = Foundation.sqrt(Foundation.pow(lhs.randomUncertainty, 2) + Foundation.pow(rhs.randomUncertainty, 2))
        let newSystematic = lhs.systematicUncertainty + rhs.systematicUncertainty
        return UncertainValue(value: newValue, randomUncertainty: newRandom, systematicUncertainty: newSystematic)
    }

    static func - (lhs: UncertainValue, rhs: UncertainValue) -> UncertainValue {
        let newValue = lhs.value - rhs.value
        let newRandom = Foundation.sqrt(Foundation.pow(lhs.randomUncertainty, 2) + Foundation.pow(rhs.randomUncertainty, 2))
        let newSystematic = lhs.systematicUncertainty + rhs.systematicUncertainty
        return UncertainValue(value: newValue, randomUncertainty: newRandom, systematicUncertainty: newSystematic)
    }
    
    // For multiplication and division, we combine the *relative* uncertainties.
    static func * (lhs: UncertainValue, rhs: UncertainValue) -> UncertainValue {
        let newValue = lhs.value * rhs.value
        
        // Handle cases where a value is zero to avoid division by zero
        if lhs.value == 0 || rhs.value == 0 {
            let unc_A_due_to_lhs = rhs.value * lhs.randomUncertainty
            let unc_A_due_to_rhs = lhs.value * rhs.randomUncertainty
            let newRandom = Foundation.sqrt(Foundation.pow(unc_A_due_to_lhs, 2) + Foundation.pow(unc_A_due_to_rhs, 2))
            
            let unc_B_due_to_lhs = abs(rhs.value * lhs.systematicUncertainty)
            let unc_B_due_to_rhs = abs(lhs.value * rhs.systematicUncertainty)
            let newSystematic = unc_B_due_to_lhs + unc_B_due_to_rhs

            return UncertainValue(value: newValue, randomUncertainty: newRandom, systematicUncertainty: newSystematic)
        }

        let relRandomL = lhs.randomUncertainty / lhs.value
        let relRandomR = rhs.randomUncertainty / rhs.value
        let combinedRelRandom = Foundation.sqrt(Foundation.pow(relRandomL, 2) + Foundation.pow(relRandomR, 2))
        
        let relSystematicL = lhs.systematicUncertainty / lhs.value
        let relSystematicR = rhs.systematicUncertainty / rhs.value
        let combinedRelSystematic = abs(relSystematicL) + abs(relSystematicR)

        return UncertainValue(
            value: newValue,
            randomUncertainty: abs(newValue * combinedRelRandom),
            systematicUncertainty: abs(newValue * combinedRelSystematic)
        )
    }

    static func / (lhs: UncertainValue, rhs: UncertainValue) throws -> UncertainValue {
        guard rhs.value != 0 else { throw MathError.divisionByZero }
        let newValue = lhs.value / rhs.value
        
        if lhs.value == 0 {
            let newRandom = abs(newValue / rhs.value) * rhs.randomUncertainty
            let newSystematic = abs(newValue / rhs.value) * rhs.systematicUncertainty
            return UncertainValue(value: newValue, randomUncertainty: newRandom, systematicUncertainty: newSystematic)
        }

        let relRandomL = lhs.randomUncertainty / lhs.value
        let relRandomR = rhs.randomUncertainty / rhs.value
        let combinedRelRandom = Foundation.sqrt(Foundation.pow(relRandomL, 2) + Foundation.pow(relRandomR, 2))
        
        let relSystematicL = lhs.systematicUncertainty / lhs.value
        let relSystematicR = rhs.systematicUncertainty / rhs.value
        let combinedRelSystematic = abs(relSystematicL) + abs(relSystematicR)

        return UncertainValue(
            value: newValue,
            randomUncertainty: abs(newValue * combinedRelRandom),
            systematicUncertainty: abs(newValue * combinedRelSystematic)
        )
    }
    
    // Power propagation: u_f = |f * n * (u_x / x)|
    func pow(_ exponent: Double) -> UncertainValue {
        guard self.value != 0 else { return self }
        let newValue = Foundation.pow(self.value, exponent)
        let relativeUncertainty = self.totalUncertainty / self.value
        let newTotalUncertainty = abs(newValue * exponent * relativeUncertainty)
        
        // We'll assume the ratio of random to systematic uncertainty remains the same.
        // A more rigorous treatment might be needed for complex cases, but this is a reasonable approximation.
        let randomRatio = self.totalUncertainty > 0 ? self.randomUncertainty / self.totalUncertainty : 0
        
        return UncertainValue(
            value: newValue,
            randomUncertainty: newTotalUncertainty * randomRatio,
            systematicUncertainty: newTotalUncertainty * (1 - randomRatio)
        )
    }
    
    // Unary minus flips the value but leaves uncertainties as positive values.
    static prefix func - (operand: UncertainValue) -> UncertainValue {
        return UncertainValue(value: -operand.value, randomUncertainty: operand.randomUncertainty, systematicUncertainty: operand.systematicUncertainty)
    }
    
    // General function propagation: u_f = |f'(x)| * u_x
    // This is applied to both random and systematic components.
    func propagate(derivative: Double) -> UncertainValue {
        return UncertainValue(
            value: self.value,
            randomUncertainty: abs(derivative * self.randomUncertainty),
            systematicUncertainty: abs(derivative * self.systematicUncertainty)
        )
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

// --- MODIFIED: Vector is now unit-aware ---
struct Vector: Equatable, Codable {
    let values: [Double]
    let dimensions: UnitDimension // Added
    var dimension: Int { values.count }
    
    init(values: [Double], dimensions: UnitDimension = [:]) {
        self.values = values
        self.dimensions = dimensions // Added
    }
    
    subscript(index: Int) -> Double {
        return values[index]
    }
    
    // --- Vector Operations (Now Unit-Aware) ---
    func dot(with other: Vector) throws -> UnitValue {
        guard self.dimension == other.dimension else {
            throw MathError.dimensionMismatch(reason: "Vectors must have the same dimension for dot product.")
        }
        let resultValue = zip(self.values, other.values).map(*).reduce(0, +)
        let newDimensions = self.dimensions.merging(other.dimensions, uniquingKeysWith: +).filter { $0.value != 0 }
        return UnitValue(value: resultValue, dimensions: newDimensions)
    }
    
    func cross(with other: Vector) throws -> Vector {
        guard self.dimension == 3 && other.dimension == 3 else {
            throw MathError.dimensionMismatch(reason: "Cross product is only defined for 3D vectors.")
        }
        let u = self.values; let v = other.values
        let newValues = [u[1] * v[2] - u[2] * v[1], u[2] * v[0] - u[0] * v[2], u[0] * v[1] - u[1] * v[0]]
        let newDimensions = self.dimensions.merging(other.dimensions, uniquingKeysWith: +).filter { $0.value != 0 }
        return Vector(values: newValues, dimensions: newDimensions)
    }
    
    func hadamard(with other: Vector) throws -> Vector {
        guard self.dimension == other.dimension else { throw MathError.dimensionMismatch(reason: "Vectors must have same dimensions for element-wise multiplication.") }
        let newDimensions = self.dimensions.merging(other.dimensions, uniquingKeysWith: +).filter { $0.value != 0 }
        return Vector(values: zip(self.values, other.values).map(*), dimensions: newDimensions)
    }

    func hadamardDivision(with other: Vector) throws -> Vector {
        guard self.dimension == other.dimension else { throw MathError.dimensionMismatch(reason: "Vectors must have same dimensions for element-wise division.") }
        guard !other.values.contains(0) else { throw MathError.divisionByZero }
        let negatedRhsDimensions = other.dimensions.mapValues { -$0 }
        let newDimensions = self.dimensions.merging(negatedRhsDimensions, uniquingKeysWith: +).filter { $0.value != 0 }
        return Vector(values: zip(self.values, other.values).map(/), dimensions: newDimensions)
    }
    
    func unit() -> Vector {
        let mag = self.magnitude().value
        guard mag != 0 else { return self }
        // A unit vector is dimensionless by definition.
        return Vector(values: self.values.map { $0 / mag }, dimensions: [:])
    }

    func angle(with other: Vector) throws -> Double {
        let dotProduct = try self.dot(with: other).value
        let mag1 = self.magnitude().value
        let mag2 = other.magnitude().value
        guard mag1 != 0, mag2 != 0 else { return 0 }
        let cosTheta = dotProduct / (mag1 * mag2)
        return acos(Swift.min(Swift.max(cosTheta, -1.0), 1.0))
    }
    
    func modifying(at index: Int, with scalar: Double, operation: (Double, Double) -> Double) throws -> Vector {
        guard index >= 0 && index < self.dimension else {
            throw MathError.dimensionMismatch(reason: "Index \(index + 1) is out of bounds for vector of dimension \(self.dimension).")
        }
        var newValues = self.values
        newValues[index] = operation(newValues[index], scalar)
        return Vector(values: newValues, dimensions: self.dimensions)
    }

    // --- Vector-Vector Operators ---
    static func + (lhs: Vector, rhs: Vector) throws -> Vector {
        guard lhs.dimension == rhs.dimension else { throw MathError.dimensionMismatch(reason: "Vectors must have same dimensions for addition.") }
        guard lhs.dimensions == rhs.dimensions else { throw MathError.dimensionMismatch(reason: "Vectors must have same units for addition.") }
        return Vector(values: zip(lhs.values, rhs.values).map(+), dimensions: lhs.dimensions)
    }
    static func - (lhs: Vector, rhs: Vector) throws -> Vector {
        guard lhs.dimension == rhs.dimension else { throw MathError.dimensionMismatch(reason: "Vectors must have same dimensions for subtraction.") }
        guard lhs.dimensions == rhs.dimensions else { throw MathError.dimensionMismatch(reason: "Vectors must have same units for subtraction.") }
        return Vector(values: zip(lhs.values, rhs.values).map(-), dimensions: lhs.dimensions)
    }

    // Outer product
    static func * (lhs: Vector, rhs: Vector) -> Matrix {
        let newValues = [Double](repeating: 0, count: lhs.dimension * rhs.dimension).enumerated().map { (idx, _) -> Double in
            let i = idx / rhs.dimension
            let j = idx % rhs.dimension
            return lhs[i] * rhs[j]
        }
        let newDimensions = lhs.dimensions.merging(rhs.dimensions, uniquingKeysWith: +).filter { $0.value != 0 }
        return Matrix(values: newValues, rows: lhs.dimension, columns: rhs.dimension, dimensions: newDimensions)
    }

    // --- Scalar-Vector Operators ---
    static func + (lhs: Vector, rhs: Double) -> Vector { Vector(values: lhs.values.map { $0 + rhs }, dimensions: lhs.dimensions) }
    static func + (lhs: Double, rhs: Vector) -> Vector { rhs + lhs }
    static func - (lhs: Vector, rhs: Double) -> Vector { Vector(values: lhs.values.map { $0 - rhs }, dimensions: lhs.dimensions) }
    static func - (lhs: Double, rhs: Vector) -> Vector { Vector(values: rhs.values.map { lhs - $0 }, dimensions: rhs.dimensions) }
    static func * (lhs: Vector, rhs: Double) -> Vector { Vector(values: lhs.values.map { $0 * rhs }, dimensions: lhs.dimensions) }
    static func * (lhs: Double, rhs: Vector) -> Vector { rhs * lhs }
    static func / (lhs: Vector, rhs: Double) -> Vector { Vector(values: lhs.values.map { $0 / rhs }, dimensions: lhs.dimensions) }
    static func / (lhs: Double, rhs: Vector) -> Vector { Vector(values: rhs.values.map { lhs / $0 }, dimensions: rhs.dimensions.mapValues { -$0 }) }
    
    // --- Vector-Matrix Multiplication (Row Vector * Matrix) ---
    static func * (lhs: Vector, rhs: Matrix) throws -> Vector {
        guard lhs.dimension == rhs.rows else {
            throw MathError.dimensionMismatch(reason: "For v*M, dimension of v must equal rows of M.")
        }
        var newValues = [Double](repeating: 0, count: rhs.columns)
        for j in 0..<rhs.columns {
            newValues[j] = (0..<lhs.dimension).map { i in lhs[i] * rhs[i, j] }.reduce(0, +)
        }
        let newDimensions = lhs.dimensions.merging(rhs.dimensions, uniquingKeysWith: +).filter { $0.value != 0 }
        return Vector(values: newValues, dimensions: newDimensions)
    }
    
    // --- Statistical Helpers (Now Returning UnitValue) ---
    func sum() -> UnitValue { UnitValue(value: values.reduce(0, +), dimensions: self.dimensions) }
    func average() -> UnitValue {
        guard !values.isEmpty else { return UnitValue(value: 0, dimensions: self.dimensions) }
        return UnitValue(value: sum().value / Double(dimension), dimensions: self.dimensions)
    }
    func min() -> UnitValue? { values.min().map { UnitValue(value: $0, dimensions: self.dimensions) } }
    func max() -> UnitValue? { values.max().map { UnitValue(value: $0, dimensions: self.dimensions) } }
    func median() -> UnitValue? {
        guard !values.isEmpty else { return nil }
        let sorted = values.sorted()
        let medianValue = (dimension % 2 == 0) ? (sorted[dimension / 2 - 1] + sorted[dimension / 2]) / 2 : sorted[dimension / 2]
        return UnitValue(value: medianValue, dimensions: self.dimensions)
    }
    func stddev() -> UnitValue? {
        guard dimension > 1 else { return nil }
        let mean = average().value
        let sumOfSquaredDiffs = values.map { Foundation.pow($0 - mean, 2.0) }.reduce(0, +)
        return UnitValue(value: Foundation.sqrt(sumOfSquaredDiffs / Double(dimension - 1)), dimensions: self.dimensions)
    }
    func magnitude() -> UnitValue {
        let magValue = Foundation.sqrt(values.map { $0 * $0 }.reduce(0, +))
        return UnitValue(value: magValue, dimensions: self.dimensions)
    }
    
    func variance() -> UnitValue? {
        guard dimension > 1 else { return nil }
        let mean = average().value
        let sumOfSquaredDiffs = values.map { Foundation.pow($0 - mean, 2.0) }.reduce(0, +)
        let newDimensions = self.dimensions.mapValues { $0 * 2 }.filter { $0.value != 0 }
        return UnitValue(value: sumOfSquaredDiffs / Double(dimension - 1), dimensions: newDimensions)
    }

    func stddevp() -> UnitValue? {
        guard dimension > 0 else { return nil }
        let mean = average().value
        let sumOfSquaredDiffs = values.map { Foundation.pow($0 - mean, 2.0) }.reduce(0, +)
        return UnitValue(value: Foundation.sqrt(sumOfSquaredDiffs / Double(dimension)), dimensions: self.dimensions)
    }
}

// --- MODIFIED: Matrix is now unit-aware ---
struct Matrix: Equatable, Codable {
    let values: [Double]
    let rows: Int
    let columns: Int
    let dimensions: UnitDimension // Added
    
    init(values: [Double], rows: Int, columns: Int, dimensions: UnitDimension = [:]) {
        self.values = values
        self.rows = rows
        self.columns = columns
        self.dimensions = dimensions // Added
    }
    
    subscript(row: Int, col: Int) -> Double {
        return values[row * columns + col]
    }
    
    // --- Matrix Operations (Now Unit-Aware) ---
    func submatrix(excludingRow: Int, excludingCol: Int) -> Matrix {
        var newValues: [Double] = []
        for r in 0..<rows {
            guard r != excludingRow else { continue }
            for c in 0..<columns {
                guard c != excludingCol else { continue }
                newValues.append(self[r, c])
            }
        }
        return Matrix(values: newValues, rows: rows - 1, columns: columns - 1, dimensions: self.dimensions)
    }

    func determinant() throws -> UnitValue {
        guard rows == columns else { throw MathError.dimensionMismatch(reason: "Matrix must be square to calculate determinant.") }
        let detValue: Double
        if rows == 1 { detValue = self[0, 0] }
        else if rows == 2 { detValue = self[0, 0] * self[1, 1] - self[0, 1] * self[1, 0] }
        else {
            detValue = try (0..<columns).map { c -> Double in
                let sign = (c % 2 == 0) ? 1.0 : -1.0
                return sign * self[0, c] * (try submatrix(excludingRow: 0, excludingCol: c).determinant().value)
            }.reduce(0, +)
        }
        let newDimensions = self.dimensions.mapValues { $0 * self.rows }.filter { $0.value != 0 }
        return UnitValue(value: detValue, dimensions: newDimensions)
    }

    func inverse() throws -> Matrix {
        let det = try determinant()
        guard det.value != 0 else { throw MathError.unsupportedOperation(op: "inverse", typeA: "Singular Matrix", typeB: nil) }
        
        let adjugate: Matrix
        if rows == 1 {
            adjugate = Matrix(values: [1.0], rows: 1, columns: 1, dimensions: [:])
        } else {
            let cofactors: [Double] = try (0..<rows).flatMap { r -> [Double] in
                try (0..<columns).map { c -> Double in
                    let sign = ((r + c) % 2 == 0) ? 1.0 : -1.0
                    return sign * (try submatrix(excludingRow: r, excludingCol: c).determinant().value)
                }
            }
            adjugate = Matrix(values: cofactors, rows: rows, columns: columns, dimensions: self.dimensions.mapValues { $0 * (self.rows - 1) }).transpose()
        }

        let inverseValues = adjugate.values.map { $0 / det.value }
        let inverseDimensions = self.dimensions.mapValues { -$0 }.filter { $0.value != 0 }
        return Matrix(values: inverseValues, rows: rows, columns: columns, dimensions: inverseDimensions)
    }

    func transpose() -> Matrix {
        var newValues = [Double](repeating: 0, count: values.count)
        for r in 0..<rows {
            for c in 0..<columns {
                newValues[c * rows + r] = self[r, c]
            }
        }
        return Matrix(values: newValues, rows: columns, columns: rows, dimensions: self.dimensions)
    }
    
    func trace() throws -> UnitValue {
        guard rows == columns else { throw MathError.dimensionMismatch(reason: "Matrix must be square for trace.") }
        let traceValue = (0..<rows).map { self[$0, $0] }.reduce(0, +)
        return UnitValue(value: traceValue, dimensions: self.dimensions)
    }
    
    func frobeniusNorm() -> UnitValue {
        let normValue = sqrt(self.values.map { $0 * $0 }.reduce(0, +))
        return UnitValue(value: normValue, dimensions: self.dimensions)
    }

    func rank() -> Int {
        var matrix = self.values.chunks(ofCount: self.columns).map { Array($0) }
        var rank = 0; var pivotRow = 0
        let rowCount = self.rows; let colCount = self.columns
        for j in 0..<colCount {
            if pivotRow >= rowCount { break }
            var i = pivotRow
            while i < rowCount && abs(matrix[i][j]) < 1e-10 { i += 1 }
            if i < rowCount {
                matrix.swapAt(pivotRow, i)
                let pivotValue = matrix[pivotRow][j]
                for k in 0..<colCount { matrix[pivotRow][k] /= pivotValue }
                for i in 0..<rowCount {
                    if i != pivotRow {
                        let factor = matrix[i][j]
                        for k in 0..<colCount { matrix[i][k] -= factor * matrix[pivotRow][k] }
                    }
                }
                pivotRow += 1; rank += 1
            }
        }
        return rank
    }

    func hadamard(with other: Matrix) throws -> Matrix {
        guard self.rows == other.rows && self.columns == other.columns else { throw MathError.dimensionMismatch(reason: "Matrices must have same dimensions for element-wise multiplication.") }
        let newDimensions = self.dimensions.merging(other.dimensions, uniquingKeysWith: +).filter { $0.value != 0 }
        return Matrix(values: zip(self.values, other.values).map(*), rows: self.rows, columns: self.columns, dimensions: newDimensions)
    }
    
    func hadamardDivision(with other: Matrix) throws -> Matrix {
        guard self.rows == other.rows && self.columns == other.columns else { throw MathError.dimensionMismatch(reason: "Matrices must have same dimensions for element-wise division.") }
        guard !other.values.contains(0) else { throw MathError.divisionByZero }
        let negatedRhsDimensions = other.dimensions.mapValues { -$0 }
        let newDimensions = self.dimensions.merging(negatedRhsDimensions, uniquingKeysWith: +).filter { $0.value != 0 }
        return Matrix(values: zip(self.values, other.values).map(/), rows: self.rows, columns: self.columns, dimensions: newDimensions)
    }
    
    func getcolumn(index: Int) throws -> Vector {
        let zeroBasedIndex = index - 1
        guard zeroBasedIndex >= 0 && zeroBasedIndex < columns else {
            throw MathError.dimensionMismatch(reason: "Column index \(index) is out of bounds for a matrix with \(columns) columns.")
        }
        let columnValues = (0..<rows).map { self[$0, zeroBasedIndex] }
        return Vector(values: columnValues, dimensions: self.dimensions)
    }
    
    func getrow(index: Int) throws -> Vector {
        let zeroBasedIndex = index - 1
        guard zeroBasedIndex >= 0 && zeroBasedIndex < rows else {
            throw MathError.dimensionMismatch(reason: "Row index \(index) is out of bounds for a matrix with \(rows) rows.")
        }
        let rowValues = (0..<columns).map { self[zeroBasedIndex, $0] }
        return Vector(values: rowValues, dimensions: self.dimensions)
    }
    
    // --- Matrix-Matrix Operators ---
    static func + (lhs: Matrix, rhs: Matrix) throws -> Matrix {
        guard lhs.rows == rhs.rows && lhs.columns == rhs.columns else { throw MathError.dimensionMismatch(reason: "Matrices must have same dimensions for +/-.") }
        guard lhs.dimensions == rhs.dimensions else { throw MathError.dimensionMismatch(reason: "Matrices must have the same units for addition.") }
        return Matrix(values: zip(lhs.values, rhs.values).map(+), rows: lhs.rows, columns: lhs.columns, dimensions: lhs.dimensions)
    }

    static func - (lhs: Matrix, rhs: Matrix) throws -> Matrix {
        guard lhs.rows == rhs.rows && lhs.columns == rhs.columns else { throw MathError.dimensionMismatch(reason: "Matrices must have same dimensions for +/-.") }
        guard lhs.dimensions == rhs.dimensions else { throw MathError.dimensionMismatch(reason: "Matrices must have the same units for subtraction.") }
        return Matrix(values: zip(lhs.values, rhs.values).map(-), rows: lhs.rows, columns: lhs.columns, dimensions: lhs.dimensions)
    }

    static func * (lhs: Matrix, rhs: Matrix) throws -> Matrix {
        guard lhs.columns == rhs.rows else { throw MathError.dimensionMismatch(reason: "For A*B, columns of A must equal rows of B.") }
        var newValues = [Double](repeating: 0, count: lhs.rows * rhs.columns)
        for i in 0..<lhs.rows { for j in 0..<rhs.columns { newValues[i * rhs.columns + j] = (0..<lhs.columns).map { k in lhs[i, k] * rhs[k, j] }.reduce(0, +) } }
        let newDimensions = lhs.dimensions.merging(rhs.dimensions, uniquingKeysWith: +).filter { $0.value != 0 }
        return Matrix(values: newValues, rows: lhs.rows, columns: rhs.columns, dimensions: newDimensions)
    }

    // --- Matrix-Vector Multiplication ---
    static func * (lhs: Matrix, rhs: Vector) throws -> Vector {
        guard lhs.columns == rhs.dimension else {
            throw MathError.dimensionMismatch(reason: "For M*v, columns of M must equal dimension of v.")
        }
        let newValues = (0..<lhs.rows).map { i in (0..<lhs.columns).map { j in lhs[i, j] * rhs[j] }.reduce(0, +) }
        let newDimensions = lhs.dimensions.merging(rhs.dimensions, uniquingKeysWith: +).filter { $0.value != 0 }
        return Vector(values: newValues, dimensions: newDimensions)
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

// --- MODIFIED: ComplexVector is now unit-aware ---
struct ComplexVector: Equatable, Codable {
    let values: [Complex]
    let dimensions: UnitDimension // Added
    var dimension: Int { values.count }
    
    init(values: [Complex], dimensions: UnitDimension = [:]) { self.values = values; self.dimensions = dimensions }
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
    
    // Outer Product
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

// --- MODIFIED: ComplexMatrix is now unit-aware ---
struct ComplexMatrix: Equatable, Codable {
    let values: [Complex]
    let rows: Int
    let columns: Int
    let dimensions: UnitDimension // Added
    
    init(values: [Complex], rows: Int, columns: Int, dimensions: UnitDimension = [:]) { self.values = values; self.rows = rows; self.columns = columns; self.dimensions = dimensions }
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
        let newDimensions = self.dimensions.mapValues { $0 * self.rows }.filter { $0.value != 0 }
        return (detValue, newDimensions)
    }

    func inverse() throws -> ComplexMatrix {
        let (detValue, _) = try determinant()
        guard detValue != .zero else { throw MathError.unsupportedOperation(op: "inverse", typeA: "Singular Complex Matrix", typeB: nil) }
        
        let adjugate: ComplexMatrix
        if rows == 1 {
            adjugate = ComplexMatrix(values: [Complex(real: 1, imaginary: 0)], rows: 1, columns: 1, dimensions: [:])
        } else {
            let cofactors = try (0..<rows).flatMap { r -> [Complex] in
                try (0..<columns).map { c -> Complex in
                    let sign: Double = ((r + c) % 2 == 0) ? 1.0 : -1.0
                    return Complex(real: sign, imaginary: 0) * (try submatrix(excludingRow: r, excludingCol: c).determinant().value)
                }
            }
            let adjugateDimensions = self.dimensions.mapValues { $0 * (self.rows - 1) }.filter { $0.value != 0 }
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
    case polar(Complex)
    case constant(String)
    case regressionResult(slope: Double, intercept: Double)
    case polynomialFit(coefficients: Vector)
    case plot(PlotData)
    case triggerCSVImport
    case uncertain(UncertainValue)
    case roots([MathValue])

    var typeName: String {
        switch self {
        case .dimensionless: return "Dimensionless"; case .unitValue: return "UnitValue"; case .complex: return "Complex"; case .vector: return "Vector"; case .matrix: return "Matrix"; case .tuple: return "Tuple"; case .functionDefinition: return "FunctionDefinition"; case .complexVector: return "ComplexVector"; case .complexMatrix: return "ComplexMatrix"; case .polar: return "Polar"; case .regressionResult: return "RegressionResult"; case .polynomialFit: return "PolynomialFit"; case .plot: return "Plot"; case .triggerCSVImport: return "CSVImportTrigger"; case .constant: return "Constant"; case .uncertain: return "UncertainValue"; case .roots: return "Roots"
        }
    }
    
    enum CodingKeys: String, CodingKey { case type, value, slope, intercept, coefficients, plotData }

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
        case .polar(let p): try container.encode("polar", forKey: .type); try container.encode(p, forKey: .value)
        case .regressionResult(let slope, let intercept):
            try container.encode("regressionResult", forKey: .type); try container.encode(slope, forKey: .slope); try container.encode(intercept, forKey: .intercept)
        case .polynomialFit(let coeffs):
            try container.encode("polynomialFit", forKey: .type); try container.encode(coeffs, forKey: .coefficients)
        case .constant(let s):
            try container.encode("constant", forKey: .type); try container.encode(s, forKey: .value)
        case .uncertain(let u):
            try container.encode("uncertain", forKey: .type); try container.encode(u, forKey: .value)
        case .roots(let r):
            try container.encode("roots", forKey: .type); try container.encode(r, forKey: .value)
        case .plot:
            try container.encode("plot", forKey: .type)
        case .triggerCSVImport:
            throw EncodingError.invalidValue(self, .init(codingPath: [], debugDescription: "triggerCSVImport is a transient value and should not be saved."))
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
        case "polar": self = .polar(try container.decode(Complex.self, forKey: .value))
        case "regressionResult":
            let slope = try container.decode(Double.self, forKey: .slope); let intercept = try container.decode(Double.self, forKey: .intercept)
            self = .regressionResult(slope: slope, intercept: intercept)
        case "polynomialFit":
            self = .polynomialFit(coefficients: try container.decode(Vector.self, forKey: .coefficients))
        case "constant":
             self = .constant(try container.decode(String.self, forKey: .value))
        case "uncertain":
            self = .uncertain(try container.decode(UncertainValue.self, forKey: .value))
        case "roots":
            if let doubleRoots = try? container.decode([Double].self, forKey: .value) { self = .roots(doubleRoots.map { .dimensionless($0) }) }
            else { self = .roots(try container.decode([MathValue].self, forKey: .value)) }
        case "plot": self = .plot(PlotData(expression: "Empty", series: [], plotType: .line, explicitYRange: nil))
        default: throw DecodingError.dataCorruptedError(forKey: .type, in: container, debugDescription: "Invalid MathValue type '\(type)'")
        }
    }
    
    static func == (lhs: MathValue, rhs: MathValue) -> Bool {
        switch (lhs, rhs) {
        case (.dimensionless(let a), .dimensionless(let b)): return a == b
        case (.unitValue(let a), .unitValue(let b)): return a == b
        case (.complex(let a), .complex(let b)): return a == b
        case (.vector(let a), .vector(let b)): return a == b
        case (.matrix(let a), .matrix(let b)): return a == b
        case (.tuple(let a), .tuple(let b)): return a == b
        case (.functionDefinition(let a), .functionDefinition(let b)): return a == b
        case (.complexVector(let a), .complexVector(let b)): return a == b
        case (.complexMatrix(let a), .complexMatrix(let b)): return a == b
        case (.polar(let a), .polar(let b)): return a == b
        case (.regressionResult(let s1, let i1), .regressionResult(let s2, let i2)): return s1 == s2 && i1 == i2
        case (.polynomialFit(let c1), .polynomialFit(let c2)): return c1 == c2
        case (.plot(let d1), .plot(let d2)): return d1 == d2
        case (.triggerCSVImport, .triggerCSVImport): return true
        case (.constant(let a), .constant(let b)): return a == b
        case (.uncertain(let a), .uncertain(let b)): return a == b
        case (.roots(let a), .roots(let b)): return a == b
        default: return false
        }
    }
}

extension MathValue {
    func asScalar() throws -> Double {
        switch self {
        case .dimensionless(let d):
            return d
        case .unitValue(let u):
            guard u.dimensions.isEmpty else {
                throw MathError.typeMismatch(expected: "Dimensionless value", found: "Value with units")
            }
            return u.value
        case .uncertain(let u):
            return u.value
        // --- ADDED: Prevent dimensioned vectors/matrices from being treated as scalars ---
        case .vector(let v):
            guard v.dimensions.isEmpty else {
                throw MathError.typeMismatch(expected: "Dimensionless vector", found: "Vector with units")
            }
            throw MathError.typeMismatch(expected: "Scalar", found: "Vector")
        case .matrix(let m):
            guard m.dimensions.isEmpty else {
                throw MathError.typeMismatch(expected: "Dimensionless matrix", found: "Matrix with units")
            }
            throw MathError.typeMismatch(expected: "Scalar", found: "Matrix")
        default:
            throw MathError.typeMismatch(expected: "Scalar value", found: self.typeName)
        }
    }
}


// Helper extension to chunk an array into smaller arrays.
extension Array {
    func chunks(ofCount chunkSize: Int) -> [[Element]] {
        return stride(from: 0, to: self.count, by: chunkSize).map {
            Array(self[$0..<Swift.min($0 + chunkSize, self.count)])
        }
    }
}
