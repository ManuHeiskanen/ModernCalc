//
//  EvaluatorDSP.swift
//  ModernCalc
//
//  Created by Manu Heiskanen on 21.9.2025.
//

import Foundation
import Accelerate

/// This extension adds Digital Signal Processing (DSP) functions like FFT
/// using the Accelerate framework.
extension Evaluator {

    /// A static helper containing the core FFT logic. It is called by both the `fft()`
    /// function and the new `powerspectrum()` function.
    static func performFFT(inputVector: ComplexVector, direction: FFTDirection) throws -> ComplexVector {
        let n_original = inputVector.dimension
        let opName = direction == kFFTDirection_Forward ? "fft" : "ifft"
        
        guard n_original > 0 else {
            // FFT of an empty vector is an empty vector.
            return ComplexVector(values: [], dimensions: inputVector.dimensions)
        }

        var paddedVector = inputVector
        let isPowerOfTwo = (n_original > 0) && (n_original & (n_original - 1) == 0)

        if !isPowerOfTwo {
            // Calculate the next power of two and pad the input vector with zeros.
            let nextPowerOfTwo = Int(pow(2, ceil(log2(Double(n_original)))))
            let zerosToPad = nextPowerOfTwo - n_original
            if zerosToPad > 0 {
                let padding = [Complex](repeating: .zero, count: zerosToPad)
                paddedVector = ComplexVector(values: inputVector.values + padding, dimensions: inputVector.dimensions)
            }
        }
        
        // Continue FFT calculation with the (potentially padded) vector.
        let n = paddedVector.dimension
        let log2n = vDSP_Length(log2(Double(n)))

        // 1. Create the double-precision FFT setup object.
        guard let fftSetup = vDSP_create_fftsetupD(log2n, FFTRadix(kFFTRadix2)) else {
            throw MathError.unsupportedOperation(op: opName, typeA: "Failed to create FFT setup.", typeB: nil)
        }
        defer {
            // 2. Clean up the setup object when we're done.
            vDSP_destroy_fftsetupD(fftSetup)
        }

        // 3. Prepare the input data arrays from the padded vector.
        var realp = paddedVector.values.map { $0.real }
        var imagp = paddedVector.values.map { $0.imaginary }

        // 4. Perform the FFT in-place.
        realp.withUnsafeMutableBufferPointer { realpBuffer in
            imagp.withUnsafeMutableBufferPointer { imagpBuffer in
                var splitComplexInput = DSPDoubleSplitComplex(
                    realp: realpBuffer.baseAddress!,
                    imagp: imagpBuffer.baseAddress!
                )
                vDSP_fft_zipD(fftSetup, &splitComplexInput, 1, vDSP_Length(log2n), direction)
            }
        }

        // 5. Pack the results back into our Complex type, scaling if necessary for IFFT.
        if direction == kFFTDirection_Inverse {
            var scale = 1.0 / Double(n)
            vDSP_vsmulD(realp, 1, &scale, &realp, 1, vDSP_Length(n))
            vDSP_vsmulD(imagp, 1, &scale, &imagp, 1, vDSP_Length(n))
        }
        
        let resultValues = (0..<n).map { i in Complex(real: realp[i], imaginary: imagp[i]) }
        return ComplexVector(values: resultValues, dimensions: inputVector.dimensions)
    }

    /// Evaluates the `fft` or `ifft` function call from the parser.
    func evaluateDSPFunction(_ node: FunctionCallNode, variables: inout [String: MathValue], functions: inout [String: FunctionDefinitionNode], angleMode: AngleMode, direction: FFTDirection) throws -> (result: MathValue, usedAngle: Bool) {
        guard node.arguments.count == 1 else {
            throw MathError.incorrectArgumentCount(function: node.name, expected: "1", found: node.arguments.count)
        }

        let (inputValue, usedAngle) = try _evaluateSingle(node: node.arguments[0], variables: &variables, functions: &functions, angleMode: angleMode)

        let inputVector: ComplexVector
        switch inputValue {
        case .vector(let realVector):
            inputVector = ComplexVector(from: realVector)
        case .complexVector(let complexVector):
            inputVector = complexVector
        case .matrix(let matrix):
            guard matrix.rows == 1 || matrix.columns == 1 else {
                throw MathError.typeMismatch(expected: "Vector or ComplexVector", found: "Matrix with dimensions \(matrix.rows)x\(matrix.columns)")
            }
            let complexValues = matrix.values.map { Complex(real: $0, imaginary: 0) }
            inputVector = ComplexVector(values: complexValues, dimensions: matrix.dimensions)
        case .complexMatrix(let complexMatrix):
             guard complexMatrix.rows == 1 || complexMatrix.columns == 1 else {
                throw MathError.typeMismatch(expected: "Vector or ComplexVector", found: "ComplexMatrix with dimensions \(complexMatrix.rows)x\(complexMatrix.columns)")
            }
            inputVector = ComplexVector(values: complexMatrix.values, dimensions: complexMatrix.dimensions)
        default:
            throw MathError.typeMismatch(expected: "Vector or ComplexVector", found: inputValue.typeName)
        }
        
        // Call the refactored static helper function to perform the calculation
        let resultVector = try Evaluator.performFFT(inputVector: inputVector, direction: direction)
        
        return (.complexVector(resultVector), usedAngle)
    }
}
