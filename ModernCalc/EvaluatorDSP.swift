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
        let n = inputVector.dimension
        let log2n = vDSP_Length(log2(Double(n)))

        let opName = direction == kFFTDirection_Forward ? "fft" : "ifft"
        
        // FFT works most efficiently with powers of 2.
        guard n > 0 && (n & (n - 1)) == 0 else {
            throw MathError.unsupportedOperation(op: opName, typeA: "Input vector size must be a power of 2.", typeB: nil)
        }

        // 1. Create the double-precision FFT setup object.
        guard let fftSetup = vDSP_create_fftsetupD(log2n, FFTRadix(kFFTRadix2)) else {
            throw MathError.unsupportedOperation(op: opName, typeA: "Failed to create FFT setup.", typeB: nil)
        }
        defer {
            // 2. Clean up the setup object when we're done.
            vDSP_destroy_fftsetupD(fftSetup)
        }

        // 3. Prepare the input data arrays.
        var realp = inputVector.values.map { $0.real }
        var imagp = inputVector.values.map { $0.imaginary }

        realp.withUnsafeMutableBufferPointer { realpBuffer in
            imagp.withUnsafeMutableBufferPointer { imagpBuffer in
                var splitComplexInput = DSPDoubleSplitComplex(
                    realp: realpBuffer.baseAddress!,
                    imagp: imagpBuffer.baseAddress!
                )
                vDSP_fft_zipD(fftSetup, &splitComplexInput, 1, vDSP_Length(log2n), direction)
            }
        }

        // 5. Pack the results back into our Complex type, scaling if necessary.
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

