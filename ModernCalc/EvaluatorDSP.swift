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

    /// Performs a forward or inverse Fast Fourier Transform on a complex vector.
    /// - Parameters:
    ///   - node: The function call node for `fft` or `ifft`.
    ///   - direction: The direction of the transform (forward or inverse).
    /// - Returns: A `MathValue.complexVector` containing the transform result.
    func evaluateDSPFunction(_ node: FunctionCallNode, variables: inout [String: MathValue], functions: inout [String: FunctionDefinitionNode], angleMode: AngleMode, direction: FFTDirection) throws -> (result: MathValue, usedAngle: Bool) {
        guard node.arguments.count == 1 else {
            throw MathError.incorrectArgumentCount(function: node.name, expected: "1", found: node.arguments.count)
        }

        // Evaluate the input argument
        let (inputValue, usedAngle) = try _evaluateSingle(node: node.arguments[0], variables: &variables, functions: &functions, angleMode: angleMode)

        // The input can be a real vector or a complex vector
        let inputVector: ComplexVector
        switch inputValue {
        case .vector(let realVector):
            inputVector = ComplexVector(from: realVector)
        case .complexVector(let complexVector):
            inputVector = complexVector
        // FIX: Handle matrices that are essentially vectors (Nx1 or 1xN).
        // This resolves the issue where `vector()` creates a Matrix type.
        case .matrix(let matrix):
            guard matrix.rows == 1 || matrix.columns == 1 else {
                throw MathError.typeMismatch(expected: "Vector or ComplexVector", found: "Matrix with dimensions \(matrix.rows)x\(matrix.columns)")
            }
            // Convert the real matrix values to complex values for the FFT.
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

        let n = inputVector.dimension
        let log2n = vDSP_Length(log2(Double(n)))

        // FFT works most efficiently with powers of 2.
        guard n > 0 && (n & (n - 1)) == 0 else {
            throw MathError.unsupportedOperation(op: node.name, typeA: "Input vector size must be a power of 2.", typeB: nil)
        }

        // 1. Create the double-precision FFT setup object.
        guard let fftSetup = vDSP_create_fftsetupD(log2n, FFTRadix(kFFTRadix2)) else {
            throw MathError.unsupportedOperation(op: node.name, typeA: "Failed to create FFT setup.", typeB: nil)
        }
        defer {
            // 2. Clean up the setup object when we're done.
            vDSP_destroy_fftsetupD(fftSetup)
        }

        // 3. Prepare the input data arrays.
        var realp = inputVector.values.map { $0.real }
        var imagp = inputVector.values.map { $0.imaginary }

        // The Accelerate framework's vDSP functions operate on C-style pointers.
        // To safely interact with them from Swift, we use `withUnsafeMutableBufferPointer`.
        // This provides a closure with a guaranteed-valid pointer to the array's memory.
        // The FFT function will modify the data in these arrays "in-place".
        realp.withUnsafeMutableBufferPointer { realpBuffer in
            imagp.withUnsafeMutableBufferPointer { imagpBuffer in
                
                // Create the "split complex" structure required by vDSP,
                // using the safe pointers from the buffers.
                var splitComplexInput = DSPDoubleSplitComplex(
                    realp: realpBuffer.baseAddress!,
                    imagp: imagpBuffer.baseAddress!
                )
                
                // 4. Perform the FFT. The results are written back into the
                //    `realp` and `imagp` arrays via the pointers.
                vDSP_fft_zipD(fftSetup, &splitComplexInput, 1, vDSP_Length(log2n), direction)
            }
        }

        // At this point, `realp` and `imagp` contain the transformed data.
        
        // 5. Pack the results back into our Complex type.
        var resultValues: [Complex]
        
        if direction == kFFTDirection_Inverse {
            // For inverse FFT, vDSP requires manual scaling by 1/n.
            var scale = 1.0 / Double(n)
            
            // We can perform the scaling operation in-place on our result arrays.
            vDSP_vsmulD(realp, 1, &scale, &realp, 1, vDSP_Length(n))
            vDSP_vsmulD(imagp, 1, &scale, &imagp, 1, vDSP_Length(n))
            
            resultValues = (0..<n).map { i in Complex(real: realp[i], imaginary: imagp[i]) }
        } else {
            // For the forward transform, just combine the real and imaginary parts.
            resultValues = (0..<n).map { i in Complex(real: realp[i], imaginary: imagp[i]) }
        }
        
        let resultVector = ComplexVector(values: resultValues, dimensions: inputVector.dimensions)
        return (.complexVector(resultVector), usedAngle)
    }
}

