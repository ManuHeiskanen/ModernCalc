//
//  EvaluatorEigenvalue.swift
//  ModernCalc
//
//  Created by Manu Heiskanen on 6.10.2025.
//

import Foundation
import Accelerate

/// This extension contains the logic for calculating eigenvalues and eigenvectors
/// using the LAPACK dgeev algorithm from the Accelerate framework.
extension Evaluator {
    
    /// Calculates the eigenvalues and eigenvectors of a square matrix.
    /// - Parameter matrix: The input matrix (must be square and dimensionless).
    /// - Returns: A tuple containing the eigenvector matrix (V) and a diagonal matrix of eigenvalues (D).
    ///            The results are wrapped in MathValue to support both Real and Complex matrices.
    func performEigenvalueDecomposition(matrix: Matrix, maxIterations: Int = 100, tolerance: Double = 1e-10) throws -> (eigenvectors: MathValue, eigenvalues: MathValue) {
        guard matrix.rows == matrix.columns else {
            throw MathError.dimensionMismatch(reason: "Matrix must be square for eigenvalue decomposition.")
        }
        guard matrix.dimensions.isEmpty else {
            throw MathError.unsupportedOperation(op: "eig", typeA: "Matrix with units", typeB: nil)
        }

        // Use var for mutable pointer passing required by LAPACK
        let n = Int(matrix.rows)
        
        // LAPACK expects column-major. We pass our row-major matrix 'a'.
        // Since Eigenvalues(A) == Eigenvalues(A^T), passing A as A^T doesn't change eigenvalues.
        var a = matrix.transpose().values
        
        // Arrays for real and imaginary parts of eigenvalues
        var wr = [Double](repeating: 0, count: n)
        var wi = [Double](repeating: 0, count: n)
        
        // Arrays for left (vl) and right (vr) eigenvectors
        var vl = [Double](repeating: 0, count: n * n) // Not referenced
        var vr = [Double](repeating: 0, count: n * n)
        
        var lwork = Int(-1)
        var work = [Double](repeating: 0, count: 1)
        var info: Int = 0
        
        // Local variables for dimension arguments
        var n_query = n
        var lda_query = n
        var ldvl_query = n
        var ldvr_query = n
        
        var jobN: CChar = 78 // ASCII for 'N'
        var jobV: CChar = 86 // ASCII for 'V'
        
        // Query optimal workspace size
        dgeev_(&jobN, &jobV, &n_query, &a, &lda_query, &wr, &wi, &vl, &ldvl_query, &vr, &ldvr_query, &work, &lwork, &info)
        
        lwork = Int(work[0])
        work = [Double](repeating: 0, count: lwork)
        
        // Local variables for actual calculation
        var n_calc = n
        var lda_calc = n
        var ldvl_calc = n
        var ldvr_calc = n
        
        // Perform calculation
        dgeev_(&jobN, &jobV, &n_calc, &a, &lda_calc, &wr, &wi, &vl, &ldvl_calc, &vr, &ldvr_calc, &work, &lwork, &info)
        
        if info != 0 {
            throw MathError.solverFailed(reason: "Eigenvalue calculation failed with error code \(info)")
        }
        
        // Check if we have complex eigenvalues
        var isComplex = false
        for i in 0..<n {
            if abs(wi[i]) > tolerance {
                isComplex = true
                break
            }
        }
        
        if !isComplex {
            // --- REAL CASE ---
            // Construct Eigenvalues Matrix (Diagonal)
            var dValues = [Double](repeating: 0, count: n * n)
            for i in 0..<n {
                dValues[i * n + i] = wr[i]
            }
            let D = Matrix(values: dValues, rows: n, columns: n)
            
            // Construct Eigenvectors Matrix
            // The eigenvectors are stored in 'vr' in column-major order.
            // Interpreting column-major data as row-major results in the transpose.
            // So 'Matrix(values: vr)' creates V^T. We transpose it to get V.
            let V_transposed = Matrix(values: vr, rows: n, columns: n)
            let V = V_transposed.transpose()
            
            return (eigenvectors: .matrix(V), eigenvalues: .matrix(D))
            
        } else {
            // --- COMPLEX CASE ---
            // Construct Complex Eigenvalues Matrix (Diagonal)
            var dValues = [Complex]()
            for r in 0..<n {
                for c in 0..<n {
                    if r == c {
                        dValues.append(Complex(real: wr[r], imaginary: wi[r]))
                    } else {
                        dValues.append(.zero)
                    }
                }
            }
            let complexD = ComplexMatrix(values: dValues, rows: n, columns: n)
            
            // Construct Complex Eigenvectors Matrix (V)
            // LAPACK packs complex eigenvectors into 'vr'.
            // If wi[j] == 0, column j is the real eigenvector.
            // If wi[j] != 0, column j is the real part and column j+1 is the imaginary part.
            // Eigenvector j = VR[:,j] + i*VR[:,j+1]
            // Eigenvector j+1 = VR[:,j] - i*VR[:,j+1]
            
            var vValues = [Complex](repeating: .zero, count: n * n) // We will fill this in Row-Major order
            
            var j = 0
            while j < n {
                if abs(wi[j]) <= tolerance {
                    // Real Eigenvector
                    // VR is column-major, so VR[r + j*n] is row r, col j
                    for r in 0..<n {
                        let val = vr[r + j*n]
                        vValues[r * n + j] = Complex(real: val, imaginary: 0)
                    }
                    j += 1
                } else {
                    // Complex Conjugate Pair
                    // Column j stores Real part, Column j+1 stores Imaginary part
                    for r in 0..<n {
                        let realPart = vr[r + j*n]
                        let imagPart = vr[r + (j+1)*n]
                        
                        // v[j] = u + i*v
                        vValues[r * n + j] = Complex(real: realPart, imaginary: imagPart)
                        // v[j+1] = u - i*v
                        vValues[r * n + (j+1)] = Complex(real: realPart, imaginary: -imagPart)
                    }
                    j += 2
                }
            }
            
            let complexV = ComplexMatrix(values: vValues, rows: n, columns: n)
            return (eigenvectors: .complexMatrix(complexV), eigenvalues: .complexMatrix(complexD))
        }
    }
}
