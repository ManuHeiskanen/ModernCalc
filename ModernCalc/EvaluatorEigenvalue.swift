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
    func performEigenvalueDecomposition(matrix: Matrix, maxIterations: Int = 100, tolerance: Double = 1e-10) throws -> (eigenvectors: Matrix, eigenvalues: Matrix) {
        guard matrix.rows == matrix.columns else {
            throw MathError.dimensionMismatch(reason: "Matrix must be square for eigenvalue decomposition.")
        }
        guard matrix.dimensions.isEmpty else {
            throw MathError.unsupportedOperation(op: "eig", typeA: "Matrix with units", typeB: nil)
        }

        // Use var for mutable pointer passing required by LAPACK
        // CHANGED: Int32 -> Int for ILP64 compatibility
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
        
        // CHANGED: Int32 -> Int
        var lwork = Int(-1)
        var work = [Double](repeating: 0, count: 1)
        var info: Int = 0
        
        // Local variables for dimension arguments
        var n_query = n
        var lda_query = n
        var ldvl_query = n
        var ldvr_query = n
        
        // Fix for dangling pointer warnings: Define chars explicitly
        var jobN: CChar = 78 // ASCII for 'N'
        var jobV: CChar = 86 // ASCII for 'V'
        
        // Query optimal workspace size using standard dgeev_ (underscore required in Swift)
        dgeev_(&jobN,       // Left vectors: No
              &jobV,       // Right vectors: Yes
              &n_query, &a, &lda_query, &wr, &wi, &vl, &ldvl_query, &vr, &ldvr_query, &work, &lwork, &info)
        
        lwork = Int(work[0])
        work = [Double](repeating: 0, count: lwork)
        
        // Local variables for actual calculation
        var n_calc = n
        var lda_calc = n
        var ldvl_calc = n
        var ldvr_calc = n
        
        // Perform calculation
        dgeev_(&jobN,
              &jobV,
              &n_calc, &a, &lda_calc, &wr, &wi, &vl, &ldvl_calc, &vr, &ldvr_calc, &work, &lwork, &info)
        
        if info != 0 {
            throw MathError.solverFailed(reason: "Eigenvalue calculation failed with error code \(info)")
        }
        
        // Construct Eigenvalues Matrix (Diagonal)
        var dValues = [Double](repeating: 0, count: n * n)
        for i in 0..<n {
            dValues[i * n + i] = wr[i]
        }
        
        let D = Matrix(values: dValues, rows: n, columns: n)
        
        // Construct Eigenvectors Matrix
        // The eigenvectors are stored in 'vr' in column-major order.
        // We transpose it to match our Row-Major system.
        let V_transposed = Matrix(values: vr, rows: n, columns: n)
        let V = V_transposed.transpose()
        
        return (eigenvectors: V, eigenvalues: D)
    }
}
