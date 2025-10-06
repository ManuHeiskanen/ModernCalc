//
//  EvaluatorEigenvalue.swift
//  ModernCalc
//
//  Created by Manu Heiskanen on 6.10.2025.
//

import Foundation

/// This extension contains the logic for calculating eigenvalues and eigenvectors
/// using the QR algorithm.
extension Evaluator {
    
    /// Calculates the eigenvalues and eigenvectors of a square matrix.
    /// - Parameter matrix: The input matrix (must be square and dimensionless).
    /// - Returns: A tuple containing the eigenvector matrix (V) and a diagonal matrix of eigenvalues (D).
    func performEigenvalueDecomposition(matrix: Matrix, maxIterations: Int = 100, tolerance: Double = 1e-10) throws -> (eigenvectors: Matrix, eigenvalues: Matrix) {
        guard matrix.rows == matrix.columns else {
            throw MathError.dimensionMismatch(reason: "Matrix must be square for eigenvalue decomposition.")
        }
        // Eigenvalue decomposition is not well-defined for matrices with units.
        guard matrix.dimensions.isEmpty else {
            throw MathError.unsupportedOperation(op: "eig", typeA: "Matrix with units", typeB: nil)
        }

        var Ak = matrix
        let n = matrix.rows

        // Q_total will accumulate the orthogonal transformations. It starts as the identity matrix.
        var Q_total = Matrix(values: (0..<n*n).map { Double($0 / n == $0 % n ? 1 : 0) }, rows: n, columns: n)

        for _ in 0..<maxIterations {
            // Perform QR decomposition on the current iteration matrix Ak
            let (Q, R) = try qrDecomposition(Ak)
            // Update Ak for the next iteration: Ak+1 = R * Q
            Ak = try R * Q
            // Accumulate the Q matrices
            Q_total = try Q_total * Q

            // Check for convergence by summing the absolute values of the lower triangular elements.
            // If this sum is close to zero, Ak is close to being upper triangular.
            var offDiagonalSum: Double = 0
            for i in 1..<n {
                for j in 0..<i {
                    offDiagonalSum += abs(Ak[i, j])
                }
            }

            if offDiagonalSum < tolerance {
                break // The algorithm has converged.
            }
        }

        // The eigenvalues are the diagonal elements of the resulting upper triangular matrix Ak.
        let eigenvalues = (0..<n).map { Ak[$0, $0] }
        
        // The columns of the accumulated Q_total matrix are the corresponding eigenvectors.
        let eigenvectors = Q_total
        
        // Create a diagonal matrix D from the eigenvalues for the final result.
        var D_values = [Double](repeating: 0, count: n * n)
        for i in 0..<n {
            D_values[i * n + i] = eigenvalues[i]
        }
        let D = Matrix(values: D_values, rows: n, columns: n)

        return (eigenvectors: eigenvectors, eigenvalues: D)
    }

    /// Performs QR decomposition of a matrix A into an orthogonal matrix Q and an upper triangular matrix R
    /// using the modified Gram-Schmidt process for numerical stability.
    private func qrDecomposition(_ A: Matrix) throws -> (Q: Matrix, R: Matrix) {
        let m = A.rows
        let n = A.columns
        
        var qValues = [Double](repeating: 0.0, count: m * n)
        var rValues = [Double](repeating: 0.0, count: n * n)

        for j in 0..<n {
            var v = (0..<m).map { A[$0, j] } // Get the j-th column of A

            for i in 0..<j {
                // Project a_j onto q_i: r_ij = q_i^T * a_j
                var r_ij: Double = 0
                for k in 0..<m {
                    r_ij += qValues[k * n + i] * A[k, j]
                }
                rValues[i * n + j] = r_ij
                
                // Subtract the projection from v: v = v - r_ij * q_i
                for k in 0..<m {
                    v[k] -= r_ij * qValues[k * n + i]
                }
            }

            // Find the norm of the resulting vector v: r_jj = ||v||
            var r_jj = sqrt(v.map { $0 * $0 }.reduce(0, +))
            rValues[j * n + j] = r_jj

            // Normalize v to get the next orthogonal vector q_j
            if r_jj > 1e-12 {
                for k in 0..<m {
                    qValues[k * n + j] = v[k] / r_jj
                }
            }
        }
        
        let Q = Matrix(values: qValues, rows: m, columns: n)
        let R = Matrix(values: rValues, rows: n, columns: n)
        
        return (Q, R)
    }
}
