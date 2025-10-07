//
//  Solver.swift
//  ModernCalc
//
//  Created by Manu Heiskanen on 15.9.2025.
//


import Foundation

/// A container for advanced numerical solving algorithms.
struct Solver {
    
    /// Finds a root of a function within a given interval using Brent's method.
    ///
    /// Brent's method combines the reliability of the bisection method with the speed of the secant method
    /// and inverse quadratic interpolation. It is guaranteed to find a root if one is bracketed.
    ///
    /// - Parameters:
    ///   - f: The function `(Double) -> Double` for which to find a root.
    ///   - x1: The first endpoint of the search interval.
    ///   - x2: The second endpoint of the search interval.
    ///   - tol: The desired tolerance (accuracy) for the root.
    ///   - maxIter: The maximum number of iterations to perform.
    /// - Returns: The root as a `Double` if found, otherwise `nil`.
    static func brent(f: (Double) throws -> Double, x1: Double, x2: Double, tol: Double = 1e-9, maxIter: Int = 100) throws -> Double? {
        var a = x1
        var b = x2
        var fa = try f(a)
        var fb = try f(b)
        
        // Ensure that the root is bracketed.
        guard fa * fb < 0 else {
            if abs(fa) < tol { return a }
            if abs(fb) < tol { return b }
            return nil
        }
        
        // Start with b as the best guess.
        if abs(fa) < abs(fb) {
            swap(&a, &b)
            swap(&fa, &fb)
        }
        
        var c = a
        var fc = fa
        var d = b - a // Last step size
        var e = d     // Previous step size
        
        for _ in 0..<maxIter {
            // Check for convergence.
            if abs(b - c) < tol || abs(fb) < tol {
                return b
            }
            
            // Attempt interpolation (Secant or Inverse Quadratic).
            if abs(e) > tol && abs(fa) > abs(fb) {
                let s: Double
                if a == c { // Linear interpolation (Secant method)
                    s = b - fb * (b - a) / (fb - fa)
                } else { // Inverse quadratic interpolation
                    let q = fa / fc
                    let r = fb / fc
                    let p = fb / fa
                    s = b + p * (q * (r - q) * (c - b) - (1 - r) * (q - 1) * (b - a)) / ((q - 1) * (r - 1) * (p - 1))
                }
                
                let condition1 = (s < (3 * a + b) / 4 || s > b)
                let condition2 = abs(s - b) >= abs(e) / 2
                
                if !condition1 && !condition2 {
                    d = s - b
                    e = d
                } else {
                    // Interpolation failed, fall back to bisection.
                    d = (c - b) / 2
                    e = d
                }
            } else {
                // Bounds are too close, must use bisection.
                d = (c - b) / 2
                e = d
            }
            
            // Update the position of the root.
            a = b
            fa = fb
            
            if abs(d) > tol {
                b += d
            } else {
                // Step is too small, move by tolerance.
                b += (c > b ? tol : -tol)
            }
            
            fb = try f(b)
            
            // Update the contrapoint c.
            if fa * fb < 0 {
                c = a
                fc = fa
            }
        }
        
        return nil // Failed to converge within max iterations.
    }
    
    // --- NEW: ode45 implementation using the Dormand-Prince method ---
    /// Solves a system of ordinary differential equations using the Dormand-Prince 5(4) method with adaptive step sizing.
    /// - Parameters:
    ///   - f: The function `(Double, Vector) throws -> Vector` representing the system `dy/dt = f(t, y)`.
    ///   - t_span: A 2-element array `[t_start, t_end]` specifying the integration interval.
    ///   - y0: The initial state vector at `t_start`.
    ///   - tol: The desired relative tolerance for the error control.
    /// - Returns: A tuple containing a `Vector` of time points and a `Matrix` of corresponding state vectors.
    static func ode45(f: (Double, Vector) throws -> Vector, t_span: [Double], y0: Vector, tol: Double = 1e-6) throws -> (time: Vector, states: Matrix) {
        // Butcher Tableau for Dormand-Prince 5(4) "RKDP"
        let c: [Double] = [0, 1/5, 3/10, 4/5, 8/9, 1, 1]
        let a: [[Double]] = [
            [],
            [1/5],
            [3/40, 9/40],
            [44/45, -56/15, 32/9],
            [19372/6561, -25360/2187, 64448/6561, -212/729],
            [9017/3168, -355/33, 46732/5247, 49/176, -5103/18656],
            [35/384, 0, 500/1113, 125/192, -2187/6784, 11/84]
        ]
        // 5th order solution coefficients
        let b: [Double] = [35/384, 0, 500/1113, 125/192, -2187/6784, 11/84, 0]
        // 4th order (embedded) solution coefficients for error estimation
        let b_star: [Double] = [5179/57600, 0, 7571/16695, 393/640, -92097/339200, 187/2100, 1/40]

        let t0 = t_span[0]
        let tf = t_span[1]

        var t = t0
        var y = y0
        // Initial step size guess
        var h = abs(tf - t0) * 0.01 * (tf > t0 ? 1 : -1)

        var timePoints = [t0]
        var stateVectors = [y0]

        let maxIter = 100000
        var iter = 0
        
        while t < tf && iter < maxIter {
            // Ensure the final step lands exactly on tf
            if t + h > tf { h = tf - t }

            // Calculate k stages
            var k: [Vector] = []
            k.append(try f(t, y) * h)
            k.append(try f(t + c[1]*h, y + (k[0] * a[1][0])) * h)
            k.append(try f(t + c[2]*h, y + (k[0] * a[2][0] + k[1] * a[2][1])) * h)
            k.append(try f(t + c[3]*h, y + (k[0] * a[3][0] + k[1] * a[3][1] + k[2] * a[3][2])) * h)
            k.append(try f(t + c[4]*h, y + (k[0] * a[4][0] + k[1] * a[4][1] + k[2] * a[4][2] + k[3] * a[4][3])) * h)
            k.append(try f(t + c[5]*h, y + (k[0] * a[5][0] + k[1] * a[5][1] + k[2] * a[5][2] + k[3] * a[5][3] + k[4] * a[5][4])) * h)
            k.append(try f(t + c[6]*h, y + (k[0] * a[6][0] + k[1] * a[6][1] + k[2] * a[6][2] + k[3] * a[6][3] + k[4] * a[6][4] + k[5] * a[6][5])) * h)

            // Calculate error by comparing 5th and 4th order results
            var errorVec = Vector(values: Array(repeating: 0.0, count: y0.dimension))
            for i in 0..<7 {
                errorVec = try errorVec + (k[i] * (b[i] - b_star[i]))
            }
            let error = errorVec.magnitude().value / tol

            // Adapt step size
            let h_new: Double
            if error <= 1.0 { // Step is accepted
                t += h
                var y_next = y
                for i in 0..<7 {
                    y_next = try y_next + (k[i] * b[i])
                }
                y = y_next
                
                timePoints.append(t)
                stateVectors.append(y)
                
                // Increase step size for next iteration
                h_new = h * min(5.0, 0.9 * pow(error, -0.20))
            } else { // Step is rejected
                // Decrease step size and retry
                h_new = h * max(0.1, 0.9 * pow(error, -0.25))
            }
            
            h = h_new
            
            // Prevent step size from becoming excessively small
            if abs(h) < 1e-15 { break }

            iter += 1
        }
        
        if iter >= maxIter {
            print("Warning: ode45 reached the maximum number of iterations.")
        }
        
        // Format the output into the required MathValue types
        let timeResult = Vector(values: timePoints)
        let stateValues = stateVectors.flatMap { $0.values }
        let stateMatrix = Matrix(values: stateValues, rows: stateVectors.count, columns: y0.dimension)
        
        return (time: timeResult, states: stateMatrix)
    }
}
