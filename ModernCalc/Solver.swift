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
}
