//
//  DisplayFormatter.swift
//  ModernCalc
//
//  Created by Manu Heiskanen on 11.9.2025.
//

import Foundation

/// A helper struct with static methods for formatting mathematical results for display.
struct DisplayFormatter {
    
    /// Formats a double into a string with a limited number of significant figures, suitable for equations.
    static func formatScalar(_ value: Double) -> String {
        // Use up to 4 significant digits for a cleaner look in the equation.
        let tempFormatted = String(format: "%.4g", value)
        
        // This regex helps remove trailing zeros (e.g., "2.500" -> "2.5").
        if let regex = try? NSRegularExpression(pattern: "\\.?0+$") {
            let nsString = tempFormatted as NSString
            let range = NSRange(location: 0, length: nsString.length)
            let modString = regex.stringByReplacingMatches(in: tempFormatted, options: [], range: range, withTemplate: "")
            if modString.hasSuffix(".") {
                return String(modString.dropLast())
            }
            return modString.isEmpty ? "0" : modString
        }
        return tempFormatted
    }

    /// Converts a vector of polynomial coefficients into a readable equation string.
    /// The coefficients are expected in ascending order of power (constant, x, x^2, ...).
    static func formatPolynomialEquation(coeffs: Vector) -> String {
        var result = "y = "
        // Reverse the coefficients to process from the highest power down to the constant term.
        let reversedCoeffs = coeffs.values.enumerated().reversed()
        
        for (i, coeff) in reversedCoeffs {
            // Skip terms with coefficients that are essentially zero, unless it's the only term.
            if abs(coeff) < 1e-9 && coeffs.dimension > 1 { continue }

            let isFirstTerm = (result == "y = ")
            let absCoeff = abs(coeff)
            
            // Determine the correct sign to display (+ or -).
            let sign: String
            if isFirstTerm {
                sign = (coeff < 0) ? "- " : ""
            } else {
                sign = (coeff < 0) ? " - " : " + "
            }
            result += sign
            
            // Display the coefficient value, but omit '1' for terms like '1x^2'.
            if abs(absCoeff - 1.0) > 1e-9 || i == 0 {
                 result += formatScalar(absCoeff)
            }
            
            // Append the variable 'x' and its power, if applicable.
            if i > 0 {
                result += "x"
                if i > 1 {
                    // Use a superscript character for a cleaner look than '^'.
                    result += "ˆ\(i)"
                }
            }
        }
        // Handle the case where all coefficients are zero.
        return result == "y = " ? "y = 0" : result
    }
}
