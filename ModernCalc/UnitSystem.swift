//
//  UnitSystem.swift
//  ModernCalc
//
//  Created by Manu Heiskanen on 13.9.2025.
//

import Foundation

// Defines the seven base dimensions of the International System of Units (SI).
// This ensures type-safety and prevents errors from using string literals.
enum BaseUnit: String, Codable, CaseIterable {
    case meter, kilogram, second, ampere, kelvin, mole, candela
}

// A type alias for a dictionary that represents the "dimensional vector" of any unit.
// The keys are the base units, and the values are their integer exponents.
// Example: Velocity (m/s) would be represented as [.meter: 1, .second: -1]
// Example: A dimensionless quantity would have an empty dictionary.
typealias UnitDimension = [BaseUnit: Int]

// Represents the definition of a single unit, like a meter, foot, or joule.
struct UnitDefinition {
    let symbol: String
    let dimensions: UnitDimension
    // The multiplicative factor to convert a value in this unit to its
    // equivalent in base SI units.
    // Example: For "km", the conversionFactor is 1000.0, as 1 km = 1000 m.
    // Example: For "m", the conversionFactor is 1.0.
    let conversionFactor: Double
}

// A static store for all predefined units known to the calculator.
// This acts as a central database for unit conversions and dimensional analysis.
struct UnitStore {
    static let units: [String: UnitDefinition] = buildUnitMap()

    private static func buildUnitMap() -> [String: UnitDefinition] {
        var unitMap: [String: UnitDefinition] = [:]

        // SI Base Units
        unitMap["m"] = UnitDefinition(symbol: "m", dimensions: [.meter: 1], conversionFactor: 1.0)
        unitMap["kg"] = UnitDefinition(symbol: "kg", dimensions: [.kilogram: 1], conversionFactor: 1.0)
        unitMap["s"] = UnitDefinition(symbol: "s", dimensions: [.second: 1], conversionFactor: 1.0)
        unitMap["A"] = UnitDefinition(symbol: "A", dimensions: [.ampere: 1], conversionFactor: 1.0)
        unitMap["K"] = UnitDefinition(symbol: "K", dimensions: [.kelvin: 1], conversionFactor: 1.0)
        unitMap["mol"] = UnitDefinition(symbol: "mol", dimensions: [.mole: 1], conversionFactor: 1.0)
        unitMap["cd"] = UnitDefinition(symbol: "cd", dimensions: [.candela: 1], conversionFactor: 1.0)
        
        // Common base-10 prefixes for grams
        unitMap["g"] = UnitDefinition(symbol: "g", dimensions: [.kilogram: 1], conversionFactor: 0.001)

        // SI Derived Units
        unitMap["Hz"] = UnitDefinition(symbol: "Hz", dimensions: [.second: -1], conversionFactor: 1.0)
        unitMap["N"] = UnitDefinition(symbol: "N", dimensions: [.kilogram: 1, .meter: 1, .second: -2], conversionFactor: 1.0)
        unitMap["Pa"] = UnitDefinition(symbol: "Pa", dimensions: [.kilogram: 1, .meter: -1, .second: -2], conversionFactor: 1.0)
        unitMap["J"] = UnitDefinition(symbol: "J", dimensions: [.kilogram: 1, .meter: 2, .second: -2], conversionFactor: 1.0)
        unitMap["W"] = UnitDefinition(symbol: "W", dimensions: [.kilogram: 1, .meter: 2, .second: -3], conversionFactor: 1.0)
        unitMap["C"] = UnitDefinition(symbol: "C", dimensions: [.second: 1, .ampere: 1], conversionFactor: 1.0)
        unitMap["V"] = UnitDefinition(symbol: "V", dimensions: [.kilogram: 1, .meter: 2, .second: -3, .ampere: -1], conversionFactor: 1.0)
        unitMap["Ohm"] = UnitDefinition(symbol: "Ohm", dimensions: [.kilogram: 1, .meter: 2, .second: -3, .ampere: -2], conversionFactor: 1.0)
        unitMap["F"] = UnitDefinition(symbol: "F", dimensions: [.kilogram: -1, .meter: -2, .second: 4, .ampere: 2], conversionFactor: 1.0)
        unitMap["H"] = UnitDefinition(symbol: "H", dimensions: [.kilogram: 1, .meter: 2, .second: -2, .ampere: -2], conversionFactor: 1.0)
        unitMap["T"] = UnitDefinition(symbol: "T", dimensions: [.kilogram: 1, .second: -2, .ampere: -1], conversionFactor: 1.0)
        
        // Common Non-SI Units
        unitMap["min"] = UnitDefinition(symbol: "min", dimensions: [.second: 1], conversionFactor: 60.0)
        unitMap["h"] = UnitDefinition(symbol: "h", dimensions: [.second: 1], conversionFactor: 3600.0)
        unitMap["day"] = UnitDefinition(symbol: "day", dimensions: [.second: 1], conversionFactor: 86400.0)
        
        unitMap["deg"] = UnitDefinition(symbol: "deg", dimensions: [:], conversionFactor: .pi / 180.0) // Dimensionless, but convertible
        unitMap["rad"] = UnitDefinition(symbol: "rad", dimensions: [:], conversionFactor: 1.0) // Dimensionless
        
        // Volume
        unitMap["L"] = UnitDefinition(symbol: "L", dimensions: [.meter: 3], conversionFactor: 0.001)
        
        // Area
        unitMap["ha"] = UnitDefinition(symbol: "ha", dimensions: [.meter: 2], conversionFactor: 10000.0)
        
        // Pressure
        unitMap["bar"] = UnitDefinition(symbol: "bar", dimensions: [.kilogram: 1, .meter: -1, .second: -2], conversionFactor: 100000.0)
        
        // Energy
        unitMap["eV"] = UnitDefinition(symbol: "eV", dimensions: [.kilogram: 1, .meter: 2, .second: -2], conversionFactor: 1.602176634e-19)
        unitMap["cal"] = UnitDefinition(symbol: "cal", dimensions: [.kilogram: 1, .meter: 2, .second: -2], conversionFactor: 4.184)


        // Add SI Prefixes (kilo, milli, etc.) to meters, grams, and liters
        let prefixes = ["k": 1e3, "c": 1e-2, "m": 1e-3, "μ": 1e-6, "n": 1e-9]
        let baseSymbols = ["m", "g", "L"]

        for (prefix, factor) in prefixes {
            for symbol in baseSymbols {
                if let baseUnit = unitMap[symbol] {
                    let newSymbol = prefix + symbol
                    let newFactor = baseUnit.conversionFactor * factor
                    unitMap[newSymbol] = UnitDefinition(symbol: newSymbol, dimensions: baseUnit.dimensions, conversionFactor: newFactor)
                }
            }
        }
        
        return unitMap
    }
}
