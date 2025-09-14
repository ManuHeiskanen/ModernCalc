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
    
    /// FIX: Added the missing 'baseUnitSymbols' dictionary.
    /// Maps a BaseUnit enum to its standard SI symbol string. This is required by the LaTeXEngine
    /// to correctly format unit dimensions.
    static let baseUnitSymbols: [BaseUnit: String] = [
        .meter: "m",
        .kilogram: "kg",
        .second: "s",
        .ampere: "A",
        .kelvin: "K",
        .mole: "mol",
        .candela: "cd"
    ]
    
    static let units: [String: UnitDefinition] = buildUnitMap()

    private static func buildUnitMap() -> [String: UnitDefinition] {
        var unitMap: [String: UnitDefinition] = [:]

        // SI Base Units
        unitMap["m"] = UnitDefinition(symbol: "m", dimensions: [.meter: 1], conversionFactor: 1.0) // meter
        unitMap["kg"] = UnitDefinition(symbol: "kg", dimensions: [.kilogram: 1], conversionFactor: 1.0) // kilogram
        unitMap["s"] = UnitDefinition(symbol: "s", dimensions: [.second: 1], conversionFactor: 1.0) // second
        unitMap["A"] = UnitDefinition(symbol: "A", dimensions: [.ampere: 1], conversionFactor: 1.0) // Ampere
        unitMap["K"] = UnitDefinition(symbol: "K", dimensions: [.kelvin: 1], conversionFactor: 1.0) // Kelvin
        unitMap["mol"] = UnitDefinition(symbol: "mol", dimensions: [.mole: 1], conversionFactor: 1.0) // mol
        unitMap["cd"] = UnitDefinition(symbol: "cd", dimensions: [.candela: 1], conversionFactor: 1.0) // candela
        
        // Common base-10 prefixes for grams
        unitMap["g"] = UnitDefinition(symbol: "g", dimensions: [.kilogram: 1], conversionFactor: 0.001) // gram

        // SI Derived Units
        unitMap["Hz"] = UnitDefinition(symbol: "Hz", dimensions: [.second: -1], conversionFactor: 1.0) // Hz
        unitMap["N"] = UnitDefinition(symbol: "N", dimensions: [.kilogram: 1, .meter: 1, .second: -2], conversionFactor: 1.0) // Newton
        unitMap["Pa"] = UnitDefinition(symbol: "Pa", dimensions: [.kilogram: 1, .meter: -1, .second: -2], conversionFactor: 1.0) // Pascal
        unitMap["J"] = UnitDefinition(symbol: "J", dimensions: [.kilogram: 1, .meter: 2, .second: -2], conversionFactor: 1.0) // Joule
        unitMap["W"] = UnitDefinition(symbol: "W", dimensions: [.kilogram: 1, .meter: 2, .second: -3], conversionFactor: 1.0) // Watt
        unitMap["C"] = UnitDefinition(symbol: "C", dimensions: [.second: 1, .ampere: 1], conversionFactor: 1.0) // Couloumb
        unitMap["V"] = UnitDefinition(symbol: "V", dimensions: [.kilogram: 1, .meter: 2, .second: -3, .ampere: -1], conversionFactor: 1.0) // Volt
        unitMap["Ohm"] = UnitDefinition(symbol: "Ohm", dimensions: [.kilogram: 1, .meter: 2, .second: -3, .ampere: -2], conversionFactor: 1.0) // Ohm
        unitMap["Ω"] = UnitDefinition(symbol: "Ω", dimensions: [.kilogram: 1, .meter: 2, .second: -3, .ampere: -2], conversionFactor: 1.0) // Alias for Ohm
        unitMap["F"] = UnitDefinition(symbol: "F", dimensions: [.kilogram: -1, .meter: -2, .second: 4, .ampere: 2], conversionFactor: 1.0) // Farad
        unitMap["H"] = UnitDefinition(symbol: "H", dimensions: [.kilogram: 1, .meter: 2, .second: -2, .ampere: -2], conversionFactor: 1.0) // Henry
        unitMap["T"] = UnitDefinition(symbol: "T", dimensions: [.kilogram: 1, .second: -2, .ampere: -1], conversionFactor: 1.0) // Tesla
        unitMap["lm"] = UnitDefinition(symbol: "lm", dimensions: [.candela: 1],
            conversionFactor: 1.0) // Lumen
        unitMap["lx"] = UnitDefinition(symbol: "lx", dimensions: [.candela: 1, .meter: -2], conversionFactor: 1.0) // Lux
        unitMap["Wb"] = UnitDefinition(symbol: "Wb", dimensions: [.kilogram: 1, .meter: 2, .second: -2, .ampere: -1], conversionFactor: 1.0) // Weber
        unitMap["S"] = UnitDefinition(symbol: "S", dimensions: [.kilogram: -1, .meter: -2, .second: 3, .ampere: 2], conversionFactor: 1.0) // Siemens
        unitMap["Bq"] = UnitDefinition(symbol: "Bq", dimensions: [.second: -1],
            conversionFactor: 1.0) // Becquerel
        unitMap["Gy"] = UnitDefinition(symbol: "Gy", dimensions: [.meter: 2, .second: -2], conversionFactor: 1.0) // Gray
        unitMap["Sv"] = UnitDefinition(symbol: "Sv", dimensions: [.meter: 2, .second: -2], conversionFactor: 1.0) // Sievert
        unitMap["kat"] = UnitDefinition(symbol: "kat", dimensions: [.mole: 1, .second: -1], conversionFactor: 1.0) // Katal
        
        
        // Common Non-SI Units
        unitMap["Wh"] = UnitDefinition(symbol: "Wh", dimensions: [.kilogram: 1, .meter: 2, .second: -2], conversionFactor: 1.0) // Watt Hour
        unitMap["min"] = UnitDefinition(symbol: "min", dimensions: [.second: 1], conversionFactor: 60.0) // Minute
        unitMap["h"] = UnitDefinition(symbol: "h", dimensions: [.second: 1], conversionFactor: 3600.0) // Hour
        unitMap["day"] = UnitDefinition(symbol: "day", dimensions: [.second: 1], conversionFactor: 86400.0) // Day
        unitMap["week"] = UnitDefinition(symbol: "week", dimensions: [.second: 1], conversionFactor: 604800.0) // Week
        unitMap["month"] = UnitDefinition(symbol: "month", dimensions: [.second: 1], conversionFactor: 2592000.0) // Month, 30 days
        unitMap["year"] = UnitDefinition(symbol: "year", dimensions: [.second: 1], conversionFactor: 31536000.0) // Year, 365 days
        
        unitMap["deg"] = UnitDefinition(symbol: "deg", dimensions: [:], conversionFactor: .pi / 180.0) // Dimensionless, but convertible
        unitMap["rad"] = UnitDefinition(symbol: "rad", dimensions: [:], conversionFactor: 1.0) // Dimensionless
        unitMap["sr"] = UnitDefinition(symbol: "sr", dimensions: [:], conversionFactor: 1.0) // Steradian
        
        // Volume
        unitMap["L"] = UnitDefinition(symbol: "L", dimensions: [.meter: 3], conversionFactor: 0.001) // Liter
        
        // Area
        unitMap["m^2"] = UnitDefinition(symbol: "m^2", dimensions: [.meter: 2], conversionFactor: 1.0) // Square meter
        unitMap["ca"] = UnitDefinition(symbol: "ca", dimensions: [.meter: 2], conversionFactor: 1.0) // Centiare
        unitMap["da"] = UnitDefinition(symbol: "da", dimensions: [.meter: 2], conversionFactor: 10.0) // Deciare
        unitMap["a"] = UnitDefinition(symbol: "a", dimensions: [.meter: 2], conversionFactor: 100.0) // Are
        unitMap["daa"] = UnitDefinition(symbol: "daa", dimensions: [.meter: 2], conversionFactor: 1000.0) // Decare
        unitMap["ha"] = UnitDefinition(symbol: "ha", dimensions: [.meter: 2], conversionFactor: 10000.0) // Hectare
        
        
        // Energy
        unitMap["eV"] = UnitDefinition(symbol: "eV", dimensions: [.kilogram: 1, .meter: 2, .second: -2], conversionFactor: 1.602176634e-19) // Electronvolt
        unitMap["cal"] = UnitDefinition(symbol: "cal", dimensions: [.kilogram: 1, .meter: 2, .second: -2], conversionFactor: 4.184) // Calorie
        
        
        // Bad Units
        
        // Length
        unitMap["in"] = UnitDefinition(symbol: "in", dimensions: [.meter: 1], conversionFactor: 0.0254) // Inch
        unitMap["ft"] = UnitDefinition(symbol: "ft", dimensions: [.meter: 1], conversionFactor: 0.3048) // Foot
        unitMap["yd"] = UnitDefinition(symbol: "yd", dimensions: [.meter: 1], conversionFactor: 0.9144) // Yard
        unitMap["mi"] = UnitDefinition(symbol: "mi", dimensions: [.meter: 1], conversionFactor: 1609.344) // Mile
        unitMap["nmi"] = UnitDefinition(symbol: "nmi", dimensions: [.meter: 1], conversionFactor: 1852) // Nautical Mile
        
        
        // Area
        unitMap["acre"] = UnitDefinition(symbol: "acre", dimensions: [.meter: 2], conversionFactor: 4046.8564224) // Acre
        unitMap["sqin"] = UnitDefinition(symbol: "sqin", dimensions: [.meter: 2], conversionFactor: 0.00064516) // Square Inch
        unitMap["sqft"] = UnitDefinition(symbol: "sqft", dimensions: [.meter: 2], conversionFactor: 0.09290304) // Square Foot
        unitMap["sqyd"] = UnitDefinition(symbol: "sqyd", dimensions: [.meter: 2], conversionFactor: 0.83612736) // Square Yard
        unitMap["sqmil"] = UnitDefinition(symbol: "sqmil", dimensions: [.meter: 2], conversionFactor: 2589988.11) // Square Mile

        
        // Volyme
        unitMap["USGal"] = UnitDefinition(symbol: "USGal", dimensions: [.meter: 3], conversionFactor: 0.00378541) // US Gallon
        unitMap["UKGal"] = UnitDefinition(symbol: "UKGal", dimensions: [.meter: 3], conversionFactor: 0.00454609) // UK Gallon
        unitMap["qt"] = UnitDefinition(symbol: "qt", dimensions: [.meter: 3], conversionFactor: 0.000946) // Quart
        unitMap["pt"] = UnitDefinition(symbol: "pt", dimensions: [.meter: 3], conversionFactor: 0.000473) // Pint
        unitMap["floz"] = UnitDefinition(symbol: "floz", dimensions: [.meter: 3], conversionFactor: 0.00002957352965157) // Fluid Ounce
        
        // Mass
        unitMap["lb"] = UnitDefinition(symbol: "lb", dimensions: [.kilogram: 1], conversionFactor: 0.45359237) // Pound
        unitMap["oz"] = UnitDefinition(symbol: "oz", dimensions: [.kilogram: 1], conversionFactor: 0.028349523125) // Ounce
        unitMap["ston"] = UnitDefinition(symbol: "ston", dimensions: [.kilogram: 1], conversionFactor: 907.1847) // Short Ton
        unitMap["lton"] = UnitDefinition(symbol: "lton", dimensions: [.kilogram: 1], conversionFactor: 1016.0469088) // Short Ton
        
        // Pressure
        unitMap["torr"] = UnitDefinition(symbol: "torr", dimensions: [.kilogram: 1, .meter: -1, .second: -2], conversionFactor: 133.32) // Torr (Evangelista Torrice)
        unitMap["atm"] = UnitDefinition(symbol: "atm", dimensions: [.kilogram: 1, .meter: -1, .second: -2], conversionFactor: 101325.0) // Standard Atmosphere
        unitMap["bar"] = UnitDefinition(symbol: "bar", dimensions: [.kilogram: 1, .meter: -1, .second: -2], conversionFactor: 100000.0) // Bar
        
        // Energy
        unitMap["BTU"] = UnitDefinition(symbol: "BTU", dimensions: [.kilogram: 1, .meter: 2, .second: -2], conversionFactor: 1055.05585262) // BTU
        
        
        // Speed
        unitMap["mph"] = UnitDefinition(symbol: "mph", dimensions: [.meter: 1, .second: -1], conversionFactor: 0.44704) // Mile per Hour
        unitMap["knot"] = UnitDefinition(symbol: "knot", dimensions: [.meter: 1, .second: -1], conversionFactor: 0.514444) // Knot
        

        // These don't seem to work for other units than mass, volyme and length units
        // Add SI Prefixes (kilo, milli, etc.) to meters, grams, and liters
        let prefixes: [String: Double] = [
            "Q": 1e30, "R": 1e27, "Y": 1e24, "Z": 1e21, "E": 1e18, "P": 1e15,
            "T": 1e12, "G": 1e9, "M": 1e6, "k": 1e3, "h": 1e2, "da": 1e1,
            "d": 1e-1, "c": 1e-2, "m": 1e-3, "u": 1e-6, "µ": 1e-6, "n": 1e-9, "p": 1e-12,
            "f": 1e-15, "a": 1e-18, "z": 1e-21, "y": 1e-24, "r": 1e-27, "q": 1e-30
        ]

        let baseSymbols = [
            "m", "g", "L", "s", "A", "V", "Ohm", "Ω", "W", "Hz",
            "J", "N", "Pa", "F", "H", "T", "S", "Wb", "C",
            "kat", "Bq", "Gy", "Sv", "mol", "Wh"
        ]

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
