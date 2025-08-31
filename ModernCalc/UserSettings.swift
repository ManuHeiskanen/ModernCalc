//
//  UserSettings.swift
//  ModernCalc
//
//  Created by Manu Heiskanen on 31.8.2025.
//

import Foundation
import Combine

// Enum for the different ways numbers can be formatted in the UI.
enum NumberDisplayMode: String, Codable, CaseIterable {
    case auto = "Auto"
    case scientific = "Scientific"
    case fixed = "Fixed"
}

// Enum for the decimal separator character.
enum DecimalSeparator: String, Codable, CaseIterable {
    case period = "."
    case comma = ","
    
    var character: Character {
        return self.rawValue.first!
    }
}

// A new class to manage all user settings and their persistence.
class UserSettings: ObservableObject {
    
    // Published properties will automatically update the UI when they change.
    @Published var displayMode: NumberDisplayMode {
        didSet {
            UserDefaults.standard.set(displayMode.rawValue, forKey: "numberDisplayMode")
        }
    }
    
    @Published var decimalSeparator: DecimalSeparator {
        didSet {
            UserDefaults.standard.set(decimalSeparator.rawValue, forKey: "decimalSeparator")
        }
    }
    
    @Published var fixedDecimalPlaces: Int {
        didSet {
            UserDefaults.standard.set(fixedDecimalPlaces, forKey: "fixedDecimalPlaces")
        }
    }
    
    init() {
        // Load saved settings on initialization, or use defaults.
        let defaults = UserDefaults.standard
        
        let savedDisplayMode = defaults.string(forKey: "numberDisplayMode") ?? ""
        self.displayMode = NumberDisplayMode(rawValue: savedDisplayMode) ?? .auto
        
        let savedSeparator = defaults.string(forKey: "decimalSeparator") ?? ""
        self.decimalSeparator = DecimalSeparator(rawValue: savedSeparator) ?? .period
        
        self.fixedDecimalPlaces = defaults.integer(forKey: "fixedDecimalPlaces")
        if self.fixedDecimalPlaces == 0 {
            self.fixedDecimalPlaces = 4 // Default to 4 if not set
        }
    }
}
