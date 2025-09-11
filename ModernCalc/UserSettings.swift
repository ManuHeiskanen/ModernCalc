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
    
    @Published var enableLiveRounding: Bool {
        didSet {
            UserDefaults.standard.set(enableLiveRounding, forKey: "enableLiveRounding")
        }
    }
    
    @Published var livePreviewDecimalPlaces: Int {
        didSet {
            UserDefaults.standard.set(livePreviewDecimalPlaces, forKey: "livePreviewDecimalPlaces")
        }
    }
    
    // --- NEW PROPERTIES FOR CSV ROUNDING ---
    @Published var enableCSVRounding: Bool {
        didSet {
            UserDefaults.standard.set(enableCSVRounding, forKey: "enableCSVRounding")
        }
    }
    
    @Published var csvDecimalPlaces: Int {
        didSet {
            UserDefaults.standard.set(csvDecimalPlaces, forKey: "csvDecimalPlaces")
        }
    }
    
    init() {
        let defaults = UserDefaults.standard
        
        // --- Phase 1: Initialize ALL stored properties from UserDefaults ---
        let savedDisplayMode = defaults.string(forKey: "numberDisplayMode") ?? ""
        self.displayMode = NumberDisplayMode(rawValue: savedDisplayMode) ?? .auto
        
        let savedSeparator = defaults.string(forKey: "decimalSeparator") ?? ""
        self.decimalSeparator = DecimalSeparator(rawValue: savedSeparator) ?? .period
        
        self.fixedDecimalPlaces = defaults.integer(forKey: "fixedDecimalPlaces")
        
        self.enableLiveRounding = defaults.bool(forKey: "enableLiveRounding")
        
        self.livePreviewDecimalPlaces = defaults.integer(forKey: "livePreviewDecimalPlaces")
        
        self.enableCSVRounding = defaults.bool(forKey: "enableCSVRounding")
        self.csvDecimalPlaces = defaults.integer(forKey: "csvDecimalPlaces")

        
        // --- Phase 2: After 'self' is initialized, check and apply default values ---
        if self.fixedDecimalPlaces == 0 {
            self.fixedDecimalPlaces = 4 // Default to 4 if not set
        }
        
        if self.livePreviewDecimalPlaces == 0 {
            self.livePreviewDecimalPlaces = 2 // Default to 2 if not set
        }
        
        if self.csvDecimalPlaces == 0 {
            self.csvDecimalPlaces = 4 // Default to 4 if not set
        }
    }
}
