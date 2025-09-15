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
    
    // This flag controls whether changes to properties are saved to UserDefaults.
    private var isPersistenceEnabled = true
    
    // Published properties will automatically update the UI when they change.
    @Published var displayMode: NumberDisplayMode {
        didSet {
            // Only save if persistence is enabled for this instance.
            if isPersistenceEnabled {
                UserDefaults.standard.set(displayMode.rawValue, forKey: "numberDisplayMode")
            }
        }
    }
    
    @Published var decimalSeparator: DecimalSeparator {
        didSet {
            if isPersistenceEnabled {
                UserDefaults.standard.set(decimalSeparator.rawValue, forKey: "decimalSeparator")
            }
        }
    }
    
    @Published var fixedDecimalPlaces: Int {
        didSet {
            if isPersistenceEnabled {
                UserDefaults.standard.set(fixedDecimalPlaces, forKey: "fixedDecimalPlaces")
            }
        }
    }
    
    @Published var enableLiveRounding: Bool {
        didSet {
            if isPersistenceEnabled {
                UserDefaults.standard.set(enableLiveRounding, forKey: "enableLiveRounding")
            }
        }
    }
    
    @Published var livePreviewDecimalPlaces: Int {
        didSet {
            if isPersistenceEnabled {
                UserDefaults.standard.set(livePreviewDecimalPlaces, forKey: "livePreviewDecimalPlaces")
            }
        }
    }
    
    @Published var enableCSVRounding: Bool {
        didSet {
            if isPersistenceEnabled {
                UserDefaults.standard.set(enableCSVRounding, forKey: "enableCSVRounding")
            }
        }
    }
    
    @Published var csvDecimalPlaces: Int {
        didSet {
            if isPersistenceEnabled {
                UserDefaults.standard.set(csvDecimalPlaces, forKey: "csvDecimalPlaces")
            }
        }
    }
    
    @Published var enableSolverRounding: Bool {
        didSet {
            if isPersistenceEnabled {
                UserDefaults.standard.set(enableSolverRounding, forKey: "enableSolverRounding")
            }
        }
    }
    
    @Published var solverDecimalPlaces: Int {
        didSet {
            if isPersistenceEnabled {
                UserDefaults.standard.set(solverDecimalPlaces, forKey: "solverDecimalPlaces")
            }
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
        
        self.enableSolverRounding = defaults.bool(forKey: "enableSolverRounding")
        self.solverDecimalPlaces = defaults.integer(forKey: "solverDecimalPlaces")

        
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
        
        if self.solverDecimalPlaces == 0 {
            self.solverDecimalPlaces = 4 // Default to 4 if not set
        }
    }

    /// Creates a non-persistent, in-memory copy of the settings object.
    /// Changes made to the returned object will not be saved to UserDefaults.
    func makeTemporaryCopy() -> UserSettings {
        let temporarySettings = UserSettings()
        temporarySettings.isPersistenceEnabled = false
        
        // --- FIX: Manually copy all current properties to the new instance ---
        temporarySettings.displayMode = self.displayMode
        temporarySettings.decimalSeparator = self.decimalSeparator
        temporarySettings.fixedDecimalPlaces = self.fixedDecimalPlaces
        temporarySettings.enableLiveRounding = self.enableLiveRounding
        temporarySettings.livePreviewDecimalPlaces = self.livePreviewDecimalPlaces
        temporarySettings.enableCSVRounding = self.enableCSVRounding
        temporarySettings.csvDecimalPlaces = self.csvDecimalPlaces
        temporarySettings.enableSolverRounding = self.enableSolverRounding
        temporarySettings.solverDecimalPlaces = self.solverDecimalPlaces
        
        return temporarySettings
    }
}

