//
//  ModernCalcApp.swift
//  ModernCalc
//
//  Created by Manu Heiskanen on 28.8.2025.
//

import SwiftUI

@main
struct ModernCalcApp: App {
    // Create a single UserSettings object that will be used by the entire app.
    @StateObject private var userSettings = UserSettings()
    
    var body: some Scene {
        WindowGroup {
            // Pass the settings object into the main content view.
            ContentView(settings: userSettings)
        }
    }
}
