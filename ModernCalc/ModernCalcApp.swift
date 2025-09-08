//
//  ModernCalcApp.swift
//  ModernCalc
//
//  Created by Manu Heiskanen on 28.8.2025.
//

import SwiftUI

@main
struct ModernCalcApp: App {
    @StateObject private var userSettings = UserSettings()
    // The view model now manages plot windows
    @StateObject private var calculatorViewModel: CalculatorViewModel

    init() {
        let settings = UserSettings()
        _userSettings = StateObject(wrappedValue: settings)
        _calculatorViewModel = StateObject(wrappedValue: CalculatorViewModel(settings: settings))
    }
    
    var body: some Scene {
        WindowGroup {
            ContentView(settings: userSettings, viewModel: calculatorViewModel)
        }
        .commands {
            // You can add custom commands for your app here if needed
        }
        
        // This WindowGroup will handle all the plot windows.
        // SwiftUI will automatically create a new window for each item in the plotData array.
        WindowGroup(for: PlotData.ID.self) { $plotID in
            if let plotViewModel = calculatorViewModel.plotViewModels.first(where: { $0.plotData.id == plotID }) {
                PlotView(viewModel: plotViewModel)
                    .onDisappear {
                        // When a plot window is closed, remove its view model.
                        calculatorViewModel.closePlotWindow(id: plotID)
                    }
            } else {
                Text("Plot data not available.")
            }
        }
    }
}
