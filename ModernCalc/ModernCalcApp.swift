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
                    // Add a background view that uses the accessor to get the window
                    .background(WindowAccessor { window in
                        // This tells macOS not to save and restore this specific window
                        window?.isRestorable = false
                    })
            } else {
                Text("Plot data not available.")
            }
        }
        .defaultSize(width: 480, height: 650)
    }
}

// A helper view to access the underlying NSWindow of a SwiftUI view.
struct WindowAccessor: NSViewRepresentable {
    var callback: (NSWindow?) -> Void

    func makeNSView(context: Context) -> NSView {
        let view = NSView()
        // The window is available after the view is added to the hierarchy.
        // We use DispatchQueue.main.async to safely access it.
        DispatchQueue.main.async {
            self.callback(view.window)
        }
        return view
    }

    func updateNSView(_ nsView: NSView, context: Context) {}
}
