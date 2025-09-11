//
//  CSVViewModel.swift
//  ModernCalc
//
//  Created by Manu Heiskanen on 10.9.2025.
//

import Foundation
import SwiftUI

@MainActor
class CSVViewModel: ObservableObject {
    
    // The raw data from the CSV file.
    let sourceData: CSVData
    
    // The user's settings, passed in to control formatting.
    let settings: UserSettings
    
    // A weak reference to the main view model to assign variables back to it.
    private weak var mainViewModel: CalculatorViewModel?
    
    // --- UI State Properties ---
    @Published var columnVariableNames: [String]
    @Published var useFirstRowAsHeader: Bool = true {
        didSet { updateInfoMessage() }
    }
    @Published var startRow: Int = 1 {
        didSet {
            // Ensure start row is not greater than end row
            if startRow > endRow { endRow = startRow }
            updateInfoMessage()
        }
    }
    @Published var endRow: Int {
        didSet {
            // Ensure end row is not less than start row
            if endRow < startRow { startRow = endRow }
            updateInfoMessage()
        }
    }
    @Published var infoMessage: String = ""

    // --- Computed Properties for TextField Bindings ---
    // These act as a bridge between the Int model values and the String UI values.
    var startRowString: String {
        get { "\(startRow)" }
        set {
            if let value = Int(newValue), value > 0 {
                startRow = value
            }
        }
    }
    
    var endRowString: String {
        get { "\(endRow)" }
        set {
            if let value = Int(newValue), value >= startRow {
                endRow = value
            }
        }
    }

    init(csvData: CSVData, mainViewModel: CalculatorViewModel, settings: UserSettings) {
        self.sourceData = csvData
        self.mainViewModel = mainViewModel
        self.settings = settings
        
        let totalRows = csvData.grid.count + (csvData.headers.isEmpty ? 0 : 1)
        
        // Initialize state properties
        self.endRow = totalRows
        self.columnVariableNames = Array(repeating: "", count: csvData.headers.count)
        
        // Set the initial info message
        updateInfoMessage()
    }
    
    // --- Computed Properties for Display ---
    
    /// The effective headers to be displayed in the view, accounting for the toggle.
    var headers: [String] {
        if useFirstRowAsHeader {
            return sourceData.headers
        } else {
            // Generate generic headers if the file's first row is data
            return (1...sourceData.headers.count).map { "Column \($0)" }
        }
    }
    
    /// The grid data to be displayed, sliced based on the selected row range.
    var displayGrid: [[String]] {
        // The full dataset includes the header row if 'useFirstRowAsHeader' is false.
        let fullGrid = useFirstRowAsHeader ? sourceData.grid : [sourceData.headers] + sourceData.grid
        
        let startIndex = max(0, startRow - 1)
        let endIndex = min(fullGrid.count, endRow)
        
        guard startIndex < endIndex else { return [] }
        
        let slicedGrid = Array(fullGrid[startIndex..<endIndex])
        
        // --- FIX: Apply formatting to the sliced data ---
        return slicedGrid.map { row in
            row.map { cell in
                formatCell(cell) // Format each cell before displaying it
            }
        }
    }
    
    // MARK: - Intents
    
    /// Assigns the selected columns as vectors to the main calculator view model.
    func assignVariables() {
        guard let mainVM = mainViewModel else {
            infoMessage = "Error: Could not access calculator."
            return
        }
        
        var assignedCount = 0
        // Determine the correct data source based on whether the first row is a header.
        let dataToProcess = useFirstRowAsHeader ? sourceData.grid : [sourceData.headers] + sourceData.grid
        
        for (colIndex, varName) in columnVariableNames.enumerated() {
            let trimmedVarName = varName.trimmingCharacters(in: .whitespacesAndNewlines)
            guard !trimmedVarName.isEmpty else { continue }
            
            let startIndex = max(0, startRow - 1)
            let endIndex = min(dataToProcess.count, endRow)
            
            guard startIndex < endIndex else {
                infoMessage = "Error: Invalid row range."
                return
            }
            
            // Extract the numerical data for the current column from the specified range.
            let columnData = dataToProcess[startIndex..<endIndex].compactMap { row -> Double? in
                guard colIndex < row.count else { return nil }
                // Use the original string for conversion, not the formatted one
                let originalCell = row[colIndex].trimmingCharacters(in: .whitespaces)
                if settings.decimalSeparator == .comma {
                    return Double(originalCell.replacingOccurrences(of: ",", with: "."))
                }
                return Double(originalCell)
            }

            let vector = Vector(values: columnData)
            mainVM.variables[trimmedVarName] = .vector(vector)
            assignedCount += 1
        }
        
        if assignedCount > 0 {
            infoMessage = "Assigned \(assignedCount) variable(s)."
            // Hide the window after a short delay
            DispatchQueue.main.asyncAfter(deadline: .now() + 0.5) {
                mainVM.showCSVView = false
            }
        } else {
            infoMessage = "Error: No variable names entered."
        }
    }
    
    /// Updates the informational message at the bottom of the view.
    private func updateInfoMessage() {
        let fullGrid = useFirstRowAsHeader ? sourceData.grid : [sourceData.headers] + sourceData.grid
        
        let validStart = max(1, startRow)
        let validEnd = min(fullGrid.count, endRow)
        
        let rowsToImport = max(0, (validEnd - validStart) + 1)
        infoMessage = "Will import \(rowsToImport) rows."
    }
    
    // --- FIX: New helper function to format cell values based on settings ---
    /// Formats a string value from a cell if it's a number and rounding is enabled.
    private func formatCell(_ value: String) -> String {
        guard settings.enableCSVRounding,
              let number = Double(value.trimmingCharacters(in: .whitespaces)) else {
            return value
        }
        
        // Format the number to the correct number of decimal places.
        let formattedString = String(format: "%.\(settings.csvDecimalPlaces)f", number)
        
        // Apply the correct decimal separator.
        if settings.decimalSeparator == .comma {
            return formattedString.replacingOccurrences(of: ".", with: ",")
        }
        
        return formattedString
    }
}
