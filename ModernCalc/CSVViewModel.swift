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
    @Published var matrixVariableName: String = ""
    @Published var selectedColumns: [Bool]
    @Published var useFirstRowAsHeader: Bool = true
    @Published var errorMessage: String? = nil
    
    // --- MODIFIED: Row properties now use didSet for robust validation ---
    @Published var startRow: Int = 1 {
        didSet {
            // After startRow changes, ensure it's at least 1.
            if startRow <= 0 {
                startRow = 1
            }
            // If the new startRow is greater than endRow, endRow must be adjusted.
            if startRow > endRow {
                endRow = startRow
            }
        }
    }
    @Published var endRow: Int {
        didSet {
            // After endRow changes, ensure it's not less than startRow.
            // If it is, revert to the previous valid value to prevent an invalid state.
            if endRow < startRow {
                endRow = oldValue
            }
        }
    }
    
    // --- MODIFIED: Computed properties are now simpler bridges ---
    var startRowString: String {
        get { "\(startRow)" }
        set {
            // The setter now just attempts to update the underlying Int property.
            // The validation logic is handled by the property's `didSet` observer.
            if let value = Int(newValue) {
                startRow = value
            }
        }
    }
    
    var endRowString: String {
        get { "\(endRow)" }
        set {
            if let value = Int(newValue) {
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
        self.selectedColumns = Array(repeating: true, count: csvData.headers.count)
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
        
        // Apply formatting to the sliced data
        return slicedGrid.map { row in
            row.map { cell in
                formatCell(cell) // Format each cell before displaying it
            }
        }
    }
    
    /// A computed property for the informational message at the bottom of the view.
    var infoMessage: String {
        // If there's an error message, display it instead of the info message.
        if let error = errorMessage {
            return error
        }
        
        let fullGrid = useFirstRowAsHeader ? sourceData.grid : [sourceData.headers] + sourceData.grid
        
        let validStart = max(1, startRow)
        let validEnd = min(fullGrid.count, endRow)
        
        let rowsToImport = max(0, (validEnd - validStart) + 1)
        return "Will import \(rowsToImport) rows."
    }
    
    // MARK: - Intents
    
    /// Assigns the selected columns as a single matrix to the main calculator view model.
    func assignMatrix() {
        // Clear any previous error messages when trying to assign again.
        errorMessage = nil
        
        guard let mainVM = mainViewModel else {
            // This is a programmatic error.
            print("Error: Could not access calculator.")
            return
        }
        
        let trimmedVarName = matrixVariableName.trimmingCharacters(in: .whitespacesAndNewlines)
        guard !trimmedVarName.isEmpty else {
            errorMessage = "Error: Please enter a matrix name."
            return
        }

        let numberOfSelectedColumns = selectedColumns.filter { $0 }.count
        guard numberOfSelectedColumns > 0 else {
            errorMessage = "Error: No columns selected."
            return
        }

        let dataToProcess = useFirstRowAsHeader ? sourceData.grid : [sourceData.headers] + sourceData.grid
        
        let startIndex = max(0, startRow - 1)
        let endIndex = min(dataToProcess.count, endRow)
        
        guard startIndex < endIndex else {
            errorMessage = "Error: Invalid row range."
            return
        }
        
        let slicedRows = dataToProcess[startIndex..<endIndex]
        var matrixValues: [Double] = []
        
        for row in slicedRows {
            for (colIndex, cell) in row.enumerated() {
                // Only include the cell if its column is selected
                if colIndex < selectedColumns.count && selectedColumns[colIndex] {
                    let originalCell = cell.trimmingCharacters(in: .whitespaces)
                    let doubleValue: Double?
                    if settings.decimalSeparator == .comma {
                        doubleValue = Double(originalCell.replacingOccurrences(of: ",", with: "."))
                    } else {
                        doubleValue = Double(originalCell)
                    }
                    // If a cell isn't a valid number, use 0.0 as a default.
                    matrixValues.append(doubleValue ?? 0.0)
                }
            }
        }
        
        let matrix = Matrix(values: matrixValues, rows: slicedRows.count, columns: numberOfSelectedColumns)
        mainVM.variables[trimmedVarName] = .matrix(matrix)
        
        // Hide the window after a short delay
        DispatchQueue.main.asyncAfter(deadline: .now() + 0.25) {
            mainVM.showCSVView = false
        }
    }
    
    /// Formats a string value from a cell if it's a number and rounding is enabled.
    private func formatCell(_ value: String) -> String {
        guard settings.enableCSVRounding,
              let number = Double(value.trimmingCharacters(in: .whitespaces).replacingOccurrences(of: ",", with: ".")) else {
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

