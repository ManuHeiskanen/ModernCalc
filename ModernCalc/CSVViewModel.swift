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
        self.selectedColumns = Array(repeating: true, count: csvData.headers.count)
        
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
    
    /// Assigns the selected columns as a single matrix to the main calculator view model.
    func assignMatrix() {
        guard let mainVM = mainViewModel else {
            infoMessage = "Error: Could not access calculator."
            return
        }
        
        let trimmedVarName = matrixVariableName.trimmingCharacters(in: .whitespacesAndNewlines)
        guard !trimmedVarName.isEmpty else {
            infoMessage = "Error: Please enter a name for the matrix."
            return
        }

        let numberOfSelectedColumns = selectedColumns.filter { $0 }.count
        guard numberOfSelectedColumns > 0 else {
            infoMessage = "Error: No columns selected for the matrix."
            return
        }

        let dataToProcess = useFirstRowAsHeader ? sourceData.grid : [sourceData.headers] + sourceData.grid
        
        let startIndex = max(0, startRow - 1)
        let endIndex = min(dataToProcess.count, endRow)
        
        guard startIndex < endIndex else {
            infoMessage = "Error: Invalid row range."
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
        
        infoMessage = "Assigned \(trimmedVarName) as a \(matrix.rows)x\(matrix.columns) matrix."
        
        // Hide the window after a short delay
        DispatchQueue.main.asyncAfter(deadline: .now() + 0.75) {
            mainVM.showCSVView = false
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

