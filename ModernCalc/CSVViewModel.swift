//
//  CSVViewModel.swift
//  ModernCalc
//
//  Created by Manu Heiskanen on 10.9.2025.
//

import Foundation
import SwiftUI

// MARK: - Enums for UI Controls

/// Defines the available column separators for parsing.
enum ColumnSeparator: String, CaseIterable, Identifiable {
    case comma = "Comma (,)"
    case semicolon = "Semicolon (;)"

    var id: String { self.rawValue }

    var character: Character {
        switch self {
        case .comma: return ","
        case .semicolon: return ";"
        }
    }
}

@Observable
@MainActor
class CSVViewModel {
    
    // The file name and raw content are stored directly for re-parsing.
    let sourceFileName: String
    let rawContent: String
    
    // The user's settings, passed in to control formatting.
    let settings: UserSettings
    
    // A weak reference to the main view model to assign variables back to it.
    private weak var mainViewModel: CalculatorViewModel?
    
    // --- UI State Properties ---
    var columnSeparator: ColumnSeparator = .comma {
        didSet {
            // Re-parse data whenever the separator is changed.
            reparseData()
        }
    }
    
    // --- Properties to hold the currently parsed data ---
    private var parsedHeaders: [String] = []
    private var parsedGrid: [[String]] = []
    
    // --- UI State Properties ---
    var matrixVariableName: String = ""
    var selectedColumns: [Bool] = []
    var useFirstRowAsHeader: Bool = true {
        didSet {
            // No need to re-parse, just update the effective row count.
            updateRowCounts()
        }
    }
    var errorMessage: String? = nil
    
    // --- Row properties now use didSet for robust validation ---
    var startRow: Int = 1 {
        didSet {
            if startRow <= 0 { startRow = 1 }
            if startRow > endRow { endRow = startRow }
        }
    }
    var endRow: Int = 0 {
        didSet {
            if endRow < startRow { endRow = oldValue }
            if endRow > totalRowCount { endRow = totalRowCount }
        }
    }
    
    // --- Computed properties are now simpler bridges ---
    var startRowString: String {
        get { "\(startRow)" }
        set { if let value = Int(newValue) { startRow = value } }
    }
    
    var endRowString: String {
        get { "\(endRow)" }
        set { if let value = Int(newValue) { endRow = value } }
    }

    init(fileName: String, content: String, mainViewModel: CalculatorViewModel, settings: UserSettings) {
        self.sourceFileName = fileName
        self.rawContent = content
        self.mainViewModel = mainViewModel
        self.settings = settings
        
        // Perform the initial parse based on default settings.
        reparseData()
    }
    
    // --- Computed Properties for Display ---
    
    var headers: [String] {
        if useFirstRowAsHeader {
            return parsedHeaders
        } else {
            guard !parsedHeaders.isEmpty else { return [] }
            return (1...parsedHeaders.count).map { "Column \($0)" }
        }
    }
    
    var displayGrid: [[String]] {
        let fullGrid = useFirstRowAsHeader ? parsedGrid : [parsedHeaders] + parsedGrid
        
        let startIndex = max(0, startRow - 1)
        let endIndex = min(fullGrid.count, endRow)
        
        guard startIndex < endIndex else { return [] }
        
        let slicedGrid = Array(fullGrid[startIndex..<endIndex])
        
        return slicedGrid.map { row in
            row.map { cell in
                // Apply automatic decimal conversion and then format the cell.
                let convertedCell = applyAutomaticDecimalConversion(to: cell)
                return formatCell(convertedCell)
            }
        }
    }
    
    var infoMessage: String {
        if let error = errorMessage { return error }
        
        let fullGrid = useFirstRowAsHeader ? parsedGrid : [parsedHeaders] + parsedGrid
        let validStart = max(1, startRow)
        let validEnd = min(fullGrid.count, endRow)
        
        let rowsToImport = max(0, (validEnd - validStart) + 1)
        return "Importing \(rowsToImport)/\(totalRowCount) rows"
    }
    
    /// A computed property that always returns the total number of available rows.
    var totalRowCount: Int {
        let headerAsDataRow = (useFirstRowAsHeader == false && !parsedHeaders.isEmpty) ? 1 : 0
        return parsedGrid.count + headerAsDataRow
    }
    
    // MARK: - Intents
    
    func assignMatrix() {
        errorMessage = nil
        guard let mainVM = mainViewModel else { return }
        
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

        let dataToProcess = useFirstRowAsHeader ? parsedGrid : [parsedHeaders] + parsedGrid
        
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
                if colIndex < selectedColumns.count && selectedColumns[colIndex] {
                    // Use the automatic conversion for the final assignment as well.
                    let standardizedCell = applyAutomaticDecimalConversion(to: cell).trimmingCharacters(in: .whitespaces)
                    matrixValues.append(Double(standardizedCell) ?? 0.0)
                }
            }
        }
        
        let matrix = Matrix(values: matrixValues, rows: slicedRows.count, columns: numberOfSelectedColumns)
        mainVM.variables[trimmedVarName] = .matrix(matrix)
        
        DispatchQueue.main.asyncAfter(deadline: .now() + 0.25) {
            mainVM.showCSVView = false
        }
    }
    
    // MARK: - Private Helpers
    
    private func reparseData() {
        let parser = CSVParser(content: self.rawContent)
        (self.parsedHeaders, self.parsedGrid) = parser.parse(separator: columnSeparator.character)
        
        self.selectedColumns = Array(repeating: true, count: parsedHeaders.count)
        updateRowCounts()
        self.errorMessage = nil
    }
    
    private func updateRowCounts() {
        let totalRows = self.totalRowCount
        self.startRow = totalRows > 0 ? 1 : 0
        self.endRow = totalRows
    }
    
    /// **UPDATED:** This function now automatically converts CSV decimal separators to match the app's settings.
    /// It makes the import process seamless without requiring manual user intervention.
    private func applyAutomaticDecimalConversion(to cell: String) -> String {
        // If the app's preferred separator is a period, any commas in the CSV are treated as decimal separators and converted.
        if settings.decimalSeparator == .period {
            return cell.replacingOccurrences(of: ",", with: ".")
        }
        // If the app's preferred separator is a comma, any periods in the CSV are treated as decimal separators and converted.
        else {
            return cell.replacingOccurrences(of: ".", with: ",")
        }
    }
    
    private func formatCell(_ value: String) -> String {
        // First, standardize the string to use a period for Double conversion, regardless of display format.
        let standardValue = value.trimmingCharacters(in: .whitespaces).replacingOccurrences(of: ",", with: ".")
        
        guard settings.enableCSVRounding, let number = Double(standardValue) else {
            return value
        }
        
        let formattedString = String(format: "%.\(settings.csvDecimalPlaces)f", number)
        
        // After formatting, apply the user's preferred decimal separator for display.
        if settings.decimalSeparator == .comma {
            return formattedString.replacingOccurrences(of: ".", with: ",")
        }
        
        return formattedString
    }
}


