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

/// Defines the decimal conversion options available in the UI.
enum DecimalConversionOption: String, CaseIterable, Identifiable {
    case none = "None"
    case periodToComma = "Convert . to ,"
    case commaToPeriod = "Convert , to ."
    
    var id: String { self.rawValue }
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
    
    // --- NEW UI State Properties ---
    var columnSeparator: ColumnSeparator = .comma {
        didSet {
            // Re-parse data whenever the separator is changed.
            reparseData()
        }
    }
    var decimalConversion: DecimalConversionOption = .none
    
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
                let convertedCell = applyDecimalConversion(to: cell)
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
        return "Will import \(rowsToImport) rows."
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
                    let convertedCell = applyDecimalConversion(to: cell)
                    let standardizedCell = convertedCell.trimmingCharacters(in: .whitespaces).replacingOccurrences(of: ",", with: ".")
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
    
    /// **FIXED:** Correctly calculates the total number of rows when the header is treated as data.
    private func updateRowCounts() {
        let headerAsDataRow = (useFirstRowAsHeader == false && !parsedHeaders.isEmpty) ? 1 : 0
        let totalRows = parsedGrid.count + headerAsDataRow
        self.startRow = totalRows > 0 ? 1 : 0
        self.endRow = totalRows
    }
    
    private func applyDecimalConversion(to cell: String) -> String {
        switch decimalConversion {
        case .none:
            return cell
        case .periodToComma:
            return cell.replacingOccurrences(of: ".", with: ",")
        case .commaToPeriod:
            return cell.replacingOccurrences(of: ",", with: ".")
        }
    }
    
    private func formatCell(_ value: String) -> String {
        guard settings.enableCSVRounding,
              let number = Double(value.trimmingCharacters(in: .whitespaces).replacingOccurrences(of: ",", with: ".")) else {
            return value
        }
        
        let formattedString = String(format: "%.\(settings.csvDecimalPlaces)f", number)
        
        if settings.decimalSeparator == .comma {
            return formattedString.replacingOccurrences(of: ".", with: ",")
        }
        
        return formattedString
    }
}
