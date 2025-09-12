//
//  CSVParser.swift
//  ModernCalc
//
//  Created by Manu Heiskanen on 11.9.2025.
//

import Foundation

/// A simple parser for handling comma-separated value (CSV) data.
struct CSVParser {
    let content: String

    init(content: String) {
        self.content = content
    }

    /// Parses the CSV content into headers and a data grid.
    /// This implementation has been upgraded to correctly handle fields that are enclosed in quotes,
    /// allowing delimiters (like commas) to exist inside the data itself.
    /// - Parameter separator: The character used to separate columns (e.g., ',' or ';').
    /// - Returns: A tuple containing an array of header strings and a 2D array of data strings.
    func parse(separator: Character) -> (headers: [String], grid: [[String]]) {
        // Trim whitespace and newlines from the entire file content first.
        let trimmedContent = content.trimmingCharacters(in: .whitespacesAndNewlines)
        
        // If the file is empty or just contains whitespace, return empty arrays.
        guard !trimmedContent.isEmpty else {
            return ([], [])
        }

        // Split the content into lines, removing any blank lines that might exist.
        var lines = trimmedContent.components(separatedBy: .newlines).filter { !$0.trimmingCharacters(in: .whitespaces).isEmpty }
        
        guard !lines.isEmpty else {
            return ([], [])
        }

        // Assume the first line is the header.
        let headerLine = lines.removeFirst()
        // Split the header line and process each cell.
        let headers = splitAndClean(line: headerLine, separator: separator)
        
        // Process the remaining lines as the data grid.
        let grid = lines.map { line in
            splitAndClean(line: line, separator: separator)
        }
        
        return (headers, grid)
    }
    
    /// **UPDATED:** A more robust parser that correctly handles quoted fields and escaped quotes.
    /// This allows the separator character to exist within a field's content if that field is properly quoted.
    private func splitAndClean(line: String, separator: Character) -> [String] {
        var result = [String]()
        var currentField = ""
        var inQuotes = false
        var i = line.startIndex

        while i < line.endIndex {
            let char = line[i]

            if char == "\"" {
                // Check for an escaped quote (""), which is standard CSV format for a literal quote.
                let nextIndex = line.index(after: i)
                if inQuotes && nextIndex < line.endIndex && line[nextIndex] == "\"" {
                    currentField.append("\"")
                    i = nextIndex // Advance past the second quote to treat it as a single literal
                } else {
                    // It's a regular quote, which toggles the 'inQuotes' state.
                    inQuotes.toggle()
                }
            } else if char == separator && !inQuotes {
                // If we encounter a separator and we're NOT inside a quoted field,
                // finalize the current field and start a new one.
                result.append(currentField)
                currentField = ""
            } else {
                // It's a regular character, so append it to the current field.
                currentField.append(char)
            }
            
            i = line.index(after: i)
        }
        
        // Add the very last field to the result.
        result.append(currentField)

        // Finally, trim whitespace from each parsed field.
        // The more aggressive quote removal from the previous version is no longer needed
        // because this new parser handles them as part of the structure.
        return result.map { $0.trimmingCharacters(in: .whitespaces) }
    }
}

