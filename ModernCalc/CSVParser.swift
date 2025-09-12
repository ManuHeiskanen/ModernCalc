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
    /// This is a basic implementation and doesn't handle complex cases like quoted fields.
    /// - Parameter separator: The character used to separate columns.
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
        // Split the header line by the specified separator and trim whitespace.
        let headers = headerLine.components(separatedBy: String(separator)).map { $0.trimmingCharacters(in: .whitespaces) }
        
        // Process the remaining lines as the data grid.
        let grid = lines.map { line in
            line.components(separatedBy: String(separator)).map { $0.trimmingCharacters(in: .whitespaces) }
        }
        
        return (headers, grid)
    }
}

