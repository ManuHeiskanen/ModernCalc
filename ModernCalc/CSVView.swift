//
//  CSVView.swift
//  ModernCalc
//
//  Created by Manu Heiskanen on 11.9.2025.
//

import SwiftUI

/// A view that displays CSV data in a grid and provides interactive controls for importing it.
struct CSVView: View {
    @ObservedObject var viewModel: CSVViewModel
    
    @Environment(\.dismiss) private var dismiss

    private let cellPadding: CGFloat = 8
    private let cellMinWidth: CGFloat = 100

    var body: some View {
        VStack(spacing: 0) {
            headerView
            Divider()
            ScrollView([.horizontal, .vertical]) {
                gridContent
            }
            Divider()
            controlPanelView
        }
        .frame(minWidth: 700, minHeight: 400)
        .navigationTitle(viewModel.sourceFileName)
    }
    
    /// Creates a binding for the column selection toggle that is safe from out-of-bounds crashes.
    /// This is necessary because when the data is re-parsed with a new separator, the number of columns
    /// can change, and the UI might briefly try to access an index that no longer exists.
    private func safeColumnBinding(for index: Int) -> Binding<Bool> {
        Binding(
            get: {
                // Safely get the value, returning false if the index is out of bounds.
                guard index < viewModel.selectedColumns.count else {
                    return false
                }
                return viewModel.selectedColumns[index]
            },
            set: { newValue in
                // Safely set the value, doing nothing if the index is out of bounds.
                guard index < viewModel.selectedColumns.count else {
                    return
                }
                viewModel.selectedColumns[index] = newValue
            }
        )
    }
    
    @ViewBuilder
    private var gridContent: some View {
        let columnCount = viewModel.headers.count
        
        if columnCount > 0 {
            LazyVGrid(columns: Array(repeating: GridItem(.flexible(minimum: cellMinWidth)), count: columnCount), spacing: 0) {
                let rowCount = viewModel.displayGrid.count
                
                ForEach(0..<(rowCount * columnCount), id: \.self) { index in
                    let rowIndex = index / columnCount
                    let colIndex = index % columnCount
                    
                    if rowIndex < viewModel.displayGrid.count && colIndex < viewModel.displayGrid[rowIndex].count {
                        Text(viewModel.displayGrid[rowIndex][colIndex])
                            .padding(cellPadding)
                            .frame(minWidth: cellMinWidth, alignment: .leading)
                            .background(rowIndex % 2 == 0 ? Color.clear : Color.primary.opacity(0.05))
                            .border(Color.gray.opacity(0.2), width: 0.5)
                    } else {
                        Text("")
                            .padding(cellPadding)
                            .frame(minWidth: cellMinWidth, alignment: .leading)
                            .border(Color.gray.opacity(0.2), width: 0.5)
                    }
                }
            }
        } else {
            Text("No data to display. Try changing the column separator.")
                .padding()
        }
    }
    
    /// **FIXED:** Replaced the direct array access with the `safeColumnBinding` helper.
    /// This creates a robust binding that won't crash even if the underlying array changes size during a view update.
    private var headerView: some View {
        HStack(spacing: 0) {
            ForEach(0..<viewModel.headers.count, id: \.self) { index in
                VStack(spacing: 4) {
                    Text(viewModel.headers[index])
                        .font(.headline)
                        .lineLimit(1)
                        .padding(.top, cellPadding)
                    
                    // Use the safe binding instead of direct access with an `if` check.
                    Toggle("Include", isOn: safeColumnBinding(for: index))
                        .labelsHidden()
                        .padding(.bottom, cellPadding)
                }
                .frame(minWidth: cellMinWidth)
                .border(Color.gray.opacity(0.2), width: 0.5)
            }
        }
        .background(Color.primary.opacity(0.1))
    }
    
    private var controlPanelView: some View {
        VStack(alignment: .leading, spacing: 12) {
            HStack {
                HStack {
                    Text("Separator:").fixedSize()
                    Picker("Separator", selection: $viewModel.columnSeparator) {
                        ForEach(ColumnSeparator.allCases) { separator in
                            Text(separator.rawValue).tag(separator)
                        }
                    }
                    .labelsHidden()
                }
                
                Divider().frame(height: 20)
                
                HStack {
                    Text("Fix Decimals:").fixedSize()
                    Picker("Fix Decimals", selection: $viewModel.decimalConversion) {
                        ForEach(DecimalConversionOption.allCases) { option in
                            Text(option.rawValue).tag(option)
                        }
                    }
                    .labelsHidden()
                }
                
                Spacer()
                
                Toggle("First row is header", isOn: $viewModel.useFirstRowAsHeader)
            }
            
            HStack {
                Text(viewModel.infoMessage)
                    .foregroundColor(viewModel.infoMessage.starts(with: "Error") ? .red : .secondary)
                
                Spacer()
                
                Text("Import Rows:")
                TextField("Start", text: $viewModel.startRowString)
                    .textFieldStyle(RoundedBorderTextFieldStyle())
                    .frame(width: 50)
                Text("to")
                TextField("End", text: $viewModel.endRowString)
                    .textFieldStyle(RoundedBorderTextFieldStyle())
                    .frame(width: 50)

                Divider().frame(height: 20)

                TextField("Matrix Name", text: $viewModel.matrixVariableName)
                    .textFieldStyle(RoundedBorderTextFieldStyle())
                    .frame(width: 150)

                Button("Cancel") {
                    dismiss()
                }
                
                Button("Assign Matrix") {
                    viewModel.assignMatrix()
                }
                .keyboardShortcut(.defaultAction)
            }
        }
        .padding()
        .background(.bar)
    }
}

