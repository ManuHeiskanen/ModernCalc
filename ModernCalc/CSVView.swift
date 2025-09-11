//
//  CSVView.swift
//  ModernCalc
//
//  Created by Manu Heiskanen on 11.9.2025.
//

import SwiftUI

/// A view that displays CSV data in a grid and provides interactive controls for importing it.
struct CSVView: View {
    // Use @ObservedObject when a view receives an existing object that it doesn't own.
    @ObservedObject var viewModel: CSVViewModel
    
    // Used to dismiss the window programmatically.
    @Environment(\.dismiss) private var dismiss

    private let cellPadding: CGFloat = 8
    private let cellMinWidth: CGFloat = 100

    var body: some View {
        VStack(spacing: 0) {
            // --- Header row with column selection toggles ---
            headerView
            
            Divider()

            // --- Main data grid ---
            ScrollView([.horizontal, .vertical]) {
                // The LazyVGrid and its contents have been extracted to a computed property
                // to help the compiler with type-checking.
                gridContent
            }
            
            Divider()
            
            // --- Control panel at the bottom ---
            controlPanelView
        }
        .frame(minWidth: 600, minHeight: 400)
        .navigationTitle(viewModel.sourceData.fileName)
    }
    
    /// A computed property for the main data grid's content.
    /// This has been updated to use a single ForEach loop to prevent duplicate view IDs.
    @ViewBuilder
    private var gridContent: some View {
        let columnCount = viewModel.headers.count
        
        // A LazyVGrid requires a non-zero number of columns to be initialized.
        if columnCount > 0 {
            LazyVGrid(columns: Array(repeating: GridItem(.flexible(minimum: cellMinWidth)), count: columnCount), spacing: 0) {
                let rowCount = viewModel.displayGrid.count
                
                // We iterate over a single, unique range from 0 to the total number of cells.
                // This provides a unique ID (\.self, i.e., the index) for each cell view,
                // which resolves the "ID used by multiple child views" error.
                ForEach(0..<(rowCount * columnCount), id: \.self) { index in
                    // Calculate the row and column from the flat index.
                    let rowIndex = index / columnCount
                    let colIndex = index % columnCount
                    
                    // This defensive check handles jagged data (rows with inconsistent column counts)
                    // and prevents the app from crashing.
                    if rowIndex < viewModel.displayGrid.count && colIndex < viewModel.displayGrid[rowIndex].count {
                        Text(viewModel.displayGrid[rowIndex][colIndex])
                            .padding(cellPadding)
                            .frame(minWidth: cellMinWidth, alignment: .leading)
                            .background(rowIndex % 2 == 0 ? Color.clear : Color.primary.opacity(0.05))
                            .border(Color.gray.opacity(0.2), width: 0.5)
                    } else {
                        // If data is missing for a cell, display an empty placeholder
                        // to keep the grid aligned correctly.
                        Text("")
                            .padding(cellPadding)
                            .frame(minWidth: cellMinWidth, alignment: .leading)
                            .border(Color.gray.opacity(0.2), width: 0.5)
                    }
                }
            }
        } else {
            Text("No data to display.")
                .padding()
        }
    }
    
    /// The view for the column headers and selection toggles.
    private var headerView: some View {
        HStack(spacing: 0) {
            ForEach(0..<viewModel.headers.count, id: \.self) { index in
                VStack(spacing: 4) {
                    Text(viewModel.headers[index])
                        .font(.headline)
                        .padding(.top, cellPadding)
                    
                    Toggle("Include", isOn: $viewModel.selectedColumns[index])
                        .labelsHidden()
                        .padding(.bottom, cellPadding)
                }
                .frame(minWidth: cellMinWidth)
                .border(Color.gray.opacity(0.2), width: 0.5)
            }
        }
        .background(Color.primary.opacity(0.1))
    }
    
    /// The view for the controls like checkboxes, row ranges, and the final import button.
    private var controlPanelView: some View {
        VStack {
            HStack {
                Toggle("First row contains headers", isOn: $viewModel.useFirstRowAsHeader)
                
                Spacer()
                
                Text("Import Rows:")
                TextField("Start", text: $viewModel.startRowString)
                    .textFieldStyle(RoundedBorderTextFieldStyle())
                    .frame(width: 50)
                Text("to")
                TextField("End", text: $viewModel.endRowString)
                    .textFieldStyle(RoundedBorderTextFieldStyle())
                    .frame(width: 50)
            }
            .padding()

            HStack {
                Text(viewModel.infoMessage)
                    .foregroundColor(viewModel.infoMessage.starts(with: "Error") ? .red : .secondary)
                
                Spacer()

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
            .padding([.horizontal, .bottom])
        }
        .background(.bar)
    }
}
