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
            // --- Header row with variable name input fields ---
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
    /// Breaking this out helps the Swift compiler avoid "unable to type-check" errors.
    private var gridContent: some View {
        LazyVGrid(columns: Array(repeating: GridItem(.flexible(minimum: cellMinWidth)), count: viewModel.headers.count), spacing: 0) {
            // Iterate over the rows of the displayable grid data.
            ForEach(0..<viewModel.displayGrid.count, id: \.self) { rowIndex in
                // For each row, iterate over the columns.
                let row = viewModel.displayGrid[rowIndex]
                ForEach(0..<row.count, id: \.self) { colIndex in
                    Text(row[colIndex])
                        .padding(cellPadding)
                        .frame(minWidth: cellMinWidth, alignment: .leading)
                        .background(rowIndex % 2 == 0 ? Color.clear : Color.primary.opacity(0.05))
                        .border(Color.gray.opacity(0.2), width: 0.5)
                }
            }
        }
    }
    
    /// The view for the column headers and variable assignment text fields.
    private var headerView: some View {
        HStack(spacing: 0) {
            ForEach(0..<viewModel.headers.count, id: \.self) { index in
                VStack(spacing: 4) {
                    Text(viewModel.headers[index])
                        .font(.headline)
                        .padding(.top, cellPadding)
                    
                    TextField("Variable Name", text: $viewModel.columnVariableNames[index])
                        .textFieldStyle(RoundedBorderTextFieldStyle())
                        .padding(.horizontal, 4)
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
                    .frame(width: 50)
                Text("to")
                TextField("End", text: $viewModel.endRowString)
                    .frame(width: 50)
            }
            .padding()

            HStack {
                Text(viewModel.infoMessage)
                    .foregroundColor(viewModel.infoMessage.starts(with: "Error") ? .red : .secondary)
                
                Spacer()

                Button("Cancel") {
                    dismiss()
                }
                
                Button("Assign Variables") {
                    viewModel.assignVariables()
                }
                .keyboardShortcut(.defaultAction)
            }
            .padding([.horizontal, .bottom])
        }
        .background(.bar)
    }
}
