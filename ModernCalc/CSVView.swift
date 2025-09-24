//
//  CSVView.swift
//  ModernCalc
//
//  Created by Manu Heiskanen on 11.9.2025.
//

import SwiftUI

/// A view that displays CSV data in a grid and provides interactive controls for importing it,
/// styled with the "LiquidGlass" aesthetic.
struct CSVView: View {
    @Bindable var viewModel: CSVViewModel
    
    @Environment(\.dismiss) private var dismiss

    var body: some View {
        VStack(spacing: 0) {
            Text(viewModel.sourceFileName)
                .font(.title2)
                .fontWeight(.medium)
                .padding(.top)

            // The main content area with header, grid, and controls.
            CSVHeaderView(viewModel: viewModel)
            
            Divider()
            
            CSVGridView(viewModel: viewModel)
            
            Divider()

            CSVControlPanelView(viewModel: viewModel, dismiss: dismiss)
        }
        .frame(minWidth: 800, minHeight: 500)
        .background(.regularMaterial)
    }
}

// MARK: - Subviews for better organization

/// The header view that displays column titles and selection toggles.
private struct CSVHeaderView: View {
    @Bindable var viewModel: CSVViewModel
    private let cellMinWidth: CGFloat = 120

    var body: some View {
        ScrollView(.horizontal, showsIndicators: false) {
            HStack(spacing: 0) {
                ForEach(0..<viewModel.headers.count, id: \.self) { index in
                    HeaderCellView(
                        title: viewModel.headers[index],
                        isSelected: safeColumnBinding(for: index)
                    )
                    .frame(minWidth: cellMinWidth)
                }
            }
        }
        .frame(height: 60)
    }
    
    /// Creates a binding for the column selection toggle that is safe from out-of-bounds crashes.
    private func safeColumnBinding(for index: Int) -> Binding<Bool> {
        Binding(
            get: {
                guard index < viewModel.selectedColumns.count else { return false }
                return viewModel.selectedColumns[index]
            },
            set: { newValue in
                guard index < viewModel.selectedColumns.count else { return }
                viewModel.selectedColumns[index] = newValue
            }
        )
    }
}

/// A single cell within the header.
private struct HeaderCellView: View {
    let title: String
    @Binding var isSelected: Bool
    @State private var isHovering = false

    var body: some View {
        VStack(spacing: 4) {
            Text(title)
                .font(.headline)
                .lineLimit(1)
            
            Image(systemName: isSelected ? "checkmark.circle.fill" : "circle")
                .foregroundColor(isSelected ? .accentColor : .secondary)
                .font(.body)
        }
        .padding(.vertical, 8)
        .frame(maxWidth: .infinity)
        .background(isHovering ? Color.primary.opacity(0.1) : Color.clear)
        .cornerRadius(8)
        .onHover { hovering in
            withAnimation(.easeInOut(duration: 0.1)) {
                isHovering = hovering
            }
        }
        .onTapGesture {
            withAnimation(.spring()) {
                isSelected.toggle()
            }
        }
    }
}


/// The scrollable grid that displays the CSV data.
private struct CSVGridView: View {
    @Bindable var viewModel: CSVViewModel
    private let cellMinWidth: CGFloat = 120
    private let cellPadding: CGFloat = 8

    var body: some View {
        ScrollView([.horizontal, .vertical]) {
            let columnCount = viewModel.headers.count
            
            if columnCount > 0 {
                LazyVGrid(columns: Array(repeating: GridItem(.flexible(minimum: cellMinWidth)), count: columnCount), spacing: 0) {
                    let grid = viewModel.displayGrid
                    let rowCount = grid.count
                    
                    // A single ForEach is more robust for LazyVGrid than nested loops.
                    // This iterates through each cell index and calculates its row and column.
                    ForEach(0..<(rowCount * columnCount), id: \.self) { index in
                        let rowIndex = index / columnCount
                        let colIndex = index % columnCount
                        
                        // Safety check for jagged data (rows with different numbers of columns)
                        if colIndex < grid[rowIndex].count {
                            Text(grid[rowIndex][colIndex])
                                .font(.system(.body, design: .monospaced))
                                .padding(cellPadding)
                                .frame(minWidth: cellMinWidth, alignment: .leading)
                                .background(rowIndex % 2 != 0 ? Color.clear : Color.primary.opacity(0.04))
                        } else {
                            // Render an empty cell to maintain grid alignment
                            Text("")
                                .padding(cellPadding)
                                .frame(minWidth: cellMinWidth, alignment: .leading)
                        }
                    }
                }
            } else {
                Text("No data to display. Try changing the column separator.")
                    .foregroundColor(.secondary)
                    .padding()
                    .frame(maxWidth: .infinity, maxHeight: .infinity)
            }
        }
    }
}

/// The bottom control panel for import settings and actions.
private struct CSVControlPanelView: View {
    @Bindable var viewModel: CSVViewModel
    let dismiss: DismissAction
    @State private var isAssignHovered = false

    var body: some View {
        VStack(spacing: 16) {
            // First row of controls: Separator, Decimals, Header Toggle
            HStack(spacing: 20) {
                CustomPicker(title: "Separator", selection: $viewModel.columnSeparator) { option in
                    String(option.rawValue.split(separator: " ").first ?? "")
                }
                
                Spacer()
                
                Toggle("First row is header", isOn: $viewModel.useFirstRowAsHeader)
                    .toggleStyle(.switch)
            }
            
            // Second row of controls: Info, Row Selection, Matrix Name, Actions
            HStack(spacing: 12) {
                Text(viewModel.infoMessage)
                    .font(.callout)
                    .foregroundColor(viewModel.errorMessage != nil ? .red : .secondary)
                
                Spacer()
                
                Text("Import Rows:")
                StyledTextField(placeholder: "Start", text: $viewModel.startRowString)
                Text("to")
                StyledTextField(placeholder: "End", text: $viewModel.endRowString)
                
                Divider().frame(height: 20)
                
                Text("Assign Matrix Name:")
                StyledTextField(placeholder: "e.g., my_data", text: $viewModel.matrixVariableName, width: 150)
                
                Button("Cancel") {
                    dismiss()
                }
                .buttonStyle(.plain)
                
                Button("Assign Matrix") {
                    viewModel.assignMatrix()
                }
                .buttonStyle(.plain)
                .keyboardShortcut(.defaultAction)
                .padding(.horizontal, 16)
                .padding(.vertical, 8)
                .background(isAssignHovered ? Color.accentColor.opacity(0.9) : Color.accentColor, in: Capsule())
                .foregroundColor(.white)
                .onHover { hovering in
                    withAnimation(.easeInOut(duration: 0.1)) {
                        isAssignHovered = hovering
                    }
                }
            }
        }
        .padding()
    }
}

// MARK: - Custom Reusable Components

/// A custom picker styled to look like a segmented control, now with a label closure for flexibility.
private struct CustomPicker<T: CaseIterable & Hashable>: View {
    let title: String
    @Binding var selection: T
    let label: (T) -> String
    
    var body: some View {
        HStack {
            Text("\(title):").fixedSize()
            HStack(spacing: 0) {
                ForEach(Array(T.allCases), id: \.self) { option in
                    Button(action: {
                        withAnimation(.spring(response: 0.3, dampingFraction: 0.7)) {
                            selection = option
                        }
                    }) {
                        Text(label(option))
                            .padding(.horizontal, 12)
                            .padding(.vertical, 4)
                            .foregroundColor(selection == option ? .primary : .secondary)
                    }
                    .background {
                        if selection == option {
                            Capsule()
                                .fill(Color.primary.opacity(0.1))
                                .shadow(radius: 1)
                        }
                    }
                }
            }
            .buttonStyle(.plain)
            .background(.ultraThinMaterial, in: Capsule())
        }
    }
}


/// A custom TextField with a material background.
private struct StyledTextField: View {
    let placeholder: String
    @Binding var text: String
    var width: CGFloat = 50
    
    var body: some View {
        TextField(placeholder, text: $text)
            .textFieldStyle(.plain)
            .multilineTextAlignment(.center)
            .padding(.horizontal, 8)
            .padding(.vertical, 4)
            .background(.thinMaterial, in: RoundedRectangle(cornerRadius: 6))
            .frame(width: width)
    }
}

