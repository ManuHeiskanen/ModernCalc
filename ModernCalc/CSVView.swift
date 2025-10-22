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
    
    private let cellMinWidth: CGFloat = 120

    var body: some View {
        VStack(spacing: 0) {
            Text(viewModel.sourceFileName)
                .font(.title2)
                .fontWeight(.medium)
                .padding(.top)

            // Define the grid columns once to be shared by header and data grid.
            let columns = Array(repeating: GridItem(.flexible(minimum: cellMinWidth)), count: viewModel.headers.count)
            
            // to ensure their column widths are calculated identically and they scroll in sync.
            if !columns.isEmpty {
                ScrollView([.horizontal, .vertical]) {
                    VStack(spacing: 0) {
                        CSVHeaderView(viewModel: viewModel, columns: columns)
                        Divider()
                        CSVGridView(viewModel: viewModel, columns: columns)
                    }
                }
            } else {
                // Display a message if there's no data to form columns.
                Spacer()
                Text("No data to display. Try changing the column separator.")
                    .foregroundColor(.secondary)
                Spacer()
            }
            
            Divider()

            CSVControlPanelView(viewModel: viewModel, dismiss: dismiss)
        }
        .frame(minWidth: 800, minHeight: 500)
        .background(.regularMaterial)
    }
}

// MARK: - Subviews for better organization

/// The header view, now using LazyVGrid to perfectly align with the data grid.
private struct CSVHeaderView: View {
    @Bindable var viewModel: CSVViewModel
    let columns: [GridItem]

    var body: some View {
        LazyVGrid(columns: columns, spacing: 0) {
            ForEach(0..<viewModel.headers.count, id: \.self) { index in
                HeaderCellView(
                    title: viewModel.headers[index],
                    isSelected: safeColumnBinding(for: index)
                )
            }
        }
        .frame(height: 60)
    }
    
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


/// The scrollable grid that displays the CSV data, now without its own ScrollView.
private struct CSVGridView: View {
    @Bindable var viewModel: CSVViewModel
    let columns: [GridItem]
    private let cellPadding: CGFloat = 8

    var body: some View {
        LazyVGrid(columns: columns, spacing: 0) {
            let grid = viewModel.displayGrid
            let rowCount = grid.count
            let columnCount = columns.count
            
            ForEach(0..<(rowCount * columnCount), id: \.self) { index in
                let rowIndex = index / columnCount
                let colIndex = index % columnCount
                
                if colIndex < grid[rowIndex].count {
                    Text(grid[rowIndex][colIndex])
                        .font(.system(.body, design: .monospaced))
                        .padding(cellPadding)
                        .frame(maxWidth: .infinity, alignment: .center)
                        .background(rowIndex % 2 != 0 ? Color.clear : Color.primary.opacity(0.04))
                } else {
                    Text("")
                        .padding(cellPadding)
                        .frame(maxWidth: .infinity, alignment: .center)
                }
            }
        }
        .animation(.easeInOut(duration: 0.25), value: viewModel.displayGrid.count)
    }
}

/// The bottom control panel for import settings and actions.
private struct CSVControlPanelView: View {
    @Bindable var viewModel: CSVViewModel
    let dismiss: DismissAction
    @State private var isAssignHovered = false

    var body: some View {
        VStack(spacing: 16) {
            HStack(spacing: 20) {
                CustomPicker(title: "Separator", selection: $viewModel.columnSeparator) { option in
                    String(option.rawValue.split(separator: " ").first ?? "")
                }
                
                Spacer()
                
                Toggle("First row is header", isOn: $viewModel.useFirstRowAsHeader)
                    .toggleStyle(.switch)
            }
            
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

