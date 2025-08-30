//
//  VariableEditorView.swift
//  ModernCalc
//
//  Created by Manu Heiskanen on 29.8.2025.
//

import SwiftUI

struct VariableEditorView: View {
    // Use @ObservedObject for views passed in an environment, especially in sheets.
    @ObservedObject var viewModel: CalculatorViewModel
    @Environment(\.dismiss) var dismiss

    var body: some View {
        VStack(spacing: 0) {
            // --- Header ---
            HStack {
                Text("Variables & Functions")
                    .font(.title2).bold()
                Spacer()
                Button("Done") {
                    dismiss()
                }
            }
            .padding()
            .background(.bar)
            
            Divider()

            // --- Main Content ---
            List {
                // --- Variables Section ---
                Section {
                    // Use a VStack to resolve ambiguity
                    VStack {
                        if viewModel.sortedVariables.isEmpty {
                            Text("No variables defined.")
                                .foregroundColor(.secondary)
                        } else {
                            ForEach(viewModel.sortedVariables, id: \.0) { name, value in
                                HStack {
                                    Text(name)
                                        .font(.system(.body, design: .monospaced))
                                    Spacer()
                                    Text(viewModel.formatLivePreview(value))
                                        .foregroundColor(.secondary)
                                    Button(role: .destructive) {
                                        viewModel.deleteVariable(name: name)
                                    } label: {
                                        Image(systemName: "trash")
                                    }
                                    .buttonStyle(.plain)
                                }
                                .padding(.vertical, 4)
                            }
                        }
                    }
                } header: {
                    Text("Variables").fontWeight(.bold)
                }

                // --- Functions Section ---
                Section {
                    // Use a VStack to resolve ambiguity
                    VStack {
                        if viewModel.sortedFunctions.isEmpty {
                            Text("No functions defined.")
                                .foregroundColor(.secondary)
                        } else {
                            ForEach(viewModel.sortedFunctions, id: \.0) { name, node in
                                HStack {
                                    Text(node.description)
                                        .font(.system(.body, design: .monospaced))
                                    Spacer()
                                    Button(role: .destructive) {
                                        viewModel.deleteFunction(name: name)
                                    } label: {
                                        Image(systemName: "trash")
                                    }
                                    .buttonStyle(.plain)
                                }
                                .padding(.vertical, 4)
                            }
                        }
                    }
                } header: {
                    Text("Functions").fontWeight(.bold)
                }
            }
            .listStyle(.sidebar) // A clean style suitable for macOS
        }
        .frame(minWidth: 400, minHeight: 400, idealHeight: 600)
    }
}

