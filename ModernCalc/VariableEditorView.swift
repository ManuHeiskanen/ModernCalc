//
//  VariableEditorView.swift
//  ModernCalc
//
//  Created by Manu Heiskanen on 29.8.2025.
//

import SwiftUI

struct VariableEditorView: View {
    @ObservedObject var viewModel: CalculatorViewModel
    @Environment(\.dismiss) var dismiss
    // NEW: State for the search text and the selected tab.
    @State private var searchText = ""
    @State private var selectedTab = 0

    // NEW: A computed property to filter the built-in functions based on search text.
    var filteredBuiltinFunctions: [BuiltinFunction] {
        if searchText.isEmpty {
            return viewModel.builtinFunctions
        } else {
            return viewModel.builtinFunctions.filter {
                $0.name.localizedCaseInsensitiveContains(searchText) ||
                $0.description.localizedCaseInsensitiveContains(searchText)
            }
        }
    }

    var body: some View {
        VStack(spacing: 0) {
            // --- Header ---
            HStack {
                // The title changes based on the selected tab.
                Text(selectedTab == 0 ? "Variables & Functions" : "Built-in Functions")
                    .font(.title2).bold()
                Spacer()
                Button("Done") {
                    dismiss()
                }
            }
            .padding()
            .background(.bar)
            
            Divider()

            // MODIFIED: Replaced the List with a TabView.
            TabView(selection: $selectedTab) {
                // --- Tab 1: User-Defined Items ---
                userDefinedView
                    .tabItem { Text("User Defined") }
                    .tag(0)

                // --- Tab 2: Built-in Functions ---
                builtinFunctionsView
                    .tabItem { Text("Built-in Functions") }
                    .tag(1)
            }
        }
        .frame(minWidth: 450, minHeight: 400, idealHeight: 600)
    }
    
    // NEW: A view for the user-defined variables and functions.
    private var userDefinedView: some View {
        List {
            Section {
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

            Section {
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
        .listStyle(.sidebar)
    }
    
    // NEW: A view for the searchable list of built-in functions.
    private var builtinFunctionsView: some View {
        VStack {
            List {
                ForEach(filteredBuiltinFunctions) { function in
                    VStack(alignment: .leading, spacing: 4) {
                        Text(function.signature)
                            .font(.system(.body, design: .monospaced)).fontWeight(.bold)
                        Text(function.description)
                            .foregroundColor(.secondary)
                    }
                    .padding(.vertical, 6)
                }
            }
            .listStyle(.sidebar)
            .searchable(text: $searchText, prompt: "Search Functions")
        }
    }
}
