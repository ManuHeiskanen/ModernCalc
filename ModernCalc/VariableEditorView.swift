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
    
    // Define an enum for the tabs for better readability and safety.
    enum EditorTab {
        case userDefined, builtIn, constants
    }
    
    @State private var searchText = ""
    @State private var selectedTab: EditorTab = .userDefined

    // A computed property to filter the built-in functions based on search text.
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
    
    // A computed property to filter the physical constants based on search text.
    var filteredConstants: [PhysicalConstant] {
        if searchText.isEmpty {
            return viewModel.physicalConstants
        } else {
            return viewModel.physicalConstants.filter {
                $0.name.localizedCaseInsensitiveContains(searchText) ||
                $0.symbol.localizedCaseInsensitiveContains(searchText)
            }
        }
    }

    var body: some View {
        VStack(spacing: 0) {
            // --- Header ---
            HStack {
                // The title changes based on the selected tab.
                Text(headerTitle)
                    .font(.title2).bold()
                Spacer()
                Button("Done") {
                    dismiss()
                }
            }
            .padding()
            .background(.bar)
            
            Divider()

            // Replaced the List with a TabView that uses the enum.
            TabView(selection: $selectedTab) {
                // --- Tab 1: User-Defined Items ---
                userDefinedView
                    .tabItem { Text("User Defined") }
                    .tag(EditorTab.userDefined)

                // --- Tab 2: Built-in Functions ---
                builtinFunctionsView
                    .tabItem { Text("Built-in Functions") }
                    .tag(EditorTab.builtIn)
                    
                // --- Tab 3: Physical Constants ---
                constantsView
                    .tabItem { Text("Constants") }
                    .tag(EditorTab.constants)
            }
            // Clear search text when switching tabs for a better user experience.
            .onChange(of: selectedTab) { _ in searchText = "" }
        }
        .frame(minWidth: 450, minHeight: 400, idealHeight: 600)
    }
    
    // A computed property for the header title based on the selected tab.
    private var headerTitle: String {
        switch selectedTab {
        case .userDefined: return "Variables & Functions"
        case .builtIn: return "Built-in Functions"
        case .constants: return "Physical Constants"
        }
    }
    
    // A view for the user-defined variables and functions.
    private var userDefinedView: some View {
        List {
            Section {
                // Check if there are any variables to show
                if viewModel.sortedVariables.isEmpty {
                    Text("No variables defined.")
                        .foregroundColor(.secondary)
                        .padding()
                } else {
                    ForEach(viewModel.sortedVariables, id: \.0) { name, value in
                        Button(action: {
                            viewModel.rawExpression += name
                            dismiss()
                        }) {
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
                        }
                        .buttonStyle(.plain)
                        .padding(.vertical, 4)
                    }
                }
            } header: {
                Text("Variables").fontWeight(.bold)
            }

            Section {
                if viewModel.sortedFunctions.isEmpty {
                    Text("No functions defined.")
                        .foregroundColor(.secondary)
                        .padding()
                } else {
                    ForEach(viewModel.sortedFunctions, id: \.0) { name, node in
                        Button(action: {
                            viewModel.rawExpression += "\(name)("
                            dismiss()
                        }) {
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
                        }
                        .buttonStyle(.plain)
                        .padding(.vertical, 4)
                    }
                }
            } header: {
                Text("Functions").fontWeight(.bold)
            }
        }
        .listStyle(.sidebar)
    }
    
    // A view for the searchable list of built-in functions.
    private var builtinFunctionsView: some View {
        VStack {
            List {
                ForEach(filteredBuiltinFunctions) { function in
                    Button(action: {
                        viewModel.rawExpression += "\(function.name)("
                        dismiss()
                    }) {
                        VStack(alignment: .leading, spacing: 4) {
                            Text(function.signature)
                                .font(.system(.body, design: .monospaced)).fontWeight(.bold)
                            Text(function.description)
                                .foregroundColor(.secondary)
                        }
                    }
                    .buttonStyle(.plain)
                    .padding(.vertical, 6)
                }
            }
            .listStyle(.sidebar)
            .searchable(text: $searchText, prompt: "Search Functions")
        }
    }
    
    // A view for the searchable list of physical constants.
    private var constantsView: some View {
        VStack {
            List {
                ForEach(filteredConstants) { constant in
                    Button(action: {
                        viewModel.rawExpression += constant.symbol
                        dismiss()
                    }) {
                        HStack(alignment: .top) {
                            Text(constant.symbol)
                                .font(.system(.body, design: .monospaced)).fontWeight(.bold)
                                .frame(width: 40, alignment: .leading)
                            
                            VStack(alignment: .leading, spacing: 4) {
                                Text(constant.name)
                                    .foregroundColor(.primary)
                                Text(String(constant.value))
                                    .foregroundColor(.secondary)
                                    .font(.system(.body, design: .monospaced))
                            }
                            
                            Spacer()
                        }
                    }
                    .buttonStyle(.plain)
                    .padding(.vertical, 6)
                }
            }
            .listStyle(.sidebar)
            .searchable(text: $searchText, prompt: "Search Constants")
        }
    }
}

