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
    
    @State private var searchText = ""
    @State private var selectedTab = 0

    // Computed properties to filter lists based on search text
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
    
    var filteredConstants: [PhysicalConstant] {
        if searchText.isEmpty {
            return viewModel.physicalConstants
        } else {
            return viewModel.physicalConstants.filter {
                $0.symbol.localizedCaseInsensitiveContains(searchText) ||
                $0.name.localizedCaseInsensitiveContains(searchText)
            }
        }
    }

    var body: some View {
        VStack(spacing: 0) {
            // --- Header ---
            HStack {
                // The title changes based on the selected tab.
                Text(tabTitle)
                    .font(.title2).bold()
                Spacer()
                Button("Done") {
                    dismiss()
                }
            }
            .padding()
            .background(.bar)
            
            Divider()

            // Main TabView for different sections
            TabView(selection: $selectedTab) {
                userDefinedView
                    .tabItem { Text("User Defined") }
                    .tag(0)

                builtinFunctionsView
                    .tabItem { Text("Built-in Functions") }
                    .tag(1)
                
                constantsView
                    .tabItem { Text("Constants") }
                    .tag(2)
            }
            // Use the new, modern syntax for onChange
            .onChange(of: selectedTab) {
                searchText = ""
            }
        }
        .frame(minWidth: 450, minHeight: 400, idealHeight: 600)
    }
    
    private var tabTitle: String {
        switch selectedTab {
        case 0: return "Variables & Functions"
        case 1: return "Built-in Functions"
        case 2: return "Constants"
        default: return ""
        }
    }
    
    // --- Views for each tab ---
    
    private var userDefinedView: some View {
        List {
            Section {
                if viewModel.sortedVariables.isEmpty {
                    Text("No variables defined.")
                        .foregroundColor(.secondary)
                } else {
                    ForEach(viewModel.sortedVariables, id: \.0) { name, value in
                        HStack {
                            Button(action: {
                                viewModel.rawExpression += name
                                dismiss()
                            }) {
                                Text(name)
                                    .font(.system(.body, design: .monospaced))
                                    .foregroundColor(.primary) // Ensure button text is readable
                            }
                            .buttonStyle(.plain)
                            
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
            } header: {
                Text("Variables").fontWeight(.bold)
            }

            Section {
                if viewModel.sortedFunctions.isEmpty {
                    Text("No functions defined.")
                        .foregroundColor(.secondary)
                } else {
                    ForEach(viewModel.sortedFunctions, id: \.0) { name, node in
                        HStack {
                            Button(action: {
                                viewModel.rawExpression += name + "("
                                dismiss()
                            }) {
                                Text(node.description)
                                    .font(.system(.body, design: .monospaced))
                                    .foregroundColor(.primary)
                            }
                            .buttonStyle(.plain)
                            
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
            } header: {
                Text("Functions").fontWeight(.bold)
            }
        }
        .listStyle(.sidebar)
    }
    
    private var builtinFunctionsView: some View {
        // Use a searchable list for built-in functions
        List(filteredBuiltinFunctions) { function in
            Button(action: {
                viewModel.rawExpression += function.name + "("
                dismiss()
            }) {
                VStack(alignment: .leading, spacing: 4) {
                    Text(function.signature)
                        .font(.system(.body, design: .monospaced)).fontWeight(.bold)
                    Text(function.description)
                        .foregroundColor(.secondary)
                }
                .foregroundColor(.primary) // Ensure text color is correct
                .padding(.vertical, 6)
            }
            .buttonStyle(.plain)
        }
        .listStyle(.sidebar)
        .searchable(text: $searchText, prompt: "Search Functions")
    }
    
    private var constantsView: some View {
        // Use a searchable list for physical constants
        List(filteredConstants) { constant in
            Button(action: {
                viewModel.rawExpression += constant.symbol
                dismiss()
            }) {
                HStack {
                    Text(constant.symbol)
                        .font(.system(.body, design: .monospaced)).fontWeight(.bold)
                        .frame(minWidth: 40, alignment: .leading)
                    
                    VStack(alignment: .leading, spacing: 4) {
                        Text(constant.name)
                        Text(String(constant.value))
                            .foregroundColor(.secondary)
                            .font(.system(.body, design: .monospaced))
                    }
                }
                .foregroundColor(.primary)
                .padding(.vertical, 6)
            }
            .buttonStyle(.plain)
        }
        .listStyle(.sidebar)
        .searchable(text: $searchText, prompt: "Search Constants")
    }
}

