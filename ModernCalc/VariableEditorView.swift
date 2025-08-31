//
//  VariableEditorView.swift
//  ModernCalc
//
//  Created by Manu Heiskanen on 29.8.2025.
//

import SwiftUI

struct VariableEditorView: View {
    @ObservedObject var viewModel: CalculatorViewModel
    @ObservedObject var settings: UserSettings
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
                    .tabItem { Label("User Defined", systemImage: "person.text.rectangle") }
                    .tag(0)

                builtinFunctionsView
                    .tabItem { Label("Functions", systemImage: "function") }
                    .tag(1)
                
                constantsView
                    .tabItem { Label("Constants", systemImage: "number.square") }
                    .tag(2)
                
                settingsView
                    .tabItem { Label("Settings", systemImage: "gear") }
                    .tag(3)
                
                helpView
                    .tabItem { Label("Help", systemImage: "questionmark.circle") }
                    .tag(4)
            }
            .onChange(of: selectedTab) {
                searchText = ""
            }
        }
        .frame(minWidth: 500, minHeight: 450, idealHeight: 600)
    }
    
    private var tabTitle: String {
        switch selectedTab {
        case 0: return "Variables & Functions"
        case 1: return "Built-in Functions"
        case 2: return "Constants"
        case 3: return "Settings"
        case 4: return "Help"
        default: return ""
        }
    }
    
    // --- Views for each tab ---
    
    private var userDefinedView: some View {
        List {
            Section {
                if viewModel.sortedVariables.isEmpty {
                    Text("No variables defined. Use `x := 5` to create one.").foregroundColor(.secondary)
                } else {
                    ForEach(viewModel.sortedVariables, id: \.0) { name, value in
                        HStack {
                            Button(action: {
                                viewModel.insertTextAtCursor(name)
                                dismiss()
                            }) {
                                Text(name).font(.system(.body, design: .monospaced)).foregroundColor(.primary)
                            }.buttonStyle(.plain)
                            
                            Spacer()
                            Text(viewModel.formatForHistory(value)).foregroundColor(.secondary)
                            Button(role: .destructive) { viewModel.deleteVariable(name: name) } label: { Image(systemName: "trash") }.buttonStyle(.plain)
                        }.padding(.vertical, 4)
                    }
                }
            } header: { Text("Variables").fontWeight(.bold) }

            Section {
                if viewModel.sortedFunctions.isEmpty {
                    Text("No functions defined. Use `f(x) := x^2` to create one.").foregroundColor(.secondary)
                } else {
                    ForEach(viewModel.sortedFunctions, id: \.0) { name, node in
                        HStack {
                            Button(action: {
                                viewModel.insertTextAtCursor(name + "(")
                                dismiss()
                            }) {
                                Text(node.description).font(.system(.body, design: .monospaced)).foregroundColor(.primary)
                            }.buttonStyle(.plain)
                            
                            Spacer()
                            Button(role: .destructive) { viewModel.deleteFunction(name: name) } label: { Image(systemName: "trash") }.buttonStyle(.plain)
                        }.padding(.vertical, 4)
                    }
                }
            } header: { Text("Functions").fontWeight(.bold) }
        }.listStyle(.sidebar)
    }
    
    private var builtinFunctionsView: some View {
        List(filteredBuiltinFunctions) { function in
            Button(action: {
                viewModel.insertTextAtCursor(function.name + "(")
                dismiss()
            }) {
                VStack(alignment: .leading, spacing: 4) {
                    Text(function.signature).font(.system(.body, design: .monospaced)).fontWeight(.bold)
                    Text(function.description).foregroundColor(.secondary)
                }.foregroundColor(.primary).padding(.vertical, 6)
            }.buttonStyle(.plain)
        }
        .listStyle(.sidebar)
        .searchable(text: $searchText, prompt: "Search Functions")
    }
    
    private var constantsView: some View {
        List(filteredConstants) { constant in
            Button(action: {
                viewModel.insertTextAtCursor(constant.symbol)
                dismiss()
            }) {
                HStack {
                    Text(constant.symbol).font(.system(.body, design: .monospaced)).fontWeight(.bold).frame(minWidth: 40, alignment: .leading)
                    VStack(alignment: .leading, spacing: 4) {
                        Text(constant.name)
                        Text(String(constant.value)).foregroundColor(.secondary).font(.system(.body, design: .monospaced))
                    }
                }.foregroundColor(.primary).padding(.vertical, 6)
            }.buttonStyle(.plain)
        }
        .listStyle(.sidebar)
        .searchable(text: $searchText, prompt: "Search Constants")
    }
    
    private var settingsView: some View {
        VStack {
            Form {
                Section(header: Text("Number Formatting")) {
                    Picker("Display Mode", selection: $settings.displayMode) {
                        ForEach(NumberDisplayMode.allCases, id: \.self) {
                            Text($0.rawValue)
                        }
                    }
                    .pickerStyle(.segmented)
                    
                    if settings.displayMode == .fixed {
                        Stepper("Decimal Places: \(settings.fixedDecimalPlaces)", value: $settings.fixedDecimalPlaces, in: 0...10)
                    }
                    
                    Picker("Decimal Separator", selection: $settings.decimalSeparator) {
                        ForEach(DecimalSeparator.allCases, id: \.self) {
                            Text($0.rawValue)
                        }
                    }
                    .pickerStyle(.segmented)
                }
            }
            .padding()
            
            Spacer()
        }
    }
    
    private var helpView: some View {
        ScrollView {
            VStack(alignment: .leading, spacing: 20) {
                ForEach(viewModel.helpTopics) { topic in
                    VStack(alignment: .leading, spacing: 8) {
                        Text(topic.title)
                            .font(.headline)
                        Text(.init(topic.content))
                            .fixedSize(horizontal: false, vertical: true)
                    }
                }
            }
            .padding()
        }
    }
}

