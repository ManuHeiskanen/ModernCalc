//
//  VariableEditorView.swift
//  ModernCalc
//
//  Created by Manu Heiskanen on 29.8.2025.
//

import SwiftUI

// --- ADDED: A struct to hold themed groups of functions for better organization. ---
struct FunctionCategory: Identifiable {
    let id = UUID()
    let name: String
    var functions: [BuiltinFunction]
}

struct VariableEditorView: View {
    @ObservedObject var viewModel: CalculatorViewModel
    @ObservedObject var settings: UserSettings
    @Environment(\.dismiss) var dismiss
    
    @State private var searchText = ""
    @State private var selectedTab = 0

    // --- MODIFIED: The filtering logic now operates on the new categorized structure. ---
    // It filters functions within each category and preserves the group structure.
    var filteredFunctionCategories: [FunctionCategory] {
        let categories = groupFunctions(viewModel.builtinFunctions)
        if searchText.isEmpty {
            return categories
        }
        
        return categories.compactMap { category in
            let filteredFunctions = category.functions.filter {
                $0.name.localizedCaseInsensitiveContains(searchText) ||
                $0.description.localizedCaseInsensitiveContains(searchText) ||
                $0.signature.localizedCaseInsensitiveContains(searchText)
            }
            // Only include the category in the result if it has matching functions.
            return filteredFunctions.isEmpty ? nil : FunctionCategory(name: category.name, functions: filteredFunctions)
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
    
    // --- MODIFIED: This function organizes the flat list of functions into themed categories. ---
    private func groupFunctions(_ functions: [BuiltinFunction]) -> [FunctionCategory] {
        // This dictionary defines the keywords that assign a function to a category.
        let categoryKeywords: [String: [String]] = [
            "Calculus": ["derivative", "grad", "integral"],
            "Complex Numbers": ["abs", "arg", "conj", "imag", "polar", "real"],
            "Data Generation & IO": ["importcsv", "linspace", "random", "randm", "randv", "range"],
            "Geometry": ["angle", "area_circle", "area_rect", "area_tri", "circum_circle", "hypot", "side", "unit", "vol_cone", "vol_cube", "vol_cylinder", "vol_sphere"],
            "Number Theory": ["fact", "factor", "find", "gcd", "isprime", "lcm", "mod", "nCr", "nPr"],
            "Plotting": ["autoplot", "plot", "scatterplot"],
            "Statistics": ["avg", "corr", "count", "countabove", "countbelow", "linreg", "max", "median", "min", "percentile", "polyfit", "sort", "stddev", "stddevp", "sum", "unique", "variance"],
            "Trigonometry": ["acos", "acosh", "asin", "asinh", "atan", "atan2", "atanh", "cos", "cosh", "sin", "sinh", "tan", "tanh"],
            "Uncertainty": ["uncert"],
            "Utility & Conversion": ["ceil", "floor", "lg", "ln", "log", "root", "round", "sqrt"],
            "Vector & Matrix": ["cmatrix", "cross", "cvector", "det", "dot", "getcolumn", "getrow", "inv", "linsolve", "matrix", "trace", "transpose", "vector"]
        ]

        var categories: [String: [BuiltinFunction]] = [:]
        var assignedFunctions = Set<String>()

        // Assign functions to categories based on the keywords.
        for (categoryName, keywords) in categoryKeywords {
            categories[categoryName] = []
            for keyword in keywords {
                if let function = functions.first(where: { $0.name == keyword }) {
                    categories[categoryName]?.append(function)
                    assignedFunctions.insert(function.name)
                }
            }
        }
        
        // All remaining functions are grouped into a "General" category.
        let generalFunctions = functions.filter { !assignedFunctions.contains($0.name) }
        if !generalFunctions.isEmpty {
            categories["General"] = generalFunctions
        }

        // Sort the categories and the functions within them alphabetically.
        return categories
            .map { FunctionCategory(name: $0.key, functions: $0.value.sorted(by: { $0.name < $1.name })) }
            .sorted { $0.name < $1.name }
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
    
    // --- MODIFIED: This view now iterates over categories and displays functions in sections. ---
    private var builtinFunctionsView: some View {
        List {
            ForEach(filteredFunctionCategories) { category in
                Section(header: Text(category.name).fontWeight(.bold)) {
                    ForEach(category.functions) { function in
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
                }
            }
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
                Section(header: Text("History & Export Formatting")) {
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
                
                Section(header: Text("Live Preview Formatting")) {
                    Toggle("Round live result", isOn: $settings.enableLiveRounding.animation())
                    
                    if settings.enableLiveRounding {
                        Stepper("Decimal Places: \(settings.livePreviewDecimalPlaces)", value: $settings.livePreviewDecimalPlaces, in: 0...10)
                    }
                }
                
                Section(header: Text("CSV Import Formatting")) {
                    Toggle("Round displayed values", isOn: $settings.enableCSVRounding.animation())
                    
                    if settings.enableCSVRounding {
                        Stepper("Decimal Places: \(settings.csvDecimalPlaces)", value: $settings.csvDecimalPlaces, in: 0...10)
                    }
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
