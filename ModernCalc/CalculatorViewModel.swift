//
//  CalculatorViewModel.swift
//  ModernCalc
//
//  Created by Manu Heiskanen on 28.8.25.
//

import Foundation
import Combine
import SwiftUI

@MainActor
class CalculatorViewModel: ObservableObject {

    @Published var rawExpression: String = ""
    @Published var history: [Calculation] = []
    @Published var liveHelpText: String = ""
    @Published var liveErrorText: String = ""
    @Published var liveLaTeXPreview: String = ""
    @Published var previewText: String = ""
    @Published var variables: [String: MathValue] = [:]
    @Published var functions: [String: FunctionDefinitionNode] = [:]
    @Published var livePreviewHeight: CGFloat = 60.0
    
    @Published var angleMode: AngleMode = .degrees {
        didSet { saveState() }
    }
    @Published var userFunctionDefinitions: [String: String] = [:]
    @Published var cursorPosition = NSRange()
    
    @Published var plotViewModels: [PlotViewModel] = []
    @Published var plotToShow: PlotData.ID? = nil
    
    @Published var csvViewModel: CSVViewModel? = nil
    @Published var showCSVView: Bool = false
    
    private var settings: UserSettings
    private let evaluator = Evaluator()
    private var lastSuccessfulValue: MathValue?
    private var lastUsedAngleFlag: Bool = false
    private var cancellables = Set<AnyCancellable>()
    private let navigationManager = NavigationManager()
    private let ansVariable = "ans"
    
    private var lastCalculatedExpression: String?
    private var lastCalculatedAngleMode: AngleMode?
    
    let siPrefixes: Set<String>
    let operatorSymbols: [MathSymbol]
    let greekSymbols: [MathSymbol]
    let constantSymbols: [MathSymbol]
    let builtinFunctions: [BuiltinFunction]
    let physicalConstants: [PhysicalConstant]
    let helpTopics: [HelpTopic]

    init(settings: UserSettings) {
        self.settings = settings
        
        self.siPrefixes = TextContents.siPrefixes
        self.operatorSymbols = TextContents.operatorSymbols
        self.greekSymbols = TextContents.greekSymbols
        self.builtinFunctions = TextContents.builtinFunctions
        self.physicalConstants = TextContents.physicalConstants
        self.helpTopics = TextContents.helpTopics
        
        self.constantSymbols = TextContents.physicalConstants.map { .init(symbol: $0.symbol, name: $0.name, insertionText: $0.symbol) }
        
        Publishers.CombineLatest3($rawExpression, $cursorPosition, $angleMode)
            .debounce(for: .milliseconds(50), scheduler: RunLoop.main)
            .sink { [weak self] (expression, position, angle) in
                guard let self = self else { return }

                guard !expression.trimmingCharacters(in: .whitespaces).isEmpty else {
                    self.lastSuccessfulValue = nil
                    self.liveHelpText = ""
                    self.liveErrorText = ""
                    self.liveLaTeXPreview = ""
                    self.lastCalculatedExpression = nil
                    self.lastCalculatedAngleMode = nil
                    self.livePreviewHeight = 60.0
                    return
                }
                
                if expression == self.lastCalculatedExpression && angle == self.lastCalculatedAngleMode {
                    self.liveHelpText = self.getContextualHelp(expression: expression, cursor: position) ?? ""
                    return
                }
                
                Task {
                    await self.calculate(expression: expression, cursor: position)
                    self.lastCalculatedExpression = expression
                    self.lastCalculatedAngleMode = angle
                }
            }
            .store(in: &cancellables)
            
        loadState()
    }
    
    func triggerCSVImport() {
        _ = openCSVFile()
    }
    
    func addPlotViewModel(for plotData: PlotData) {
        if !plotViewModels.contains(where: { $0.plotData.id == plotData.id }) {
            let newPlotViewModel = PlotViewModel(plotData: plotData)
            Task {
                plotViewModels.append(newPlotViewModel)
            }
        }
    }

    func requestOpenPlotWindow(for plotData: PlotData) {
        addPlotViewModel(for: plotData)
        plotToShow = plotData.id
    }

    func closePlotWindow(id: PlotData.ID?) {
        plotViewModels.removeAll { $0.plotData.id == id }
    }

    private func getContextualHelp(expression: String, cursor: NSRange) -> String? {
        guard cursor.location <= expression.utf16.count else { return nil }
        
        let startIndex = expression.startIndex
        let cursorIndex = expression.index(startIndex, offsetBy: cursor.location, limitedBy: expression.endIndex) ?? expression.endIndex
        let textBeforeCursor = expression[..<cursorIndex]

        var openParenCount = 0
        var lastOpenParenIndex: String.Index?
        
        for index in textBeforeCursor.indices.reversed() {
            let char = textBeforeCursor[index]
            if char == ")" {
                openParenCount += 1
            } else if char == "(" {
                if openParenCount == 0 {
                    lastOpenParenIndex = index
                    break
                } else {
                    openParenCount -= 1
                }
            }
        }
        
        guard let parenIndex = lastOpenParenIndex else { return nil }
        
        let textBeforeParen = textBeforeCursor[..<parenIndex]
        
        let pattern = "\\b([a-zA-Z_][a-zA-Z0-9_]*)$"
        if let range = textBeforeParen.range(of: pattern, options: .regularExpression),
           let function = builtinFunctions.first(where: { $0.name == textBeforeParen[range] }) {
            return "\(function.signature)\n\(function.description)"
        }
        
        return nil
    }

    private func calculate(expression: String, cursor: NSRange) async {
        let helpText = getContextualHelp(expression: expression, cursor: cursor)

        var tempVars = self.variables
        var tempFuncs = self.functions
        
        var finalLiveHelpText: String = ""
        var finalLiveErrorText: String = ""
        let finalLiveLaTeXPreview: String
        
        do {
            let lexer = Lexer(input: expression, decimalSeparator: settings.decimalSeparator)
            let tokens = lexer.tokenize()
            let parser = Parser(tokens: tokens)
            let expressionTree = try parser.parse()
            let expressionLaTeX = LaTeXEngine.formatNode(expressionTree, evaluator: self.evaluator, settings: self.settings)

            let (value, usedAngle) = try evaluator.evaluate(node: expressionTree, variables: &tempVars, functions: &tempFuncs, angleMode: self.angleMode)
            
            self.lastSuccessfulValue = value
            self.lastUsedAngleFlag = usedAngle
            self.variables = tempVars
            self.functions = tempFuncs
            
            let isSimpleVariableDefinition = expressionTree is AssignmentNode && ((expressionTree as! AssignmentNode).expression is NumberNode || (expressionTree as! AssignmentNode).expression is UnaryOpNode)
            
            if case .plot = value {
                finalLiveLaTeXPreview = expressionLaTeX
            } else if case .functionDefinition = value {
                finalLiveLaTeXPreview = expressionLaTeX
            } else if isSimpleVariableDefinition {
                finalLiveLaTeXPreview = expressionLaTeX
            } else {
                let resultLaTeX: String
                let maxLivePreviewRows = 50

                var isResultTooLargeForPreview = false
                if case .vector(let v) = value, v.dimension > maxLivePreviewRows { isResultTooLargeForPreview = true }
                else if case .matrix(let m) = value, m.rows > maxLivePreviewRows { isResultTooLargeForPreview = true }
                else if case .complexVector(let cv) = value, cv.dimension > maxLivePreviewRows { isResultTooLargeForPreview = true }
                else if case .complexMatrix(let cm) = value, cm.rows > maxLivePreviewRows { isResultTooLargeForPreview = true }

                if isResultTooLargeForPreview {
                    switch value {
                    case .vector(let v): resultLaTeX = "\\text{\(v.dimension)-element Vector}"
                    case .matrix(let m): resultLaTeX = "\\text{\(m.rows)x\(m.columns) Matrix}"
                    case .complexVector(let cv): resultLaTeX = "\\text{\(cv.dimension)-element Complex Vector}"
                    case .complexMatrix(let cm): resultLaTeX = "\\text{\(cm.rows)x\(cm.columns) Complex Matrix}"
                    default: resultLaTeX = LaTeXEngine.formatMathValue(value, angleMode: self.angleMode, settings: self.settings)
                    }
                } else {
                    if self.settings.enableLiveRounding {
                        let liveSettings = self.settings.makeTemporaryCopy()
                        liveSettings.displayMode = .fixed
                        liveSettings.fixedDecimalPlaces = self.settings.livePreviewDecimalPlaces
                        resultLaTeX = LaTeXEngine.formatMathValue(value, angleMode: self.angleMode, settings: liveSettings)
                    } else {
                        resultLaTeX = LaTeXEngine.formatMathValue(value, angleMode: self.angleMode, settings: self.settings)
                    }
                }
                finalLiveLaTeXPreview = "\(expressionLaTeX) = \(resultLaTeX)"
            }
            
            finalLiveHelpText = helpText ?? ""

        } catch let error {
            self.lastSuccessfulValue = nil
            
            let errorMessage = (error as? CustomStringConvertible)?.description ?? "An unknown error occurred."
            finalLiveHelpText = helpText ?? ""
            finalLiveErrorText = errorMessage
            finalLiveLaTeXPreview = LaTeXEngine.formatExpression(expression, evaluator: self.evaluator, settings: self.settings)
        }
        
        let contentForHeightCheck: String
        if !finalLiveErrorText.isEmpty {
            contentForHeightCheck = finalLiveErrorText
        } else if !finalLiveHelpText.isEmpty {
            contentForHeightCheck = finalLiveHelpText
        } else {
            contentForHeightCheck = finalLiveLaTeXPreview
        }

        let latexNewlines = contentForHeightCheck.components(separatedBy: "\\\\").count - 1
        let textNewlines = contentForHeightCheck.components(separatedBy: "\n").count - 1
        let fractionCount = contentForHeightCheck.components(separatedBy: "\\frac").count - 1
        let verticalityScore = max(latexNewlines, textNewlines) + fractionCount

        let baseHeight: CGFloat = 60.0
        let heightPerUnit: CGFloat = 20.0
        let maxHeight: CGFloat = 180.0
        
        var calculatedHeight = baseHeight + (CGFloat(verticalityScore) * heightPerUnit)

        if verticalityScore == 0 && contentForHeightCheck.count > 80 {
            calculatedHeight = 100.0
        }

        self.livePreviewHeight = min(calculatedHeight, maxHeight)
        
        self.liveHelpText = finalLiveHelpText
        self.liveErrorText = finalLiveErrorText
        self.liveLaTeXPreview = finalLiveLaTeXPreview
    }
    
    func commitCalculation() -> PlotData? {
        if let selectedId = navigationManager.selectedHistoryId, let selectedItem = history.first(where: { $0.id == selectedId }) {

            if case .plot(let plotData) = selectedItem.result {
                resetNavigation()
                requestOpenPlotWindow(for: plotData)
                return nil
            }

            let textToInsert = self.previewText
            resetNavigation()
            
            if !textToInsert.isEmpty {
                self.insertTextAtCursor(textToInsert)
            }
            
            return nil

        } else {
            guard !rawExpression.isEmpty, let valueToCommit = lastSuccessfulValue else {
                 if rawExpression == "importcsv()" {
                     _ = openCSVFile()
                 }
                return nil
            }
            
            var plotDataToReturn: PlotData?
            let calcType: CalculationType
            
            if case .plot(let plotData) = valueToCommit {
                calcType = .plot
                addPlotViewModel(for: plotData)
                plotDataToReturn = plotData
            } else if case .triggerCSVImport = valueToCommit {
                Task {
                    self.rawExpression = ""
                    _ = openCSVFile()
                }
                return nil
            } else if valueToCommit.typeName == "FunctionDefinition" {
                calcType = .functionDefinition
            } else if rawExpression.contains(":=") {
                calcType = .variableAssignment
            } else {
                calcType = .evaluation
            }

            let newCalculation = Calculation(expression: rawExpression, result: valueToCommit, type: calcType, usedAngleSensitiveFunction: self.lastUsedAngleFlag, angleMode: self.angleMode)
            
            Task {
                if calcType != .functionDefinition { self.variables[self.ansVariable] = valueToCommit }
                if calcType == .variableAssignment {
                    saveState()
                } else if calcType == .functionDefinition, case .functionDefinition(let name) = valueToCommit {
                    self.userFunctionDefinitions[name] = self.rawExpression
                    saveState()
                }
                
                self.history.append(newCalculation)
                self.rawExpression = ""
            }
            return plotDataToReturn
        }
    }
    
    private func openCSVFile() -> Bool {
         let openPanel = NSOpenPanel()
         openPanel.allowedContentTypes = [.commaSeparatedText]
         openPanel.allowsMultipleSelection = false
         openPanel.canChooseDirectories = false
         openPanel.canChooseFiles = true

         if openPanel.runModal() == .OK {
             if let url = openPanel.url {
                 do {
                     let content = try String(contentsOf: url, encoding: .utf8)
                     self.csvViewModel = CSVViewModel(fileName: url.lastPathComponent, content: content, mainViewModel: self, settings: self.settings)
                     self.showCSVView = true
                     
                 } catch {
                     print("Error reading CSV file: \(error.localizedDescription)")
                 }
                 return true
             }
         }
         return false
     }

    func handleKeyPress(keys: Set<KeyEquivalent>) -> Bool {
        if let selectedText = navigationManager.handleKeyPress(keys: keys, history: history, viewModel: self) {
            Task {
                self.previewText = selectedText
            }
            return true
        } else {
            Task {
                self.previewText = ""
            }
            return false
        }
    }

    func resetNavigation() {
        navigationManager.resetSelection()
        Task {
            self.previewText = ""
        }
    }

    var selectedHistoryId: UUID? { navigationManager.selectedHistoryId }
    var selectedHistoryPart: SelectionPart { navigationManager.selectedPart }
    var sortedVariables: [(String, MathValue)] { variables.sorted { $0.key < $1.key } }
    var sortedFunctions: [(String, FunctionDefinitionNode)] { functions.sorted { $0.key < $1.key } }
    
    func deleteVariable(name: String) { variables.removeValue(forKey: name); saveState() }
    func deleteFunction(name: String) { functions.removeValue(forKey: name); userFunctionDefinitions.removeValue(forKey: name); saveState() }
    
    func insertTextAtCursor(_ textToInsert: String) {
        guard let range = Range(cursorPosition, in: rawExpression) else {
            rawExpression += textToInsert; let newLocation = rawExpression.utf16.count; cursorPosition = NSRange(location: newLocation, length: 0)
            return
        }
        rawExpression.replaceSubrange(range, with: textToInsert)
        let newLocation = cursorPosition.location + textToInsert.utf16.count; cursorPosition = NSRange(location: newLocation, length: 0)
    }

    private func saveState() {
        do {
            let variablesData = try JSONEncoder().encode(variables); let functionsData = try JSONEncoder().encode(userFunctionDefinitions)
            UserDefaults.standard.set(variablesData, forKey: "userVariables"); UserDefaults.standard.set(functionsData, forKey: "userFunctionDefinitions")
            UserDefaults.standard.set(angleMode.rawValue, forKey: "angleModeSetting")
        } catch { print("Error saving state: \(error.localizedDescription)") }
    }

    private func loadState() {
        do {
            if let variablesData = UserDefaults.standard.data(forKey: "userVariables") {
                let decodedVars = try JSONDecoder().decode([String: MathValue].self, from: variablesData)
                self.variables = decodedVars.filter { $0.key != ansVariable }
            }
            if let functionsData = UserDefaults.standard.data(forKey: "userFunctionDefinitions") {
                self.userFunctionDefinitions = try JSONDecoder().decode([String: String].self, from: functionsData)
                rebuildFunctionsFromDefinitions()
            }
        } catch {
            print("Error loading variable/function state: \(error.localizedDescription)"); self.variables = [:]; self.userFunctionDefinitions = [:]
        }
        if let savedAngleMode = UserDefaults.standard.string(forKey: "angleModeSetting") { self.angleMode = AngleMode(rawValue: savedAngleMode) ?? .degrees }
    }
    
    private func rebuildFunctionsFromDefinitions() {
        let definitionsToRebuild = self.userFunctionDefinitions; guard !definitionsToRebuild.isEmpty else { return }
        var tempVars = self.variables; var tempFuncs: [String: FunctionDefinitionNode] = [:]

        for (_, definitionString) in definitionsToRebuild {
            do {
                let lexer = Lexer(input: definitionString, decimalSeparator: settings.decimalSeparator)
                let tokens = lexer.tokenize(); let parser = Parser(tokens: tokens); let expressionTree = try parser.parse()
                _ = try evaluator.evaluate(node: expressionTree, variables: &tempVars, functions: &tempFuncs, angleMode: self.angleMode)
            } catch { print("Error rebuilding function '\(definitionString)': \(error)") }
        }
        self.functions = tempFuncs; self.variables = tempVars; self.liveHelpText = ""; self.liveErrorText = ""
    }
    
    func formatCalculationAsLaTeX(_ calculation: Calculation) -> String {
        return LaTeXEngine.format(calculation: calculation, evaluator: self.evaluator, angleMode: self.angleMode, settings: self.settings)
    }

    func formatForHistory(_ value: MathValue) -> String {
        switch value {
        case .dimensionless(let d): return formatScalarForDisplay(d)
        case .unitValue(let u):
            if let bestUnit = findBestUnitFor(dimensions: u.dimensions) {
                let convertedValue = u.value / bestUnit.conversionFactor
                if bestUnit.dimensions.isEmpty && !["deg", "rad"].contains(bestUnit.symbol) {
                     return formatScalarForDisplay(convertedValue)
                }
                return "\(formatScalarForDisplay(convertedValue)) \(bestUnit.symbol)"
            }
            
            let valStr = formatScalarForDisplay(u.value)
            let unitStr = formatDimensionsForHistory(u.dimensions)
            if unitStr.isEmpty { return valStr }
            return "\(valStr) \(unitStr)"
        case .complex(let c): return formatComplexForDisplay(c)
        case .vector(let v): return formatVectorForDisplay(v)
        case .matrix(let m): return formatMatrixForDisplay(m)
        case .tuple(let t): return t.map { formatForHistory($0) }.joined(separator: " OR ")
        case .complexVector(let cv): return formatComplexVectorForDisplay(cv)
        case .complexMatrix(let cm): return formatComplexMatrixForDisplay(cm)
        case .functionDefinition: return ""
        case .polar(let p): return formatPolarForDisplay(p)
        case .regressionResult(let s, let i): return "m = \(formatScalarForDisplay(s)), b = \(formatScalarForDisplay(i))"
        case .polynomialFit(let coeffs): return DisplayFormatter.formatPolynomialEquation(coeffs: coeffs)
        case .plot(let plotData): return "Plot: \(plotData.expression)"
        case .triggerCSVImport: return "Importing CSV..."
        case .constant(let s): return s
        case .uncertain(let u):
            let val = formatScalarForDisplay(u.value)
            let unc = formatScalarForDisplay(u.totalUncertainty)
            let rand = formatScalarForDisplay(u.randomUncertainty)
            let sys = formatScalarForDisplay(u.systematicUncertainty)
            if u.randomUncertainty > 0 && u.systematicUncertainty > 0 {
                return "\(val) ± \(unc) (R: \(rand), S: \(sys))"
            }
            return "\(val) ± \(unc)"
        }
    }
    
    func formatForParsing(_ value: MathValue) -> String {
        let argumentSeparator = settings.decimalSeparator == .period ? "," : "."
        switch value {
        case .dimensionless(let d): return formatScalarForParsing(d)
        case .unitValue(let u):
            let valStr = formatScalarForParsing(u.value)
            let unitStr = formatDimensionsForParsing(u.dimensions)
            if unitStr.isEmpty { return valStr }
            return "(\(valStr))*\(unitStr)"
        case .complex(let c): return formatComplexForParsing(c)
        case .vector(let v): return "vector(\(v.values.map { formatScalarForParsing($0) }.joined(separator: ";")))"
        case .matrix(let m): return "matrix(\((0..<m.rows).map { r in (0..<m.columns).map { c in formatScalarForParsing(m[r, c]) }.joined(separator: argumentSeparator) }.joined(separator: ";")))"
        case .tuple(let t): return t.map { formatForParsing($0) }.first ?? ""
        case .complexVector(let cv): return "cvector(\(cv.values.map { formatForParsing(.complex($0)) }.joined(separator: ";")))"
        case .complexMatrix(let cm): return "cmatrix(\((0..<cm.rows).map { r in (0..<cm.columns).map { c in formatForParsing(.complex(cm[r, c])) }.joined(separator: argumentSeparator) }.joined(separator: ";")))"
        case .functionDefinition: return ""
        case .polar(let p): return formatPolarForParsing(p)
        case .regressionResult, .polynomialFit: return ""
        case .plot(let plotData): return "autoplot(\(plotData.expression))"
        case .triggerCSVImport: return "importcsv()"
        case .constant(let s): return s
        case .uncertain(let u):
            var parts = [formatScalarForParsing(u.value)]
            if u.randomUncertainty > 0 {
                parts.append("random:\(formatScalarForParsing(u.randomUncertainty))")
            }
            if u.systematicUncertainty > 0 {
                let accuracyEquiv = u.systematicUncertainty * sqrt(3.0)
                parts.append("accuracy:\(formatScalarForParsing(accuracyEquiv))")
            }
            return "uncert(\(parts.joined(separator: ", ")))"
        }
    }

    func formatScalarForDisplay(_ value: Double) -> String {
        let formattedString: String
        switch settings.displayMode {
        case .auto:
            if value.truncatingRemainder(dividingBy: 1) == 0 { formattedString = String(format: "%.0f", value) }
            else {
                let absValue = abs(value)
                if absValue > 0 && (absValue < 1e-4 || absValue >= 1e15) { formattedString = String(format: "%.4g", value) }
                else {
                    let tempFormatted = String(format: "%.10f", value)
                    if let regex = try? NSRegularExpression(pattern: "\\.?0+$") {
                        let nsString = tempFormatted as NSString
                        let range = NSRange(location: 0, length: nsString.length)
                        let modString = regex.stringByReplacingMatches(in: tempFormatted, options: [], range: range, withTemplate: "")
                        let finalString = modString.isEmpty ? "0" : modString
                        formattedString = finalString.hasSuffix(".") ? String(finalString.dropLast()) : finalString
                    } else { formattedString = tempFormatted }
                }
            }
        case .scientific: formattedString = String(format: "%.*e", settings.fixedDecimalPlaces, value)
        case .fixed: formattedString = String(format: "%.\(settings.fixedDecimalPlaces)f", value)
        }
        return settings.decimalSeparator == .comma ? formattedString.replacingOccurrences(of: ".", with: ",") : formattedString
    }
    
    private func findBestUnitFor(dimensions: UnitDimension) -> UnitDefinition? {
        // Special case: The user prefers to see volume in m^3 rather than converting to L.
        // This prevents the automatic conversion for results that are already in base SI volume units.
        if dimensions == [.meter: 3] {
            return nil
        }
        
        let preferredSymbols = ["N", "J", "W", "Pa", "Hz", "C", "V", "Ohm", "F", "H", "T", "L", "eV", "cal", "bar", "ha", "g"]

        var potentialMatches: [UnitDefinition] = []
        for (_, unitDef) in UnitStore.units {
            if unitDef.dimensions == dimensions {
                potentialMatches.append(unitDef)
            }
        }

        if potentialMatches.isEmpty {
            return nil
        }

        for symbol in preferredSymbols {
            if let match = potentialMatches.first(where: { $0.symbol == symbol }) {
                return match
            }
        }
        
        return potentialMatches.first(where: { !$0.symbol.contains(where: "kcmμn".contains) }) ?? potentialMatches.first
    }
    
    private func formatDimensionsForHistory(_ dimensions: UnitDimension) -> String {
        let positiveDims = dimensions.filter { $0.value > 0 }
        let negativeDims = dimensions.filter { $0.value < 0 }

        let sortedPositive = positiveDims.sorted { $0.key.rawValue < $1.key.rawValue }
        let sortedNegative = negativeDims.sorted { $0.key.rawValue < $1.key.rawValue }

        let formatPart = { (dims: [(key: BaseUnit, value: Int)]) -> String in
            dims.map { (unit, exponent) -> String in
                let symbol = UnitStore.units.first(where: { $0.value.dimensions == [unit: 1] && $0.value.conversionFactor == 1.0 })?.key ?? unit.rawValue
                return abs(exponent) == 1 ? "\(symbol)" : "\(symbol)^\(abs(exponent))"
            }.joined(separator: " ")
        }

        let numerator = formatPart(sortedPositive)
        let denominator = formatPart(sortedNegative)

        if numerator.isEmpty && denominator.isEmpty {
            return ""
        } else if !denominator.isEmpty {
            if numerator.isEmpty {
                return "1/\(denominator)"
            } else {
                return "\(numerator)/\(denominator)"
            }
        } else {
            return numerator
        }
    }
    
    private func formatDimensionsForParsing(_ dimensions: UnitDimension) -> String {
        if let bestUnit = findBestUnitFor(dimensions: dimensions) {
            if bestUnit.dimensions.isEmpty && !["deg", "rad"].contains(bestUnit.symbol) {
                 return ""
            }
            return bestUnit.symbol
        }
        
        let allDims = dimensions.sorted { $0.key.rawValue < $1.key.rawValue }
        
        return allDims.map { (unit, exponent) -> String in
            let symbol = UnitStore.units.first(where: { $0.value.dimensions == [unit: 1] && $0.value.conversionFactor == 1.0 })?.key ?? unit.rawValue
            return "\(symbol)\(exponent == 1 ? "" : "^\(exponent)")"
        }.joined(separator: " ")
    }

    private func formatComplexForDisplay(_ value: Complex) -> String {
        if value.real != 0 && value.imaginary != 0 { return "\(formatScalarForDisplay(value.real)) \(value.imaginary < 0 ? "-" : "+") \(formatScalarForDisplay(abs(value.imaginary)))i" }
        else if value.real != 0 { return formatScalarForDisplay(value.real) }
        else if value.imaginary != 0 { return "\(formatScalarForDisplay(value.imaginary))i" }
        else { return "0" }
    }
    
    private func formatPolarForDisplay(_ value: Complex) -> String {
        let magnitude = value.abs(); let angle = value.argument()
        if self.angleMode == .degrees { let angleDegrees = angle * (180.0 / .pi); return "\(formatScalarForDisplay(magnitude)) ∠ \(formatScalarForDisplay(angleDegrees))°" }
        else { return "\(formatScalarForDisplay(magnitude)) ∠ \(formatScalarForDisplay(angle)) rad" }
    }

    private func formatVectorForDisplay(_ vector: Vector) -> String {
        let maxDisplayRows = 10
        if vector.dimension <= maxDisplayRows {
            return (0..<vector.dimension).map { "[ \(formatScalarForDisplay(vector[$0])) ]" }.joined(separator: "\n")
        } else {
            var lines: [String] = []
            let headCount = 5
            let tailCount = 4
            for i in 0..<headCount {
                lines.append("[ \(formatScalarForDisplay(vector[i])) ]")
            }
            lines.append("... (\(vector.dimension - (headCount + tailCount)) more items) ...")
            for i in (vector.dimension - tailCount)..<vector.dimension {
                lines.append("[ \(formatScalarForDisplay(vector[i])) ]")
            }
            return lines.joined(separator: "\n")
        }
    }
    
    private func formatMatrixForDisplay(_ matrix: Matrix) -> String {
        let maxDisplayRows = 10
        if matrix.rows == 0 || matrix.columns == 0 { return "[]" }

        func formatRow(_ r: Int, columnWidths: [Int]) -> String {
            let rowContent = (0..<matrix.columns).map { c in
                let formattedNumber = formatScalarForDisplay(matrix[r, c])
                let padding = String(repeating: " ", count: columnWidths[c] - formattedNumber.count)
                return padding + formattedNumber
            }.joined(separator: "  ")
            return "[ \(rowContent) ]"
        }

        var columnWidths = [Int](repeating: 0, count: matrix.columns)
        for c in 0..<matrix.columns {
            var maxWidth = 0
            for r in 0..<matrix.rows {
                let formattedNumber = formatScalarForDisplay(matrix[r, c])
                if formattedNumber.count > maxWidth { maxWidth = formattedNumber.count }
            }
            columnWidths[c] = maxWidth
        }

        if matrix.rows <= maxDisplayRows {
            return (0..<matrix.rows).map { r in formatRow(r, columnWidths: columnWidths) }.joined(separator: "\n")
        } else {
            var lines: [String] = []
            let headCount = 5
            let tailCount = 4
            for r in 0..<headCount {
                lines.append(formatRow(r, columnWidths: columnWidths))
            }
            lines.append("... (\(matrix.rows - (headCount + tailCount)) more rows) ...")
            for r in (matrix.rows - tailCount)..<matrix.rows {
                lines.append(formatRow(r, columnWidths: columnWidths))
            }
            return lines.joined(separator: "\n")
        }
    }
    
    private func formatComplexVectorForDisplay(_ vector: ComplexVector) -> String {
        let maxDisplayRows = 10
        if vector.dimension <= maxDisplayRows {
            return (0..<vector.dimension).map { "[ \(formatComplexForDisplay(vector[$0])) ]" }.joined(separator: "\n")
        } else {
            var lines: [String] = []
            let headCount = 5
            let tailCount = 4
            for i in 0..<headCount {
                lines.append("[ \(formatComplexForDisplay(vector[i])) ]")
            }
            lines.append("... (\(vector.dimension - tailCount)) more items) ...")
            for i in (vector.dimension - tailCount)..<vector.dimension {
                lines.append("[ \(formatComplexForDisplay(vector[i])) ]")
            }
            return lines.joined(separator: "\n")
        }
    }
    
    private func formatComplexMatrixForDisplay(_ matrix: ComplexMatrix) -> String {
        let maxDisplayRows = 10
        if matrix.rows == 0 || matrix.columns == 0 { return "[]" }
        
        func formatRow(_ r: Int, columnWidths: [Int]) -> String {
            let rowContent = (0..<matrix.columns).map { c in
                let formattedNumber = "(\(formatComplexForDisplay(matrix[r, c])))"
                let padding = String(repeating: " ", count: columnWidths[c] - formattedNumber.count)
                return padding + formattedNumber
            }.joined(separator: "  ")
            return "[ \(rowContent) ]"
        }

        var columnWidths = [Int](repeating: 0, count: matrix.columns)
        for c in 0..<matrix.columns {
            var maxWidth = 0
            for r in 0..<matrix.rows {
                let formattedNumber = "(\(formatComplexForDisplay(matrix[r, c])))"
                if formattedNumber.count > maxWidth { maxWidth = formattedNumber.count }
            }
            columnWidths[c] = maxWidth
        }

        if matrix.rows <= maxDisplayRows {
            return (0..<matrix.rows).map { r in formatRow(r, columnWidths: columnWidths) }.joined(separator: "\n")
        } else {
            var lines: [String] = []
            let headCount = 5
            let tailCount = 4
            for r in 0..<headCount {
                lines.append(formatRow(r, columnWidths: columnWidths))
            }
            lines.append("... (\(matrix.rows - (headCount + tailCount)) more rows) ...")
            for r in (matrix.rows - tailCount)..<matrix.rows {
                lines.append(formatRow(r, columnWidths: columnWidths))
            }
            return lines.joined(separator: "\n")
        }
    }
    
    private func formatScalarForParsing(_ value: Double) -> String {
        let stringValue = String(value)
        if stringValue.lowercased().contains("e") {
            let parts = stringValue.lowercased().components(separatedBy: "e")
            if parts.count == 2 {
                 let mantissa = parts[0].replacingOccurrences(of: ".", with: settings.decimalSeparator.rawValue)
                 return "(\(mantissa)*10^\(parts[1]))"
            }
        }
        return stringValue.replacingOccurrences(of: ".", with: settings.decimalSeparator.rawValue)
    }
    
    private func formatComplexForParsing(_ value: Complex) -> String {
        let sign = value.imaginary < 0 ? "" : "+"; return "(\(formatScalarForParsing(value.real))\(sign)\(formatScalarForParsing(value.imaginary))i)"
    }
    
    private func formatPolarForParsing(_ value: Complex) -> String {
        let magnitude = value.abs(); let angleDegrees = value.argument() * (180.0 / .pi)
        return "\(formatScalarForParsing(magnitude))∠\(formatScalarForParsing(angleDegrees))"
    }
}

