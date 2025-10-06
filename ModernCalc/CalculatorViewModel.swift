//
//  CalculatorViewModel.swift
//  ModernCalc
//
//  Created by Manu Heiskanen on 28.8.25.
//

import Foundation
import SwiftUI

/// A structure to hold information about a single autocomplete suggestion.
struct AutocompleteSuggestion: Identifiable, Hashable, Equatable {
    let id = UUID()
    var name: String
    var type: String
    var description: String
    
    /// Provides the text that should be inserted into the expression field.
    /// For functions, it automatically adds an opening parenthesis.
    var insertionText: String {
        if type == "function" || type == "user_function" {
            if description.contains("(") {
                return name + "("
            }
        }
        return name
    }
    
    /// Provides the text that should be displayed in the autocomplete pill.
    /// e.g., "cos()", "f(x, y)", "my_variable"
    var displayText: String {
        if type == "function" {
            return name + "()"
        }
        if type == "user_function" {
            // The description field for user functions is already the desired signature.
            return description
        }
        return name
    }
}


@Observable
@MainActor
class CalculatorViewModel {

    var rawExpression: String = "" {
        didSet { handleInputChange() }
    }
    var history: [Calculation] = []
    var liveHelpText: String = ""
    var liveErrorText: String = ""
    var liveLaTeXPreview: String = ""
    var previewText: String = ""
    var variables: [String: MathValue] = [:]
    var functions: [String: FunctionDefinitionNode] = [:]
    var livePreviewHeight: CGFloat = 60.0
    
    // --- State for autocomplete ---
    var autocompleteSuggestions: [AutocompleteSuggestion] = []
    var showAutocomplete = false
    
    var angleMode: AngleMode = .degrees {
        didSet {
            saveState()
            handleInputChange()
        }
    }
    var userFunctionDefinitions: [String: String] = [:]
    var cursorPosition = NSRange() {
        didSet { handleInputChange() }
    }
    
    var plotViewModels: [PlotViewModel] = []
    var plotToShow: PlotData.ID? = nil
    
    var csvViewModel: CSVViewModel? = nil
    var showCSVView: Bool = false
    
    private var settings: UserSettings
    private let evaluator = Evaluator()
    private var lastSuccessfulValue: MathValue?
    private var lastUsedAngleFlag: Bool = false
    private var debounceTask: Task<Void, Never>?
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
            
        loadState()
    }
    
    private func handleInputChange() {
        // Cancel any previously scheduled task to reset the debounce timer
        debounceTask?.cancel()

        // Schedule a new task to run after a 50ms delay
        debounceTask = Task {
            do {
                try await Task.sleep(for: .milliseconds(50))
                
                // If the task wasn't cancelled while it was sleeping, perform the update
                guard !Task.isCancelled else { return }
                
                await performLiveUpdate()

            } catch {
                // This catch block is needed to handle cancellation errors from Task.sleep
            }
        }
    }

    private func performLiveUpdate() async {
        // This is the logic that was previously inside the Combine .sink { ... } block
        let expression = self.rawExpression
        let position = self.cursorPosition
        let angle = self.angleMode

        self.updateAutocompleteSuggestions(for: expression, at: position)

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
            let newHelpText = self.getContextualHelp(expression: expression, cursor: position) ?? ""
            if newHelpText != self.liveHelpText {
                // Recalculate height and update view properties when only help text changes.
                updateLivePreview(latex: self.liveLaTeXPreview, helpText: newHelpText, errorText: self.liveErrorText)
            }
            return
        }
        
        await self.calculate(expression: expression, cursor: position)
        self.lastCalculatedExpression = expression
        self.lastCalculatedAngleMode = angle
    }
    
    /// Replaces the currently typed word with the selected suggestion.
    func selectAutocomplete(suggestion: AutocompleteSuggestion) {
        let nsExpression = rawExpression as NSString
        let textBeforeCursor = nsExpression.substring(to: cursorPosition.location)
        
        let separators = CharacterSet(charactersIn: " +-*/^()=,;[]{}").union(.whitespacesAndNewlines)
        let rangeOfLastSeparator = textBeforeCursor.rangeOfCharacter(from: separators, options: .backwards)
        
        let locationOfWordStart: Int
        if let range = rangeOfLastSeparator {
            locationOfWordStart = textBeforeCursor.distance(from: textBeforeCursor.startIndex, to: range.upperBound)
        } else {
            locationOfWordStart = 0
        }
        
        let lengthOfWord = cursorPosition.location - locationOfWordStart
        let replacementNSRange = NSRange(location: locationOfWordStart, length: lengthOfWord)

        guard let replacementRange = Range(replacementNSRange, in: rawExpression) else { return }
        
        var newExpression = rawExpression
        newExpression.replaceSubrange(replacementRange, with: suggestion.insertionText)
        
        self.rawExpression = newExpression
        
        let newLocation = locationOfWordStart + suggestion.insertionText.utf16.count
        self.cursorPosition = NSRange(location: newLocation, length: 0)

        self.showAutocomplete = false
        self.autocompleteSuggestions = []
    }

    
    /// Finds the word currently being typed and searches for matching variables and functions.
    private func updateAutocompleteSuggestions(for expression: String, at position: NSRange) {
        // Run as a Task to avoid blocking the main thread during typing
        Task {
            guard position.location <= (expression as NSString).length else {
                // This prevents a crash if the expression updates faster than the cursor position
                self.autocompleteSuggestions = []
                self.showAutocomplete = false
                return
            }
            
            let textBeforeCursor = (expression as NSString).substring(to: position.location)
            let separators = CharacterSet(charactersIn: " +-*/^()=,;[]{}").union(.whitespacesAndNewlines)
            
            let word: String
            if let lastSeparatorRange = textBeforeCursor.rangeOfCharacter(from: separators, options: .backwards) {
                let wordStartIndex = lastSeparatorRange.upperBound
                word = String(textBeforeCursor[wordStartIndex...])
            } else {
                word = textBeforeCursor
            }

            guard !word.isEmpty else {
                self.autocompleteSuggestions = []
                self.showAutocomplete = false
                return
            }

            var suggestions: [AutocompleteSuggestion] = []
            let lowercasedWord = word.lowercased()

            suggestions.append(contentsOf: builtinFunctions
                .filter { $0.name.lowercased().starts(with: lowercasedWord) }
                .map { AutocompleteSuggestion(name: $0.name, type: "function", description: $0.signature) }
            )
            suggestions.append(contentsOf: functions.keys
                .filter { $0.lowercased().starts(with: lowercasedWord) }
                .map { name in
                    let node = functions[name]!
                    let signature = "\(name)(\(node.parameterNames.joined(separator: ", ")))"
                    return AutocompleteSuggestion(name: name, type: "user_function", description: signature)
                }
            )
            suggestions.append(contentsOf: variables.keys
                .filter { $0.lowercased().starts(with: lowercasedWord) && $0 != ansVariable }
                .map { AutocompleteSuggestion(name: $0, type: "variable", description: self.formatForHistory(variables[$0]!)) }
            )
            suggestions.append(contentsOf: physicalConstants
                .filter { $0.symbol.lowercased().starts(with: lowercasedWord) }
                .map { AutocompleteSuggestion(name: $0.symbol, type: "constant", description: $0.name) }
            )
            
            let newSuggestions = Array(Set(suggestions)).sorted { $0.name.localizedStandardCompare($1.name) == .orderedAscending }
            
            // Hide popover if there's only one suggestion and it's an exact match
            if newSuggestions.count == 1, let suggestion = newSuggestions.first, suggestion.name.lowercased() == lowercasedWord {
                self.showAutocomplete = false
                self.autocompleteSuggestions = []
                return
            }
            
            if self.autocompleteSuggestions != newSuggestions {
                self.autocompleteSuggestions = newSuggestions
            }
            self.showAutocomplete = !newSuggestions.isEmpty
        }
    }
    
    func triggerCSVImport() {
        _ = openCSVFile()
    }

    private func evaluateExpression(_ expression: String, vars: [String: MathValue], funcs: [String: FunctionDefinitionNode], angleMode: AngleMode, settings: UserSettings) async throws -> MathValue {
        var tempVars = vars
        var tempFuncs = funcs
        let lexer = Lexer(input: expression, decimalSeparator: settings.decimalSeparator)
        let tokens = lexer.tokenize()
        let parser = Parser(tokens: tokens)
        let node = try parser.parse()
        let (value, _) = try evaluator.evaluate(node: node, variables: &tempVars, functions: &tempFuncs, angleMode: angleMode)
        return value
    }
    
    func addPlotViewModel(for plotData: PlotData) {
        if !plotViewModels.contains(where: { $0.plotData.id == plotData.id }) {
            
            let expression = plotData.expression
            let capturedVars = self.variables
            let capturedFuncs = self.functions
            let capturedSettings = self.settings
            let capturedAngleMode = self.angleMode

            let handler = { (newDomain: ClosedRange<Double>) -> Task<[PlotSeries]?, Never> in
                Task {
                    do {
                        var expressionToEvaluate = expression
                        let trimmedExpression = expression.trimmingCharacters(in: .whitespaces)
                        
                        // FIX: Calculate a wider render domain to pre-load data for panning.
                        let span = newDomain.upperBound - newDomain.lowerBound
                        let renderDomain = (newDomain.lowerBound - span)...(newDomain.upperBound + span)

                        if trimmedExpression.starts(with: "autoplot") {
                            let pattern = #"autoplot\s*\((.*)\)"#
                            if let regex = try? NSRegularExpression(pattern: pattern),
                               let match = regex.firstMatch(in: expression, range: NSRange(expression.startIndex..., in: expression)),
                               let range = Range(match.range(at: 1), in: expression) {
                                let innerExpression = String(expression[range])
                                // FIX: Use the new, wider renderDomain for evaluation.
                                expressionToEvaluate = "plot(\(innerExpression), x=(\(renderDomain.lowerBound), \(renderDomain.upperBound)))"
                            }
                        } else if trimmedExpression.starts(with: "plot") {
                            let pattern = #"plot\s*\(([^,]+).*\)"#
                            if let regex = try? NSRegularExpression(pattern: pattern),
                               let match = regex.firstMatch(in: expression, range: NSRange(expression.startIndex..., in: expression)),
                               let range = Range(match.range(at: 1), in: expression) {
                                let functionExpression = String(expression[range])
                                // FIX: Use the new, wider renderDomain for evaluation.
                                expressionToEvaluate = "plot(\(functionExpression), x=(\(renderDomain.lowerBound), \(renderDomain.upperBound)))"
                            }
                        }
                                        
                        let resultValue = try await self.evaluateExpression(
                            expressionToEvaluate,
                            vars: capturedVars,
                            funcs: capturedFuncs,
                            angleMode: capturedAngleMode,
                            settings: capturedSettings
                        )
                                            
                        if case .plot(let newPlotData) = resultValue {
                            return newPlotData.series
                        }
                        return nil
                    } catch {
                        print("Regeneration failed: \(error)")
                        return nil
                    }
                }
            }
            
            let newPlotViewModel = PlotViewModel(plotData: plotData, regenerationHandler: handler)
            Task {
                plotViewModels.append(newPlotViewModel)
            }
        }
    }

    func requestOpenPlotWindow(for plotData: PlotData) {
        addPlotViewModel(for: plotData)
        Task {
            plotToShow = plotData.id
        }
    }

    func closePlotWindow(id: PlotData.ID?) {
        Task {
            plotViewModels.removeAll { $0.plotData.id == id }
        }
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
        var finalLiveLaTeXPreview: String
        
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
                    default: resultLaTeX = LaTeXEngine.formatMathValue(value, angleMode: self.angleMode, settings: self.settings, expression: expression)
                    }
                } else {
                    if self.settings.enableLiveRounding {
                        let liveSettings = self.settings.makeTemporaryCopy()
                        liveSettings.displayMode = .fixed
                        liveSettings.fixedDecimalPlaces = self.settings.livePreviewDecimalPlaces
                        resultLaTeX = LaTeXEngine.formatMathValue(value, angleMode: self.angleMode, settings: liveSettings, expression: expression)
                    } else {
                        resultLaTeX = LaTeXEngine.formatMathValue(value, angleMode: self.angleMode, settings: self.settings, expression: expression)
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
        
        updateLivePreview(latex: finalLiveLaTeXPreview, helpText: finalLiveHelpText, errorText: finalLiveErrorText)
    }
    
    private func updateLivePreview(latex: String, helpText: String, errorText: String) {
        // --- HEIGHT CALCULATION LOGIC ---
        var verticalityScore = 0

        // First, check for help/error text. This always takes precedence for height calculation.
        if !errorText.isEmpty || !helpText.isEmpty {
            var totalLines = 0
            let charsPerLine = 55 // Approximate characters per line in the view.

            if !helpText.isEmpty {
                // A multi-line help string might have explicit newlines.
                let explicitHelpNewlines = helpText.components(separatedBy: "\n").count
                // Also estimate lines if a single line is very long and would wrap.
                let wrappedHelpLines = Int(ceil(Double(helpText.count) / Double(charsPerLine)))
                totalLines += max(explicitHelpNewlines, wrappedHelpLines)
            }

            if !errorText.isEmpty {
                let explicitErrorNewlines = errorText.components(separatedBy: "\n").count
                let wrappedErrorLines = Int(ceil(Double(errorText.count) / Double(charsPerLine)))
                totalLines += max(explicitErrorNewlines, wrappedErrorLines)
            }
            
            // The score is based on extra lines beyond the first one (which is covered by baseHeight).
            verticalityScore = totalLines > 1 ? totalLines - 1 : 0
            
        } else {
            // ONLY if there's no help/error text, calculate height based on the visual structure of the LaTeX.
            // This is more reliable as it directly reflects what is being rendered.
            let latexNewlines = latex.components(separatedBy: "\\\\").count - 1
            let fractionCount = latex.components(separatedBy: "\\frac").count - 1
            
            verticalityScore = max(0, latexNewlines) + fractionCount
        }

        // Define sizing parameters.
        let baseHeight: CGFloat = 60.0
        let heightPerUnit: CGFloat = 22.0
        let maxHeight: CGFloat = 200.0

        var calculatedHeight = baseHeight + (CGFloat(verticalityScore) * heightPerUnit)

        // A fallback for long, single-line LaTeX that doesn't have vertical operators.
        if verticalityScore == 0 && latex.count > 80 && errorText.isEmpty && helpText.isEmpty {
            calculatedHeight = 100.0
        }

        Task {
            self.livePreviewHeight = min(calculatedHeight, maxHeight)
            self.liveHelpText = helpText
            self.liveErrorText = errorText
            self.liveLaTeXPreview = latex
        }
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
                Task {
                    self.insertTextAtCursor(textToInsert)
                }
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
    
    func deleteVariable(name: String) {
        Task {
            variables.removeValue(forKey: name)
            saveState()
        }
    }
    func deleteFunction(name: String) {
        Task {
            functions.removeValue(forKey: name)
            userFunctionDefinitions.removeValue(forKey: name)
            saveState()
        }
    }
    
    func insertTextAtCursor(_ textToInsert: String) {
        guard let range = Range(cursorPosition, in: rawExpression) else {
            Task {
                rawExpression += textToInsert; let newLocation = rawExpression.utf16.count; cursorPosition = NSRange(location: newLocation, length: 0)
            }
            return
        }
        rawExpression.replaceSubrange(range, with: textToInsert)
        Task {
            let newLocation = cursorPosition.location + textToInsert.utf16.count; cursorPosition = NSRange(location: newLocation, length: 0)
        }
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
        Task {
            self.functions = tempFuncs; self.variables = tempVars; self.liveHelpText = ""; self.liveErrorText = ""
        }
    }
    
    // MARK: - Formatting Proxies
    
    func formatCalculationAsLaTeX(_ calculation: Calculation) -> String {
        return LaTeXEngine.format(calculation: calculation, evaluator: self.evaluator, angleMode: self.angleMode, settings: self.settings)
    }

    func formatForHistory(_ value: MathValue) -> String {
        return DisplayFormatter.formatForHistory(value, with: self.settings, angleMode: self.angleMode)
    }
    
    func formatRootsForDisplay(_ roots: [MathValue]) -> [String] {
        return DisplayFormatter.formatRootsForDisplay(roots, with: self.settings, angleMode: self.angleMode)
    }
    
    func formatForParsing(_ value: MathValue) -> String {
        return DisplayFormatter.formatForParsing(value, with: self.settings)
    }
    
    func formatScalarForDisplay(_ value: Double) -> String {
        return DisplayFormatter.formatScalarForDisplay(value, with: self.settings)
    }
}

