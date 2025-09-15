//
//  ContentView.swift
//  ModernCalc
//
//  Created by Manu Heiskanen on 28.8.2025.
//a
import SwiftUI

struct ContentView: View {
    @ObservedObject var settings: UserSettings
    @StateObject var viewModel: CalculatorViewModel
    
    @FocusState private var isInputFocused: Bool
    @State private var isShowingSheet = false
    @Environment(\.openWindow) private var openWindow
    
    // --- ADDED: State for toolbar button hover effects ---
    @State private var isHoveringOnMenuButton = false
    @State private var isHoveringOnCSVButton = false
    
    init(settings: UserSettings, viewModel: CalculatorViewModel) {
        self.settings = settings
        _viewModel = StateObject(wrappedValue: viewModel)
    }

    var body: some View {
        VStack(spacing: 0) {
            HistoryView(
                viewModel: viewModel,
                history: viewModel.history,
                rawExpression: $viewModel.rawExpression,
                selectedHistoryId: viewModel.selectedHistoryId,
                selectedHistoryPart: viewModel.selectedHistoryPart
            )
            Divider()
            
            FormattedExpressionWithButtonsView(
                viewModel: viewModel,
                latexPreview: viewModel.liveLaTeXPreview,
                helpText: viewModel.liveHelpText,
                errorText: viewModel.liveErrorText,
                greekSymbols: viewModel.greekSymbols
            )
            .frame(height: viewModel.livePreviewHeight)
            Divider()

            CalculatorInputView(
                viewModel: viewModel,
                expression: $viewModel.rawExpression,
                cursorPosition: $viewModel.cursorPosition,
                previewText: viewModel.previewText,
                operatorSymbols: viewModel.operatorSymbols,
                constantSymbols: viewModel.constantSymbols,
                onTap: { viewModel.resetNavigation() }
            )
            .focused($isInputFocused)
            .onAppear {
                isInputFocused = true
            }
            .onChange(of: viewModel.plotToShow) { oldID, newID in
                if let id = newID {
                    openWindow(value: id)
                    viewModel.plotToShow = nil // Reset after opening
                }
            }
            .onKeyPress(keys: [.upArrow, .downArrow, .leftArrow, .rightArrow, .return]) { key in
                if key.key == .return {
                    if let plotData = viewModel.commitCalculation() {
                        openWindow(value: plotData.id)
                    }
                    return .handled
                }
                
                if viewModel.handleKeyPress(keys: [key.key]) {
                    return .handled
                }
                return .ignored
            }
            .sheet(isPresented: $isShowingSheet) {
                VariableEditorView(viewModel: viewModel, settings: settings)
            }
            .sheet(isPresented: $viewModel.showCSVView) {
                if let csvViewModel = viewModel.csvViewModel {
                    CSVView(viewModel: csvViewModel)
                }
            }
        }
        .animation(.easeInOut(duration: 0.25), value: viewModel.livePreviewHeight)
        .frame(minWidth: 410, minHeight: 300)
        .toolbar {
            ToolbarItemGroup(placement: .principal) {
                HStack(spacing: 12) {
                    Button(action: { isShowingSheet = true }) {
                        Image(systemName: "ellipsis.circle.fill")
                            .font(.system(size: 20))
                            .foregroundColor(isHoveringOnMenuButton ? .primary : .secondary)
                    }
                    .buttonStyle(.plain)
                    .padding(8)
                    .background(Color.primary.opacity(isHoveringOnMenuButton ? 0.1 : 0))
                    .cornerRadius(8)
                    .scaleEffect(isHoveringOnMenuButton ? 1.1 : 1.0)
                    .onHover { hovering in
                        withAnimation(.easeInOut(duration: 0.15)) {
                            isHoveringOnMenuButton = hovering
                        }
                    }
                    
                    Button(action: {
                        viewModel.triggerCSVImport()
                    }) {
                        Text(".csv")
                            .font(.system(size: 14, weight: .bold, design: .monospaced))
                            .foregroundColor(.secondary)
                            .padding(.horizontal, 10)
                            .padding(.vertical, 5)
                            .overlay(
                                RoundedRectangle(cornerRadius: 8)
                                    .stroke(Color.secondary.opacity(0.5), lineWidth: 1)
                            )
                    }
                    .buttonStyle(.plain)
                    .background(Color.primary.opacity(isHoveringOnCSVButton ? 0.1 : 0))
                    .cornerRadius(8)
                    .scaleEffect(isHoveringOnCSVButton ? 1.05 : 1.0)
                    .onHover { hovering in
                        withAnimation(.easeInOut(duration: 0.15)) {
                            isHoveringOnCSVButton = hovering
                        }
                    }
                }
            }
            
            ToolbarItemGroup {
                HStack(spacing: 0) {
                    Button("DEG") {
                        viewModel.angleMode = .degrees
                    }
                    .padding(.horizontal, 12)
                    .padding(.vertical, 4)
                    .background(viewModel.angleMode == .degrees ? Color.orange : Color.clear)
                    .foregroundColor(viewModel.angleMode == .degrees ? .white : .primary)
                    .clipShape(RoundedRectangle(cornerRadius: 6, style: .continuous))

                    Button("RAD") {
                        viewModel.angleMode = .radians
                    }
                    .padding(.horizontal, 12)
                    .padding(.vertical, 4)
                    .background(viewModel.angleMode == .radians ? Color.purple : Color.clear)
                    .foregroundColor(viewModel.angleMode == .radians ? .white : .primary)
                    .clipShape(RoundedRectangle(cornerRadius: 6, style: .continuous))
                }
                .buttonStyle(.plain)
                .background(Color.secondary.opacity(0.15))
                .clipShape(RoundedRectangle(cornerRadius: 8, style: .continuous))
            }
        }
    }
}

// --- Subviews ---

struct HistoryView: View {
    @ObservedObject var viewModel: CalculatorViewModel
    var history: [Calculation]
    @Binding var rawExpression: String
    var selectedHistoryId: UUID?
    var selectedHistoryPart: SelectionPart
    
    @State private var lastAddedId: UUID?
    @State private var hoveredItem: (id: UUID, part: SelectionPart)?
    @State private var hoveredRowId: UUID?
    @State private var showCopyMessage = false

    var body: some View {
        ZStack {
            ScrollViewReader { scrollViewProxy in
                ScrollView {
                    LazyVStack(spacing: 0) {
                        ForEach(Array(history.enumerated()), id: \.element.id) { index, calculation in
                            ZStack(alignment: .topLeading) {
                                VStack(alignment: .trailing, spacing: 4) {
                                    switch calculation.type {
                                    case .functionDefinition, .variableAssignment:
                                        HStack {
                                            Text(highlightSIPrefixes(in: calculation.expression))
                                                .font(.system(size: 24, weight: .light, design: .monospaced))
                                                .foregroundColor(.primary)
                                                .padding(.horizontal, 2)
                                                .padding(.vertical, 2)
                                                .background((hoveredItem?.id == calculation.id || selectedHistoryId == calculation.id) ? Color.accentColor.opacity(0.25) : Color.clear)
                                                .onHover { isHovering in
                                                    withAnimation(.easeOut(duration: 0.15)) { hoveredItem = isHovering ? (id: calculation.id, part: .equation) : nil }
                                                    if isHovering { NSCursor.pointingHand.push() } else { NSCursor.pop() }
                                                }
                                                .onTapGesture { viewModel.insertTextAtCursor(calculation.expression) }
                                            
                                            Button(action: { copyToClipboard(calculation: calculation) }) { Image(systemName: "doc.on.doc") }.buttonStyle(.plain).opacity(hoveredRowId == calculation.id ? 1.0 : 0.2)
                                        }
                                        .padding(.vertical, 8).padding(.horizontal)
                                        .frame(maxWidth: .infinity, alignment: .trailing)
                                    case .plot:
                                        HStack {
                                            Image(systemName: "chart.xyaxis.line")
                                                .font(.system(size: 20))
                                                .foregroundColor(.accentColor)
                                            Text(calculation.expression)
                                                 .font(.system(size: 24, weight: .light, design: .monospaced))
                                                 .foregroundColor(.primary)
                                                 .padding(.horizontal, 2)
                                                 .padding(.vertical, 2)
                                                 .background((hoveredItem?.id == calculation.id || selectedHistoryId == calculation.id) ? Color.accentColor.opacity(0.25) : Color.clear)
                                                 .onHover { isHovering in
                                                     withAnimation(.easeOut(duration: 0.15)) { hoveredItem = isHovering ? (id: calculation.id, part: .equation) : nil }
                                                     if isHovering { NSCursor.pointingHand.push() } else { NSCursor.pop() }
                                                 }
                                                 .onTapGesture {
                                                    if case .plot(let plotData) = calculation.result {
                                                        viewModel.requestOpenPlotWindow(for: plotData)
                                                    }
                                                 }
                                            Button(action: { copyToClipboard(calculation: calculation) }) { Image(systemName: "doc.on.doc") }.buttonStyle(.plain).opacity(hoveredRowId == calculation.id ? 1.0 : 0.2)
                                        }
                                        .padding(.vertical, 8).padding(.horizontal)
                                        .frame(maxWidth: .infinity, alignment: .trailing)
                                        
                                    case .evaluation:
                                        HStack(alignment: .bottom, spacing: 8) {
                                            HStack(spacing: 8) {
                                                if calculation.usedAngleSensitiveFunction {
                                                    Circle().fill(calculation.angleMode == .degrees ? .orange : .purple).frame(width: 8, height: 8).offset(y: -4)
                                                }
                                                Text(highlightSIPrefixes(in: calculation.expression))
                                            }
                                            .font(.system(size: 24, weight: .light, design: .monospaced)).foregroundColor(.primary).padding(.horizontal, 2).padding(.vertical, 2)
                                            .background((hoveredItem?.id == calculation.id && hoveredItem?.part == .equation) || (selectedHistoryId == calculation.id && selectedHistoryPart == .equation) ? Color.accentColor.opacity(0.25) : Color.clear)
                                            .onHover { isHovering in
                                                withAnimation(.easeOut(duration: 0.15)) { hoveredItem = isHovering ? (id: calculation.id, part: .equation) : nil }
                                                if isHovering { NSCursor.pointingHand.push() } else { NSCursor.pop() }
                                            }
                                            .onTapGesture { viewModel.insertTextAtCursor(calculation.expression) }

                                            Text("=").font(.system(size: 24, weight: .light, design: .monospaced)).foregroundColor(.secondary)
                                            
                                            CalculationResultView(
                                                viewModel: viewModel,
                                                calculation: calculation,
                                                hoveredItem: $hoveredItem,
                                                selectedHistoryId: selectedHistoryId,
                                                selectedHistoryPart: selectedHistoryPart
                                            )
                                            
                                            Button(action: { copyToClipboard(calculation: calculation) }) { Image(systemName: "doc.on.doc") }.buttonStyle(.plain).opacity(hoveredRowId == calculation.id ? 1.0 : 0.2)
                                        }
                                        .padding(.vertical, 12).padding(.horizontal)
                                        .frame(maxWidth: .infinity, alignment: .trailing)
                                    }
                                    
                                    Divider().opacity(0.4)
                                }
                                .animation(.default, value: selectedHistoryId)
                            }
                            .id(calculation.id).transition(.opacity)
                            .background(calculation.id == lastAddedId ? Color.accentColor.opacity(0.1) : Color.clear)
                            .onHover { isHovering in withAnimation(.easeInOut(duration: 0.15)) { hoveredRowId = isHovering ? calculation.id : nil } }
                            .onAppear {
                                if calculation.id == lastAddedId {
                                    DispatchQueue.main.asyncAfter(deadline: .now() + 0.2) {
                                        withAnimation(.easeOut(duration: 0.5)) {
                                            lastAddedId = nil
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
                .onChange(of: history) {
                    if let lastEntry = history.last {
                        lastAddedId = lastEntry.id
                        withAnimation {
                            scrollViewProxy.scrollTo(lastEntry.id, anchor: .bottom)
                        }
                    }
                }
            }
            .frame(maxHeight: .infinity)
            
            if showCopyMessage { Text("Copied to Clipboard").padding(.horizontal, 16).padding(.vertical, 10).background(.ultraThickMaterial).cornerRadius(12).transition(.opacity.animation(.easeInOut)) }
        }
    }
    
    private func copyToClipboard(calculation: Calculation) {
        let latexString = viewModel.formatCalculationAsLaTeX(calculation)
        NSPasteboard.general.clearContents()
        NSPasteboard.general.setString(latexString, forType: .string)
        withAnimation { showCopyMessage = true }
        Task { try? await Task.sleep(nanoseconds: 2_000_000_000); withAnimation { showCopyMessage = false } }
    }
    
    private func highlightSIPrefixes(in expression: String) -> AttributedString {
        var attributedString = AttributedString(expression)
        let pattern = "(?<![a-zA-Z])(" + viewModel.siPrefixes.joined(separator: "|") + ")\\b"
        do {
            let regex = try NSRegularExpression(pattern: pattern)
            let nsRange = NSRange(expression.startIndex..., in: expression)
            let matches = regex.matches(in: expression, options: [], range: nsRange)
            for match in matches {
                if let range = Range(match.range(at: 1), in: expression) {
                    if let attrRange = attributedString.range(of: expression[range]) { attributedString[attrRange].foregroundColor = .teal }
                }
            }
        } catch { print("Regex error for SI prefix highlighting: \(error)") }
        return attributedString
    }
}

/// A new subview to render the different types of calculation results.
/// This simplifies the main HistoryView and fixes the compiler error.
struct CalculationResultView: View {
    @ObservedObject var viewModel: CalculatorViewModel
    let calculation: Calculation
    
    @Binding var hoveredItem: (id: UUID, part: SelectionPart)?
    var selectedHistoryId: UUID?
    var selectedHistoryPart: SelectionPart
    
    @State private var isHoverForExpansion = false
    private let columns = [GridItem(.adaptive(minimum: 60))]
    
    var body: some View {
        if case .tuple(let values) = calculation.result {
            HStack(spacing: 0) {
                ForEach(Array(values.enumerated()), id: \.offset) { index, value in
                    Text(viewModel.formatForHistory(value))
                        .font(.system(size: 24, weight: .light, design: .monospaced)).multilineTextAlignment(.trailing).foregroundColor(.primary).padding(.horizontal, 2).padding(.vertical, 2)
                        .background(isResultSelected(calculation: calculation, index: index) ? Color.accentColor.opacity(0.25) : Color.clear)
                        .onHover { isHovering in
                            withAnimation(.easeOut(duration: 0.15)) { hoveredItem = isHovering ? (id: calculation.id, part: .result(index: index)) : nil }
                            if isHovering { NSCursor.pointingHand.push() } else { NSCursor.pop() }
                        }
                        .onTapGesture { viewModel.insertTextAtCursor(viewModel.formatForParsing(value)) }
                    
                    if index < values.count - 1 {
                        Text(" OR ").font(.system(size: 20, weight: .light, design: .monospaced)).foregroundColor(.secondary)
                    }
                }
            }
        } else if case .regressionResult(let slope, let intercept) = calculation.result {
            HStack(spacing: 0) {
                Text("m = \(viewModel.formatScalarForDisplay(slope))")
                    .font(.system(size: 24, weight: .light, design: .monospaced)).multilineTextAlignment(.trailing).foregroundColor(.primary).padding(.horizontal, 2).padding(.vertical, 2)
                    .background(isResultSelected(calculation: calculation, index: 0) ? Color.accentColor.opacity(0.25) : Color.clear)
                    .onHover { isHovering in
                        withAnimation(.easeOut(duration: 0.15)) { hoveredItem = isHovering ? (id: calculation.id, part: .result(index: 0)) : nil }
                        if isHovering { NSCursor.pointingHand.push() } else { NSCursor.pop() }
                    }
                    .onTapGesture { viewModel.insertTextAtCursor(viewModel.formatForParsing(.dimensionless(slope))) }
                
                Text(", ")
                    .font(.system(size: 20, weight: .light, design: .monospaced)).foregroundColor(.secondary)

                Text("b = \(viewModel.formatScalarForDisplay(intercept))")
                    .font(.system(size: 24, weight: .light, design: .monospaced)).multilineTextAlignment(.trailing).foregroundColor(.primary).padding(.horizontal, 2).padding(.vertical, 2)
                    .background(isResultSelected(calculation: calculation, index: 1) ? Color.accentColor.opacity(0.25) : Color.clear)
                    .onHover { isHovering in
                        withAnimation(.easeOut(duration: 0.15)) { hoveredItem = isHovering ? (id: calculation.id, part: .result(index: 1)) : nil }
                        if isHovering { NSCursor.pointingHand.push() } else { NSCursor.pop() }
                    }
                    .onTapGesture { viewModel.insertTextAtCursor(viewModel.formatForParsing(.dimensionless(intercept))) }
            }
        } else if case .roots(let roots) = calculation.result {
            let isKeyboardSelectedForExpansion = selectedHistoryId == calculation.id && {
                if case .result = selectedHistoryPart { return true }
                return false
            }()

            let isExpanded = isKeyboardSelectedForExpansion || isHoverForExpansion
            
            Group {
                if isExpanded {
                    VStack(alignment: .trailing, spacing: 8) {
                        HStack {
                            Text("\(getVariableName(from: calculation.expression)) ≈ { ... }")
                                .font(.system(size: 20, weight: .light, design: .monospaced))
                                .foregroundColor(.secondary)
                                .onTapGesture { viewModel.insertTextAtCursor(viewModel.formatForParsing(calculation.result)) }
                            
                            Image(systemName: "arrow.down.right.and.arrow.up.left.circle")
                                .foregroundColor(.secondary)
                        }
                        
                        LazyVGrid(columns: columns, alignment: .trailing, spacing: 8) {
                            ForEach(Array(roots.enumerated()), id: \.offset) { index, root in
                                 Text(formatRoot(root))
                                    .font(.system(size: 22, weight: .light, design: .monospaced))
                                    .padding(.horizontal, 8)
                                    .padding(.vertical, 4)
                                    .background(isResultSelected(calculation: calculation, index: index) ? Color.accentColor.opacity(0.25) : Color.clear)
                                    .cornerRadius(6)
                                    .onHover { isHovering in
                                        guard selectedHistoryId != calculation.id else {
                                            hoveredItem = nil
                                            return
                                        }
                                        withAnimation(.easeOut(duration: 0.15)) { hoveredItem = isHovering ? (id: calculation.id, part: .result(index: index)) : nil }
                                        if isHovering { NSCursor.pointingHand.push() } else { NSCursor.pop() }
                                    }
                                    .onTapGesture { viewModel.insertTextAtCursor(viewModel.formatForParsing(.dimensionless(root))) }
                            }
                        }
                    }
                    .padding(8)
                    .background(Color.primary.opacity(0.05))
                    .cornerRadius(8)

                } else {
                    HStack(spacing: 4) {
                        let variableName = getVariableName(from: calculation.expression)
                        Text("\(variableName) ≈")
                            .font(.system(size: 24, weight: .light, design: .monospaced))
                            .foregroundColor(.primary)
                        
                        Text("{")
                            .font(.system(size: 24, weight: .light, design: .monospaced))
                            .foregroundColor(.secondary)
                        
                        let maxDisplayRoots = 4
                        let displayRoots = roots.count > maxDisplayRoots ? Array(roots.prefix(maxDisplayRoots)) : roots
                        
                        ForEach(Array(displayRoots.enumerated()), id: \.offset) { index, root in
                            Text(formatRoot(root))
                                .font(.system(size: 24, weight: .light, design: .monospaced)).multilineTextAlignment(.trailing).foregroundColor(.primary).padding(.horizontal, 2).padding(.vertical, 2)
                                .background(isResultSelected(calculation: calculation, index: index) ? Color.accentColor.opacity(0.25) : Color.clear)
                                .onHover { isHovering in
                                    guard selectedHistoryId != calculation.id else {
                                        hoveredItem = nil
                                        return
                                    }
                                    withAnimation(.easeOut(duration: 0.15)) { hoveredItem = isHovering ? (id: calculation.id, part: .result(index: index)) : nil }
                                    if isHovering { NSCursor.pointingHand.push() } else { NSCursor.pop() }
                                }
                                .onTapGesture { viewModel.insertTextAtCursor(viewModel.formatForParsing(.dimensionless(root))) }
                            
                            if index < displayRoots.count - 1 {
                                Text(",")
                                    .font(.system(size: 20, weight: .light, design: .monospaced)).foregroundColor(.secondary)
                            }
                        }
                        
                        if roots.count > maxDisplayRoots {
                            Text("... (\(roots.count - maxDisplayRoots) more)")
                                .font(.system(size: 18, weight: .light, design: .monospaced)).foregroundColor(.secondary)
                        }
                        
                        Text("}")
                            .font(.system(size: 24, weight: .light, design: .monospaced))
                            .foregroundColor(.secondary)
                    }
                    .onTapGesture { viewModel.insertTextAtCursor(viewModel.formatForParsing(calculation.result)) }
                }
            }
            .onHover { hovering in
                withAnimation {
                    isHoverForExpansion = hovering
                }
            }
        } else {
            Text(viewModel.formatForHistory(calculation.result))
                .font(.system(size: 24, weight: .light, design: .monospaced)).multilineTextAlignment(.trailing).foregroundColor(.primary).padding(.horizontal, 2).padding(.vertical, 2)
                .background(isResultSelected(calculation: calculation, index: 0) ? Color.accentColor.opacity(0.25) : Color.clear)
                .onHover { isHovering in
                    withAnimation(.easeOut(duration: 0.15)) { hoveredItem = isHovering ? (id: calculation.id, part: .result(index: 0)) : nil }
                    if isHovering { NSCursor.pointingHand.push() } else { NSCursor.pop() }
                }
                .onTapGesture { viewModel.insertTextAtCursor(viewModel.formatForParsing(calculation.result)) }
        }
    }
    
    private func formatRoot(_ root: Double) -> String {
        let tolerance = 1e-9
        let roundedRoot = root.rounded()
        let valueToDisplay = abs(root - roundedRoot) < tolerance ? roundedRoot : root
        return viewModel.formatScalarForDisplay(valueToDisplay)
    }
    
    private func isResultSelected(calculation: Calculation, index: Int) -> Bool {
        if let hoveredPart = hoveredItem?.part, case .result(let hoveredIndex) = hoveredPart, hoveredItem?.id == calculation.id { return hoveredIndex == index }
        if case .result(let selectedIndex) = selectedHistoryPart, selectedHistoryId == calculation.id { return selectedIndex == index }
        return false
    }
    
    private func getVariableName(from expression: String) -> String {
        let pattern = #"(?:solve|nsolve)\s*\([^,]+,\s*([a-zA-Z_][a-zA-Z0-9_]*)"#
        do {
            let regex = try NSRegularExpression(pattern: pattern)
            if let match = regex.firstMatch(in: expression, range: NSRange(expression.startIndex..., in: expression)),
               let range = Range(match.range(at: 1), in: expression) {
                return String(expression[range])
            }
        } catch {}
        return "x" // Default
    }
}


struct FormattedExpressionWithButtonsView: View {
    var viewModel: CalculatorViewModel
    var latexPreview: String
    var helpText: String
    var errorText: String
    var greekSymbols: [MathSymbol]
    @State private var isShowingGreekPopover = false
    
    var body: some View {
        ZStack {
            Color.gray.opacity(0.1)
            HStack {
                Button(action: { isShowingGreekPopover = true }) {
                    Text("α").font(.system(size: 22))
                }
                .buttonStyle(.plain)
                .padding(.horizontal)
                .popover(isPresented: $isShowingGreekPopover, arrowEdge: .top) {
                    GreekSymbolsGridView(viewModel: viewModel, greekSymbols: greekSymbols)
                }
                
                if !helpText.isEmpty || !errorText.isEmpty {
                    VStack(alignment: .leading, spacing: 4) {
                        if !helpText.isEmpty {
                            Text(helpText)
                                .font(.system(.body, design: .monospaced))
                                .foregroundColor(.primary)
                                .textSelection(.enabled)
                        }
                        if !errorText.isEmpty {
                            Text(errorText)
                                .font(.system(.body, design: .monospaced))
                                .foregroundColor(.red)
                                .textSelection(.enabled)
                        }
                    }
                    .padding(.trailing)
                    
                    Spacer()
                } else {
                    MathJaxView(latex: latexPreview)
                    Spacer()
                }
            }
        }
    }
}


struct CalculatorInputView: View {
    var viewModel: CalculatorViewModel
    @Binding var expression: String
    @Binding var cursorPosition: NSRange
    var previewText: String
    var operatorSymbols: [MathSymbol]
    var constantSymbols: [MathSymbol]
    var onTap: () -> Void
    
    @State private var isShowingSymbolsPopover = false

    var body: some View {
        HStack(spacing: 0) {
            Button(action: { isShowingSymbolsPopover = true }) { Image(systemName: "plus.slash.minus").font(.system(size: 20)) }.buttonStyle(.plain).padding()
                .popover(isPresented: $isShowingSymbolsPopover, arrowEdge: .bottom) {
                    SymbolsGridView(viewModel: viewModel, operatorSymbols: operatorSymbols, constantSymbols: constantSymbols)
                }
            
            ZStack(alignment: .leading) {
                HStack(spacing: 0) {
                    Text(expression).font(.system(size: 26, weight: .regular, design: .monospaced)).opacity(0)
                    if !previewText.isEmpty && !expression.isEmpty { Text(previewText).font(.system(size: 26, weight: .regular, design: .monospaced)).foregroundColor(.secondary) }
                    if expression.isEmpty { Text(previewText.isEmpty ? "Enter expression..." : previewText).font(.system(size: 26, weight: .regular, design: .monospaced)).foregroundColor(.secondary) }
                }
                .padding(.horizontal)

                CursorAwareTextField(text: $expression, selectedRange: $cursorPosition)
                    .padding(.horizontal)
                    .onTapGesture { onTap() }
            }
            Spacer()
        }
        .frame(height: 70)
    }
}

struct SymbolsGridView: View {
    var viewModel: CalculatorViewModel
    let operatorSymbols: [MathSymbol]
    let constantSymbols: [MathSymbol]
    let columns = [GridItem(.adaptive(minimum: 45))]

    var body: some View {
        VStack(spacing: 15) {
            LazyVGrid(columns: columns, spacing: 10) {
                ForEach(operatorSymbols) { symbol in
                    Button(action: { viewModel.insertTextAtCursor(symbol.insertionText ?? symbol.symbol) }) {
                        Text(symbol.symbol).font(.title2).frame(width: 40, height: 40).background(Color.secondary.opacity(0.1)).clipShape(RoundedRectangle(cornerRadius: 8, style: .continuous)).help(symbol.name)
                    }.buttonStyle(.plain)
                }
            }
            Divider()
            LazyVGrid(columns: columns, spacing: 10) {
                ForEach(constantSymbols) { symbol in
                    Button(action: { viewModel.insertTextAtCursor(symbol.insertionText ?? symbol.symbol) }) {
                        Text(symbol.symbol).font(.title3).frame(width: 40, height: 40).background(Color.blue.opacity(0.2)).clipShape(RoundedRectangle(cornerRadius: 8, style: .continuous)).help(symbol.name)
                    }.buttonStyle(.plain)
                }
            }
        }
        .padding().frame(width: 320)
    }
}

struct GreekSymbolsGridView: View {
    var viewModel: CalculatorViewModel
    let greekSymbols: [MathSymbol]
    let columns = [GridItem(.adaptive(minimum: 45))]
    
    var body: some View {
        VStack {
            LazyVGrid(columns: columns, spacing: 10) {
                ForEach(greekSymbols) { symbol in
                    Button(action: { viewModel.insertTextAtCursor(symbol.insertionText ?? symbol.symbol) }) {
                        Text(symbol.symbol).font(.title2).frame(width: 40, height: 40).background(Color.green.opacity(0.2)).clipShape(RoundedRectangle(cornerRadius: 8, style: .continuous)).help(symbol.name)
                    }.buttonStyle(.plain)
                }
            }
        }
        .padding().frame(width: 320)
    }
}

