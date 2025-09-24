//
//  ContentView.swift
//  ModernCalc
//
//  Created by Manu Heiskanen on 28.8.2025.
//

import SwiftUI

struct ContentView: View {
    var settings: UserSettings
    @State var viewModel: CalculatorViewModel
    
    @FocusState private var isInputFocused: Bool
    @State private var isShowingSheet = false
    @Environment(\.openWindow) private var openWindow
    
    // Namespace for the animated DEG/RAD selector
    @Namespace private var angleSelectorAnimation
    
    init(settings: UserSettings, viewModel: CalculatorViewModel) {
        self.settings = settings
        self._viewModel = State(initialValue: viewModel)
    }

    var body: some View {
        VStack(spacing: 0) {
            // --- The history view now scrolls freely ---
            HistoryView(
                viewModel: viewModel,
                history: viewModel.history,
                rawExpression: $viewModel.rawExpression,
                selectedHistoryId: viewModel.selectedHistoryId,
                selectedHistoryPart: viewModel.selectedHistoryPart
            )
            
            // --- The live preview sits above the input area ---
            FormattedExpressionWithButtonsView(
                viewModel: viewModel,
                latexPreview: viewModel.liveLaTeXPreview,
                helpText: viewModel.liveHelpText,
                errorText: viewModel.liveErrorText,
                greekSymbols: viewModel.greekSymbols
            )
            .frame(height: viewModel.livePreviewHeight)
            .animation(.easeInOut(duration: 0.25), value: viewModel.livePreviewHeight)

            // --- A new unified input area with a glass-like material background ---
            UnifiedInputView(
                viewModel: viewModel,
                expression: $viewModel.rawExpression,
                cursorPosition: $viewModel.cursorPosition,
                previewText: viewModel.previewText,
                operatorSymbols: viewModel.operatorSymbols,
                constantSymbols: viewModel.constantSymbols,
                onTap: { viewModel.resetNavigation() }
            )
            .focused($isInputFocused)
        }
        .frame(minWidth: 410, minHeight: 300)
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
        .toolbar {
            // --- Redesigned Toolbar ---
            ModernToolbarView(
                viewModel: viewModel,
                isShowingSheet: $isShowingSheet,
                angleSelectorAnimation: angleSelectorAnimation
            )
        }
    }
}

// MARK: - Redesigned Toolbar

struct ModernToolbarView: ToolbarContent {
    @Bindable var viewModel: CalculatorViewModel
    @Binding var isShowingSheet: Bool
    var angleSelectorAnimation: Namespace.ID
    
    @State private var isHoveringOnMenuButton = false
    @State private var isHoveringOnCSVButton = false
    
    var body: some ToolbarContent {
        ToolbarItemGroup(placement: .primaryAction) {
            HStack(spacing: 12) {
                Button(action: { isShowingSheet = true }) {
                    Image(systemName: "ellipsis.circle")
                        .font(.system(size: 20))
                        .foregroundColor(.secondary)
                }
                .buttonStyle(.plain)
                .padding(4)
                .background(isHoveringOnMenuButton ? Color.primary.opacity(0.1) : Color.clear, in: Circle())
                .onHover { hovering in
                    withAnimation(.easeInOut(duration: 0.15)) {
                        isHoveringOnMenuButton = hovering
                    }
                }
                
                Button(action: { viewModel.triggerCSVImport() }) {
                    Image(systemName: "doc")
                        .font(.system(size: 20))
                        .foregroundColor(.secondary)
                }
                .buttonStyle(.plain)
                .padding(4)
                .background(isHoveringOnCSVButton ? Color.primary.opacity(0.1) : Color.clear, in: Circle())
                .onHover { hovering in
                    withAnimation(.easeInOut(duration: 0.15)) {
                        isHoveringOnCSVButton = hovering
                    }
                }

                HStack(spacing: 4) {
                    ForEach([AngleMode.degrees, AngleMode.radians], id: \.self) { mode in
                        Button(mode.shortName) {
                            withAnimation(.spring(response: 0.3, dampingFraction: 0.7)) {
                                viewModel.angleMode = mode
                            }
                        }
                        .padding(.horizontal, 12)
                        .padding(.vertical, 4)
                        .foregroundColor(viewModel.angleMode == mode ? .white : .primary)
                        .background {
                            if viewModel.angleMode == mode {
                                let color = mode == .degrees ? Color.orange : Color.purple
                                Capsule()
                                    .fill(color)
                                    .matchedGeometryEffect(id: "angleSelector", in: angleSelectorAnimation)
                            }
                        }
                    }
                }
                .buttonStyle(.plain)
                .background(.ultraThinMaterial, in: Capsule())
            }
        }
    }
}

// MARK: - Unified Input Area

struct UnifiedInputView: View {
    @Bindable var viewModel: CalculatorViewModel
    @Binding var expression: String
    @Binding var cursorPosition: NSRange
    var previewText: String
    var operatorSymbols: [MathSymbol]
    var constantSymbols: [MathSymbol]
    var onTap: () -> Void
    
    @State private var isShowingSymbolsPopover = false
    
    var body: some View {
        VStack(spacing: 0) {
            ActionShelfView(viewModel: viewModel)
                .frame(height: 45)
            
            Divider()

            HStack(spacing: 0) {
                Button(action: { isShowingSymbolsPopover = true }) { Image(systemName: "plus.slash.minus").font(.system(size: 20)) }.buttonStyle(.plain).padding()
                    .popover(isPresented: $isShowingSymbolsPopover, arrowEdge: .bottom) {
                        SymbolsGridView(viewModel: viewModel, operatorSymbols: operatorSymbols, constantSymbols: constantSymbols)
                    }
                
                CursorAwareTextField(text: $expression, selectedRange: $cursorPosition)
                    .onTapGesture { onTap() }
                    .padding(.horizontal)
                    .font(.system(size: 26, weight: .regular, design: .monospaced))
                    .overlay(alignment: .leading) {
                        if expression.isEmpty {
                            Text(previewText.isEmpty ? "Enter expression..." : previewText)
                                .font(.system(size: 26, weight: .regular, design: .monospaced))
                                .foregroundColor(.secondary)
                                .padding(.horizontal)
                                .allowsHitTesting(false)
                        }
                    }
            }
            .frame(height: 70)
        }
        .background(.regularMaterial)
    }
}


// MARK: - Subviews (with LiquidGlass updates)

struct ActionShelfView: View {
    @Bindable var viewModel: CalculatorViewModel

    var body: some View {
        ZStack {
            if viewModel.showAutocomplete && !viewModel.autocompleteSuggestions.isEmpty {
                AutocompletePillView(viewModel: viewModel)
                    .transition(.opacity.animation(.easeInOut(duration: 0.2)))
            } else {
                ShimmerView()
                    .transition(.opacity.animation(.easeInOut(duration: 0.2)))
            }
        }
        .clipped()
    }
}

struct AutocompletePillView: View {
    @Bindable var viewModel: CalculatorViewModel

    var body: some View {
        ScrollView(.horizontal, showsIndicators: false) {
            HStack(spacing: 8) {
                ForEach(viewModel.autocompleteSuggestions) { suggestion in
                    Button(action: {
                        viewModel.selectAutocomplete(suggestion: suggestion)
                    }) {
                        Text(suggestion.displayText)
                            .font(.system(.callout, design: .monospaced))
                            .padding(.horizontal, 12)
                            .padding(.vertical, 6)
                            .foregroundColor(.primary)
                            .background(
                                .ultraThinMaterial,
                                in: Capsule()
                            )
                            .overlay(
                                Capsule().stroke(pillBorderColor(for: suggestion.type), lineWidth: 1)
                            )
                    }
                    .buttonStyle(.plain)
                }
            }
            .padding(.horizontal)
        }
    }
    
    private func pillBorderColor(for type: String) -> Color {
        switch type {
        case "function": return .indigo.opacity(0.4)
        case "user_function": return .purple.opacity(0.4)
        case "variable": return .teal.opacity(0.4)
        case "constant": return .yellow.opacity(0.4)
        default: return Color.primary.opacity(0.2)
        }
    }
}

struct ShimmerView: View {
    @Environment(\.controlActiveState) private var controlActiveState
    @State private var phase: CGFloat = -2.0
    @State private var timer: Timer?

    var body: some View {
        ZStack {
            Rectangle() // The shimmer effect itself
                .fill(
                    LinearGradient(
                        gradient: Gradient(colors: [
                            .clear,
                            .white.opacity(0.1),
                            .clear
                        ]),
                        startPoint: .init(x: phase, y: 0.5),
                        endPoint: .init(x: phase + 2.0, y: 0.5)
                    )
                )
        }
        .drawingGroup()
        .onAppear(perform: startAnimations)
        .onDisappear(perform: stopTimer)
        .onChange(of: controlActiveState) { oldState, newState in
            if newState == .active { startAnimations() } else { stopTimer() }
        }
    }

    private func startAnimations() {
        stopTimer()
        DispatchQueue.main.async {
            phase = -2.0
            withAnimation(.linear(duration: 1.5)) { phase = 2.0 }
        }
        timer = Timer.scheduledTimer(withTimeInterval: 10.0, repeats: true) { _ in
            phase = -2.0
            withAnimation(.linear(duration: 1.5)) { phase = 2.0 }
        }
    }

    private func stopTimer() {
        timer?.invalidate()
        timer = nil
    }
}


struct HistoryView: View {
    @Bindable var viewModel: CalculatorViewModel
    var history: [Calculation]
    @Binding var rawExpression: String
    var selectedHistoryId: UUID?
    var selectedHistoryPart: SelectionPart
    
    @State private var lastAddedId: UUID?
    @State private var hoveredItem: (id: UUID, part: SelectionPart)?
    @State private var showCopyMessage = false

    var body: some View {
        ZStack {
            ScrollViewReader { scrollViewProxy in
                ScrollView {
                    LazyVStack(spacing: 0) {
                        ForEach(history) { calculation in
                            HistoryRowView(
                                viewModel: viewModel,
                                calculation: calculation,
                                selectedHistoryId: selectedHistoryId,
                                selectedHistoryPart: selectedHistoryPart,
                                hoveredItem: $hoveredItem,
                                showCopyMessage: $showCopyMessage
                            )
                            .id(calculation.id).transition(.opacity)
                            .background(calculation.id == lastAddedId ? Color.accentColor.opacity(0.1) : Color.clear)
                            .onAppear {
                                if calculation.id == lastAddedId {
                                    DispatchQueue.main.asyncAfter(deadline: .now() + 0.2) {
                                        withAnimation(.easeOut(duration: 0.5)) { lastAddedId = nil }
                                    }
                                }
                            }
                        }
                    }
                    .padding(.top) // Add padding to avoid the notch
                }
                .onChange(of: history) {
                    if let lastEntry = history.last {
                        lastAddedId = lastEntry.id
                        withAnimation { scrollViewProxy.scrollTo(lastEntry.id, anchor: .bottom) }
                    }
                }
            }
            .frame(maxHeight: .infinity)
            
            if showCopyMessage { Text("Copied as LaTeX").padding(.horizontal, 16).padding(.vertical, 10).background(.ultraThickMaterial).cornerRadius(12).transition(.opacity.animation(.easeInOut)) }
        }
    }
}

// --- New History Row View for cleaner context menus ---
struct HistoryRowView: View {
    @Bindable var viewModel: CalculatorViewModel
    let calculation: Calculation
    var selectedHistoryId: UUID?
    var selectedHistoryPart: SelectionPart
    
    @Binding var hoveredItem: (id: UUID, part: SelectionPart)?
    @Binding var showCopyMessage: Bool

    var body: some View {
        VStack(alignment: .trailing, spacing: 4) {
            switch calculation.type {
            case .functionDefinition, .variableAssignment:
                Text(highlightSIPrefixes(in: calculation.expression))
                    .font(.system(size: 24, weight: .light, design: .monospaced))
                    .padding(.horizontal, 2).padding(.vertical, 2)
                    .background((hoveredItem?.id == calculation.id || selectedHistoryId == calculation.id) ? Color.accentColor.opacity(0.2) : Color.clear, in: RoundedRectangle(cornerRadius: 6))
                    .onHover { isHovering in
                        withAnimation(.easeOut(duration: 0.15)) { hoveredItem = isHovering ? (id: calculation.id, part: .equation) : nil }
                        if isHovering { NSCursor.pointingHand.push() } else { NSCursor.pop() }
                    }
                    .onTapGesture { viewModel.insertTextAtCursor(calculation.expression) }
                
            case .plot:
                HStack {
                    Image(systemName: "chart.xyaxis.line")
                        .font(.system(size: 20))
                        .foregroundColor(.accentColor)
                    Text(calculation.expression)
                         .font(.system(size: 24, weight: .light, design: .monospaced))
                         .padding(.horizontal, 2).padding(.vertical, 2)
                         .background((hoveredItem?.id == calculation.id || selectedHistoryId == calculation.id) ? Color.accentColor.opacity(0.2) : Color.clear, in: RoundedRectangle(cornerRadius: 6))
                         .onHover { isHovering in
                             withAnimation(.easeOut(duration: 0.15)) { hoveredItem = isHovering ? (id: calculation.id, part: .equation) : nil }
                             if isHovering { NSCursor.pointingHand.push() } else { NSCursor.pop() }
                         }
                         .onTapGesture {
                            if case .plot(let plotData) = calculation.result {
                                viewModel.requestOpenPlotWindow(for: plotData)
                            }
                         }
                }

            case .evaluation:
                HStack(alignment: .bottom, spacing: 8) {
                    HStack(spacing: 8) {
                        if calculation.usedAngleSensitiveFunction {
                             // --- "Glass Orb" Angle Indicator ---
                            Circle()
                                .fill(.regularMaterial)
                                .overlay(Circle().fill(calculation.angleMode == .degrees ? .orange.opacity(0.5) : .purple.opacity(0.5)))
                                .frame(width: 8, height: 8)
                                .offset(y: -4)
                        }
                        Text(highlightSIPrefixes(in: calculation.expression))
                    }
                    .font(.system(size: 24, weight: .light, design: .monospaced))
                    .padding(.horizontal, 2).padding(.vertical, 2)
                    .background((hoveredItem?.id == calculation.id && hoveredItem?.part == .equation) || (selectedHistoryId == calculation.id && selectedHistoryPart == .equation) ? Color.accentColor.opacity(0.2) : Color.clear, in: RoundedRectangle(cornerRadius: 6))
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
                }
            }
            Divider().padding(.top, 8)
        }
        .padding(.vertical, 8).padding(.horizontal)
        .frame(maxWidth: .infinity, alignment: .trailing)
        .animation(.default, value: selectedHistoryId)
        .contentShape(Rectangle()) // Makes the whole area tappable for the context menu
        .contextMenu {
            Button(action: { copyToClipboard(calculation: calculation) }) {
                Label("Copy as LaTeX", systemImage: "doc.on.doc")
            }
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
        if let regex = try? NSRegularExpression(pattern: pattern) {
            let nsRange = NSRange(expression.startIndex..., in: expression)
            regex.matches(in: expression, options: [], range: nsRange).forEach { match in
                if let range = Range(match.range(at: 1), in: expression),
                   let attrRange = attributedString.range(of: expression[range]) {
                    attributedString[attrRange].foregroundColor = .teal
                }
            }
        }
        return attributedString
    }
}

// --- The rest of the subviews remain largely the same, but without opaque backgrounds ---

struct CalculationResultView: View {
    @Bindable var viewModel: CalculatorViewModel
    let calculation: Calculation
    @Binding var hoveredItem: (id: UUID, part: SelectionPart)?
    var selectedHistoryId: UUID?
    var selectedHistoryPart: SelectionPart
    @State private var isHoverForExpansion = false
    private let columns = [GridItem(.adaptive(minimum: 100))]
    
    var body: some View {
        // This view's logic is complex and remains unchanged, but its backgrounds are now transparent
        // so they sit nicely on the new material layers. I've updated the selection highlight color.
        
        let selectionColor = Color.accentColor.opacity(0.2)
        
        if case .tuple(let values) = calculation.result {
            HStack(spacing: 0) {
                ForEach(Array(values.enumerated()), id: \.offset) { index, value in
                    Text(viewModel.formatForHistory(value))
                        .padding(.horizontal, 2).padding(.vertical, 2)
                        .background(isResultSelected(calculation: calculation, index: index) ? selectionColor : Color.clear, in: RoundedRectangle(cornerRadius: 6))
                        .onHover { isHovering in
                            withAnimation(.easeOut(duration: 0.15)) { hoveredItem = isHovering ? (id: calculation.id, part: .result(index: index)) : nil }
                            if isHovering { NSCursor.pointingHand.push() } else { NSCursor.pop() }
                        }
                        .onTapGesture { viewModel.insertTextAtCursor(viewModel.formatForParsing(value)) }
                    if index < values.count - 1 { Text(" OR ").foregroundColor(.secondary) }
                }
            }
        } else if case .regressionResult(let slope, let intercept) = calculation.result {
            HStack(spacing: 0) {
                Text("m = \(viewModel.formatForHistory(.unitValue(slope)))")
                    .padding(.horizontal, 2).padding(.vertical, 2)
                    .background(isResultSelected(calculation: calculation, index: 0) ? selectionColor : Color.clear, in: RoundedRectangle(cornerRadius: 6))
                    .onHover { isHovering in
                        withAnimation(.easeOut(duration: 0.15)) { hoveredItem = isHovering ? (id: calculation.id, part: .result(index: 0)) : nil }
                        if isHovering { NSCursor.pointingHand.push() } else { NSCursor.pop() }
                    }
                    .onTapGesture { viewModel.insertTextAtCursor(viewModel.formatForParsing(.unitValue(slope))) }
                Text(", ").foregroundColor(.secondary)
                Text("b = \(viewModel.formatForHistory(.unitValue(intercept)))")
                    .padding(.horizontal, 2).padding(.vertical, 2)
                    .background(isResultSelected(calculation: calculation, index: 1) ? selectionColor : Color.clear, in: RoundedRectangle(cornerRadius: 6))
                    .onHover { isHovering in
                        withAnimation(.easeOut(duration: 0.15)) { hoveredItem = isHovering ? (id: calculation.id, part: .result(index: 1)) : nil }
                        if isHovering { NSCursor.pointingHand.push() } else { NSCursor.pop() }
                    }
                    .onTapGesture { viewModel.insertTextAtCursor(viewModel.formatForParsing(.unitValue(intercept))) }
            }
        } else if case .roots(let roots) = calculation.result {
            let formattedRoots = viewModel.formatRootsForDisplay(roots)
            let isKeyboardSelectedForExpansion = selectedHistoryId == calculation.id && { if case .result = selectedHistoryPart { return true } else { return false } }()
            let showExpansion = roots.count > 1
            let isExpanded = (isKeyboardSelectedForExpansion || isHoverForExpansion) && showExpansion
            Group {
                if isExpanded {
                    VStack(alignment: .trailing, spacing: 8) {
                        HStack {
                            Text("\(getVariableName(from: calculation.expression)) ≈ { ... }").foregroundColor(.secondary)
                                .onTapGesture { viewModel.insertTextAtCursor(viewModel.formatForParsing(calculation.result)) }
                            Image(systemName: "arrow.down.right.and.arrow.up.left.circle").foregroundColor(.secondary)
                        }
                        LazyVGrid(columns: columns, alignment: .trailing, spacing: 8) {
                            ForEach(Array(formattedRoots.enumerated()), id: \.offset) { index, rootString in
                                Text(rootString)
                                    .padding(.horizontal, 8).padding(.vertical, 4)
                                    .background(isResultSelected(calculation: calculation, index: index) ? selectionColor : Color.clear, in: RoundedRectangle(cornerRadius: 6))
                                    .onHover { isHovering in
                                        guard selectedHistoryId != calculation.id else { hoveredItem = nil; return }
                                        withAnimation(.easeOut(duration: 0.15)) { hoveredItem = isHovering ? (id: calculation.id, part: .result(index: index)) : nil }
                                        if isHovering { NSCursor.pointingHand.push() } else { NSCursor.pop() }
                                    }
                                    .onTapGesture { viewModel.insertTextAtCursor(viewModel.formatForParsing(roots[index])) }
                            }
                        }
                    }
                    .padding(8).background(.thinMaterial, in: RoundedRectangle(cornerRadius: 8))
                } else {
                    HStack(spacing: 4) {
                        Text("\(getVariableName(from: calculation.expression)) ≈")
                        if showExpansion { Text("{").foregroundColor(.secondary) }
                        let maxDisplayRoots = showExpansion ? 4 : 1
                        let displayRoots = formattedRoots.count > maxDisplayRoots ? Array(formattedRoots.prefix(maxDisplayRoots)) : formattedRoots
                        ForEach(Array(displayRoots.enumerated()), id: \.offset) { index, rootString in
                            Text(rootString)
                                .padding(.horizontal, 2).padding(.vertical, 2)
                                .background(isResultSelected(calculation: calculation, index: index) ? selectionColor : Color.clear, in: RoundedRectangle(cornerRadius: 6))
                                .onHover { isHovering in
                                    guard selectedHistoryId != calculation.id else { hoveredItem = nil; return }
                                    withAnimation(.easeOut(duration: 0.15)) { hoveredItem = isHovering ? (id: calculation.id, part: .result(index: index)) : nil }
                                    if isHovering { NSCursor.pointingHand.push() } else { NSCursor.pop() }
                                }
                                .onTapGesture { viewModel.insertTextAtCursor(viewModel.formatForParsing(roots[index])) }
                            if index < displayRoots.count - 1 { Text(",").foregroundColor(.secondary) }
                        }
                        if roots.count > maxDisplayRoots { Text("... (\(roots.count - maxDisplayRoots) more)").font(.system(size: 18)).foregroundColor(.secondary) }
                        if showExpansion { Text("}").foregroundColor(.secondary) }
                    }
                    .onTapGesture { viewModel.insertTextAtCursor(viewModel.formatForParsing(calculation.result)) }
                }
            }
            .font(.system(size: 24, weight: .light, design: .monospaced))
            .onHover { hovering in withAnimation { isHoverForExpansion = hovering } }
        } else {
            Text(viewModel.formatForHistory(calculation.result))
                .font(.system(size: 24, weight: .light, design: .monospaced)).multilineTextAlignment(.trailing)
                .padding(.horizontal, 2).padding(.vertical, 2)
                .background(isResultSelected(calculation: calculation, index: 0) ? selectionColor : Color.clear, in: RoundedRectangle(cornerRadius: 6))
                .onHover { isHovering in
                    withAnimation(.easeOut(duration: 0.15)) { hoveredItem = isHovering ? (id: calculation.id, part: .result(index: 0)) : nil }
                    if isHovering { NSCursor.pointingHand.push() } else { NSCursor.pop() }
                }
                .onTapGesture { viewModel.insertTextAtCursor(viewModel.formatForParsing(calculation.result)) }
        }
    }
    
    private func isResultSelected(calculation: Calculation, index: Int) -> Bool {
        if let hoveredPart = hoveredItem?.part, case .result(let hoveredIndex) = hoveredPart, hoveredItem?.id == calculation.id { return hoveredIndex == index }
        if case .result(let selectedIndex) = selectedHistoryPart, selectedHistoryId == calculation.id { return selectedIndex == index }
        return false
    }
    
    private func getVariableName(from expression: String) -> String {
        let pattern = #"(?:solve|nsolve)\s*\([^,]+,\s*([a-zA-Z_][a-zA-Z0-9_]*)"#
        if let regex = try? NSRegularExpression(pattern: pattern),
           let match = regex.firstMatch(in: expression, range: NSRange(expression.startIndex..., in: expression)),
           let range = Range(match.range(at: 1), in: expression) {
            return String(expression[range])
        }
        return "x"
    }
}

struct FormattedExpressionWithButtonsView: View {
    @Bindable var viewModel: CalculatorViewModel
    var latexPreview: String
    var helpText: String
    var errorText: String
    var greekSymbols: [MathSymbol]
    @State private var isShowingGreekPopover = false
    
    var body: some View {
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
                    if !helpText.isEmpty { Text(helpText).font(.system(.body, design: .monospaced)).textSelection(.enabled) }
                    if !errorText.isEmpty { Text(errorText).font(.system(.body, design: .monospaced)).foregroundColor(.red).textSelection(.enabled) }
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

struct SymbolsGridView: View {
    @Bindable var viewModel: CalculatorViewModel
    let operatorSymbols: [MathSymbol]
    let constantSymbols: [MathSymbol]
    let columns = [GridItem(.adaptive(minimum: 45))]

    var body: some View {
        VStack(spacing: 15) {
            LazyVGrid(columns: columns, spacing: 10) {
                ForEach(operatorSymbols) { symbol in
                    Button(action: { viewModel.insertTextAtCursor(symbol.insertionText ?? symbol.symbol) }) {
                        Text(symbol.symbol).font(.title2).frame(width: 40, height: 40).background(.thinMaterial).clipShape(RoundedRectangle(cornerRadius: 8, style: .continuous)).help(symbol.name)
                    }.buttonStyle(.plain)
                }
            }
            Divider()
            LazyVGrid(columns: columns, spacing: 10) {
                ForEach(constantSymbols) { symbol in
                    Button(action: { viewModel.insertTextAtCursor(symbol.insertionText ?? symbol.symbol) }) {
                        Text(symbol.symbol).font(.title3).frame(width: 40, height: 40).background(.thinMaterial).overlay(RoundedRectangle(cornerRadius: 8, style: .continuous).stroke(Color.blue.opacity(0.4))).help(symbol.name)
                    }.buttonStyle(.plain)
                }
            }
        }
        .padding().frame(width: 320)
    }
}

struct GreekSymbolsGridView: View {
    @Bindable var viewModel: CalculatorViewModel
    let greekSymbols: [MathSymbol]
    let columns = [GridItem(.adaptive(minimum: 45))]
    
    var body: some View {
        LazyVGrid(columns: columns, spacing: 10) {
            ForEach(greekSymbols) { symbol in
                Button(action: { viewModel.insertTextAtCursor(symbol.insertionText ?? symbol.symbol) }) {
                    Text(symbol.symbol).font(.title2).frame(width: 40, height: 40).background(.thinMaterial).overlay(RoundedRectangle(cornerRadius: 8, style: .continuous).stroke(Color.green.opacity(0.4))).help(symbol.name)
                }.buttonStyle(.plain)
            }
        }
        .padding().frame(width: 320)
    }
}

// Helper extension for AngleMode
extension AngleMode {
    var shortName: String {
        switch self {
        case .degrees: return "DEG"
        case .radians: return "RAD"
        }
    }
}

