//
//  ContentView.swift
//  ModernCalc
//
//  Created by Manu Heiskanen on 28.8.2025.
//

import SwiftUI

struct ContentView: View {
    @StateObject private var viewModel = CalculatorViewModel()
    @FocusState private var isInputFocused: Bool
    @State private var isHoveringOnMenuButton = false
    @State private var isShowingSheet = false

    var body: some View {
        ZStack(alignment: .topLeading) {
            VStack(spacing: 0) {
                // This spacer creates room for the menu button
                Spacer().frame(height: 40)
                
                HistoryView(
                    viewModel: viewModel,
                    history: viewModel.history,
                    rawExpression: $viewModel.rawExpression,
                    selectedHistoryId: viewModel.selectedHistoryId,
                    selectedHistoryPart: viewModel.selectedHistoryPart
                )
                Divider()
                
                FormattedExpressionWithButtonsView(
                    result: viewModel.liveResult,
                    expression: $viewModel.rawExpression,
                    greekSymbols: viewModel.greekSymbols
                )
                Divider()

                CalculatorInputView(
                    expression: $viewModel.rawExpression,
                    previewText: viewModel.previewText,
                    operatorSymbols: viewModel.operatorSymbols,
                    constantSymbols: viewModel.constantSymbols,
                    onTap: { viewModel.resetNavigation() }
                )
                .focused($isInputFocused)
                .onAppear {
                    isInputFocused = true
                }
                .onKeyPress(keys: [.upArrow, .downArrow, .leftArrow, .rightArrow, .return]) { key in
                    if key.key == .return {
                        viewModel.commitCalculation()
                        return .handled
                    }
                    
                    if viewModel.handleKeyPress(keys: [key.key]) {
                        return .handled
                    }
                    return .ignored
                }
                .sheet(isPresented: $isShowingSheet) {
                    VariableEditorView(viewModel: viewModel)
                }
            }
            .frame(minWidth: 400, minHeight: 500)
            
            // Menu button now floats on top of the view
            HStack {
                Button(action: { isShowingSheet = true }) {
                    Image(systemName: "ellipsis.circle.fill")
                        .font(.system(size: 24))
                        .foregroundColor(isHoveringOnMenuButton ? .primary : .secondary)
                }
                .buttonStyle(.plain)
                .padding(10)
                .background(Color.primary.opacity(isHoveringOnMenuButton ? 0.1 : 0))
                .cornerRadius(8)
                .scaleEffect(isHoveringOnMenuButton ? 1.1 : 1.0)
                .onHover { hovering in
                    withAnimation(.easeInOut(duration: 0.15)) {
                        isHoveringOnMenuButton = hovering
                    }
                }
                Spacer()
            }
            .padding(.horizontal, 8)
            .padding(.top, 8)
        }
        .toolbar {
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

    var body: some View {
        ScrollViewReader { scrollViewProxy in
            ScrollView {
                LazyVStack(spacing: 0) {
                    ForEach(history) { calculation in
                        VStack(alignment: .trailing, spacing: 4) {
                            switch calculation.type {
                            case .functionDefinition:
                                Text(highlightSIPrefixes(in: calculation.expression))
                                    .font(.system(size: 24, weight: .light, design: .monospaced))
                                    .foregroundColor(.primary)
                                    .padding(.horizontal, 2)
                                    .padding(.vertical, 2)
                                    .background(
                                        (hoveredItem?.id == calculation.id || selectedHistoryId == calculation.id) ?
                                        Color.accentColor.opacity(0.25) : Color.clear
                                    )
                                    .onHover { isHovering in
                                        withAnimation(.easeOut(duration: 0.15)) {
                                            hoveredItem = isHovering ? (id: calculation.id, part: .equation) : nil
                                        }
                                        if isHovering { NSCursor.pointingHand.push() } else { NSCursor.pop() }
                                    }
                                    .onTapGesture { rawExpression += calculation.expression.replacingOccurrences(of: " ", with: "") }
                                    .padding(.vertical, 12)
                                    .padding(.horizontal)
                                    .frame(maxWidth: .infinity, alignment: .trailing)
                            
                            case .evaluation, .variableAssignment:
                                HStack(alignment: .bottom, spacing: 8) {
                                    HStack(spacing: 8) {
                                        if calculation.usedAngleSensitiveFunction {
                                            Circle()
                                                .fill(calculation.angleMode == .degrees ? .orange : .purple)
                                                .frame(width: 8, height: 8)
                                                .offset(y: -4)
                                        }
                                        Text(highlightSIPrefixes(in: calculation.expression))
                                    }
                                    .font(.system(size: 24, weight: .light, design: .monospaced))
                                    .foregroundColor(.primary)
                                    .padding(.horizontal, 2)
                                    .padding(.vertical, 2)
                                    .background(
                                        (hoveredItem?.id == calculation.id && hoveredItem?.part == .equation) || (selectedHistoryId == calculation.id && selectedHistoryPart == .equation) ?
                                        Color.accentColor.opacity(0.25) : Color.clear
                                    )
                                    .onHover { isHovering in
                                        withAnimation(.easeOut(duration: 0.15)) {
                                            hoveredItem = isHovering ? (id: calculation.id, part: .equation) : nil
                                        }
                                        if isHovering { NSCursor.pointingHand.push() } else { NSCursor.pop() }
                                    }
                                    .onTapGesture { rawExpression += calculation.expression.replacingOccurrences(of: " ", with: "") }

                                    Text("=")
                                        .font(.system(size: 24, weight: .light, design: .monospaced))
                                        .foregroundColor(.secondary)
                                    
                                    if case .tuple(let values) = calculation.result {
                                        HStack(spacing: 0) {
                                            ForEach(Array(values.enumerated()), id: \.offset) { index, value in
                                                Text(viewModel.formatForHistory(value))
                                                    .font(.system(size: 24, weight: .light, design: .monospaced))
                                                    .multilineTextAlignment(.trailing)
                                                    .foregroundColor(.primary)
                                                    .padding(.horizontal, 2)
                                                    .padding(.vertical, 2)
                                                    .background(
                                                        isResultSelected(calculation: calculation, index: index) ? Color.accentColor.opacity(0.25) : Color.clear
                                                    )
                                                    .onHover { isHovering in
                                                        withAnimation(.easeOut(duration: 0.15)) {
                                                            hoveredItem = isHovering ? (id: calculation.id, part: .result(index: index)) : nil
                                                        }
                                                        if isHovering { NSCursor.pointingHand.push() } else { NSCursor.pop() }
                                                    }
                                                    .onTapGesture { rawExpression += viewModel.formatForParsing(value) }
                                                
                                                if index < values.count - 1 {
                                                    Text(" OR ")
                                                        .font(.system(size: 20, weight: .light, design: .monospaced))
                                                        .foregroundColor(.secondary)
                                                }
                                            }
                                        }
                                    } else {
                                        Text(viewModel.formatForHistory(calculation.result))
                                            .font(.system(size: 24, weight: .light, design: .monospaced))
                                            .multilineTextAlignment(.trailing)
                                            .foregroundColor(.primary)
                                            .padding(.horizontal, 2)
                                            .padding(.vertical, 2)
                                            .background(
                                                isResultSelected(calculation: calculation, index: 0) ? Color.accentColor.opacity(0.25) : Color.clear
                                            )
                                            .onHover { isHovering in
                                                withAnimation(.easeOut(duration: 0.15)) {
                                                    hoveredItem = isHovering ? (id: calculation.id, part: .result(index: 0)) : nil
                                                }
                                                if isHovering { NSCursor.pointingHand.push() } else { NSCursor.pop() }
                                            }
                                            .onTapGesture { rawExpression += viewModel.formatForParsing(calculation.result) }
                                    }
                                }
                                .padding(.vertical, 12)
                                .padding(.horizontal)
                                .frame(maxWidth: .infinity, alignment: .trailing)
                            }
                            
                            Divider().opacity(0.4)
                        }
                        .id(calculation.id)
                        .transition(.opacity)
                        .background(calculation.id == lastAddedId ? Color.accentColor.opacity(0.1) : Color.clear)
                        .onAppear {
                            if calculation.id == lastAddedId {
                                DispatchQueue.main.asyncAfter(deadline: .now() + 0.1) {
                                    withAnimation(.easeInOut(duration: 0.6)) { lastAddedId = nil }
                                }
                            }
                        }
                    }
                }
            }
            .onChange(of: history) {
                if let lastEntry = history.last {
                    lastAddedId = lastEntry.id
                    withAnimation { scrollViewProxy.scrollTo(lastEntry.id, anchor: .bottom) }
                }
            }
        }
        .frame(maxHeight: .infinity)
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
                    if let attrRange = attributedString.range(of: expression[range]) {
                         attributedString[attrRange].foregroundColor = .teal
                    }
                }
            }
        } catch {
            print("Regex error for SI prefix highlighting: \(error)")
        }
        
        return attributedString
    }
    
    private func isResultSelected(calculation: Calculation, index: Int) -> Bool {
        let isHovered = (hoveredItem?.id == calculation.id)
        let isSelected = (selectedHistoryId == calculation.id)
        
        if let hoveredPart = hoveredItem?.part, case .result(let hoveredIndex) = hoveredPart, isHovered {
            return hoveredIndex == index
        }
        if case .result(let selectedIndex) = selectedHistoryPart, isSelected {
            return selectedIndex == index
        }
        return false
    }
}

struct FormattedExpressionWithButtonsView: View {
    var result: String
    @Binding var expression: String
    var greekSymbols: [MathSymbol]
    @State private var isShowingGreekPopover = false
    
    var body: some View {
        ZStack {
            Color.gray.opacity(0.1)
            HStack {
                Button(action: { isShowingGreekPopover = true }) {
                    Text("α")
                        .font(.system(size: 22))
                }
                .buttonStyle(.plain)
                .padding(.horizontal)
                .popover(isPresented: $isShowingGreekPopover, arrowEdge: .top) {
                    GreekSymbolsGridView(expression: $expression, greekSymbols: greekSymbols)
                }
                
                Text(result)
                    .font(.system(size: 22, weight: .regular))
                    .padding(.vertical)
                    .frame(maxWidth: .infinity, alignment: .leading)
                    .textSelection(.enabled)
            }
        }
        .frame(height: 60)
    }
}

struct CalculatorInputView: View {
    @Binding var expression: String
    var previewText: String
    var operatorSymbols: [MathSymbol]
    var constantSymbols: [MathSymbol]
    var onTap: () -> Void
    
    @State private var isShowingSymbolsPopover = false

    var body: some View {
        HStack(spacing: 0) {
            Button(action: { isShowingSymbolsPopover = true }) {
                Image(systemName: "plus.slash.minus")
                    .font(.system(size: 20))
            }
            .buttonStyle(.plain)
            .padding()
            .popover(isPresented: $isShowingSymbolsPopover, arrowEdge: .bottom) {
                SymbolsGridView(expression: $expression, operatorSymbols: operatorSymbols, constantSymbols: constantSymbols)
            }
            
            ZStack(alignment: .leading) {
                HStack(spacing: 0) {
                    Text(expression)
                        .font(.system(size: 26, weight: .regular, design: .monospaced)).opacity(0)
                    if !previewText.isEmpty && !expression.isEmpty {
                         Text(previewText)
                            .font(.system(size: 26, weight: .regular, design: .monospaced)).foregroundColor(.secondary)
                    }
                    if expression.isEmpty {
                         Text(previewText.isEmpty ? "Enter expression..." : previewText)
                            .font(.system(size: 26, weight: .regular, design: .monospaced)).foregroundColor(.secondary)
                    }
                }
                .padding(.horizontal)

                TextField("", text: $expression)
                    .font(.system(size: 26, weight: .regular, design: .monospaced))
                    .textFieldStyle(.plain)
                    .padding(.horizontal)
                    .onTapGesture { onTap() }
            }
            Spacer()
        }
        .frame(height: 70)
    }
}

struct SymbolsGridView: View {
    @Binding var expression: String
    let operatorSymbols: [MathSymbol]
    let constantSymbols: [MathSymbol]

    let columns = [GridItem(.adaptive(minimum: 45))]

    var body: some View {
        VStack(spacing: 15) {
            LazyVGrid(columns: columns, spacing: 10) {
                ForEach(operatorSymbols) { symbol in
                    Button(action: { expression += symbol.insertionText ?? symbol.symbol }) {
                        Text(symbol.symbol)
                            .font(.title2).frame(width: 40, height: 40)
                            .background(Color.secondary.opacity(0.1))
                            .clipShape(RoundedRectangle(cornerRadius: 8, style: .continuous))
                            .help(symbol.name)
                    }.buttonStyle(.plain)
                }
            }
            Divider()
            LazyVGrid(columns: columns, spacing: 10) {
                ForEach(constantSymbols) { symbol in
                    Button(action: { expression += symbol.insertionText ?? symbol.symbol }) {
                        Text(symbol.symbol)
                            .font(.title3).frame(width: 40, height: 40)
                            .background(Color.blue.opacity(0.2))
                            .clipShape(RoundedRectangle(cornerRadius: 8, style: .continuous))
                            .help(symbol.name)
                    }.buttonStyle(.plain)
                }
            }
        }
        .padding()
        .frame(width: 320)
    }
}

struct GreekSymbolsGridView: View {
    @Binding var expression: String
    let greekSymbols: [MathSymbol]

    let columns = [GridItem(.adaptive(minimum: 45))]
    
    var body: some View {
        VStack {
            LazyVGrid(columns: columns, spacing: 10) {
                ForEach(greekSymbols) { symbol in
                    Button(action: { expression += symbol.insertionText ?? symbol.symbol }) {
                        Text(symbol.symbol)
                            .font(.title2).frame(width: 40, height: 40)
                            .background(Color.green.opacity(0.2))
                            .clipShape(RoundedRectangle(cornerRadius: 8, style: .continuous))
                            .help(symbol.name)
                    }.buttonStyle(.plain)
                }
            }
        }
        .padding()
        .frame(width: 320)
    }
}

