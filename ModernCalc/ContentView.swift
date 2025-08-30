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
        VStack(spacing: 0) {
            HistoryView(
                history: viewModel.history,
                rawExpression: $viewModel.rawExpression,
                selectedHistoryId: viewModel.selectedHistoryId,
                selectedHistoryPart: viewModel.selectedHistoryPart
            )
            Divider()
            
            FormattedExpressionView(result: viewModel.liveResult)
            Divider()

            CalculatorInputView(
                expression: $viewModel.rawExpression,
                previewText: viewModel.previewText,
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
            .overlay(
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
                .padding(), alignment: .trailing
            )
            .sheet(isPresented: $isShowingSheet) {
                VariableEditorView(viewModel: viewModel)
            }
        }
        .frame(minWidth: 400, minHeight: 500)
    }
}

// --- Subviews ---

struct HistoryView: View {
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
                            if calculation.isDefinition {
                                Text(calculation.expression)
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
                                    // FIX: Remove spaces when tapping
                                    .onTapGesture { rawExpression += calculation.expression.replacingOccurrences(of: " ", with: "") }
                                    .padding(.vertical, 12)
                                    .padding(.horizontal)
                                    .frame(maxWidth: .infinity, alignment: .trailing)

                            } else {
                                HStack(alignment: .bottom, spacing: 8) {
                                    Text(calculation.expression)
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
                                        // FIX: Remove spaces when tapping
                                        .onTapGesture { rawExpression += calculation.expression.replacingOccurrences(of: " ", with: "") }

                                    Text("=")
                                        .font(.system(size: 24, weight: .light, design: .monospaced))
                                        .foregroundColor(.secondary)

                                    Text(calculation.displayResult)
                                        .font(.system(size: 24, weight: .light, design: .monospaced))
                                        .multilineTextAlignment(.trailing)
                                        .foregroundColor(.primary)
                                        .padding(.horizontal, 2)
                                        .padding(.vertical, 2)
                                        .background(
                                            (hoveredItem?.id == calculation.id && hoveredItem?.part == .result) || (selectedHistoryId == calculation.id && selectedHistoryPart == .result) ?
                                            Color.accentColor.opacity(0.25) : Color.clear
                                        )
                                        .onHover { isHovering in
                                            withAnimation(.easeOut(duration: 0.15)) {
                                                hoveredItem = isHovering ? (id: calculation.id, part: .result) : nil
                                            }
                                            if isHovering { NSCursor.pointingHand.push() } else { NSCursor.pop() }
                                        }
                                        .onTapGesture { rawExpression += calculation.parsableResult }
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
}


struct FormattedExpressionView: View {
    var result: String
    var body: some View {
        ZStack {
            Color.gray.opacity(0.1)
            Text(result)
                .font(.system(size: 22, weight: .regular))
                .padding()
                .frame(maxWidth: .infinity, maxHeight: .infinity, alignment: .leading)
                .textSelection(.enabled)
        }
        .frame(height: 60)
    }
}

struct CalculatorInputView: View {
    @Binding var expression: String
    var previewText: String
    var onTap: () -> Void

    var body: some View {
        HStack(spacing: 0) {
            ZStack(alignment: .leading) {
                HStack(spacing: 0) {
                    Text(expression)
                        .font(.system(size: 26, weight: .regular, design: .monospaced))
                        .opacity(0)
                    
                    if !previewText.isEmpty && !expression.isEmpty {
                         Text(previewText)
                            .font(.system(size: 26, weight: .regular, design: .monospaced))
                            .foregroundColor(.secondary)
                    }
                    
                    if expression.isEmpty {
                         Text(previewText.isEmpty ? "Enter expression..." : previewText)
                            .font(.system(size: 26, weight: .regular, design: .monospaced))
                            .foregroundColor(.secondary)
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

