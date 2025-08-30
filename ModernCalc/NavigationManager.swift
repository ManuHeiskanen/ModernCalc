//
//  NavigationManager.swift
//  ModernCalc
//
//  Created by Manu Heiskanen on 29.8.2025.
//

import Foundation
import Combine
import SwiftUI

enum SelectionPart {
    case equation
    case result
}

class NavigationManager: ObservableObject {
    @Published var selectedHistoryId: UUID? = nil
    @Published var selectedPart: SelectionPart = .result

    // MODIFIED: Accepts the viewModel to format the result string for preview.
    func handleKeyPress(keys: Set<KeyEquivalent>, history: [Calculation], viewModel: CalculatorViewModel) -> String? {
        guard !history.isEmpty else { return nil }
        
        let currentIndex = selectedHistoryId.flatMap { id in history.firstIndex(where: { $0.id == id }) }
        
        if keys.contains(.upArrow) {
            if let index = currentIndex {
                selectedHistoryId = history[max(0, index - 1)].id
            } else {
                selectedHistoryId = history.last?.id
            }
        } else if keys.contains(.downArrow) {
            if let index = currentIndex, index < history.count - 1 {
                selectedHistoryId = history[index + 1].id
            } else {
                selectedHistoryId = nil
                return nil // Return nil when deselecting
            }
        // MODIFIED: Check the calculation type instead of the old isDefinition flag.
        } else if let selectedItem = history.first(where: { $0.id == selectedHistoryId }), selectedItem.type != .functionDefinition {
            if keys.contains(.leftArrow) {
                selectedPart = .equation
            } else if keys.contains(.rightArrow) {
                selectedPart = .result
            }
        }
        
        if let selectedItem = history.first(where: { $0.id == selectedHistoryId }) {
            // MODIFIED: Check the calculation type.
            if selectedItem.type == .functionDefinition {
                return selectedItem.expression.replacingOccurrences(of: " ", with: "")
            }
            let resultString = viewModel.formatForParsing(selectedItem.result)
            return selectedPart == .equation ? selectedItem.expression.replacingOccurrences(of: " ", with: "") : resultString
        } else {
            return nil
        }
    }

    func resetSelection() {
        selectedHistoryId = nil
        selectedPart = .result
    }
}

