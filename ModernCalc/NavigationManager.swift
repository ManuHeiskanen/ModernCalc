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

    func handleKeyPress(keys: Set<KeyEquivalent>, history: [Calculation]) -> String? {
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
            }
        // --- NEW: Logic to prevent left/right on definitions ---
        } else if let selectedItem = history.first(where: { $0.id == selectedHistoryId }), !selectedItem.isDefinition {
            if keys.contains(.leftArrow) {
                selectedPart = .equation
            } else if keys.contains(.rightArrow) {
                selectedPart = .result
            }
        }
        
        if let selectedItem = history.first(where: { $0.id == selectedHistoryId }) {
            // For definitions, always return the full expression
            if selectedItem.isDefinition {
                return selectedItem.expression
            }
            return selectedPart == .equation ? selectedItem.expression : selectedItem.result
        } else {
            return nil
        }
    }

    func resetSelection() {
        selectedHistoryId = nil
        selectedPart = .result
    }
}

