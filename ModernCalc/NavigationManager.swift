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

    // MODIFIED: The function now takes a tuple to access all necessary data without the full Calculation struct
    typealias HistoryItem = (id: UUID, expression: String, parsableResult: String, isDefinition: Bool)

    func handleKeyPress(keys: Set<KeyEquivalent>, history: [HistoryItem]) -> String? {
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
                resetSelection()
            }
        } else if let selectedItem = history.first(where: { $0.id == selectedHistoryId }), !selectedItem.isDefinition {
            if keys.contains(.leftArrow) {
                selectedPart = .equation
            } else if keys.contains(.rightArrow) {
                selectedPart = .result
            }
        }
        
        if let selectedItem = history.first(where: { $0.id == selectedHistoryId }) {
            if selectedItem.isDefinition {
                return selectedItem.expression
            }
            // MODIFIED: Use the parsableResult when the result part is selected
            return selectedPart == .equation ? selectedItem.expression : selectedItem.parsableResult
        } else {
            return nil
        }
    }

    func resetSelection() {
        selectedHistoryId = nil
        selectedPart = .result
    }
}
