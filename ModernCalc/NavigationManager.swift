//
//  NavigationManager.swift
//  ModernCalc
//
//  Created by Manu Heiskanen on 29.8.2025.
//

import Foundation
import Combine
import SwiftUI

// MODIFIED: SelectionPart now holds the index of the selected result.
enum SelectionPart: Equatable {
    case equation
    case result(index: Int)
}

class NavigationManager: ObservableObject {
    @Published var selectedHistoryId: UUID? = nil
    @Published var selectedPart: SelectionPart = .result(index: 0)

    func handleKeyPress(keys: Set<KeyEquivalent>, history: [Calculation], viewModel: CalculatorViewModel) -> String? {
        guard !history.isEmpty else { return nil }
        
        let currentIndex = selectedHistoryId.flatMap { id in history.firstIndex(where: { $0.id == id }) }
        
        if keys.contains(.upArrow) {
            if let index = currentIndex {
                selectedHistoryId = history[max(0, index - 1)].id
            } else {
                selectedHistoryId = history.last?.id
            }
            // Reset to the first result part when changing rows.
            selectedPart = .result(index: 0)
            
        } else if keys.contains(.downArrow) {
            if let index = currentIndex, index < history.count - 1 {
                selectedHistoryId = history[index + 1].id
                selectedPart = .result(index: 0)
            } else {
                selectedHistoryId = nil
                return nil
            }
            
        } else if let selectedItem = history.first(where: { $0.id == selectedHistoryId }), selectedItem.type != .functionDefinition {
            // NEW: Logic to navigate between multiple results of a tuple.
            let resultCount: Int
            if case .tuple(let values) = selectedItem.result {
                resultCount = values.count
            } else {
                resultCount = 1
            }

            if keys.contains(.leftArrow) {
                if case .result(let index) = selectedPart {
                    if index > 0 {
                        selectedPart = .result(index: index - 1)
                    } else {
                        selectedPart = .equation
                    }
                }
            } else if keys.contains(.rightArrow) {
                if selectedPart == .equation {
                    selectedPart = .result(index: 0)
                } else if case .result(let index) = selectedPart {
                    if index < resultCount - 1 {
                        selectedPart = .result(index: index + 1)
                    }
                }
            }
        }
        
        if let selectedItem = history.first(where: { $0.id == selectedHistoryId }) {
            if selectedItem.type == .functionDefinition {
                return selectedItem.expression.replacingOccurrences(of: " ", with: "")
            }
            
            // NEW: Return the specific result from the tuple based on the selected index.
            if case .result(let index) = selectedPart {
                if case .tuple(let values) = selectedItem.result {
                    if index < values.count {
                        return viewModel.formatForParsing(values[index])
                    }
                } else if index == 0 { // For single results
                    return viewModel.formatForParsing(selectedItem.result)
                }
            }
            
            return selectedItem.expression.replacingOccurrences(of: " ", with: "")
        } else {
            return nil
        }
    }

    func resetSelection() {
        selectedHistoryId = nil
        selectedPart = .result(index: 0)
    }
}

