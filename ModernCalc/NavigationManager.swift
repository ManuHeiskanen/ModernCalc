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

@MainActor
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
            // Revert to selecting the result by default, per user request.
            selectedPart = .result(index: 0)
            
        } else if keys.contains(.downArrow) {
            if let index = currentIndex, index < history.count - 1 {
                selectedHistoryId = history[index + 1].id
                // Revert to selecting the result by default.
                selectedPart = .result(index: 0)
            } else {
                selectedHistoryId = nil
                return nil
            }
            
        } else if let selectedItem = history.first(where: { $0.id == selectedHistoryId }), selectedItem.type != .functionDefinition {
            // MODIFIED: This logic now also recognizes regression results as having 2 parts.
            let resultCount: Int
            if case .tuple(let values) = selectedItem.result {
                resultCount = values.count
            } else if case .regressionResult = selectedItem.result {
                resultCount = 2 // m and b
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
            
            // Add a special case for plot items to show a user-friendly message.
            if selectedItem.type == .plot {
                return "Press enter to open plot for \(selectedItem.expression)..."
            }
            
            // Return the specific result from the tuple/regression based on the selected index.
            if case .result(let index) = selectedPart {
                if case .tuple(let values) = selectedItem.result {
                    if index < values.count {
                        return viewModel.formatForParsing(values[index])
                    }
                } else if case .regressionResult(let slope, let intercept) = selectedItem.result {
                    // FIX: Changed .scalar to .dimensionless to match the updated MathValue enum.
                    let valueToParse = (index == 0) ? MathValue.dimensionless(slope) : MathValue.dimensionless(intercept)
                    return viewModel.formatForParsing(valueToParse)
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
