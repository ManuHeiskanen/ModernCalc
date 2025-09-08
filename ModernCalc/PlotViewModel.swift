//
//  PlotViewModel.swift
//  ModernCalc
//
//  Created by Manu Heiskanen on 8.9.2025.
//

import Foundation
import SwiftUI

@MainActor
class PlotViewModel: ObservableObject {
    @Published var plotData: PlotData
    @Published var viewDomainX: ClosedRange<Double>
    @Published var viewDomainY: ClosedRange<Double>
    
    init(plotData: PlotData) {
        self.plotData = plotData
        let (xDomain, yDomain) = PlotViewModel.calculateOptimalDomains(for: plotData)
        self.viewDomainX = xDomain
        self.viewDomainY = yDomain
    }
    
    func resetView() {
        let (xDomain, yDomain) = PlotViewModel.calculateOptimalDomains(for: plotData)
        viewDomainX = xDomain
        viewDomainY = yDomain
    }
    
    // --- START: New, Robust Axis Scaling Logic ---
    static func calculateOptimalDomains(for plotData: PlotData) -> (x: ClosedRange<Double>, y: ClosedRange<Double>) {
        // 1. Get all data points, crucially filtering out any non-finite values (NaN, infinity) for BOTH x and y.
        // This is the key fix for plots that seemed to disappear.
        let allPoints = plotData.series.flatMap { $0.dataPoints }.filter { $0.x.isFinite && $0.y.isFinite }

        // 2. If there are no valid points at all, fall back to a default view.
        guard !allPoints.isEmpty else {
            return (-10...10, -10...10)
        }

        // 3. Get the true min and max from the valid data.
        var minX = allPoints.map { $0.x }.min()!
        var maxX = allPoints.map { $0.x }.max()!
        var minY = allPoints.map { $0.y }.min()!
        var maxY = allPoints.map { $0.y }.max()!

        // 4. Handle cases where the data forms a vertical or horizontal line.
        if minX == maxX {
            minX -= 1
            maxX += 1
        }
        if minY == maxY {
            minY -= 1
            maxY += 1
        }
        
        // 5. Intelligently symmetrize axes if the function is centered around the origin.
        // This makes plots like sin(x), x^2, and parametric circles look much better.
        if abs(minX + maxX) < (maxX - minX) * 0.1 {
            let largestMagnitude = max(abs(minX), abs(maxX))
            minX = -largestMagnitude
            maxX = largestMagnitude
        }
        if abs(minY + maxY) < (maxY - minY) * 0.1 {
            let largestMagnitude = max(abs(minY), abs(maxY))
            minY = -largestMagnitude
            maxY = largestMagnitude
        }

        // 6. Add sensible padding to prevent lines from touching the edges.
        let xPadding = (maxX - minX) * 0.05
        let yPadding = (maxY - minY) * 0.1
        
        // Use a minimum padding to handle flat lines or single points gracefully.
        let finalXPadding = max(xPadding, 0.5)
        let finalYPadding = max(yPadding, 0.5)

        let finalXDomain = (minX - finalXPadding)...(maxX + finalXPadding)
        let finalYDomain = (minY - finalYPadding)...(maxY + finalYPadding)
        
        return (finalXDomain, finalYDomain)
    }
    // --- END: New Logic ---
}
