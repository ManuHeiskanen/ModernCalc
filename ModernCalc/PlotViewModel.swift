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
        
        if plotData.plotType == .vector {
            // --- Vector Scaling Logic ---
            let allPoints = plotData.series.flatMap { $0.dataPoints }
            guard !allPoints.isEmpty else {
                self.viewDomainX = -5...5
                self.viewDomainY = -5...5
                return
            }

            let allX = allPoints.map { abs($0.x) }
            let allY = allPoints.map { abs($0.y) }
            let maxCoord = max(allX.max() ?? 0, allY.max() ?? 0)
            let paddedMax = maxCoord * 1.2
            
            self.viewDomainX = -paddedMax...paddedMax
            self.viewDomainY = -paddedMax...paddedMax

        } else {
            // --- Standard & Parametric Function Scaling Logic ---
            let allPoints = plotData.series.flatMap { $0.dataPoints }.filter { $0.x.isFinite && $0.y.isFinite }

            guard !allPoints.isEmpty else {
                self.viewDomainX = -10...10
                self.viewDomainY = -10...10
                return
            }

            // For parametric plots, we force a 1:1 aspect ratio by unifying the domains.
            if plotData.plotType == .parametric {
                let allX = allPoints.map { $0.x }
                let allY = allPoints.map { $0.y }

                let minX = allX.min()!
                let maxX = allX.max()!
                let minY = allY.min()!
                let maxY = allY.max()!
                
                let xSpan = maxX - minX
                let ySpan = maxY - minY
                
                // Use the larger of the two spans to define the square domain
                let maxSpan = max(xSpan, ySpan) * 1.2 // Add 20% padding
                
                let midX = (minX + maxX) / 2
                let midY = (minY + maxY) / 2
                
                self.viewDomainX = (midX - maxSpan / 2)...(midX + maxSpan / 2)
                self.viewDomainY = (midY - maxSpan / 2)...(midY + maxSpan / 2)
            } else {
                // For standard line plots, we use the original independent scaling logic.
                var minX = allPoints.map { $0.x }.min()!
                var maxX = allPoints.map { $0.x }.max()!
                
                if minX == maxX {
                    minX -= 1
                    maxX += 1
                }
                
                if abs(minX + maxX) < (maxX - minX) * 0.1 {
                    let largestMagX = max(abs(minX), abs(maxX))
                    minX = -largestMagX
                    maxX = largestMagX
                }

                let xPadding = (maxX - minX) * 0.05
                let finalXPadding = max(xPadding, 0.5)
                self.viewDomainX = (minX - finalXPadding)...(maxX + finalXPadding)
                
                if let yRange = plotData.explicitYRange {
                    self.viewDomainY = yRange.min...yRange.max
                } else {
                    var minY = allPoints.map { $0.y }.min()!
                    var maxY = allPoints.map { $0.y }.max()!

                    if minY == maxY {
                        minY -= 1
                        maxY += 1
                    }
                    
                    if abs(minY + maxY) < (maxY - minY) * 0.1 {
                        let largestMagnitude = max(abs(minY), abs(maxY))
                        minY = -largestMagnitude
                        maxY = largestMagnitude
                    }

                    let yPadding = (maxY - minY) * 0.1
                    let finalYPadding = max(yPadding, 1.0)
                    self.viewDomainY = (minY - finalYPadding)...(maxY + finalYPadding)
                }
            }
        }
    }
    
    func resetView() {
        // This function will re-run the initialization logic
        let originalPlotData = self.plotData
        let newViewModel = PlotViewModel(plotData: originalPlotData)
        self.viewDomainX = newViewModel.viewDomainX
        self.viewDomainY = newViewModel.viewDomainY
    }
}
