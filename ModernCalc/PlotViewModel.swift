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
    
    // Initializer
    init(plotData: PlotData) {
        self.plotData = plotData
        
        // Initialize domains based on data
        let xValues = plotData.dataPoints.map { $0.x }
        let yValues = plotData.dataPoints.map { $0.y }

        let minX = xValues.min() ?? 0
        let maxX = xValues.max() ?? 1
        let minY = yValues.min() ?? 0
        let maxY = yValues.max() ?? 1
        
        // Add some padding to the domain
        let xPadding = (maxX - minX) * 0.1
        let yPadding = (maxY - minY) * 0.1

        self.viewDomainX = (minX - xPadding)...(maxX + xPadding)
        self.viewDomainY = (minY - yPadding)...(maxY + yPadding)
    }
    
    func resetView() {
        // Recalculate domains based on original data
        let xValues = plotData.dataPoints.map { $0.x }
        let yValues = plotData.dataPoints.map { $0.y }

        let minX = xValues.min() ?? 0
        let maxX = xValues.max() ?? 1
        let minY = yValues.min() ?? 0
        let maxY = yValues.max() ?? 1
        
        let xPadding = (maxX - minX) * 0.1
        let yPadding = (maxY - minY) * 0.1

        viewDomainX = (minX - xPadding)...(maxX + xPadding)
        viewDomainY = (minY - yPadding)...(maxY + yPadding)
    }
}
