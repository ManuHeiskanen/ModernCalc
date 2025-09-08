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
    
    // Store the initial domains to easily reset the view
    private let initialDomainX: ClosedRange<Double>
    private let initialDomainY: ClosedRange<Double>
    
    // Initializer
    init(plotData: PlotData) {
        self.plotData = plotData
        
        // Initialize domains based on data
        let xValues = plotData.dataPoints.map { $0.x }
        let yValues = plotData.dataPoints.map { $0.y }

        // Use a safe default if there are no points
        let minX = xValues.min() ?? -10
        let maxX = xValues.max() ?? 10
        let minY = yValues.min() ?? -10
        let maxY = yValues.max() ?? 10
        
        // Add some padding to the domain
        let xPadding = (maxX - minX) * 0.1
        let yPadding = (maxY - minY) * 0.1

        let initialX = (minX - xPadding)...(maxX + xPadding)
        let initialY = (minY - yPadding)...(maxY + yPadding)
        
        self.initialDomainX = initialX
        self.initialDomainY = initialY
        
        self.viewDomainX = initialX
        self.viewDomainY = initialY
    }
    
    func resetView() {
        // Reset to the calculated initial domains
        viewDomainX = initialDomainX
        viewDomainY = initialDomainY
    }
    
    // New function to handle panning
    func pan(translation: CGSize, chartSize: CGSize) {
        let xRange = viewDomainX.upperBound - viewDomainX.lowerBound
        let yRange = viewDomainY.upperBound - viewDomainY.lowerBound
        
        // Calculate the shift based on the drag translation
        let xShift = (translation.width / chartSize.width) * xRange
        let yShift = (translation.height / chartSize.height) * yRange
        
        // Update the domains
        viewDomainX = (viewDomainX.lowerBound - xShift)...(viewDomainX.upperBound - xShift)
        viewDomainY = (viewDomainY.lowerBound + yShift)...(viewDomainY.upperBound + yShift)
    }
    
    // New function to handle zooming
    func zoom(scale: Double) {
        let currentXCenter = (viewDomainX.lowerBound + viewDomainX.upperBound) / 2
        let currentYCenter = (viewDomainY.lowerBound + viewDomainY.upperBound) / 2
        
        let newXRange = (viewDomainX.upperBound - viewDomainX.lowerBound) * scale
        let newYRange = (viewDomainY.upperBound - viewDomainY.lowerBound) * scale
        
        // Update the domains, keeping the center point fixed
        viewDomainX = (currentXCenter - newXRange / 2)...(currentXCenter + newXRange / 2)
        viewDomainY = (currentYCenter - newYRange / 2)...(currentYCenter + newYRange / 2)
    }
}

