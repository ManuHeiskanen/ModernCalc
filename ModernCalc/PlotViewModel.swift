//
//  PlotViewModel.swift
//  ModernCalc
//
//  Created by Manu Heiskanen on 8.9.2025.
//

import Foundation
import SwiftUI

@Observable
@MainActor
class PlotViewModel {
    var plotData: PlotData
    var viewDomainX: ClosedRange<Double>
    var viewDomainY: ClosedRange<Double>
    
    var xAxisLabel: String
    var yAxisLabel: String
    private let initialDomainX: ClosedRange<Double>
    private let initialDomainY: ClosedRange<Double>
    private let initialSeries: [PlotSeries]

    // --- NEW: Tracks the full range of X-values for which we have calculated data ---
    private var dataDomainX: ClosedRange<Double>
    
    // --- MODIFIED: The handler now fetches data for a specific domain ---
    var regenerationHandler: ((ClosedRange<Double>) -> Task<[PlotSeries]?, Never>)?
    private var regenerationTask: Task<Void, Never>?

    init(plotData: PlotData, regenerationHandler: ((ClosedRange<Double>) -> Task<[PlotSeries]?, Never>)? = nil) {
        self.plotData = plotData
        self.xAxisLabel = plotData.xAxisLabel
        self.yAxisLabel = plotData.yAxisLabel
        self.regenerationHandler = regenerationHandler
        self.initialSeries = plotData.series // Store the original data for reset
        
        var calculatedDomainX: ClosedRange<Double>
        var calculatedDomainY: ClosedRange<Double>

        if plotData.plotType == .vector {
            let allPoints = plotData.series.flatMap { $0.dataPoints }
            if allPoints.isEmpty {
                calculatedDomainX = -5...5
                calculatedDomainY = -5...5
            } else {
                let allX = allPoints.map { abs($0.x) }
                let allY = allPoints.map { abs($0.y) }
                let maxCoord = max(allX.max() ?? 0, allY.max() ?? 0)
                let paddedMax = maxCoord * 1.2
                calculatedDomainX = -paddedMax...paddedMax
                calculatedDomainY = -paddedMax...paddedMax
            }
        } else {
            let allPoints = plotData.series.flatMap { $0.dataPoints }.filter { $0.x.isFinite && $0.y.isFinite }
            if allPoints.isEmpty {
                calculatedDomainX = -10...10
                calculatedDomainY = -10...10
            } else {
                if plotData.plotType == .parametric {
                    let allX = allPoints.map { $0.x }; let allY = allPoints.map { $0.y }
                    let minX = allX.min()!, maxX = allX.max()!, minY = allY.min()!, maxY = allY.max()!
                    let xSpan = maxX - minX; let ySpan = maxY - minY
                    let maxSpan = max(xSpan, ySpan) * 1.2
                    let midX = (minX + maxX) / 2; let midY = (minY + maxY) / 2
                    calculatedDomainX = (midX - maxSpan / 2)...(midX + maxSpan / 2)
                    calculatedDomainY = (midY - maxSpan / 2)...(midY + maxSpan / 2)
                } else {
                    if let xRange = plotData.initialXRange {
                        calculatedDomainX = xRange.min...xRange.max
                    } else {
                        var minX = allPoints.map { $0.x }.min()!, maxX = allPoints.map { $0.x }.max()!
                        if minX == maxX { minX -= 1; maxX += 1 }
                        if abs(minX + maxX) < (maxX - minX) * 0.1 { let mag = max(abs(minX), abs(maxX)); minX = -mag; maxX = mag }
                        let xPadding = (maxX - minX) * 0.05
                        calculatedDomainX = (minX - max(xPadding, 0.5))...(maxX + max(xPadding, 0.5))
                    }
                    
                    if let yRange = plotData.explicitYRange {
                        calculatedDomainY = yRange.min...yRange.max
                    } else {
                        var minY = allPoints.map { $0.y }.min()!, maxY = allPoints.map { $0.y }.max()!
                        if minY == maxY { minY -= 1; maxY += 1 }
                        if abs(minY + maxY) < (maxY - minY) * 0.1 { let mag = max(abs(minY), abs(maxY)); minY = -mag; maxY = mag }
                        let yPadding = (maxY - minY) * 0.1
                        calculatedDomainY = (minY - max(yPadding, 1.0))...(maxY + max(yPadding, 1.0))
                    }
                    
                    let xSpan = calculatedDomainX.upperBound - calculatedDomainX.lowerBound
                    let ySpan = calculatedDomainY.upperBound - calculatedDomainY.lowerBound

                    if ySpan > 1e-9 {
                        let targetAspectRatio = 4.0
                        let currentAspectRatio = xSpan / ySpan
                        
                        if currentAspectRatio > targetAspectRatio {
                            let yCenter = (calculatedDomainY.lowerBound + calculatedDomainY.upperBound) / 2
                            let newYSpan = xSpan / targetAspectRatio
                            calculatedDomainY = (yCenter - newYSpan / 2)...(yCenter + newYSpan / 2)
                        }
                    }
                }
            }
        }
        
        self.viewDomainX = calculatedDomainX
        self.viewDomainY = calculatedDomainY
        self.initialDomainX = calculatedDomainX
        self.initialDomainY = calculatedDomainY
        
        // --- NEW: Initialize the dataDomainX based on the actual data provided ---
        let allPoints = plotData.series.flatMap { $0.dataPoints }
        if let minX = allPoints.map({ $0.x }).min(), let maxX = allPoints.map({ $0.x }).max() {
            self.dataDomainX = minX...maxX
        } else {
            self.dataDomainX = calculatedDomainX // Fallback
        }
    }
    
    func resetView() {
        self.plotData.series = initialSeries
        self.viewDomainX = initialDomainX
        self.viewDomainY = initialDomainY
        // --- NEW: Reset the data domain as well ---
        let allPoints = initialSeries.flatMap { $0.dataPoints }
        if let minX = allPoints.map({ $0.x }).min(), let maxX = allPoints.map({ $0.x }).max() {
            self.dataDomainX = minX...maxX
        } else {
            self.dataDomainX = initialDomainX
        }
    }
    
    func pan(by translation: CGSize, from startDomains: (x: ClosedRange<Double>, y: ClosedRange<Double>), plotSize: CGSize) {
        guard plotSize.width > 0, plotSize.height > 0 else { return }
        let xSpan = startDomains.x.upperBound - startDomains.x.lowerBound
        let ySpan = startDomains.y.upperBound - startDomains.y.lowerBound
        let xShift = -translation.width * (xSpan / plotSize.width)
        let yShift = translation.height * (ySpan / plotSize.height)
        viewDomainX = (startDomains.x.lowerBound + xShift)...(startDomains.x.upperBound + xShift)
        viewDomainY = (startDomains.y.lowerBound + yShift)...(startDomains.y.upperBound + yShift)
    }

    func zoom(by scale: CGFloat) {
        let currentXSpan = viewDomainX.upperBound - viewDomainX.lowerBound
        let currentYSpan = viewDomainY.upperBound - viewDomainY.lowerBound
        
        guard scale.isFinite, scale > 0 else { return }

        var newXSpan = currentXSpan / scale
        var newYSpan = currentYSpan / scale
        
        let maxSpan: Double = 1e12
        let minSpan: Double = 1e-9
        
        if newXSpan < minSpan { newXSpan = minSpan }
        if newYSpan < minSpan { newYSpan = minSpan }
        if newXSpan > maxSpan { newXSpan = maxSpan }
        if newYSpan > maxSpan { newYSpan = maxSpan }

        let xCenter = (viewDomainX.lowerBound + viewDomainX.upperBound) / 2
        let yCenter = (viewDomainY.lowerBound + viewDomainY.upperBound) / 2
        
        viewDomainX = (xCenter - newXSpan / 2)...(xCenter + newXSpan / 2)
        viewDomainY = (yCenter - newYSpan / 2)...(yCenter + newYSpan / 2)
    }
    
    // --- NEW: Smart trigger for data regeneration ---
    func triggerDataRegenerationIfNeeded() {
        guard let handler = regenerationHandler, plotData.plotType == .line || plotData.plotType == .parametric else { return }

        regenerationTask?.cancel()

        // Define a threshold (e.g., 25% from the edge) to trigger pre-fetching.
        let dataSpan = dataDomainX.upperBound - dataDomainX.lowerBound
        guard dataSpan > 0 else { return }
        let threshold = dataSpan * 0.25

        var domainToFetch: ClosedRange<Double>? = nil

        // Check if user is near the right edge
        if viewDomainX.upperBound > dataDomainX.upperBound - threshold {
            let fetchStart = dataDomainX.upperBound
            let fetchEnd = dataDomainX.upperBound + dataSpan // Fetch another "page" of data
            domainToFetch = fetchStart...fetchEnd
        }
        // Check if user is near the left edge
        else if viewDomainX.lowerBound < dataDomainX.lowerBound + threshold {
            let fetchStart = dataDomainX.lowerBound - dataSpan
            let fetchEnd = dataDomainX.lowerBound
            domainToFetch = fetchStart...fetchEnd
        }

        guard let newDomain = domainToFetch else {
            // No need to fetch, but if zooming out has revealed empty space, recalculate Y-domain
            recalculateYDomain(forVisibleXRange: viewDomainX)
            return
        }

        regenerationTask = Task {
            do {
                try await Task.sleep(for: .milliseconds(100))
                guard !Task.isCancelled else { return }

                if let newSeriesChunks = await handler(newDomain).value {
                    self.appendNewData(newSeries: newSeriesChunks)
                }
            } catch {}
        }
    }
    
    // --- NEW: Appends new data and updates domains ---
    private func appendNewData(newSeries: [PlotSeries]) {
        for newSeriesChunk in newSeries {
            if let existingSeriesIndex = self.plotData.series.firstIndex(where: { $0.name == newSeriesChunk.name }) {
                
                var combinedPoints = self.plotData.series[existingSeriesIndex].dataPoints
                combinedPoints.append(contentsOf: newSeriesChunk.dataPoints)
                
                // Sort by x-value to keep the data in order for line drawing.
                combinedPoints.sort { $0.x < $1.x }
                
                // A simple de-duplication step
                var uniquePoints: [DataPoint] = []
                if let first = combinedPoints.first {
                    uniquePoints.append(first)
                    for i in 1..<combinedPoints.count {
                        if combinedPoints[i].x != combinedPoints[i-1].x {
                            uniquePoints.append(combinedPoints[i])
                        }
                    }
                }
                
                self.plotData.series[existingSeriesIndex] = PlotSeries(
                    name: newSeriesChunk.name,
                    dataPoints: uniquePoints,
                    equation: newSeriesChunk.equation
                )
            }
        }
        
        // Update the data domain to reflect the newly expanded range
        if let minX = plotData.series.flatMap({ $0.dataPoints }).map({ $0.x }).min(),
           let maxX = plotData.series.flatMap({ $0.dataPoints }).map({ $0.x }).max() {
            self.dataDomainX = minX...maxX
        }
        
        // Recalculate Y domain based on the new visible data
        recalculateYDomain(forVisibleXRange: viewDomainX)
    }
    
    // --- MODIFIED: Recalculates Y domain only for the currently visible X range ---
    private func recalculateYDomain(forVisibleXRange: ClosedRange<Double>) {
        guard plotData.explicitYRange == nil else {
            self.viewDomainY = plotData.explicitYRange!.min...plotData.explicitYRange!.max
            return
        }

        let visiblePoints = plotData.series.flatMap { $0.dataPoints }.filter {
            $0.y.isFinite && forVisibleXRange.contains($0.x)
        }
        
        guard !visiblePoints.isEmpty else { return }

        var minY = visiblePoints.map { $0.y }.min()!
        var maxY = visiblePoints.map { $0.y }.max()!

        if abs(maxY - minY) < 1e-9 { minY -= 1; maxY += 1 }
        
        // Symmetrize around zero if the range is close to it
        if abs(minY + maxY) < (maxY - minY) * 0.1 {
            let mag = max(abs(minY), abs(maxY)); minY = -mag; maxY = mag
        }

        let yPadding = (maxY - minY) * 0.1
        withAnimation(.easeInOut(duration: 0.2)) {
            self.viewDomainY = (minY - max(yPadding, 1.0))...(maxY + max(yPadding, 1.0))
        }
    }
}
