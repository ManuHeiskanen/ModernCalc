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
    var viewDomainX: ClosedRange<Double> {
        didSet { updateStringFieldsFromDomains() }
    }
    var viewDomainY: ClosedRange<Double> {
        didSet { updateStringFieldsFromDomains() }
    }
    
    // --- NEW: String properties to bind to TextFields for manual control ---
    var xMinString: String = ""
    var xMaxString: String = ""
    var yMinString: String = ""
    var yMaxString: String = ""
    
    var xAxisLabel: String
    var yAxisLabel: String
    
    private let initialDomainX: ClosedRange<Double>
    private let initialDomainY: ClosedRange<Double>
    private let initialSeries: [PlotSeries]
    private var dataDomainX: ClosedRange<Double>
    
    // --- NEW: Flag to prevent auto-scaling when user manually sets Y-axis ---
    private var isYAxisManuallySet: Bool = false
    
    var regenerationHandler: ((ClosedRange<Double>) -> Task<[PlotSeries]?, Never>)?
    private var regenerationTask: Task<Void, Never>?

    init(plotData: PlotData, regenerationHandler: ((ClosedRange<Double>) -> Task<[PlotSeries]?, Never>)? = nil) {
        self.plotData = plotData
        self.xAxisLabel = plotData.xAxisLabel
        self.yAxisLabel = plotData.yAxisLabel
        self.regenerationHandler = regenerationHandler
        self.initialSeries = plotData.series
        
        let (calculatedDomainX, calculatedDomainY) = Self.calculateInitialDomains(for: plotData)
        
        self.viewDomainX = calculatedDomainX
        self.viewDomainY = calculatedDomainY
        self.initialDomainX = calculatedDomainX
        self.initialDomainY = calculatedDomainY
        
        // If the plot was created with an explicit Y-range, consider it "manual" from the start.
        if plotData.explicitYRange != nil {
            self.isYAxisManuallySet = true
        }
        
        let allPoints = plotData.series.flatMap { $0.dataPoints }
        if let minX = allPoints.map({ $0.x }).min(), let maxX = allPoints.map({ $0.x }).max() {
            self.dataDomainX = minX...maxX
        } else {
            self.dataDomainX = calculatedDomainX
        }
        
        updateStringFieldsFromDomains()
    }
    
    func resetView() {
        self.isYAxisManuallySet = false
        self.plotData.series = initialSeries
        self.viewDomainX = initialDomainX
        self.viewDomainY = initialDomainY
        
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
        
        newXSpan = min(max(newXSpan, minSpan), maxSpan)
        newYSpan = min(max(newYSpan, minSpan), maxSpan)

        let xCenter = (viewDomainX.lowerBound + viewDomainX.upperBound) / 2
        let yCenter = (viewDomainY.lowerBound + viewDomainY.upperBound) / 2
        
        // --- MODIFIED: The Y-axis now scales proportionally without a jarring auto-fit ---
        viewDomainX = (xCenter - newXSpan / 2)...(xCenter + newXSpan / 2)
        viewDomainY = (yCenter - newYSpan / 2)...(yCenter + newYSpan / 2)
        
        // After zooming, we re-enable auto-fitting for the *next* "Auto-Fit" click,
        // but we don't trigger it automatically.
        isYAxisManuallySet = false
    }
    
    func triggerDataRegenerationIfNeeded() {
        guard let handler = regenerationHandler, plotData.plotType == .line || plotData.plotType == .parametric else { return }

        regenerationTask?.cancel()

        let dataSpan = dataDomainX.upperBound - dataDomainX.lowerBound
        guard dataSpan > 0 else { return }
        let threshold = dataSpan * 0.25

        var domainToFetch: ClosedRange<Double>? = nil

        if viewDomainX.upperBound > dataDomainX.upperBound - threshold {
            let fetchStart = dataDomainX.upperBound
            let fetchEnd = dataDomainX.upperBound + dataSpan
            domainToFetch = fetchStart...fetchEnd
        } else if viewDomainX.lowerBound < dataDomainX.lowerBound + threshold {
            let fetchStart = dataDomainX.lowerBound - dataSpan
            let fetchEnd = dataDomainX.lowerBound
            domainToFetch = fetchStart...fetchEnd
        }

        guard let newDomain = domainToFetch else {
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
    
    private func appendNewData(newSeries: [PlotSeries]) {
        for newSeriesChunk in newSeries {
            if let existingSeriesIndex = self.plotData.series.firstIndex(where: { $0.name == newSeriesChunk.name }) {
                
                var combinedPoints = self.plotData.series[existingSeriesIndex].dataPoints
                combinedPoints.append(contentsOf: newSeriesChunk.dataPoints)
                combinedPoints.sort { $0.x < $1.x }
                
                var uniquePoints: [DataPoint] = []
                if let first = combinedPoints.first {
                    uniquePoints.append(first)
                    for i in 1..<combinedPoints.count {
                        if combinedPoints[i].x != combinedPoints[i-1].x { uniquePoints.append(combinedPoints[i]) }
                    }
                }
                
                self.plotData.series[existingSeriesIndex] = PlotSeries(name: newSeriesChunk.name, dataPoints: uniquePoints, equation: newSeriesChunk.equation)
            }
        }
        
        if let minX = plotData.series.flatMap({ $0.dataPoints }).map({ $0.x }).min(),
           let maxX = plotData.series.flatMap({ $0.dataPoints }).map({ $0.x }).max() {
            self.dataDomainX = minX...maxX
        }
        
        // --- REMOVED: Do not automatically rescale Y-axis after fetching new data from a pan ---
    }
    
    func autoFitDomains() {
        isYAxisManuallySet = false
        
        let allVisiblePoints = plotData.series.flatMap { $0.dataPoints }.filter { $0.x.isFinite && $0.y.isFinite && viewDomainX.contains($0.x) }
        
        if allVisiblePoints.isEmpty {
            resetView()
            return
        }
        
        recalculateYDomain(forVisibleXRange: viewDomainX)
    }
    
    func applyDomainsFromStrings() {
        let formatter = NumberFormatter()
        formatter.numberStyle = .decimal

        if let xMin = formatter.number(from: xMinString)?.doubleValue,
           let xMax = formatter.number(from: xMaxString)?.doubleValue,
           xMin < xMax {
            withAnimation { self.viewDomainX = xMin...xMax }
        }

        if let yMin = formatter.number(from: yMinString)?.doubleValue,
           let yMax = formatter.number(from: yMaxString)?.doubleValue,
           yMin < yMax {
            isYAxisManuallySet = true
            withAnimation { self.viewDomainY = yMin...yMax }
        }
        triggerDataRegenerationIfNeeded()
    }
    
    private func updateStringFieldsFromDomains() {
        xMinString = String(format: "%.4g", viewDomainX.lowerBound)
        xMaxString = String(format: "%.4g", viewDomainX.upperBound)
        yMinString = String(format: "%.4g", viewDomainY.lowerBound)
        yMaxString = String(format: "%.4g", viewDomainY.upperBound)
    }
    
    private func recalculateYDomain(forVisibleXRange: ClosedRange<Double>) {
        guard !isYAxisManuallySet, plotData.explicitYRange == nil else {
            return
        }
        
        let visiblePoints = plotData.series.flatMap { $0.dataPoints }.filter { $0.y.isFinite && forVisibleXRange.contains($0.x) }
        guard !visiblePoints.isEmpty else { return }
        var minY = visiblePoints.map { $0.y }.min()!
        var maxY = visiblePoints.map { $0.y }.max()!
        if abs(maxY - minY) < 1e-9 { minY -= 1; maxY += 1 }
        if abs(minY + maxY) < (maxY - minY) * 0.1 { let mag = max(abs(minY), abs(maxY)); minY = -mag; maxY = mag }
        let yPadding = (maxY - minY) * 0.1
        withAnimation(.easeInOut(duration: 0.2)) {
            self.viewDomainY = (minY - max(yPadding, 1.0))...(maxY + max(yPadding, 1.0))
        }
    }
    
    private static func calculateInitialDomains(for plotData: PlotData) -> (x: ClosedRange<Double>, y: ClosedRange<Double>) {
        var calculatedDomainX: ClosedRange<Double>
        var calculatedDomainY: ClosedRange<Double>

        if plotData.plotType == .vector {
            let allPoints = plotData.series.flatMap { $0.dataPoints }
            if allPoints.isEmpty { (calculatedDomainX, calculatedDomainY) = (-5...5, -5...5) }
            else {
                let maxCoord = max(allPoints.map { abs($0.x) }.max() ?? 0, allPoints.map { abs($0.y) }.max() ?? 0)
                let paddedMax = maxCoord * 1.2
                (calculatedDomainX, calculatedDomainY) = (-paddedMax...paddedMax, -paddedMax...paddedMax)
            }
        } else {
            let allPoints = plotData.series.flatMap { $0.dataPoints }.filter { $0.x.isFinite && $0.y.isFinite }
            if allPoints.isEmpty { (calculatedDomainX, calculatedDomainY) = (-10...10, -10...10) }
            else {
                if plotData.plotType == .parametric {
                    let (minX, maxX) = (allPoints.map { $0.x }.min()!, allPoints.map { $0.x }.max()!)
                    let (minY, maxY) = (allPoints.map { $0.y }.min()!, allPoints.map { $0.y }.max()!)
                    let maxSpan = max(maxX - minX, maxY - minY) * 1.2
                    let midX = (minX + maxX) / 2; let midY = (minY + maxY) / 2
                    calculatedDomainX = (midX - maxSpan / 2)...(midX + maxSpan / 2)
                    calculatedDomainY = (midY - maxSpan / 2)...(midY + maxSpan / 2)
                } else {
                    calculatedDomainX = plotData.initialXRange.map { $0.min...$0.max } ?? {
                        var (minX, maxX) = (allPoints.map { $0.x }.min()!, allPoints.map { $0.x }.max()!)
                        if minX == maxX { minX -= 1; maxX += 1 }
                        let padding = (maxX - minX) * 0.05
                        return (minX - max(padding, 0.5))...(maxX + max(padding, 0.5))
                    }()
                    
                    calculatedDomainY = plotData.explicitYRange.map { $0.min...$0.max } ?? {
                        let visiblePoints = allPoints.filter { calculatedDomainX.contains($0.x) }
                        if visiblePoints.isEmpty {
                            return -10...10
                        }
                        var (minY, maxY) = (visiblePoints.map { $0.y }.min()!, visiblePoints.map { $0.y }.max()!)
                        if minY == maxY { minY -= 1; maxY += 1 }
                        let padding = (maxY - minY) * 0.1
                        return (minY - max(padding, 1.0))...(maxY + max(padding, 1.0))
                    }()
                }
            }
        }
        return (calculatedDomainX, calculatedDomainY)
    }
}

