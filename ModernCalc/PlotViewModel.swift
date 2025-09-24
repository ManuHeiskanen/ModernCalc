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

    var regenerationHandler: ((ClosedRange<Double>) -> Task<[PlotSeries]?, Never>)?
    private var debounceTask: Task<Void, Never>?

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
                    var minX = allPoints.map { $0.x }.min()!, maxX = allPoints.map { $0.x }.max()!
                    if minX == maxX { minX -= 1; maxX += 1 }
                    if abs(minX + maxX) < (maxX - minX) * 0.1 { let mag = max(abs(minX), abs(maxX)); minX = -mag; maxX = mag }
                    let xPadding = (maxX - minX) * 0.05
                    calculatedDomainX = (minX - max(xPadding, 0.5))...(maxX + max(xPadding, 0.5))
                    
                    if let yRange = plotData.explicitYRange {
                        calculatedDomainY = yRange.min...yRange.max
                    } else {
                        var minY = allPoints.map { $0.y }.min()!, maxY = allPoints.map { $0.y }.max()!
                        if minY == maxY { minY -= 1; maxY += 1 }
                        if abs(minY + maxY) < (maxY - minY) * 0.1 { let mag = max(abs(minY), abs(maxY)); minY = -mag; maxY = mag }
                        let yPadding = (maxY - minY) * 0.1
                        calculatedDomainY = (minY - max(yPadding, 1.0))...(maxY + max(yPadding, 1.0))
                    }
                }
            }
        }
        
        self.viewDomainX = calculatedDomainX
        self.viewDomainY = calculatedDomainY
        self.initialDomainX = calculatedDomainX
        self.initialDomainY = calculatedDomainY
    }
    
    func resetView() {
        self.plotData.series = initialSeries
        self.viewDomainX = initialDomainX
        self.viewDomainY = initialDomainY
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
    
    func requestDataRegeneration() {
        guard plotData.plotType == .line || plotData.plotType == .parametric else { return }

        debounceTask?.cancel()
        debounceTask = Task {
            do {
                try await Task.sleep(for: .milliseconds(350))
                guard !Task.isCancelled else { return }

                if let handler = regenerationHandler, let newSeries = await handler(viewDomainX).value {
                    self.plotData.series = newSeries
                    recalculateYDomain()
                }
            } catch {}
        }
    }
    
    private func recalculateYDomain() {
        guard plotData.explicitYRange == nil else {
            self.viewDomainY = plotData.explicitYRange!.min...plotData.explicitYRange!.max
            return
        }

        let allPoints = plotData.series.flatMap { $0.dataPoints }.filter { $0.y.isFinite }
        guard !allPoints.isEmpty else { return }

        var minY = allPoints.map { $0.y }.min()!
        var maxY = allPoints.map { $0.y }.max()!

        if minY == maxY { minY -= 1; maxY += 1 }
        
        if abs(minY + maxY) < (maxY - minY) * 0.1 {
            let mag = max(abs(minY), abs(maxY)); minY = -mag; maxY = mag
        }

        let yPadding = (maxY - minY) * 0.1
        self.viewDomainY = (minY - max(yPadding, 1.0))...(maxY + max(yPadding, 1.0))
    }
}

