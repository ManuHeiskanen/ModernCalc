//
//  PlotView.swift
//  ModernCalc
//
//  Created by Manu Heiskanen on 8.9.2025.
//

import SwiftUI
import Charts

// Helper View for the vector arrowhead symbol
struct ArrowheadSymbol: View {
    let point: DataPoint

    var body: some View {
        let angle = atan2(point.y, point.x) * 180 / .pi
        Image(systemName: "triangle.fill")
            .rotationEffect(.degrees(angle - 90))
            .font(.system(size: 8))
    }
}

// --- START: New ChartContent Struct to Resolve Compiler Error ---
// This struct encapsulates all the marks needed to draw a single vector.
// This simplifies the main chart body, preventing the compiler from timing out.
struct SingleVectorMark: ChartContent {
    let series: PlotSeries

    var body: some ChartContent {
        // Loop through the single data point (the endpoint) in the vector series
        ForEach(series.dataPoints) { point in
            // Create a temporary array for the line segment data (origin to endpoint)
            let lineSegmentData = [DataPoint(x: 0, y: 0), point]

            // Plot the line by iterating over its two points. The chart connects
            // LineMarks that belong to the same series.
            ForEach(lineSegmentData) { segmentPoint in
                LineMark(
                    x: .value("X", segmentPoint.x),
                    y: .value("Y", segmentPoint.y)
                )
            }
            .foregroundStyle(by: .value("Vector", series.name))
            
            // Arrowhead at the vector's endpoint
            PointMark(
                x: .value("X", point.x),
                y: .value("Y", point.y)
            )
            .symbol {
                ArrowheadSymbol(point: point)
            }
            .foregroundStyle(by: .value("Vector", series.name))
        }
    }
}
// --- END: New ChartContent Struct ---

struct PlotView: View {
    @StateObject var viewModel: PlotViewModel

    // Helper function for creating vector plots, now simplified
    @ChartContentBuilder
    private func vectorPlotContent() -> some ChartContent {
        ForEach(viewModel.plotData.series) { series in
            // Use the dedicated struct for each vector
            SingleVectorMark(series: series)
        }
    }

    // Helper function for creating standard line and parametric plots
    @ChartContentBuilder
    private func lineAndParametricPlotContent() -> some ChartContent {
        ForEach(viewModel.plotData.series) { series in
            ForEach(series.dataPoints) { point in
                LineMark(
                    x: .value("X", point.x),
                    y: .value("Y", point.y)
                )
                .interpolationMethod(.catmullRom)
            }
            .foregroundStyle(by: .value("Function", series.name))
        }
    }

    // Main content builder now just decides which helper to use
    @ChartContentBuilder
    private func chartContent(for plotType: PlotType) -> some ChartContent {
        if plotType == .vector {
            vectorPlotContent()
        } else {
            lineAndParametricPlotContent()
        }
    }
    
    private var chartContainer: some View {
        VStack {
            Text(viewModel.plotData.expression)
                .font(.title2)
                .padding(.top)
            
            Chart {
                chartContent(for: viewModel.plotData.plotType)
            }
            .chartXScale(domain: viewModel.viewDomainX)
            .chartYScale(domain: viewModel.viewDomainY)
            .chartXAxis { AxisMarks(preset: .automatic, stroke: StrokeStyle(lineWidth: 1)) }
            .chartYAxis { AxisMarks(preset: .automatic, stroke: StrokeStyle(lineWidth: 1)) }
            .chartPlotStyle { plotArea in
                plotArea
                    .background(Color.gray.opacity(0.1))
                    .border(Color.primary.opacity(0.5), width: 1)
            }
            .padding()
        }
    }
    
    var body: some View {
        VStack {
            chartContainer

            HStack {
                Button("Save as PNG") {
                    exportChart()
                }
                Spacer()
            }
            .padding([.horizontal, .bottom])
        }
        .frame(minWidth: 500, minHeight: 400)
        .background(Color(NSColor.windowBackgroundColor))
    }
    
    private func exportChart() {
        let viewToRender = chartContainer
            .frame(width: 800, height: 600)
            .background(Color(NSColor.windowBackgroundColor))

        let renderer = ImageRenderer(content: viewToRender)
        
        if let screenScale = NSScreen.main?.backingScaleFactor {
            renderer.scale = screenScale
        }
        
        if let image = renderer.nsImage {
            let savePanel = NSSavePanel()
            savePanel.allowedContentTypes = [.png]
            savePanel.canCreateDirectories = true
            savePanel.nameFieldStringValue = "\(viewModel.plotData.expression).png"

            if savePanel.runModal() == .OK, let url = savePanel.url {
                if let tiffData = image.tiffRepresentation,
                   let bitmap = NSBitmapImageRep(data: tiffData),
                   let pngData = bitmap.representation(using: .png, properties: [:]) {
                    do {
                        try pngData.write(to: url)
                    } catch {
                        print("Failed to save PNG: \(error.localizedDescription)")
                    }
                }
            }
        }
    }
}
