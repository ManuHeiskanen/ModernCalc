//
//  PlotView.swift
//  ModernCalc
//
//  Created by Manu Heiskanen on 8.9.2025.
//

import SwiftUI
import Charts

// Predefined list of colors for the chart to cycle through.
private let chartColors: [Color] = [.blue, .green, .orange, .red, .purple, .pink, .teal, .indigo]

struct PlotView: View {
    @StateObject var viewModel: PlotViewModel

    // Helper function for creating vector plots
    @ChartContentBuilder
    private func vectorPlotContent() -> some ChartContent {
        // Add rule marks for the X and Y axes for better context
        RuleMark(x: .value("Origin", 0))
            .foregroundStyle(.gray.opacity(0.5))
            .lineStyle(StrokeStyle(lineWidth: 1))
        RuleMark(y: .value("Origin", 0))
            .foregroundStyle(.gray.opacity(0.5))
            .lineStyle(StrokeStyle(lineWidth: 1))
            
        // Enumerate the series to get an index for manual color selection
        ForEach(Array(viewModel.plotData.series.enumerated()), id: \.element.id) { index, series in
            // Determine the color for this vector
            let color = chartColors[index % chartColors.count]

            if let point = series.dataPoints.first {
                let lineSegmentData = [DataPoint(x: 0, y: 0), point]
                
                // 1. Draw the line for the series.
                ForEach(lineSegmentData) { segmentPoint in
                    LineMark(
                        x: .value("X", segmentPoint.x),
                        y: .value("Y", segmentPoint.y) // FIX: Corrected typo from segment_point
                    )
                }
                // Use foregroundStyle(by:) for the legend and automatic color grouping
                .foregroundStyle(by: .value("Vector", series.name))
                // Use a second foregroundStyle to force the specific color
                .foregroundStyle(color)

                // 2. Draw an invisible point at the end to anchor the arrowhead annotation.
                PointMark(
                    x: .value("X", point.x),
                    y: .value("Y", point.y)
                )
                .symbolSize(0)
                .annotation(position: .overlay) {
                    let angle = atan2(point.y, point.x) * 180 / .pi
                    Image(systemName: "play.fill")
                        .rotationEffect(.degrees(angle))
                        .font(.system(size: 8))
                        .foregroundStyle(color) // Apply the same color to the arrowhead
                }
            }
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
