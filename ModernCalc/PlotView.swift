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
                        y: .value("Y", segmentPoint.y)
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
                    // This new approach draws the arrowhead manually to bypass the SwiftUI bug.
                    
                    // The vector's true mathematical angle.
                    let vectorAngle = atan2(point.y, point.x)
                    
                    // The logic to correct for the chart's coordinate system remains necessary.
                    // Start angle (UP) - Target Angle.
                    let rotationInRadians = .pi / 2 - vectorAngle

                    // Use our custom Arrowhead shape.
                    Arrowhead()
                        .fill(color)
                        .frame(width: 8, height: 10) // The size of the arrowhead
                        // We must first offset the shape so the tip is at the anchor point.
                        .offset(y: -5)
                        // Now, we rotate our custom view. This is much more stable than rotating an Image.
                        .rotationEffect(.radians(rotationInRadians))
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
            .aspectRatio(1, contentMode: .fit) // Forces the graph to a square aspect ratio
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

// A custom shape for drawing a simple arrowhead triangle.
struct Arrowhead: Shape {
    func path(in rect: CGRect) -> Path {
        var path = Path()
        // The path for a triangle pointing up.
        path.move(to: CGPoint(x: rect.midX, y: rect.minY)) // Tip
        path.addLine(to: CGPoint(x: rect.minX, y: rect.maxY)) // Bottom-left
        path.addLine(to: CGPoint(x: rect.maxX, y: rect.maxY)) // Bottom-right
        path.closeSubpath()
        return path
    }
}
