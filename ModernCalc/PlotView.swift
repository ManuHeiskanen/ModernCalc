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

    // Helper to create a color map for the chart's legend and styles.
    // This ensures that the color assigned by the chart framework matches our manual color selection.
    private var colorMap: [String: Color] {
        Dictionary(uniqueKeysWithValues: viewModel.plotData.series.enumerated().map { (index, series) in
            (series.name, chartColors[index % chartColors.count])
        })
    }
    
    // Generates a user-friendly title based on the plot's content.
    private var plotTitle: String {
        switch viewModel.plotData.plotType {
        case .vector:
            return "Vector Plot"
        case .line, .parametric:
            let functionNames = viewModel.plotData.series.map { $0.name }.joined(separator: ", ")
            return "Graph of \(functionNames)"
        }
    }

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
        ForEach(viewModel.plotData.series, id: \.id) { series in
            // By grouping all marks for a single vector within a `Plot` container,
            // we ensure that chart styling modifiers apply to the entire group consistently.
            Plot {
                // *** FIX: Look up the color from the map using the series name. ***
                // This ensures the arrowhead color is derived from the same source as the line color.
                let color = colorMap[series.name] ?? .primary

                if let point = series.dataPoints.first {
                    let lineSegmentData = [DataPoint(x: 0, y: 0), point]
                    
                    // 1. Draw the line for the series.
                    ForEach(lineSegmentData) { segmentPoint in
                        LineMark(
                            x: .value("X", segmentPoint.x),
                            y: .value("Y", segmentPoint.y)
                        )
                    }

                    // 2. Draw an invisible point at the end to anchor the arrowhead annotation.
                    PointMark(
                        x: .value("X", point.x),
                        y: .value("Y", point.y)
                    )
                    .symbolSize(0)
                    .annotation(position: .overlay) {
                        // The vector's true mathematical angle.
                        let vectorAngle = atan2(point.y, point.x)
                        
                        // The logic to correct for the chart's coordinate system remains necessary.
                        // Start angle (UP) - Target Angle.
                        let rotationInRadians = .pi / 2 - vectorAngle

                        // Use our custom Arrowhead shape.
                        Arrowhead()
                            .fill(color) // The arrowhead is filled with the correctly looked-up color.
                            .frame(width: 8, height: 10) // The size of the arrowhead
                            // We must first offset the shape so the tip is at the anchor point.
                            .offset(y: -5)
                            // Now, we rotate our custom view. This is much more stable than rotating an Image.
                            .rotationEffect(.radians(rotationInRadians))
                    }
                }
            }
            // Use foregroundStyle(by:) for the legend. The color will be sourced from the
            // .chartForegroundStyleScale modifier on the Chart view, ensuring consistency.
            // Applying this to the `Plot` group ensures all marks within it are associated with the series.
            .foregroundStyle(by: .value("Vector", series.name))
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
            Text(plotTitle)
                .font(.title2)
                .padding(.top)
            
            Chart {
                chartContent(for: viewModel.plotData.plotType)
            }
            // *** FIX: Use the domain/range overload to avoid type inference errors. ***
            // We explicitly provide the series names as the domain and our colors as the range.
            .chartForegroundStyleScale(domain: viewModel.plotData.series.map { $0.name }, range: chartColors)
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

