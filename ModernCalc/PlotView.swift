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
    @State var viewModel: PlotViewModel

    // Helper to create a color map for the chart's legend and styles.
    // This ensures that the color assigned by the chart framework matches our manual color selection.
    private var colorMap: [String: Color] {
        Dictionary(uniqueKeysWithValues: viewModel.plotData.series.enumerated().map { (index, series) in
            (series.name, chartColors[index % chartColors.count])
        })
    }
    
    // --- NEW: Helper function to clean axis labels for use in titles ---
    // This will remove the unit part, e.g., "f(v) [m/s]" becomes "f(v)".
    private func cleanAxisLabel(for label: String) -> String {
        if let range = label.range(of: " [") {
            return String(label[..<range.lowerBound])
        }
        return label
    }
    
    // --- MODIFIED: A smarter, user-friendly title generator ---
    private var plotTitle: String {
        let complexityThreshold = 25

        switch viewModel.plotData.plotType {
        case .vector:
            return "Vector Plot"
        case .scatter:
            let yLabel = cleanAxisLabel(for: viewModel.yAxisLabel.isEmpty ? "Y-Axis" : viewModel.yAxisLabel)
            let xLabel = cleanAxisLabel(for: viewModel.xAxisLabel.isEmpty ? "X-Axis" : viewModel.xAxisLabel)
            var title = "\(yLabel) vs. \(xLabel)"
            if viewModel.plotData.series.contains(where: { $0.name.contains("Fit") }) {
                title += " with Trendline"
            }
            return title
        case .line, .parametric:
            let functionNames = viewModel.plotData.series.map { $0.name }
            let areAllNamesSimple = functionNames.allSatisfy { $0.count < complexityThreshold && !$0.contains("if") }

            if functionNames.count == 1, areAllNamesSimple {
                return "Graph of \(functionNames.first!)"
            } else if areAllNamesSimple {
                return "Graph of \(functionNames.joined(separator: ", "))"
            } else {
                let yLabel = cleanAxisLabel(for: viewModel.yAxisLabel.isEmpty ? "Y-Axis" : viewModel.yAxisLabel)
                let xLabel = cleanAxisLabel(for: viewModel.xAxisLabel.isEmpty ? "X-Axis" : viewModel.xAxisLabel)
                return "\(yLabel) vs. \(xLabel)"
            }
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
                        let vectorAngle = atan2(point.y, point.x)
                        let rotationInRadians = .pi / 2 - vectorAngle

                        Arrowhead()
                            .fill(color)
                            .frame(width: 8, height: 10)
                            .offset(y: -5)
                            .rotationEffect(.radians(rotationInRadians))
                    }
                }
            }
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
    
    // Helper function for creating scatter plots
    @ChartContentBuilder
    private func scatterPlotContent() -> some ChartContent {
        ForEach(viewModel.plotData.series) { series in
            if series.name.contains("Fit") {
                // Trendline
                ForEach(series.dataPoints) { point in
                    LineMark(
                        x: .value("X", point.x),
                        y: .value("Y", point.y)
                    )
                    .interpolationMethod(.catmullRom) // Use CatmullRom for smooth curves
                }
                .lineStyle(StrokeStyle(lineWidth: 2, dash: [5, 5]))
                .foregroundStyle(by: .value("Data", series.name))

            } else {
                // Raw data points
                ForEach(series.dataPoints) { point in
                    PointMark(
                        x: .value("X", point.x),
                        y: .value("Y", point.y)
                    )
                }
                .foregroundStyle(by: .value("Data", series.name))
            }
        }
    }

    // Main content builder now just decides which helper to use
    @ChartContentBuilder
    private func chartContent(for plotType: PlotType) -> some ChartContent {
        switch plotType {
        case .vector:
            vectorPlotContent()
        case .scatter:
            scatterPlotContent()
        case .line, .parametric:
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
            .chartForegroundStyleScale(
                domain: viewModel.plotData.series.map { $0.name },
                range: chartColors
            )
            .chartXScale(domain: viewModel.viewDomainX)
            .chartYScale(domain: viewModel.viewDomainY)
            .chartXAxis { AxisMarks(preset: .automatic, stroke: StrokeStyle(lineWidth: 1)) }
            .chartYAxis { AxisMarks(preset: .automatic, stroke: StrokeStyle(lineWidth: 1)) }
            .chartXAxisLabel(viewModel.xAxisLabel)
            .chartYAxisLabel(viewModel.yAxisLabel)
            .chartPlotStyle { plotArea in
                plotArea
                    .background(Color.gray.opacity(0.1))
                    .border(Color.primary.opacity(0.5), width: 1)
                    .aspectRatio(1, contentMode: .fit)
            }
            .chartOverlay { proxy in
                VStack(alignment: .leading, spacing: 4) {
                    ForEach(viewModel.plotData.series) { series in
                        if let equation = series.equation {
                            Text(equation)
                                .font(.callout)
                                .foregroundStyle(.secondary)
                                .padding(6)
                                .background(.background.opacity(0.7), in: RoundedRectangle(cornerRadius: 4))
                        }
                    }
                }
                .padding(10)
                .frame(maxWidth: .infinity, maxHeight: .infinity, alignment: .topLeading)
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
        .frame(minWidth: 350, idealWidth: 400, maxWidth: .infinity, minHeight: 350, idealHeight: 600, maxHeight: .infinity)
        .background(Color(NSColor.windowBackgroundColor))
    }
    
    private func exportChart() {
        let viewToRender = chartContainer
            .frame(width: 400, height: 600)
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
