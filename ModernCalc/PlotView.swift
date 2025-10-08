//
//  PlotView.swift
//  ModernCalc
//
//  Created by Manu Heiskanen on 8.9.2025.
//

import SwiftUI
import Charts

private let chartColors: [Color] = [.blue, .green, .orange, .red, .purple, .pink, .teal, .indigo]

struct PlotView: View {
    @State var viewModel: PlotViewModel

    @State private var plotSize: CGSize = .zero
    @State private var initialDragDomains: (x: ClosedRange<Double>, y: ClosedRange<Double>)? = nil
    @State private var cumulativeZoom: CGFloat = 1.0

    private var colorMap: [String: Color] {
        Dictionary(uniqueKeysWithValues: viewModel.plotData.series.enumerated().map { (index, series) in
            (series.name, chartColors[index % chartColors.count])
        })
    }
    
    private func cleanAxisLabel(for label: String) -> String {
        if let range = label.range(of: " [") { return String(label[..<range.lowerBound]) }
        return label
    }
    
    private var plotTitle: String {
        let complexityThreshold = 25
        switch viewModel.plotData.plotType {
        case .vector: return "Vector Plot"
        case .scatter:
            let yLabel = cleanAxisLabel(for: viewModel.yAxisLabel.isEmpty ? "Y-Axis" : viewModel.yAxisLabel)
            let xLabel = cleanAxisLabel(for: viewModel.xAxisLabel.isEmpty ? "X-Axis" : viewModel.xAxisLabel)
            var title = "\(yLabel) vs. \(xLabel)"
            if viewModel.plotData.series.contains(where: { $0.name.contains("Fit") }) { title += " with Trendline" }
            return title
        case .line, .parametric:
            let names = viewModel.plotData.series.map { $0.name }
            let simple = names.allSatisfy { $0.count < complexityThreshold && !$0.contains("if") }
            if names.count == 1, simple { return "Graph of \(names.first!)" }
            else if simple { return "Graph of \(names.joined(separator: ", "))" }
            else {
                let yLabel = cleanAxisLabel(for: viewModel.yAxisLabel.isEmpty ? "Y-Axis" : viewModel.yAxisLabel)
                let xLabel = cleanAxisLabel(for: viewModel.xAxisLabel.isEmpty ? "X-Axis" : viewModel.xAxisLabel)
                return "\(yLabel) vs. \(xLabel)"
            }
        }
    }

    private var chartContainer: some View {
        VStack {
            Text(plotTitle).font(.title2).padding(.top)
            Chart { chartContent(for: viewModel.plotData.plotType) }
                .chartForegroundStyleScale(domain: viewModel.plotData.series.map { $0.name }, range: chartColors)
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
                        .clipped()
                        .background(GeometryReader { geo in Color.clear.onAppear { plotSize = geo.size } })
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
        VStack(spacing: 0) {
            chartContainer
                .gesture(
                    DragGesture()
                        .onChanged { value in
                            if initialDragDomains == nil {
                                initialDragDomains = (viewModel.viewDomainX, viewModel.viewDomainY)
                            }
                            guard let startDomains = initialDragDomains else { return }
                            viewModel.pan(by: value.translation, from: startDomains, plotSize: plotSize)
                        }
                        .onEnded { _ in
                            initialDragDomains = nil
                            viewModel.triggerDataRegenerationIfNeeded()
                        }
                )
                .simultaneousGesture(
                    MagnificationGesture()
                        .onChanged { value in
                            let zoomFactor = value / cumulativeZoom
                            viewModel.zoom(by: zoomFactor)
                            cumulativeZoom = value
                        }
                        .onEnded { value in
                            cumulativeZoom = 1.0
                            viewModel.triggerDataRegenerationIfNeeded()
                        }
                )
                .onContinuousHover { phase in
                    switch phase {
                    case .active(_): NSCursor.openHand.push()
                    case .ended: NSCursor.pop()
                    }
                }
            
            // --- NEW: Axis Control Panel ---
            AxisControlView(viewModel: viewModel)

            Divider()

            HStack {
                Button("Save as PNG") { exportChart() }
                Spacer()
                Button("Reset View") {
                    withAnimation {
                        viewModel.resetView()
                    }
                }
            }
            .padding()
        }
        .frame(minWidth: 350, idealWidth: 400, maxWidth: .infinity, minHeight: 450, idealHeight: 650, maxHeight: .infinity)
        .background(Color(NSColor.windowBackgroundColor))
    }
    
    @ChartContentBuilder
    private func vectorPlotContent() -> some ChartContent {
        RuleMark(x: .value("Origin", 0)).foregroundStyle(.gray.opacity(0.5)).lineStyle(StrokeStyle(lineWidth: 1))
        RuleMark(y: .value("Origin", 0)).foregroundStyle(.gray.opacity(0.5)).lineStyle(StrokeStyle(lineWidth: 1))
        ForEach(viewModel.plotData.series, id: \.id) { series in
            Plot {
                let color = colorMap[series.name] ?? .primary
                if let point = series.dataPoints.first {
                    let lineSegmentData = [DataPoint(x: 0, y: 0), point]
                    ForEach(lineSegmentData) { segmentPoint in
                        LineMark(x: .value("X", segmentPoint.x), y: .value("Y", segmentPoint.y))
                    }
                    PointMark(x: .value("X", point.x), y: .value("Y", point.y))
                        .symbolSize(0)
                        .annotation(position: .overlay) {
                            let vectorAngle = atan2(point.y, point.x)
                            let rotationInRadians = .pi / 2 - vectorAngle
                            Arrowhead().fill(color).frame(width: 8, height: 10).offset(y: -5).rotationEffect(.radians(rotationInRadians))
                        }
                }
            }
            .foregroundStyle(by: .value("Vector", series.name))
        }
    }

    @ChartContentBuilder
    private func lineAndParametricPlotContent() -> some ChartContent {
        ForEach(viewModel.plotData.series) { series in
            ForEach(series.dataPoints) { point in
                LineMark(x: .value("X", point.x), y: .value("Y", point.y)).interpolationMethod(.catmullRom)
            }
            .foregroundStyle(by: .value("Function", series.name))
        }
    }
    
    @ChartContentBuilder
    private func scatterPlotContent() -> some ChartContent {
        ForEach(viewModel.plotData.series) { series in
            if series.name.contains("Fit") {
                ForEach(series.dataPoints) { point in
                    LineMark(x: .value("X", point.x), y: .value("Y", point.y)).interpolationMethod(.catmullRom)
                }
                .lineStyle(StrokeStyle(lineWidth: 2, dash: [5, 5]))
                .foregroundStyle(by: .value("Data", series.name))
            } else {
                ForEach(series.dataPoints) { point in
                    PointMark(x: .value("X", point.x), y: .value("Y", point.y))
                }
                .foregroundStyle(by: .value("Data", series.name))
            }
        }
    }
    
    @ChartContentBuilder
    private func chartContent(for plotType: PlotType) -> some ChartContent {
        switch plotType {
        case .vector: vectorPlotContent()
        case .scatter: scatterPlotContent()
        case .line, .parametric: lineAndParametricPlotContent()
        }
    }
    
    private func exportChart() {
        let viewToRender = chartContainer
            .frame(width: 400, height: 600)
            .background(Color(NSColor.windowBackgroundColor))
        let renderer = ImageRenderer(content: viewToRender)
        if let screenScale = NSScreen.main?.backingScaleFactor { renderer.scale = screenScale }
        if let image = renderer.nsImage {
            let savePanel = NSSavePanel()
            savePanel.allowedContentTypes = [.png]
            savePanel.canCreateDirectories = true
            savePanel.nameFieldStringValue = "\(viewModel.plotData.expression).png"
            if savePanel.runModal() == .OK, let url = savePanel.url {
                if let tiffData = image.tiffRepresentation,
                   let bitmap = NSBitmapImageRep(data: tiffData),
                   let pngData = bitmap.representation(using: .png, properties: [:]) {
                    do { try pngData.write(to: url) }
                    catch { print("Failed to save PNG: \(error.localizedDescription)") }
                }
            }
        }
    }
}

// --- NEW: A dedicated view for the axis controls ---
struct AxisControlView: View {
    @Bindable var viewModel: PlotViewModel
    @State private var isExpanded = false
    
    private static let numberFormatter: NumberFormatter = {
        let formatter = NumberFormatter()
        formatter.numberStyle = .decimal
        formatter.maximumFractionDigits = 8
        return formatter
    }()
    
    var body: some View {
        DisclosureGroup("Axis Controls", isExpanded: $isExpanded) {
            VStack {
                HStack {
                    Text("X-Axis:")
                        .frame(width: 50, alignment: .trailing)
                    TextField("Min", text: $viewModel.xMinString)
                        .textFieldStyle(RoundedBorderTextFieldStyle())
                    Text("to")
                    TextField("Max", text: $viewModel.xMaxString)
                        .textFieldStyle(RoundedBorderTextFieldStyle())
                }
                HStack {
                    Text("Y-Axis:")
                        .frame(width: 50, alignment: .trailing)
                    TextField("Min", text: $viewModel.yMinString)
                        .textFieldStyle(RoundedBorderTextFieldStyle())
                    Text("to")
                    TextField("Max", text: $viewModel.yMaxString)
                        .textFieldStyle(RoundedBorderTextFieldStyle())
                }
                HStack {
                    Spacer()
                    Button("Auto-Fit") {
                        viewModel.autoFitDomains()
                    }
                    Button("Apply") {
                        viewModel.applyDomainsFromStrings()
                    }
                    .keyboardShortcut(.return, modifiers: [])
                }
            }
            .padding(.top, 8)
        }
        .padding(.horizontal)
        .padding(.bottom, 8)
    }
}


struct Arrowhead: Shape {
    func path(in rect: CGRect) -> Path {
        var path = Path()
        path.move(to: CGPoint(x: rect.midX, y: rect.minY))
        path.addLine(to: CGPoint(x: rect.minX, y: rect.maxY))
        path.addLine(to: CGPoint(x: rect.maxX, y: rect.maxY))
        path.closeSubpath()
        return path
    }
}

