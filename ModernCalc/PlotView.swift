//
//  PlotView.swift
//  ModernCalc
//
//  Created by Manu Heiskanen on 8.9.2025.
//

import SwiftUI
import Charts

struct PlotView: View {
    @StateObject var viewModel: PlotViewModel

    private var chartContent: some View {
        VStack {
            // Display the original, full expression in the title
            Text(viewModel.plotData.expression)
                .font(.title2)
                .padding(.top)
            
            Chart {
                // Outer loop iterates through each function series (e.g., sin(x), cos(x))
                ForEach(viewModel.plotData.series) { series in
                    // Inner loop draws the line for each data point in the current series
                    ForEach(series.dataPoints) { point in
                        LineMark(
                            x: .value("X", point.x),
                            y: .value("Y", point.y)
                        )
                        .interpolationMethod(.catmullRom)
                    }
                    // This tells the chart to color each series differently and use its name for the legend
                    .foregroundStyle(by: .value("Function", series.name))
                }
            }
            .chartXScale(domain: viewModel.viewDomainX)
            .chartYScale(domain: viewModel.viewDomainY)
            .padding()
        }
    }
    
    var body: some View {
        VStack {
            chartContent

            HStack {
                Button("Save as PNG") {
                    exportChart()
                }
                Spacer()
            }
            .padding([.horizontal, .bottom])
        }
        .frame(minWidth: 500, minHeight: 400) // Increased minWidth to better accommodate legend
        .background(Color(NSColor.windowBackgroundColor))
    }
    
    private func exportChart() {
        let viewToRender = chartContent
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

