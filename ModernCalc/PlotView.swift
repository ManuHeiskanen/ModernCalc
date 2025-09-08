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

    // A computed property that defines the visual content of the chart.
    // This allows us to both display it and render it to an image.
    private var chartContent: some View {
        VStack {
            Text(viewModel.plotData.expression)
                .font(.title2)
                .padding(.top)
            
            Chart {
                ForEach(viewModel.plotData.dataPoints) { point in
                    LineMark(
                        x: .value("X", point.x),
                        y: .value("Y", point.y)
                    )
                    .interpolationMethod(.catmullRom)
                }
            }
            .chartXScale(domain: viewModel.viewDomainX)
            .chartYScale(domain: viewModel.viewDomainY)
            .padding()
        }
    }
    
    var body: some View {
        VStack {
            // Display the chart content defined above.
            chartContent

            HStack {
                Button("Save as PNG") {
                    exportChart()
                }
                Spacer()
            }
            .padding([.horizontal, .bottom])
        }
        .frame(minWidth: 400, minHeight: 400)
        .background(Color(NSColor.windowBackgroundColor))
    }
    
    // Function to render the chart view as an image and save it to a file.
    private func exportChart() {
        // Use ImageRenderer to capture the chartContent view.
        // We give it a fixed size for consistent export quality.
        let viewToRender = chartContent
            .frame(width: 800, height: 600)
            .background(Color(NSColor.windowBackgroundColor))

        let renderer = ImageRenderer(content: viewToRender)
        
        // Use the screen's scale factor for high-resolution (Retina) output.
        if let screenScale = NSScreen.main?.backingScaleFactor {
            renderer.scale = screenScale
        }
        
        // Render the view to an NSImage.
        if let image = renderer.nsImage {
            let savePanel = NSSavePanel()
            savePanel.allowedContentTypes = [.png]
            savePanel.canCreateDirectories = true
            savePanel.nameFieldStringValue = "\(viewModel.plotData.expression).png"

            // Show the save dialog to the user.
            if savePanel.runModal() == .OK, let url = savePanel.url {
                // Convert the NSImage to PNG data and write it to the selected file URL.
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

