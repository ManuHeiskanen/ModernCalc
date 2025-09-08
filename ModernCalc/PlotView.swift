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

    var body: some View {
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

            HStack {
                Button("Reset View", action: {
                    viewModel.resetView()
                })
                Spacer()
            }
            .padding()
        }
        .frame(minWidth: 400, minHeight: 400)
        .background(Color(NSColor.windowBackgroundColor))
    }
}
