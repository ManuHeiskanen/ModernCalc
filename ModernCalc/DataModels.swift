//
//  DataModels.swift
//  ModernCalc
//
//  Created by Manu Heiskanen on 8.9.2025.
//

import Foundation

// MARK: - Core Calculation and History Models

enum AngleMode: String, Codable {
    case degrees, radians
}

enum CalculationType {
    case evaluation
    case variableAssignment
    case functionDefinition
    case plot
}

struct Calculation: Identifiable, Hashable {
    let id = UUID()
    let expression: String
    let result: MathValue
    let type: CalculationType
    let usedAngleSensitiveFunction: Bool
    let angleMode: AngleMode

    static func == (lhs: Calculation, rhs: Calculation) -> Bool {
        return lhs.id == rhs.id
    }

    func hash(into hasher: inout Hasher) {
        hasher.combine(id)
    }
}

// MARK: - Plotting Models

// Represents a single function line or data series on a chart.
struct PlotSeries: Identifiable {
    let id = UUID()
    let name: String
    let dataPoints: [DataPoint]
    let equation: String?

    init(name: String, dataPoints: [DataPoint], equation: String? = nil) {
        self.name = name
        self.dataPoints = dataPoints
        self.equation = equation
    }
}

// Represents the complete set of data for a plot window.
struct PlotData: Identifiable, Hashable {
    let id = UUID()
    let expression: String
    var series: [PlotSeries]
    let plotType: PlotType
    let explicitYRange: (min: Double, max: Double)?
    let initialXRange: (min: Double, max: Double)?
    let generationTime: TimeInterval?

    // --- NEW PROPERTIES ---
    let xAxisLabel: String
    let yAxisLabel: String
    // We store the dimensions to allow for potential future unit conversions in the UI.
    let xAxisDimension: UnitDimension?
    let yAxisDimension: UnitDimension?
    let calculatedArea: UnitValue?

    init(expression: String, series: [PlotSeries], plotType: PlotType, explicitYRange: (min: Double, max: Double)?, initialXRange: (min: Double, max: Double)? = nil, generationTime: TimeInterval? = nil, xAxisLabel: String = "X", yAxisLabel: String = "Y", xAxisDimension: UnitDimension? = nil, yAxisDimension: UnitDimension? = nil, calculatedArea: UnitValue? = nil) {
        self.expression = expression
        self.series = series
        self.plotType = plotType
        self.explicitYRange = explicitYRange
        self.initialXRange = initialXRange
        self.generationTime = generationTime
        self.xAxisLabel = xAxisLabel
        self.yAxisLabel = yAxisLabel
        self.xAxisDimension = xAxisDimension
        self.yAxisDimension = yAxisDimension
        self.calculatedArea = calculatedArea
    }
    
    static func == (lhs: PlotData, rhs: PlotData) -> Bool {
        lhs.id == rhs.id
    }

    func hash(into hasher: inout Hasher) {
        hasher.combine(id)
    }
}

// Represents a single (x, y) coordinate.
struct DataPoint: Identifiable {
    let id = UUID()
    let x: Double
    let y: Double
    var y_end: Double? = nil
}

enum PlotType {
    case line
    case parametric
    case vector
    case scatter
    case area
}

// MARK: - CSV Data Model

// Represents the entire state of an imported CSV file.
struct CSVData: Identifiable {
    let id = UUID()
    let fileName: String
    let headers: [String]
    let grid: [[String]] // A 2D array of the raw string data from the file.
}


// MARK: - UI and Help Models

struct BuiltinFunction: Identifiable, Hashable {
    let id = UUID()
    let name: String
    let signature: String
    let description: String
}

struct PhysicalConstant: Identifiable, Hashable {
    let id = UUID()
    let symbol: String
    let name: String
    let value: Double
}

struct MathSymbol: Identifiable {
    let id = UUID()
    let symbol: String
    let name: String
    let insertionText: String?

    init(symbol: String, name: String, insertionText: String? = nil) {
        self.symbol = symbol
        self.name = name
        self.insertionText = insertionText
    }
}

struct HelpTopic: Identifiable, Hashable {
    let id = UUID()
    let title: String
    let content: String
}

