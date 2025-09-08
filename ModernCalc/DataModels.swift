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

struct PlotData: Identifiable, Hashable {
    let id = UUID()
    let expression: String
    let dataPoints: [DataPoint]
    let plotType: PlotType

    static func == (lhs: PlotData, rhs: PlotData) -> Bool {
        lhs.id == rhs.id
    }

    func hash(into hasher: inout Hasher) {
        hasher.combine(id)
    }
}

struct DataPoint: Identifiable {
    let id = UUID()
    let x: Double
    let y: Double
}

enum PlotType {
    case line
    case parametric
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
