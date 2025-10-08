import Foundation

/// A utility struct for formatting mathematical expressions and values for display.
struct DisplayFormatter {

    // MARK: - Operator Precedence for Legend Formatting

    private static let infixPrecedence: [String: Int] = [
        "||": 1, "&&": 2,
        "==": 3, "!=": 3, ">": 3, "<": 3, ">=": 3, "<=": 3,
        "in": 4, "±": 4,
        "+": 5, "-": 5,
        "*": 6, "/": 6, "∠": 6, ".*": 6, "./": 6, ":": 6, // Treat ':' like division
        ".=@": 7, ".+@": 7, ".-@": 7, ".*@": 7, "./@": 7,
        "^": 8
    ]

    private static let unaryPrecedence = 9

    // MARK: - Legend & Node Formatting
    
    /// Formats a double into a string with a limited number of significant figures for legends.
    private static func formatScalarForLegend(_ value: Double) -> String {
        // Use up to 4 significant digits for a cleaner look in the equation.
        let tempFormatted = String(format: "%.4g", value)
        
        // This regex helps remove trailing zeros (e.g., "2.500" -> "2.5").
        if let regex = try? NSRegularExpression(pattern: "\\.?0+$") {
            let nsString = tempFormatted as NSString
            let range = NSRange(location: 0, length: nsString.length)
            let modString = regex.stringByReplacingMatches(in: tempFormatted, options: [], range: range, withTemplate: "")
            if modString.hasSuffix(".") {
                return String(modString.dropLast())
            }
            return modString.isEmpty ? "0" : modString
        }
        return tempFormatted
    }

    /// Formats an abstract syntax tree node into a human-readable string, suitable for legends.
    /// This method aims to reduce unnecessary parentheses and simplify unit notation.
    static func formatNodeForLegend(node: ExpressionNode) -> String {
        return _formatNode(node, parentPrecedence: 0)
    }

    /// Formats the coefficients of a polynomial into a readable equation string.
    static func formatPolynomialEquation(coeffs: Vector) -> String {
        var parts: [String] = []
        for i in (0..<coeffs.dimension).reversed() {
            let coeff = coeffs[i]
            if abs(coeff) < 1e-9 { continue }

            let sign = (coeff < 0 && !parts.isEmpty) ? " - " : (parts.isEmpty ? "" : " + ")
            let absCoeff = abs(coeff)
            
            var coeffStr = ""
            if abs(absCoeff - 1.0) > 1e-9 || i == 0 {
                coeffStr = formatScalarForLegend(absCoeff)
            }

            var term = ""
            if i > 0 {
                term = (i == 1) ? "x" : "x^\(i)"
            }
            
            if !coeffStr.isEmpty && !term.isEmpty {
                parts.append("\(sign)\(coeffStr)·\(term)")
            } else if !term.isEmpty {
                 parts.append("\(sign)\(term)")
            } else {
                 parts.append("\(sign)\(coeffStr)")
            }
        }
        if parts.isEmpty { return "y = 0" }
        // Handle the leading sign correctly
        let firstPart = parts.first!
        if firstPart.hasPrefix(" + ") {
            parts[0] = String(firstPart.dropFirst(3))
        } else if firstPart.hasPrefix(" - ") {
            parts[0] = "-" + String(firstPart.dropFirst(3))
        }
        
        return "y = \(parts.joined())"
    }
    
    private static func _formatNode(_ node: ExpressionNode, parentPrecedence: Int) -> String {
        switch node {
        case let numberNode as NumberNode:
            return formatScalarForLegend(numberNode.value)

        case let unitNode as UnitAndExponentNode:
            if let exp = unitNode.exponent {
                // Ensure exponent doesn't get unnecessary parentheses if it's just a number
                let expStr = (exp is NumberNode) ? _formatNode(exp, parentPrecedence: 100) : "(\(_formatNode(exp, parentPrecedence: 0)))"
                return "\(unitNode.unitSymbol)^\(expStr)"
            }
            return unitNode.unitSymbol
            
        case let constantNode as ConstantNode:
            return constantNode.name
            
        case let funcCall as FunctionCallNode where funcCall.name == "if" && funcCall.arguments.count == 3:
             let cond = _formatNode(funcCall.arguments[0], parentPrecedence: 0)
             let trueVal = _formatNode(funcCall.arguments[1], parentPrecedence: 0)
             let falseVal = _formatNode(funcCall.arguments[2], parentPrecedence: 0)
             
             let maxLegendPartLength = 25
             let abbreviatedTrueVal = trueVal.count > maxLegendPartLength ? String(trueVal.prefix(maxLegendPartLength - 3)) + "..." : trueVal
             let abbreviatedFalseVal = falseVal.count > maxLegendPartLength ? String(falseVal.prefix(maxLegendPartLength - 3)) + "..." : falseVal

             return "if(\(cond), \(abbreviatedTrueVal), \(abbreviatedFalseVal))"

        case let funcCall as FunctionCallNode:
            let args = funcCall.arguments.map { _formatNode($0, parentPrecedence: 0) }.joined(separator: ", ")
            return "\(funcCall.name)(\(args))"

        case let unaryNode as UnaryOpNode:
            let childStr = _formatNode(unaryNode.child, parentPrecedence: unaryPrecedence)
            let result = "\(unaryNode.op.rawValue)\(childStr)"
            return result
            
        case let binaryNode as BinaryOpNode:
            // Special case for implicit multiplication with a unit, format as "value unit"
            if binaryNode.op.rawValue == "*" && (binaryNode.right is UnitAndExponentNode) {
                 let leftStr = _formatNode(binaryNode.left, parentPrecedence: infixPrecedence["*"]!)
                 let rightStr = _formatNode(binaryNode.right, parentPrecedence: 100)
                 return "\(leftStr) \(rightStr)"
            }
            
            let op = binaryNode.op.rawValue
            // Replace ':' with '/' for display
            let displayOp = op == ":" ? "/" : op
            
            let myPrecedence = infixPrecedence[op] ?? 0
            
            // Adjust precedence for left-associativity. For right-associative ops like '^', the right child can have same precedence.
            let rightPrecedence = myPrecedence + (op == "^" ? 0 : 1)
            let leftStr = _formatNode(binaryNode.left, parentPrecedence: myPrecedence)
            let rightStr = _formatNode(binaryNode.right, parentPrecedence: rightPrecedence)
            
            var result = "\(leftStr) \(displayOp) \(rightStr)"
            
            if myPrecedence < parentPrecedence {
                result = "(\(result))"
            }
            return result
            
        default:
            // Fallback for any other node type not explicitly handled, with basic cleanup
            return node.description
                .replacingOccurrences(of: ".", with: "")
                .replacingOccurrences(of: " : ", with: " / ")
        }
    }
    
    // MARK: - Value Formatting for History & Parsing
    
    static func formatRootsForDisplay(_ roots: [MathValue], with settings: UserSettings, angleMode: AngleMode) -> [String] {
        let formattingSettings: UserSettings
        if settings.enableSolverRounding {
            let tempSettings = settings.makeTemporaryCopy()
            tempSettings.displayMode = .fixed
            tempSettings.fixedDecimalPlaces = settings.solverDecimalPlaces
            formattingSettings = tempSettings
        } else {
            formattingSettings = settings
        }
        return roots.map { formatForHistory($0, with: formattingSettings, angleMode: angleMode) }
    }
    
    static func formatForHistory(_ value: MathValue, with settings: UserSettings, angleMode: AngleMode) -> String {
        func formatScalar(_ scalar: Double) -> String {
            return self.formatScalarForDisplay(scalar, with: settings)
        }
        
        switch value {
        case .dimensionless(let d): return formatScalar(d)
        case .unitValue(let u):
            if let preferredUnitSymbol = u.preferredDisplayUnit,
               let preferredUnitDef = UnitStore.units[preferredUnitSymbol] {
                let convertedValue = u.value / preferredUnitDef.conversionFactor
                return "\(formatScalar(convertedValue)) \(preferredUnitSymbol)"
            }

            if u.dimensions.isEmpty {
                return formatScalar(u.value)
            }
            
            if let compoundUnitString = UnitStore.commonCompoundUnits[u.dimensions] {
                return "\(formatScalar(u.value)) \(compoundUnitString)"
            }
            
            if let bestUnit = findBestUnitFor(dimensions: u.dimensions) {
                let convertedValue = u.value / bestUnit.conversionFactor
                if bestUnit.dimensions.isEmpty && !["deg", "rad"].contains(bestUnit.symbol) {
                     return formatScalar(convertedValue)
                }
                return "\(formatScalar(convertedValue)) \(bestUnit.symbol)"
            }
            
            let valStr = formatScalar(u.value)
            let unitStr = formatDimensionsForHistory(u.dimensions)
            if unitStr.isEmpty { return valStr }
            return "\(valStr) \(unitStr)"
        case .complex(let c): return formatComplexForDisplay(c, with: settings)
        case .complexUnitValue(let cu):
            let complexStr = formatComplexForDisplay(cu.value, with: settings)
            if cu.dimensions.isEmpty {
                return complexStr
            }
            let unitValueForFormatting = UnitValue(value: 1.0, dimensions: cu.dimensions)
            let unitStr = formatForHistory(.unitValue(unitValueForFormatting), with: settings, angleMode: angleMode)
                .trimmingCharacters(in: .whitespacesAndNewlines)
                .replacingOccurrences(of: "1 ", with: "")
                .trimmingCharacters(in: .whitespaces)

            if unitStr.isEmpty { return complexStr }
            return "(\(complexStr)) \(unitStr)"
        case .vector(let v):
            if let compoundUnitString = UnitStore.commonCompoundUnits[v.dimensions] {
                return formatVectorForDisplay(v, with: settings, unitString: compoundUnitString)
            }
            if let bestUnit = findBestUnitFor(dimensions: v.dimensions) {
                let convertedValues = v.values.map { $0 / bestUnit.conversionFactor }
                let convertedVector = Vector(values: convertedValues, dimensions: v.dimensions)
                return formatVectorForDisplay(convertedVector, with: settings, unitString: bestUnit.symbol)
            }
            
            let unitStr = formatDimensionsForHistory(v.dimensions)
            return formatVectorForDisplay(v, with: settings, unitString: unitStr.isEmpty ? nil : unitStr)
        case .matrix(let m):
            if let compoundUnitString = UnitStore.commonCompoundUnits[m.dimensions] {
                return formatMatrixForDisplay(m, with: settings, unitString: compoundUnitString)
            }
            if let bestUnit = findBestUnitFor(dimensions: m.dimensions) {
                let convertedValues = m.values.map { $0 / bestUnit.conversionFactor }
                let convertedMatrix = Matrix(values: convertedValues, rows: m.rows, columns: m.columns, dimensions: m.dimensions)
                return formatMatrixForDisplay(convertedMatrix, with: settings, unitString: bestUnit.symbol)
            }
            
            let unitStr = formatDimensionsForHistory(m.dimensions)
            return formatMatrixForDisplay(m, with: settings, unitString: unitStr.isEmpty ? nil : unitStr)
        case .tuple(let t):
            return t.map { formatForHistory($0, with: settings, angleMode: angleMode) }.joined(separator: " OR ")
        case .complexVector(let cv):
            let unitStr = formatDimensionsForHistory(cv.dimensions)
            return formatComplexVectorForDisplay(cv, with: settings, unitString: unitStr.isEmpty ? nil : unitStr)
        case .complexMatrix(let cm):
            let unitStr = formatDimensionsForHistory(cm.dimensions)
            return formatComplexMatrixForDisplay(cm, with: settings, unitString: unitStr.isEmpty ? nil : unitStr)
        case .functionDefinition: return ""
        case .polar(let p): return formatPolarForDisplay(p, with: settings, angleMode: angleMode)
        case .regressionResult(let slope, let intercept):
            let slopeStr = formatForHistory(.unitValue(slope), with: settings, angleMode: angleMode)
            let interceptStr = formatForHistory(.unitValue(intercept), with: settings, angleMode: angleMode)
            return "Slope: \(slopeStr)\nIntercept: \(interceptStr)"
        case .polynomialFit(let polyCoeffs):
            return formatPolynomialWithUnitsForDisplay(polyCoeffs, with: settings)
        case .plot(let plotData): return "Plot: \(plotData.expression)"
        case .triggerCSVImport: return "Importing CSV..."
        case .constant(let s): return s
        case .uncertain(let u):
            let val = formatScalar(u.value)
            let unc = formatScalar(u.totalUncertainty)
            let baseString: String
            if u.randomUncertainty > 0 && u.systematicUncertainty > 0 {
                let rand = formatScalar(u.randomUncertainty)
                let sys = formatScalar(u.systematicUncertainty)
                baseString = "\(val) ± \(unc) (R: \(rand), S: \(sys))"
            } else {
                baseString = "\(val) ± \(unc)"
            }
            
            if u.dimensions.isEmpty {
                return baseString
            }
            
            let unitValueForFormatting = UnitValue(value: 1.0, dimensions: u.dimensions)
            let unitStr = formatForHistory(.unitValue(unitValueForFormatting), with: settings, angleMode: angleMode).trimmingCharacters(in: .whitespacesAndNewlines).replacingOccurrences(of: "1 ", with: "")

            return "(\(baseString)) \(unitStr)"

        case .roots(let roots):
             if roots.isEmpty { return "No real roots found" }
            
            let formattedRoots = Self.formatRootsForDisplay(roots, with: settings, angleMode: angleMode)

            let maxDisplayRoots = 4
            if roots.count > maxDisplayRoots {
                let displayed = formattedRoots.prefix(maxDisplayRoots).joined(separator: ", ")
                return "{ \(displayed), ... (\(roots.count - maxDisplayRoots) more) }"
            } else {
                let rootsString = formattedRoots.joined(separator: ", ")
                return "{ \(rootsString) }"
            }
        case .eigenDecomposition(let eigenvectors, let eigenvalues):
            let vMatrix = formatForHistory(.matrix(eigenvectors), with: settings, angleMode: angleMode)
            let dMatrix = formatForHistory(.matrix(eigenvalues), with: settings, angleMode: angleMode)
            return "Eigenvectors (V):\n\(vMatrix)\n\nEigenvalues (D):\n\(dMatrix)"
        case .odeSolution(let time, let states):
            let tVector = formatForHistory(.vector(time), with: settings, angleMode: angleMode)
            let sMatrix = formatForHistory(.matrix(states), with: settings, angleMode: angleMode)
            return "Time Vector (T):\n\(tVector)\n\nStates Matrix (Y):\n\(sMatrix)"
        }
    }
    
    static func formatForParsing(_ value: MathValue, with settings: UserSettings) -> String {
        let argumentSeparator = settings.decimalSeparator == .period ? "," : "."
        switch value {
        case .dimensionless(let d): return formatScalarForParsing(d, with: settings)
        case .unitValue(let u):
            if let preferredUnitSymbol = u.preferredDisplayUnit,
               let preferredUnitDef = UnitStore.units[preferredUnitSymbol],
               u.dimensions == preferredUnitDef.dimensions {
                
                let convertedValue = u.value / preferredUnitDef.conversionFactor
                let valStr = formatScalarForParsing(convertedValue, with: settings)
                return "\(valStr).\(preferredUnitSymbol)"
            }

            if let bestUnit = findBestUnitFor(dimensions: u.dimensions) {
                let convertedValue = u.value / bestUnit.conversionFactor
                let valStr = formatScalarForParsing(convertedValue, with: settings)
                if bestUnit.dimensions.isEmpty && !["deg", "rad"].contains(bestUnit.symbol) {
                     return valStr
                }
                return "\(valStr).\(bestUnit.symbol)"
            }
            
            let valStr = formatScalarForParsing(u.value, with: settings)
            let unitStr = formatDimensionsForParsing(u.dimensions)
            if unitStr.isEmpty { return valStr }
            return "\(valStr)\(unitStr)"
        case .complex(let c): return formatComplexForParsing(c, with: settings)
        case .complexUnitValue(let cu):
            let complexStr = formatComplexForParsing(cu.value, with: settings)
            let unitStr = formatDimensionsForParsing(cu.dimensions)
            if unitStr.isEmpty { return complexStr }
            return "\(complexStr)\(unitStr)"
        case .vector(let v):
            let content = "vector(\(v.values.map { formatScalarForParsing($0, with: settings) }.joined(separator: ";")))"
            let unitStr = formatDimensionsForParsing(v.dimensions)
            return unitStr.isEmpty ? content : "(\(content)) * \(unitStr)"
        case .matrix(let m):
            let content = "matrix(\((0..<m.rows).map { r in (0..<m.columns).map { c in formatScalarForParsing(m[r, c], with: settings) }.joined(separator: argumentSeparator) }.joined(separator: ";")))"
            let unitStr = formatDimensionsForParsing(m.dimensions)
            return unitStr.isEmpty ? content : "(\(content)) * \(unitStr)"
        case .tuple(let t): return t.map { formatForParsing($0, with: settings) }.first ?? ""
        case .complexVector(let cv):
            let content = "cvector(\(cv.values.map { formatForParsing(.complex($0), with: settings) }.joined(separator: ";")))"
            let unitStr = formatDimensionsForParsing(cv.dimensions)
            return unitStr.isEmpty ? content : "(\(content)) * \(unitStr)"
        case .complexMatrix(let cm):
            let content = "cmatrix(\((0..<cm.rows).map { r in (0..<cm.columns).map { c in formatForParsing(.complex(cm[r, c]), with: settings) }.joined(separator: argumentSeparator) }.joined(separator: ";")))"
            let unitStr = formatDimensionsForParsing(cm.dimensions)
            return unitStr.isEmpty ? content : "(\(content)) * \(unitStr)"
        case .functionDefinition: return ""
        case .polar(let p): return formatPolarForParsing(p, with: settings)
        case .regressionResult: return ""
        case .polynomialFit(let polyCoeffs):
            return formatPolynomialForParsing(polyCoeffs, with: settings)
        case .plot(let plotData): return "autoplot(\(plotData.expression))"
        case .triggerCSVImport: return "importcsv()"
        case .constant(let s): return s
        case .uncertain(let u):
            let unitStr = formatDimensionsForParsing(u.dimensions)
            let valueStr = formatScalarForParsing(u.value, with: settings)
            var parts = [String]()

            if u.dimensions.isEmpty {
                parts.append(valueStr)
                if u.randomUncertainty > 0 {
                    parts.append("random:\(formatScalarForParsing(u.randomUncertainty, with: settings))")
                }
                if u.systematicUncertainty > 0 {
                    let accuracyEquiv = u.systematicUncertainty * sqrt(3.0)
                    parts.append("accuracy:\(formatScalarForParsing(accuracyEquiv, with: settings))")
                }
                return "uncert(\(parts.joined(separator: ", ")))"
            } else {
                parts.append("\(valueStr)\(unitStr)")
                if u.randomUncertainty > 0 {
                    parts.append("random:\(formatScalarForParsing(u.randomUncertainty, with: settings))\(unitStr)")
                }
                if u.systematicUncertainty > 0 {
                    let accuracyEquiv = u.systematicUncertainty * sqrt(3.0)
                    parts.append("accuracy:\(formatScalarForParsing(accuracyEquiv, with: settings))\(unitStr)")
                }
                 return "uncert(\(parts.joined(separator: ", ")))"
            }

        case .roots(let roots):
            if roots.isEmpty { return "" }
            return "vector(\(roots.map { formatForParsing($0, with: settings) }.joined(separator: ";")))"
        case .eigenDecomposition: return "" // Cannot be parsed back easily
        case .odeSolution: return "" // Cannot be parsed back
        }
    }

    static func formatScalarForDisplay(_ value: Double, with settings: UserSettings) -> String {
        let formattedString: String
        switch settings.displayMode {
        case .auto:
            if value.truncatingRemainder(dividingBy: 1) == 0 { formattedString = String(format: "%.0f", value) }
            else {
                let absValue = abs(value)
                if absValue > 0 && (absValue < 1e-4 || absValue >= 1e15) { formattedString = String(format: "%.4g", value) }
                else {
                    let tempFormatted = String(format: "%.10f", value)
                    if let regex = try? NSRegularExpression(pattern: "\\.?0+$") {
                        let nsString = tempFormatted as NSString
                        let range = NSRange(location: 0, length: nsString.length)
                        let modString = regex.stringByReplacingMatches(in: tempFormatted, options: [], range: range, withTemplate: "")
                        let finalString = modString.isEmpty ? "0" : modString
                        formattedString = finalString.hasSuffix(".") ? String(finalString.dropLast()) : finalString
                    } else { formattedString = tempFormatted }
                }
            }
        case .scientific: formattedString = String(format: "%.*e", settings.fixedDecimalPlaces, value)
        case .fixed: formattedString = String(format: "%.\(settings.fixedDecimalPlaces)f", value)
        }
        return settings.decimalSeparator == .comma ? formattedString.replacingOccurrences(of: ".", with: ",") : formattedString
    }
    
    // MARK: - Private Formatting Helpers
    
    private static func findBestUnitFor(dimensions: UnitDimension) -> UnitDefinition? {
        if dimensions.isEmpty {
            return nil
        }
        
        if dimensions == [.meter: 3] || dimensions == [.meter: 2] {
            return nil
        }
        
        let preferredSymbols = ["m", "s", "kg", "A", "K", "mol", "cd", "N", "J", "W", "Pa", "Hz", "C", "V", "Ohm", "F", "H", "T", "L", "eV", "cal", "bar", "g"]

        var potentialMatches: [UnitDefinition] = []
        for (_, unitDef) in UnitStore.units {
            if unitDef.dimensions == dimensions {
                potentialMatches.append(unitDef)
            }
        }

        if potentialMatches.isEmpty {
            return nil
        }

        for symbol in preferredSymbols {
            if let match = potentialMatches.first(where: { $0.symbol == symbol }) {
                return match
            }
        }
        
        return nil
    }
    
    private static func formatDimensionsForHistory(_ dimensions: UnitDimension) -> String {
        let positiveDims = dimensions.filter { $0.value > 0 }
        let negativeDims = dimensions.filter { $0.value < 0 }

        let sortedPositive = positiveDims.sorted { $0.key.rawValue < $1.key.rawValue }
        let sortedNegative = negativeDims.sorted { $0.key.rawValue < $1.key.rawValue }

        let formatPart = { (dims: [(key: BaseUnit, value: Int)]) -> String in
            dims.map { (unit, exponent) -> String in
                let symbol = UnitStore.units.first(where: { $0.value.dimensions == [unit: 1] && $0.value.conversionFactor == 1.0 })?.key ?? unit.rawValue
                return abs(exponent) == 1 ? "\(symbol)" : "\(symbol)^\(abs(exponent))"
            }.joined(separator: " ")
        }

        let numerator = formatPart(sortedPositive)
        let denominator = formatPart(sortedNegative)

        if numerator.isEmpty && denominator.isEmpty {
            return ""
        } else if !denominator.isEmpty {
            if numerator.isEmpty {
                return "1/\(denominator)"
            } else {
                return "\(numerator)/\(denominator)"
            }
        } else {
            return numerator
        }
    }
    
    private static func formatDimensionsForParsing(_ dimensions: UnitDimension) -> String {
        let positiveDims = dimensions.filter { $0.value > 0 }.sorted { $0.key.rawValue < $1.key.rawValue }
        let negativeDims = dimensions.filter { $0.value < 0 }.sorted { $0.key.rawValue < $1.key.rawValue }

        let formatPart = { (dims: [(key: BaseUnit, value: Int)]) -> String in
            return dims.map { (unit, exponent) -> String in
                let symbol = UnitStore.baseUnitSymbols[unit] ?? unit.rawValue
                let absExponent = abs(exponent)
                return ".\(symbol)\(absExponent == 1 ? "" : "^\(absExponent)")"
            }.joined()
        }

        let numeratorPart = formatPart(positiveDims)
        let denominatorPart = formatPart(negativeDims)

        if denominatorPart.isEmpty {
            return numeratorPart
        } else {
            let numeratorString = numeratorPart.isEmpty ? "1" : numeratorPart
            
            let finalNumerator = positiveDims.count > 1 ? "(\(numeratorPart))" : numeratorString
            
            let finalDenominator = "(\(denominatorPart))"
            
            return "\(finalNumerator)/\(finalDenominator)"
        }
    }
    
    private static func formatComplexForDisplay(_ value: Complex, with settings: UserSettings) -> String {
        let formatScalar = { (d: Double) in self.formatScalarForDisplay(d, with: settings) }
        if value.real != 0 && value.imaginary != 0 { return "\(formatScalar(value.real)) \(value.imaginary < 0 ? "-" : "+") \(formatScalar(abs(value.imaginary)))i" }
        else if value.real != 0 { return formatScalar(value.real) }
        else if value.imaginary != 0 { return "\(formatScalar(value.imaginary))i" }
        else { return "0" }
    }
    
    private static func formatPolarForDisplay(_ value: Complex, with settings: UserSettings, angleMode: AngleMode) -> String {
        let formatScalar = { (d: Double) in self.formatScalarForDisplay(d, with: settings) }
        let magnitude = value.abs(); let angle = value.argument()
        if angleMode == .degrees { let angleDegrees = angle * (180.0 / .pi); return "\(formatScalar(magnitude)) ∠ \(formatScalar(angleDegrees))°" }
        else { return "\(formatScalar(magnitude)) ∠ \(formatScalar(angle)) rad" }
    }
    
    private static func formatVectorForDisplay(_ vector: Vector, with settings: UserSettings, unitString: String? = nil) -> String {
        let formatScalar = { (d: Double) in self.formatScalarForDisplay(d, with: settings) }
        let maxDisplayRows = 10
        
        var lines: [String]
        if vector.dimension <= maxDisplayRows {
            lines = (0..<vector.dimension).map { "[ \(formatScalar(vector[$0])) ]" }
        } else {
            lines = []
            let headCount = 5
            let tailCount = 4
            for i in 0..<headCount {
                lines.append("[ \(formatScalar(vector[i])) ]")
            }
            lines.append("... (\(vector.dimension - (headCount + tailCount)) more items) ...")
            for i in (vector.dimension - tailCount)..<vector.dimension {
                lines.append("[ \(formatScalar(vector[i])) ]")
            }
        }
        
        let content = lines.joined(separator: "\n")

        if let unit = unitString, !unit.isEmpty {
            if lines.count > 1 {
                return "\(content)\n  \(unit)"
            } else {
                return "\(content) \(unit)"
            }
        }
        
        return content
    }
    
    private static func formatMatrixForDisplay(_ matrix: Matrix, with settings: UserSettings, unitString: String? = nil) -> String {
        let formatScalar = { (d: Double) in self.formatScalarForDisplay(d, with: settings) }
        let maxDisplayRows = 10
        if matrix.rows == 0 || matrix.columns == 0 { return "[]" }

        func formatRow(_ r: Int, columnWidths: [Int]) -> String {
            let rowContent = (0..<matrix.columns).map { c in
                let formattedNumber = formatScalar(matrix[r, c])
                let padding = String(repeating: " ", count: columnWidths[c] - formattedNumber.count)
                return padding + formattedNumber
            }.joined(separator: "  ")
            return "[ \(rowContent) ]"
        }

        var columnWidths = [Int](repeating: 0, count: matrix.columns)
        for c in 0..<matrix.columns {
            var maxWidth = 0
            for r in 0..<matrix.rows {
                let formattedNumber = formatScalar(matrix[r, c])
                if formattedNumber.count > maxWidth { maxWidth = formattedNumber.count }
            }
            columnWidths[c] = maxWidth
        }

        var lines: [String]
        if matrix.rows <= maxDisplayRows {
            lines = (0..<matrix.rows).map { r in formatRow(r, columnWidths: columnWidths) }
        } else {
            lines = []
            let headCount = 5
            let tailCount = 4
            for r in 0..<headCount {
                lines.append(formatRow(r, columnWidths: columnWidths))
            }
            lines.append("... (\(matrix.rows - (headCount + tailCount)) more rows) ...")
            for r in (matrix.rows - tailCount)..<matrix.rows {
                lines.append(formatRow(r, columnWidths: columnWidths))
            }
        }
        
        let content = lines.joined(separator: "\n")

        if let unit = unitString, !unit.isEmpty {
            if lines.count > 1 {
                return "\(content)\n  \(unit)"
            } else {
                return "\(content) \(unit)"
            }
        }
        
        return content
    }
    
    private static func formatComplexVectorForDisplay(_ vector: ComplexVector, with settings: UserSettings, unitString: String? = nil) -> String {
        let formatComplex = { (c: Complex) in self.formatComplexForDisplay(c, with: settings) }
        let maxDisplayRows = 10
        
        var lines: [String]
        if vector.dimension <= maxDisplayRows {
            lines = (0..<vector.dimension).map { "[ \(formatComplex(vector[$0])) ]" }
        } else {
            lines = []
            let headCount = 5
            let tailCount = 4
            for i in 0..<headCount {
                lines.append("[ \(formatComplex(vector[i])) ]")
            }
            lines.append("... (\(vector.dimension - (headCount + tailCount)) more items) ...")
            for i in (vector.dimension - tailCount)..<vector.dimension {
                lines.append("[ \(formatComplex(vector[i])) ]")
            }
        }

        let content = lines.joined(separator: "\n")

        if let unit = unitString, !unit.isEmpty {
            if lines.count > 1 {
                return "\(content)\n  \(unit)"
            } else {
                return "\(content) \(unit)"
            }
        }
        
        return content
    }
    
    private static func formatComplexMatrixForDisplay(_ matrix: ComplexMatrix, with settings: UserSettings, unitString: String? = nil) -> String {
        let formatComplex = { (c: Complex) in self.formatComplexForDisplay(c, with: settings) }
        let maxDisplayRows = 10
        if matrix.rows == 0 || matrix.columns == 0 { return "[]" }
        
        func formatRow(_ r: Int, columnWidths: [Int]) -> String {
            let rowContent = (0..<matrix.columns).map { c in
                let formattedNumber = "(\(formatComplex(matrix[r, c])))"
                let padding = String(repeating: " ", count: columnWidths[c] - formattedNumber.count)
                return padding + formattedNumber
            }.joined(separator: "  ")
            return "[ \(rowContent) ]"
        }

        var columnWidths = [Int](repeating: 0, count: matrix.columns)
        for c in 0..<matrix.columns {
            var maxWidth = 0
            for r in 0..<matrix.rows {
                let formattedNumber = "(\(formatComplex(matrix[r, c])))"
                if formattedNumber.count > maxWidth { maxWidth = formattedNumber.count }
            }
            columnWidths[c] = maxWidth
        }

        var lines: [String]
        if matrix.rows <= maxDisplayRows {
            lines = (0..<matrix.rows).map { r in formatRow(r, columnWidths: columnWidths) }
        } else {
            lines = []
            let headCount = 5
            let tailCount = 4
            for r in 0..<headCount {
                lines.append(formatRow(r, columnWidths: columnWidths))
            }
            lines.append("... (\(matrix.rows - (headCount + tailCount)) more rows) ...")
            for r in (matrix.rows - tailCount)..<matrix.rows {
                lines.append(formatRow(r, columnWidths: columnWidths))
            }
        }

        let content = lines.joined(separator: "\n")

        if let unit = unitString, !unit.isEmpty {
            if lines.count > 1 {
                return "\(content)\n  \(unit)"
            } else {
                return "\(content) \(unit)"
            }
        }
        
        return content
    }
    
    private static func formatPolynomialWithUnitsForDisplay(_ polyCoeffs: PolynomialCoefficients, with settings: UserSettings) -> String {
        var equation = "y ="
        let coefficients = polyCoeffs.coefficients
        
        for (i, coeffUnitValue) in coefficients.enumerated().reversed() {
            if abs(coeffUnitValue.value) < 1e-9 && coefficients.count > 1 { continue }
            
            let value = coeffUnitValue.value
            let absValue = abs(value)
            
            if equation == "y =" {
                equation += (value < 0 ? " -" : "")
            } else {
                equation += (value < 0 ? " -" : " +")
            }
            equation += " "

            let formattedCoeff = formatScalarForDisplay(absValue, with: settings)
            let dimensions = coeffUnitValue.dimensions
            var formattedUnit: String
            if dimensions.isEmpty {
                formattedUnit = ""
            } else if let compound = UnitStore.commonCompoundUnits[dimensions] {
                formattedUnit = compound
            } else if let bestUnit = findBestUnitFor(dimensions: dimensions) {
                formattedUnit = bestUnit.symbol
            } else {
                formattedUnit = formatDimensionsForHistory(dimensions)
            }
            
            let needsCoeff = abs(absValue - 1.0) > 1e-9 || i == 0
            if needsCoeff {
                equation += formattedCoeff
            }
            
            if !formattedUnit.isEmpty {
                if needsCoeff { equation += " " }
                equation += "(\(formattedUnit))"
            }
            
            if i > 0 {
                if needsCoeff || !formattedUnit.isEmpty { equation += " " }
                equation += "x"
            }
            if i > 1 {
                equation += "^\(i)"
            }
        }
        return equation
    }
    
    private static func formatPolynomialForParsing(_ polyCoeffs: PolynomialCoefficients, with settings: UserSettings) -> String {
        var equationParts: [String] = []
        let coefficients = polyCoeffs.coefficients

        for (i, coeffUnitValue) in coefficients.enumerated() {
            if abs(coeffUnitValue.value) < 1e-9 {
                continue
            }

            let coeffString = formatForParsing(.unitValue(coeffUnitValue), with: settings)

            let term: String
            if i == 0 {
                term = coeffString
            } else {
                let coeffNeedsParen = coeffString.contains(where: { "+-*/^".contains($0) })
                let finalCoeffString = coeffNeedsParen ? "(\(coeffString))" : coeffString

                if i == 1 {
                    term = "\(finalCoeffString)*x"
                } else {
                    term = "\(finalCoeffString)*x^\(i)"
                }
            }
            equationParts.append(term)
        }

        let equation = equationParts.reversed().joined(separator: " + ").replacingOccurrences(of: "+ -", with: "- ")
        return "(\(equation))"
    }
    
    static func formatScalarForParsing(_ value: Double, with settings: UserSettings) -> String {
        if value.truncatingRemainder(dividingBy: 1) == 0 && abs(value) < 1e15 {
            return String(format: "%.0f", value)
        }
        
        let formattedString = String(format: "%.14g", value)
        
        if formattedString.lowercased().contains("e") {
            let parts = formattedString.lowercased().components(separatedBy: "e")
            if parts.count == 2 {
                 let mantissa = parts[0].replacingOccurrences(of: ".", with: settings.decimalSeparator.rawValue)
                 return "(\(mantissa)*10^\(parts[1]))"
            }
        }
        
        return formattedString.replacingOccurrences(of: ".", with: settings.decimalSeparator.rawValue)
    }
    
    private static func formatComplexForParsing(_ value: Complex, with settings: UserSettings) -> String {
        let sign = value.imaginary < 0 ? "" : "+"; return "(\(formatScalarForParsing(value.real, with: settings))\(sign)\(formatScalarForParsing(value.imaginary, with: settings))i)"
    }
    
    private static func formatPolarForParsing(_ value: Complex, with settings: UserSettings) -> String {
        let magnitude = value.abs(); let angleDegrees = value.argument() * (180.0 / .pi); return "\(formatScalarForParsing(magnitude, with: settings))∠\(formatScalarForParsing(angleDegrees, with: settings))"
    }
}
