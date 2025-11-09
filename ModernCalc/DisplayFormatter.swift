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

            let sign = (coeff < 0 && !parts.isEmpty) ? " - " : (parts.isEmpty ? (coeff < 0 ? "-" : "") : " + ")
            let absCoeff = abs(coeff)

            var coeffStr = ""
            // Only show coefficient if it's not 1 (unless it's the constant term)
            if abs(absCoeff - 1.0) > 1e-9 || i == 0 {
                coeffStr = formatScalarForLegend(absCoeff)
            }

            var term = ""
            if i > 0 {
                term = (i == 1) ? "x" : "x^\(i)"
            }

            if !coeffStr.isEmpty && !term.isEmpty {
                // Use multiplication symbol for clarity if needed, or implicit
                 if i > 0 && abs(absCoeff - 1.0) <= 1e-9 { // Coeff is 1, implicit mult
                     parts.append("\(sign)\(term)")
                 } else {
                     parts.append("\(sign)\(coeffStr)·\(term)") // Use · for explicit mult
                 }
            } else if !term.isEmpty {
                 // Term exists, coeff is 1 implicitly
                 parts.append("\(sign)\(term)")
            } else {
                 // Constant term
                 parts.append("\(sign)\(coeffStr)")
            }
        }
        if parts.isEmpty { return "y = 0" }

        // Handle the leading sign correctly if the first term wasn't added first
        let firstPart = parts.first!
        if firstPart.hasPrefix(" + ") {
            parts[0] = String(firstPart.dropFirst(3))
        } else if firstPart.hasPrefix(" - ") {
            // Keep the '-' sign but remove the spaces
            parts[0] = "-" + String(firstPart.dropFirst(3))
        }
         // If the first part starts directly with '-', leave it as is.

        return "y = \(parts.joined())"
    }


    private static func _formatNode(_ node: ExpressionNode, parentPrecedence: Int) -> String {
        switch node {
        case let numberNode as NumberNode:
            return formatScalarForLegend(numberNode.value)

        case let unitNode as UnitAndExponentNode:
             // For legend, just show the symbol directly without the dot
            var base = unitNode.unitSymbol
             // Handle special symbols
             if base == "Ohm" { base = "Ω" }

            if let exp = unitNode.exponent {
                // Ensure exponent doesn't get unnecessary parentheses if it's just a number
                let expStr = (exp is NumberNode) ? _formatNode(exp, parentPrecedence: 100) : "(\(_formatNode(exp, parentPrecedence: 0)))"
                 // Don't show ^1
                 if expStr == "1" { return base }
                return "\(base)^\(expStr)"
            }
            return base


        case let constantNode as ConstantNode:
            return constantNode.name

        case let funcCall as FunctionCallNode where funcCall.name == "if" && funcCall.arguments.count == 3:
             let cond = _formatNode(funcCall.arguments[0], parentPrecedence: 0)
             let trueVal = _formatNode(funcCall.arguments[1], parentPrecedence: 0)
             let falseVal = _formatNode(funcCall.arguments[2], parentPrecedence: 0)

             // Abbreviate long results within the if statement for legend clarity
             let maxLegendPartLength = 25
             let abbreviatedTrueVal = trueVal.count > maxLegendPartLength ? String(trueVal.prefix(maxLegendPartLength - 3)) + "..." : trueVal
             let abbreviatedFalseVal = falseVal.count > maxLegendPartLength ? String(falseVal.prefix(maxLegendPartLength - 3)) + "..." : falseVal

             return "if(\(cond), \(abbreviatedTrueVal), \(abbreviatedFalseVal))"

        case let funcCall as FunctionCallNode:
            let args = funcCall.arguments.map { _formatNode($0, parentPrecedence: 0) }.joined(separator: ", ")
            return "\(funcCall.name)(\(args))"

        case let unaryNode as UnaryOpNode:
            let childPrecedence = unaryPrecedence
            let childStr = _formatNode(unaryNode.child, parentPrecedence: childPrecedence)
            let result = "\(unaryNode.op.rawValue)\(childStr)"

            // Add parentheses if this unary op has lower precedence than its parent
            if childPrecedence < parentPrecedence {
                 return "(\(result))"
            }
            return result


        case let binaryNode as BinaryOpNode:
             let op = binaryNode.op.rawValue
             // Use × and ÷ for display in legends
             let displayOp: String
             switch op {
                 case "*": displayOp = "×"
                 case "/": displayOp = "÷"
                 case ":": displayOp = "÷" // Treat ':' like division for display
                 default: displayOp = op
             }

             let myPrecedence = infixPrecedence[op] ?? 0

             // Special case: Format implicit multiplication with units cleanly (e.g., "5 m" not "5 × m")
             if op == "*" {
                 if let unitNode = binaryNode.right as? UnitAndExponentNode {
                      let leftStr = _formatNode(binaryNode.left, parentPrecedence: myPrecedence)
                      // Format unit without the dot for legend
                      let rightStr = _formatNode(unitNode, parentPrecedence: 100) // High precedence for unit part
                      return "\(leftStr) \(rightStr)"
                 }
                 // Handle implicit multiplication like 2x or (a+b)x
                 let leftIsNumber = binaryNode.left is NumberNode
                 let rightIsIdentifier = binaryNode.right is ConstantNode && !(binaryNode.right as! ConstantNode).name.starts(with: ".")
                 let rightIsFunc = binaryNode.right is FunctionCallNode
                 let rightIsParen = binaryNode.right is BinaryOpNode // Crude check if right starts with '('

                 if leftIsNumber && (rightIsIdentifier || rightIsFunc || rightIsParen) {
                     let leftStr = _formatNode(binaryNode.left, parentPrecedence: myPrecedence)
                     let rightStr = _formatNode(binaryNode.right, parentPrecedence: myPrecedence) // Use same precedence to allow e.g. 2x^2
                     return "\(leftStr)\(rightStr)" // Implicit multiplication
                 }
             }


             // Adjust precedence for left/right children based on associativity
             let leftPrec = myPrecedence
             // Make right precedence slightly higher for left-associative ops to force parens if needed
             // Keep same precedence for right-associative power operator (^)
             let rightPrec = (op == "^") ? myPrecedence : myPrecedence + 1

             let leftStr = _formatNode(binaryNode.left, parentPrecedence: leftPrec)
             let rightStr = _formatNode(binaryNode.right, parentPrecedence: rightPrec)

             var result = "\(leftStr) \(displayOp) \(rightStr)"

             // Add parentheses around the whole expression if *its* precedence is lower than its parent's
             if myPrecedence < parentPrecedence {
                 result = "(\(result))"
             }
             return result


        default:
            // Fallback for any other node type not explicitly handled
             return node.description // Raw description from the node itself
        }
    }


    // MARK: - Value Formatting for History & Parsing

    // --- NEW: Helper function ---
    /// Checks if all dimension exponents are effectively integers within a small tolerance.
    private static func allDimensionsAreIntegers(_ dims: UnitDimension) -> Bool {
        // Allow empty dimensions
        if dims.isEmpty { return true }
        // Check if all non-zero exponents are close to an integer
        return dims.values.filter { abs($0) > 1e-15 }.allSatisfy { abs($0 - round($0)) < 1e-9 }
    }


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
             // --- MODIFIED: Use integer check before looking up preferred units ---
            if allDimensionsAreIntegers(u.dimensions) {
                if let preferredUnitSymbol = u.preferredDisplayUnit,
                   let preferredUnitDef = UnitStore.units[preferredUnitSymbol],
                   preferredUnitDef.dimensions == u.dimensions { // Check exact match
                    let convertedValue = u.value / preferredUnitDef.conversionFactor
                    return "\(formatScalar(convertedValue)) \(preferredUnitSymbol)"
                }

                if u.dimensions.isEmpty { return formatScalar(u.value) }

                if let compoundUnitString = UnitStore.commonCompoundUnits[u.dimensions] {
                    return "\(formatScalar(u.value)) \(compoundUnitString)"
                }

                if let bestUnit = findBestUnitFor(dimensions: u.dimensions) {
                    let convertedValue = u.value / bestUnit.conversionFactor
                    if bestUnit.dimensions.isEmpty && !["deg", "rad"].contains(bestUnit.symbol) {
                        return formatScalar(convertedValue) // Don't show unit for dimensionless conversions unless angle
                    }
                    return "\(formatScalar(convertedValue)) \(bestUnit.symbol)"
                }
            } else if u.dimensions.isEmpty {
                 return formatScalar(u.value)
            }
             // --- END MODIFICATION ---

            // Fallback for fractional exponents or no common unit found
            let valStr = formatScalar(u.value)
            let unitStr = formatDimensionsForHistory(u.dimensions) // Updated in Phase 3
            if unitStr.isEmpty { return valStr }
            return "\(valStr) \(unitStr)"

        case .complex(let c): return formatComplexForDisplay(c, with: settings)
        case .complexUnitValue(let cu):
            let complexStr = formatComplexForDisplay(cu.value, with: settings)
            if cu.dimensions.isEmpty {
                return complexStr
            }
            // Use formatForHistory recursively to get the unit string (now handles fractions)
            let unitValueForFormatting = UnitValue(value: 1.0, dimensions: cu.dimensions)
            let unitStr = formatForHistory(.unitValue(unitValueForFormatting), with: settings, angleMode: angleMode)
                .trimmingCharacters(in: .whitespacesAndNewlines)
                 // Remove the leading "1 " that formatForHistory adds for unit values
                .replacingOccurrences(of: "1 ", with: "")
                .trimmingCharacters(in: .whitespaces)


            if unitStr.isEmpty { return complexStr }
            // Add parentheses around the complex number if it has units
            return "(\(complexStr)) \(unitStr)"

        case .vector(let v):
             // --- ADD CHECK ---
            if allDimensionsAreIntegers(v.dimensions) {
                if let compoundUnitString = UnitStore.commonCompoundUnits[v.dimensions] {
                    return formatVectorForDisplay(v, with: settings, unitString: compoundUnitString)
                }
                if let bestUnit = findBestUnitFor(dimensions: v.dimensions) {
                    let convertedValues = v.values.map { $0 / bestUnit.conversionFactor }
                    let convertedVector = Vector(values: convertedValues, dimensions: v.dimensions)
                    return formatVectorForDisplay(convertedVector, with: settings, unitString: bestUnit.symbol)
                }
            }
            // --- END CHECK ---

            let unitStr = formatDimensionsForHistory(v.dimensions) // Updated in Phase 3
            return formatVectorForDisplay(v, with: settings, unitString: unitStr.isEmpty ? nil : unitStr)
        case .matrix(let m):
            // --- ADD CHECK ---
            if allDimensionsAreIntegers(m.dimensions) {
                if let compoundUnitString = UnitStore.commonCompoundUnits[m.dimensions] {
                    return formatMatrixForDisplay(m, with: settings, unitString: compoundUnitString)
                }
                if let bestUnit = findBestUnitFor(dimensions: m.dimensions) {
                    let convertedValues = m.values.map { $0 / bestUnit.conversionFactor }
                    let convertedMatrix = Matrix(values: convertedValues, rows: m.rows, columns: m.columns, dimensions: m.dimensions)
                    return formatMatrixForDisplay(convertedMatrix, with: settings, unitString: bestUnit.symbol)
                }
            }
            // --- END CHECK ---

            let unitStr = formatDimensionsForHistory(m.dimensions) // Updated in Phase 3
            return formatMatrixForDisplay(m, with: settings, unitString: unitStr.isEmpty ? nil : unitStr)
        case .tuple(let t):
            return t.map { formatForHistory($0, with: settings, angleMode: angleMode) }.joined(separator: " OR ")
        case .complexVector(let cv):
            let unitStr = formatDimensionsForHistory(cv.dimensions) // Updated in Phase 3
            return formatComplexVectorForDisplay(cv, with: settings, unitString: unitStr.isEmpty ? nil : unitStr)
        case .complexMatrix(let cm):
            let unitStr = formatDimensionsForHistory(cm.dimensions) // Updated in Phase 3
            return formatComplexMatrixForDisplay(cm, with: settings, unitString: unitStr.isEmpty ? nil : unitStr)
        case .functionDefinition: return "" // Don't show anything in history for definitions
        case .polar(let p): return formatPolarForDisplay(p, with: settings, angleMode: angleMode)
        case .regressionResult(let slope, let intercept):
            let slopeStr = formatForHistory(.unitValue(slope), with: settings, angleMode: angleMode)
            let interceptStr = formatForHistory(.unitValue(intercept), with: settings, angleMode: angleMode)
            return "Slope: \(slopeStr)\nIntercept: \(interceptStr)"
        case .polynomialFit(let polyCoeffs):
            return formatPolynomialWithUnitsForDisplay(polyCoeffs, with: settings)
        case .plot(let plotData): return "Plot: \(plotData.expression)" // Show expression used for plot
        case .triggerCSVImport: return "Importing CSV..." // Transient message
        case .constant(let s): return s // Display constants as their string value
        case .uncertain(let u):
            let val = formatScalar(u.value)
            let unc = formatScalar(u.totalUncertainty)
            let baseString: String
            // Show breakdown only if both random and systematic are non-negligible
            let showBreakdown = u.randomUncertainty > 1e-12 && u.systematicUncertainty > 1e-12
            if showBreakdown {
                let rand = formatScalar(u.randomUncertainty)
                let sys = formatScalar(u.systematicUncertainty)
                 // Use R: and S: for brevity
                baseString = "\(val) ± \(unc) (R: \(rand), S: \(sys))"
            } else {
                baseString = "\(val) ± \(unc)"
            }

            if u.dimensions.isEmpty {
                return baseString
            }

            // Recursive call to format unit part (now handles fractions)
            let unitValueForFormatting = UnitValue(value: 1.0, dimensions: u.dimensions)
            let unitStr = formatForHistory(.unitValue(unitValueForFormatting), with: settings, angleMode: angleMode).trimmingCharacters(in: .whitespacesAndNewlines).replacingOccurrences(of: "1 ", with: "")

             // Add parentheses if there are units
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
            // Show simplified output in history
            return "Eigenvalue Decomposition (V, D)"
        case .odeSolution(let time, let states):
             // Show simplified output in history
            return "ODE Solution (T, Y)"
        }
    }


    static func formatForParsing(_ value: MathValue, with settings: UserSettings) -> String {
        let argumentSeparator = settings.decimalSeparator == .period ? "," : "."
        switch value {
        case .dimensionless(let d): return formatScalarForParsing(d, with: settings)
        case .unitValue(let u):
            // --- MODIFIED: Use integer check before looking up preferred units ---
             if allDimensionsAreIntegers(u.dimensions) {
                if let preferredUnitSymbol = u.preferredDisplayUnit,
                   let preferredUnitDef = UnitStore.units[preferredUnitSymbol],
                   u.dimensions == preferredUnitDef.dimensions { // Check exact match

                    let convertedValue = u.value / preferredUnitDef.conversionFactor
                    let valStr = formatScalarForParsing(convertedValue, with: settings)
                    // Ensure unit symbol doesn't conflict with function names etc.
                    let safeSymbol = UnitStore.baseUnitSymbols.values.contains(preferredUnitSymbol) || preferredUnitSymbol.count > 2 ? preferredUnitSymbol : preferredUnitSymbol // Basic check
                    return "\(valStr).\(safeSymbol)"
                }

                if let bestUnit = findBestUnitFor(dimensions: u.dimensions) {
                    let convertedValue = u.value / bestUnit.conversionFactor
                    let valStr = formatScalarForParsing(convertedValue, with: settings)
                    if bestUnit.dimensions.isEmpty && !["deg", "rad"].contains(bestUnit.symbol) {
                         return valStr // Don't add unit if dimensionless (except angles)
                    }
                     let safeSymbol = UnitStore.baseUnitSymbols.values.contains(bestUnit.symbol) || bestUnit.symbol.count > 2 ? bestUnit.symbol : bestUnit.symbol // Basic check
                    return "\(valStr).\(safeSymbol)"
                }
            }
            // --- END MODIFICATION ---

            // Fallback for fractional exponents or no common unit
            let valStr = formatScalarForParsing(u.value, with: settings)
            let unitStr = formatDimensionsForParsing(u.dimensions) // Updated in Phase 3
            if unitStr.isEmpty { return valStr }
            // Use multiplication for clarity if unit string is complex
            if unitStr.contains("/") || unitStr.contains("^") || unitStr.contains("(") {
                 return "(\(valStr)) * (\(unitStr))"
            } else {
                 return "\(valStr)\(unitStr)" // Simple case like 5.m
            }

        case .complex(let c): return formatComplexForParsing(c, with: settings)
        case .complexUnitValue(let cu):
            let complexStr = formatComplexForParsing(cu.value, with: settings)
            let unitStr = formatDimensionsForParsing(cu.dimensions) // Updated in Phase 3
            if unitStr.isEmpty { return complexStr }
            // Always use explicit multiplication for complex units
            return "\(complexStr) * (\(unitStr))"

        case .vector(let v):
             // Always format as column vector for parsing consistency
            let content = "vector(\(v.values.map { formatScalarForParsing($0, with: settings) }.joined(separator: ";")))"
            let unitStr = formatDimensionsForParsing(v.dimensions) // Updated in Phase 3
            return unitStr.isEmpty ? content : "(\(content)) * (\(unitStr))" // Explicit multiplication
        case .matrix(let m):
            // Format elements row by row
             let rowsString = (0..<m.rows).map { r in
                (0..<m.columns).map { c in
                    formatScalarForParsing(m[r, c], with: settings)
                }.joined(separator: argumentSeparator) // Use correct separator for columns
            }.joined(separator: ";") // Semicolon separates rows

            let content = "matrix(\(rowsString))"
            let unitStr = formatDimensionsForParsing(m.dimensions) // Updated in Phase 3
            return unitStr.isEmpty ? content : "(\(content)) * (\(unitStr))" // Explicit multiplication

        case .tuple(let t):
             // Only return the first element for parsing, as tuples aren't directly parsable
             return t.first.map { formatForParsing($0, with: settings) } ?? ""
        case .complexVector(let cv):
            let content = "cvector(\(cv.values.map { formatComplexForParsing($0, with: settings) }.joined(separator: ";")))"
            let unitStr = formatDimensionsForParsing(cv.dimensions) // Updated in Phase 3
            return unitStr.isEmpty ? content : "(\(content)) * (\(unitStr))" // Explicit multiplication
        case .complexMatrix(let cm):
            let rowsString = (0..<cm.rows).map { r in
                (0..<cm.columns).map { c in
                    formatComplexForParsing(cm[r, c], with: settings)
                }.joined(separator: argumentSeparator)
            }.joined(separator: ";")
            let content = "cmatrix(\(rowsString))"
            let unitStr = formatDimensionsForParsing(cm.dimensions) // Updated in Phase 3
            return unitStr.isEmpty ? content : "(\(content)) * (\(unitStr))" // Explicit multiplication
        case .functionDefinition: return "" // Cannot parse back
        case .polar(let p): return formatPolarForParsing(p, with: settings) // Already handled
        case .regressionResult: return "" // Cannot parse back
        case .polynomialFit(let polyCoeffs):
            return formatPolynomialForParsing(polyCoeffs, with: settings) // Already handled
        case .plot(let plotData):
            // FIX: Use the raw expression string instead of calling a non-existent function
            return "autoplot(\(plotData.expression))"
        case .triggerCSVImport: return "importcsv()" // Can parse this back
        case .constant(let s): return s // Constants parse as themselves
        case .uncertain(let u):
             // Reconstruct the uncert() call as best as possible
            let valuePart: String
            let unitStr = formatDimensionsForParsing(u.dimensions) // Updated in Phase 3
            let valueStr = formatScalarForParsing(u.value, with: settings)

             if unitStr.isEmpty {
                 valuePart = valueStr
             } else if unitStr.contains("/") || unitStr.contains("^") || unitStr.contains("(") {
                 valuePart = "(\(valueStr)) * (\(unitStr))"
             } else {
                 valuePart = "\(valueStr)\(unitStr)"
             }

            var args = [valuePart]
            // Add named arguments if uncertainties are present
             // We'll primarily store the random and systematic components
             if u.randomUncertainty > 1e-12 {
                  let randValStr = formatScalarForParsing(u.randomUncertainty, with: settings)
                  let randPart = unitStr.isEmpty ? randValStr : (unitStr.contains("/") || unitStr.contains("^") || unitStr.contains("(") ? "(\(randValStr)) * (\(unitStr))" : "\(randValStr)\(unitStr)")
                  args.append("random: \(randPart)")
             }
             // For systematic, we store it directly as 'systematic' if present, rather than back-calculating accuracy/resolution
             if u.systematicUncertainty > 1e-12 {
                  let sysValStr = formatScalarForParsing(u.systematicUncertainty, with: settings)
                  let sysPart = unitStr.isEmpty ? sysValStr : (unitStr.contains("/") || unitStr.contains("^") || unitStr.contains("(") ? "(\(sysValStr)) * (\(unitStr))" : "\(sysValStr)\(unitStr)")
                  args.append("systematic: \(sysPart)")
             }

            return "uncert(\(args.joined(separator: ", ")))" // Use comma as separator


        case .roots(let roots):
            // Format roots as a vector for parsing
            if roots.isEmpty { return "vector()" } // Empty vector
            let elements = roots.map { formatForParsing($0, with: settings) }.joined(separator: ";")
            return "vector(\(elements))"
        case .eigenDecomposition: return "" // Cannot be parsed back easily
        case .odeSolution: return "" // Cannot be parsed back
        }
    }


    static func formatScalarForDisplay(_ value: Double, with settings: UserSettings) -> String {
        var formattedString: String
        switch settings.displayMode {
        case .auto:
            if value.truncatingRemainder(dividingBy: 1) == 0 && abs(value) < 1e15 {
                formattedString = String(format: "%.0f", value)
            } else {
                let absValue = abs(value)
                if absValue > 0 && (absValue < 1e-2 || absValue >= 1e15) {
                    // Format scientific, then potentially replace 'e' with '×10^' later if needed
                     formattedString = String(format: "%.4g", value)
                } else {
                    // Limit precision and remove trailing zeros
                    let maxPrecision = settings.livePreviewDecimalPlaces > 0 ? settings.livePreviewDecimalPlaces : 10
                    let tempFormatted = String(format: "%.\(maxPrecision)f", value)
                    // Regex to remove trailing zeros *after* the decimal point
                    if let regex = try? NSRegularExpression(pattern: "(\\.\\d*[^0])0+$") {
                        let nsString = tempFormatted as NSString
                        let range = NSRange(location: 0, length: nsString.length)
                        formattedString = regex.stringByReplacingMatches(in: tempFormatted, options: [], range: range, withTemplate: "$1")
                    } else {
                         formattedString = tempFormatted // Fallback
                    }
                     // Remove trailing decimal point if it resulted from zero removal
                     if formattedString.hasSuffix(".") {
                         formattedString = String(formattedString.dropLast())
                     }
                     // Handle case where value was 0.00... becoming "" or "."
                     if formattedString.isEmpty || formattedString == "." {
                         formattedString = "0"
                     }
                }
            }
        case .scientific:
             // Format scientific, then potentially replace 'e' later
             formattedString = String(format: "%.*e", settings.fixedDecimalPlaces, value)
        case .fixed:
            formattedString = String(format: "%.\(settings.fixedDecimalPlaces)f", value)
        }

         // Handle scientific notation formatting consistently here
         var finalString = formattedString
         if finalString.lowercased().contains("e") {
             let parts = finalString.lowercased().split(separator: "e")
             if parts.count == 2, let exponent = Int(parts[1]) {
                 var mantissa = String(parts[0])
                 // Apply decimal separator to mantissa
                 mantissa = settings.decimalSeparator == .comma ? mantissa.replacingOccurrences(of: ".", with: ",") : mantissa
                 finalString = "\(mantissa)×10^\(exponent)"
             }
         } else {
            // Apply decimal separator for non-scientific numbers
             finalString = settings.decimalSeparator == .comma ? finalString.replacingOccurrences(of: ".", with: ",") : finalString
         }

        return finalString
    }


    // MARK: - Private Formatting Helpers

    private static func findBestUnitFor(dimensions: UnitDimension) -> UnitDefinition? {
         // Ensure dimensions are integers before searching
         guard allDimensionsAreIntegers(dimensions) else { return nil }

        if dimensions.isEmpty {
            return nil
        }

        if dimensions == [.meter: 2.0] || dimensions == [.meter: 3.0] {
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

        // Prioritize preferred symbols
        for symbol in preferredSymbols {
            if let match = potentialMatches.first(where: { $0.symbol == symbol }) {
                return match
            }
        }

        // Fallback: return the first match if no preferred symbol found
        return potentialMatches.first
    }


    // --- UPDATED: formatDimensionsForHistory to handle Double exponents ---
    private static func formatDimensionsForHistory(_ dimensions: UnitDimension) -> String {
        let positiveDims = dimensions.filter { $0.value > 1e-15 }.sorted { $0.key.rawValue < $1.key.rawValue }
        let negativeDims = dimensions.filter { $0.value < -1e-15 }.sorted { $0.key.rawValue < $1.key.rawValue }

        let formatPart = { (dims: [(key: BaseUnit, value: Double)]) -> String in
            dims.map { (unit, exponent) -> String in
                let symbol = UnitStore.baseUnitSymbols[unit] ?? unit.rawValue
                let absExponent = abs(exponent)

                if abs(absExponent - 0.5) < 1e-9 {
                    return "sqrt(\(symbol))" // Use sqrt() for display
                } else if abs(absExponent - round(absExponent)) < 1e-9 {
                    let intExp = Int(round(absExponent))
                    return intExp == 1 ? symbol : "\(symbol)^\(intExp)"
                } else {
                    // Format other fractions or decimals directly
                    return "\(symbol)^\(String(format: "%.3g", absExponent))"
                }
            }.joined(separator: " ") // Use space as separator
        }

        let numerator = formatPart(positiveDims)
        let denominator = formatPart(negativeDims)

        if numerator.isEmpty && denominator.isEmpty {
            return "" // Dimensionless
        } else if !denominator.isEmpty {
            if numerator.isEmpty {
                // Handle cases like 1 / s^2 or 1 / sqrt(s)
                return "1/\(denominator)"
            } else {
                 // Handle cases like m / s^2 or m / sqrt(s)
                 // Add parentheses if denominator has spaces (multiple terms)
                 let finalDenominator = denominator.contains(" ") ? "(\(denominator))" : denominator
                 return "\(numerator)/\(finalDenominator)"
            }
        } else {
            return numerator // Only numerator exists
        }
    }


    // --- UPDATED: formatDimensionsForParsing to handle Double exponents ---
    private static func formatDimensionsForParsing(_ dimensions: UnitDimension) -> String {
        let positiveDims = dimensions.filter { $0.value > 1e-15 }.sorted { $0.key.rawValue < $1.key.rawValue }
        let negativeDims = dimensions.filter { $0.value < -1e-15 }.sorted { $0.key.rawValue < $1.key.rawValue }

        let formatPart = { (dims: [(key: BaseUnit, value: Double)]) -> String in
            return dims.map { (unit, exponent) -> String in
                let symbol = UnitStore.baseUnitSymbols[unit] ?? unit.rawValue
                let absExponent = abs(exponent)

                // For parsing, always use the ^ notation
                if abs(absExponent - 1.0) < 1e-9 {
                    return ".\(symbol)" // No exponent needed for 1
                } else if abs(absExponent - round(absExponent)) < 1e-9 {
                    // Integer exponent
                    return ".\(symbol)^\(Int(round(absExponent)))"
                } else {
                    // Fractional/decimal exponent
                    return ".\(symbol)^\(String(format: "%.4g", absExponent))" // Use enough precision
                }
            }.joined() // No spaces or operators between units for parsing
        }


        let numeratorPart = formatPart(positiveDims)
        let denominatorPart = formatPart(negativeDims)

        if denominatorPart.isEmpty {
            // Only numerator: .m or .m^2 or .kg.m.s^-2 becomes .kg.m/.s^2 later
            return numeratorPart.isEmpty ? "" : numeratorPart
        } else {
            // Numerator and denominator: build the fraction string
            let numeratorString = numeratorPart.isEmpty ? "1" : numeratorPart

            // Wrap numerator/denominator in parentheses ONLY if they consist of multiple units
            // (i.e., multiple dots '.') to ensure correct precedence during parsing.
            let finalNumerator = numeratorString.filter { $0 == "." }.count > 1 ? "(\(numeratorString))" : numeratorString
            let finalDenominator = denominatorPart.filter { $0 == "." }.count > 1 ? "(\(denominatorPart))" : denominatorPart


            return "\(finalNumerator)/\(finalDenominator)"
        }
    }


    private static func formatComplexForDisplay(_ value: Complex, with settings: UserSettings) -> String {
        let formatScalar = { (d: Double) in self.formatScalarForDisplay(d, with: settings) }
        // Use tolerance for zero checks
        let realIsZero = abs(value.real) < 1e-12
        let imagIsZero = abs(value.imaginary) < 1e-12

        if realIsZero && imagIsZero { return "0" }
        if imagIsZero { return formatScalar(value.real) }
        if realIsZero {
            if abs(value.imaginary - 1.0) < 1e-12 { return "i" } // Handle just 'i'
            if abs(value.imaginary - -1.0) < 1e-12 { return "-i" } // Handle just '-i'
            return "\(formatScalar(value.imaginary))i"
        }
        // Both parts non-zero
        let sign = value.imaginary < 0 ? "-" : "+"
        let imagAbsValue = abs(value.imaginary)
        let imagPart: String
        // Handle imaginary part being 1 or -1
        if abs(imagAbsValue - 1.0) < 1e-12 {
            imagPart = "i"
        } else {
            imagPart = "\(formatScalar(imagAbsValue))i"
        }
        return "\(formatScalar(value.real)) \(sign) \(imagPart)"
    }


    private static func formatPolarForDisplay(_ value: Complex, with settings: UserSettings, angleMode: AngleMode) -> String {
        let formatScalar = { (d: Double) in self.formatScalarForDisplay(d, with: settings) }
        let magnitude = value.abs(); let angle = value.argument()
        let angleDisplayValue = angleMode == .degrees ? angle * (180.0 / .pi) : angle
        // Use ° symbol for degrees
        let angleUnitString = angleMode == .degrees ? "°" : " rad"
        return "\(formatScalar(magnitude)) ∠ \(formatScalar(angleDisplayValue))\(angleUnitString)"
    }


    private static func formatVectorForDisplay(_ vector: Vector, with settings: UserSettings, unitString: String? = nil) -> String {
        let formatScalar = { (d: Double) in self.formatScalarForDisplay(d, with: settings) }
        let maxDisplayRows = 10

        var lines: [String]
        if vector.dimension <= maxDisplayRows {
             // Basic formatting, no alignment needed for single column
            lines = (0..<vector.dimension).map { "[ \(formatScalar(vector[$0])) ]" }
        } else {
            // Abbreviated display
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

        // Append unit string if provided
        if let unit = unitString, !unit.isEmpty {
             // Add unit on new line for multi-line vectors, same line for single line
            if lines.count > 1 {
                return "\(content)\n  \(unit)" // Indent unit slightly
            } else {
                return "\(content) \(unit)"
            }
        }

        return content
    }


    private static func formatMatrixForDisplay(_ matrix: Matrix, with settings: UserSettings, unitString: String? = nil) -> String {
        let formatScalar = { (d: Double) in self.formatScalarForDisplay(d, with: settings) }
        let maxDisplayRows = 10
        let maxDisplayCols = 8 // Add column limit

        if matrix.rows == 0 || matrix.columns == 0 { return "[]" } // Handle empty matrix

        // --- Calculate Column Widths ---
        var columnWidths = [Int](repeating: 0, count: min(matrix.columns, maxDisplayCols))
        let displayCols = min(matrix.columns, maxDisplayCols)
        let displayRows = min(matrix.rows, maxDisplayRows)

        // Determine widths needed for the visible part
        for c in 0..<displayCols {
            var maxWidth = 0
            for r in 0..<displayRows {
                 // Consider abbreviation dots if needed later
                 let isAbbreviatedRow = matrix.rows > maxDisplayRows && r >= 5
                 let actualRow = isAbbreviatedRow ? matrix.rows - (displayRows - r) : r

                 let formattedNumber = formatScalar(matrix[actualRow, c])
                if formattedNumber.count > maxWidth { maxWidth = formattedNumber.count }
            }
            // Account for potential ellipsis if columns are truncated
            if matrix.columns > maxDisplayCols && c == displayCols - 2 { // Penultimate visible column
                 maxWidth = max(maxWidth, "...".count)
            }
            columnWidths[c] = maxWidth
        }

        // --- Format Rows ---
        func formatRow(_ r: Int) -> String {
            let cells = (0..<displayCols).map { c -> String in
                 // Handle column abbreviation
                 if matrix.columns > maxDisplayCols && c == displayCols - 2 { // Penultimate column shows ellipsis
                     let padding = String(repeating: " ", count: columnWidths[c] - "...".count)
                     return padding + "..."
                 }
                 if matrix.columns > maxDisplayCols && c == displayCols - 1 { // Last column shows last actual column
                     let actualCol = matrix.columns - 1
                     let formattedNumber = formatScalar(matrix[r, actualCol])
                     let padding = String(repeating: " ", count: columnWidths[c] - formattedNumber.count)
                     return padding + formattedNumber
                 }

                 // Standard cell formatting
                 let formattedNumber = formatScalar(matrix[r, c])
                 let padding = String(repeating: " ", count: columnWidths[c] - formattedNumber.count)
                 return padding + formattedNumber
            }.joined(separator: "  ") // Two spaces between columns

            return "[ \(cells) ]"
        }

        // --- Assemble Lines with Row Abbreviation ---
        var lines: [String]
        if matrix.rows <= maxDisplayRows {
            lines = (0..<matrix.rows).map { r in formatRow(r) }
        } else {
            lines = []
            let headCount = 5
            let tailCount = 4
            for r in 0..<headCount {
                lines.append(formatRow(r))
            }
            // Add abbreviation line, aligning dots roughly
            let dotsPadding = String(repeating: " ", count: (columnWidths.reduce(0, +) + (displayCols - 1) * 2) / 2)
            lines.append("\(dotsPadding)... (\(matrix.rows - (headCount + tailCount)) more rows) ...")
            for r in (matrix.rows - tailCount)..<matrix.rows {
                lines.append(formatRow(r))
            }
        }

        let content = lines.joined(separator: "\n")

        // Append unit string if provided
        if let unit = unitString, !unit.isEmpty {
            if lines.count > 1 {
                return "\(content)\n  \(unit)" // Indent unit
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
            // Calculate max width for alignment
             var maxWidth = 0
             let formattedComplex = (0..<vector.dimension).map { formatComplex(vector[$0]) }
             formattedComplex.forEach { maxWidth = max(maxWidth, $0.count) }

             lines = formattedComplex.map {
                 let padding = String(repeating: " ", count: maxWidth - $0.count)
                 return "[ \(padding)\($0) ]"
             }
        } else {
            // Abbreviated display (no special alignment needed here)
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

        // Append unit string
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
        let maxDisplayCols = 8 // Add column limit

        if matrix.rows == 0 || matrix.columns == 0 { return "[]" }

        // --- Calculate Column Widths (for complex numbers) ---
        var columnWidths = [Int](repeating: 0, count: min(matrix.columns, maxDisplayCols))
        let displayCols = min(matrix.columns, maxDisplayCols)
        let displayRows = min(matrix.rows, maxDisplayRows)

        for c in 0..<displayCols {
            var maxWidth = 0
            for r in 0..<displayRows {
                 let isAbbreviatedRow = matrix.rows > maxDisplayRows && r >= 5
                 let actualRow = isAbbreviatedRow ? matrix.rows - (displayRows - r) : r
                 let actualCol = (matrix.columns > maxDisplayCols && c == displayCols - 1) ? matrix.columns - 1 : c

                 let formattedNumber = formatComplex(matrix[actualRow, actualCol])
                 // Add 2 for parentheses "( )" around each complex number for clarity
                 if formattedNumber.count + 2 > maxWidth { maxWidth = formattedNumber.count + 2 }
            }
             // Account for ellipsis
            if matrix.columns > maxDisplayCols && c == displayCols - 2 {
                 maxWidth = max(maxWidth, "...".count)
            }
            columnWidths[c] = maxWidth
        }


        // --- Format Rows ---
        func formatRow(_ r: Int) -> String {
            let cells = (0..<displayCols).map { c -> String in
                // Handle column abbreviation
                if matrix.columns > maxDisplayCols && c == displayCols - 2 {
                    let padding = String(repeating: " ", count: columnWidths[c] - "...".count)
                    return padding + "..."
                }
                 let actualCol = (matrix.columns > maxDisplayCols && c == displayCols - 1) ? matrix.columns - 1 : c

                // Standard cell formatting with parentheses
                let formattedNumber = "(\(formatComplex(matrix[r, actualCol])))"
                let padding = String(repeating: " ", count: columnWidths[c] - formattedNumber.count)
                return padding + formattedNumber
            }.joined(separator: "  ") // Two spaces

            return "[ \(cells) ]"
        }


        // --- Assemble Lines ---
        var lines: [String]
        if matrix.rows <= maxDisplayRows {
            lines = (0..<matrix.rows).map { r in formatRow(r) }
        } else {
            lines = []
            let headCount = 5
            let tailCount = 4
            for r in 0..<headCount {
                lines.append(formatRow(r))
            }
             let dotsPadding = String(repeating: " ", count: (columnWidths.reduce(0, +) + (displayCols - 1) * 2) / 2)
            lines.append("\(dotsPadding)... (\(matrix.rows - (headCount + tailCount)) more rows) ...")
            for r in (matrix.rows - tailCount)..<matrix.rows {
                lines.append(formatRow(r))
            }
        }


        let content = lines.joined(separator: "\n")

        // Append unit string
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
            // Skip zero coefficients unless it's the only term
            if abs(coeffUnitValue.value) < 1e-9 && coefficients.count > 1 { continue }

            let value = coeffUnitValue.value
            let absValue = abs(value)

            // Add sign
            if equation == "y =" { // First term
                equation += (value < 0 ? " -" : "")
            } else { // Subsequent terms
                equation += (value < 0 ? " -" : " +")
            }
            equation += " " // Space after sign or '='

            // Format coefficient value and unit separately
            let coeffDisplay = formatForHistory(.unitValue(UnitValue(value: absValue, dimensions: coeffUnitValue.dimensions)), with: settings, angleMode: .radians) // Use history formatter for coeff+unit

            let needsCoeffValue = abs(absValue - 1.0) > 1e-9 || i == 0
            let hasXTerm = i > 0

            // Split formatted coeff+unit (e.g., "1.23 kg/m")
             let parts = coeffDisplay.split(separator: " ", maxSplits: 1).map(String.init)
             let formattedCoeffValue = parts[0]
             let unitStr = parts.count > 1 ? parts[1] : ""
             let hasUnit = !unitStr.isEmpty


            // Determine if we need to display the numeric coefficient value
             if needsCoeffValue {
                 equation += formattedCoeffValue
             } else if !hasXTerm && hasUnit && abs(absValue - 1.0) < 1e-9 {
                  // Special case: Constant term is 1 with a unit (e.g., y = 1 m)
                  equation += formattedCoeffValue
             } else if !needsCoeffValue && !hasXTerm && !hasUnit && abs(absValue - 1.0) < 1e-9 {
                  // Special case: Constant term is 1 dimensionless (y = 1)
                  equation += formattedCoeffValue
             }
             // If coefficient is 1 and it's not the constant term, we omit '1'


            // Add unit string with parentheses if needed
            if hasUnit {
                if needsCoeffValue { equation += " " } // Space between value and unit
                // Add parentheses if unit contains spaces, /, or ^
                if unitStr.contains(" ") || unitStr.contains("/") || unitStr.contains("^") || unitStr.contains("sqrt") {
                    equation += "(\(unitStr))"
                } else {
                    equation += unitStr
                }
            }

            // Add x term and exponent
            if hasXTerm {
                if needsCoeffValue || hasUnit { equation += "·" } // Multiplication symbol
                equation += "x"
            }
            if i > 1 {
                equation += "^\(i)"
            }
            // Handle constant term 0
             if !needsCoeffValue && !hasUnit && !hasXTerm && abs(absValue) < 1e-9 && coefficients.count == 1 {
                  equation += formattedCoeffValue // Show "0" if y = 0
             }
        }
        // If all coefficients were zero
        if equation == "y =" {
            equation = "y = 0"
        }
        return equation
    }



    private static func formatPolynomialForParsing(_ polyCoeffs: PolynomialCoefficients, with settings: UserSettings) -> String {
        var equationParts: [String] = []
        let coefficients = polyCoeffs.coefficients

        for (i, coeffUnitValue) in coefficients.enumerated() {
             // Skip zero coefficients entirely for parsing string
            if abs(coeffUnitValue.value) < 1e-9 {
                continue
            }

            // Format coefficient with its unit for parsing
            let coeffString = formatForParsing(.unitValue(coeffUnitValue), with: settings)

            let term: String
            if i == 0 {
                 // Constant term
                term = coeffString
            } else {
                // Determine if coefficient needs parentheses
                 // (e.g., if it contains operators or is complex like "(1+2i)*(m/s)")
                 let coeffNeedsParen = coeffString.contains(where: { "+-*/^()".contains($0) }) || coeffString.starts(with: "(")

                let finalCoeffString: String
                 // Omit coefficient if it's exactly "1" and not the constant term
                 if coeffString == "1" && i > 0 {
                     finalCoeffString = "" // Implicit 1
                 } else if coeffString == "-1" && i > 0 {
                     finalCoeffString = "-" // Just the negative sign
                 } else {
                     finalCoeffString = coeffNeedsParen ? "(\(coeffString))" : coeffString
                 }


                 // Add multiplication symbol only if coefficient is shown explicitly
                 let multiplier = finalCoeffString.isEmpty || finalCoeffString == "-" ? "" : "*"

                if i == 1 {
                    term = "\(finalCoeffString)\(multiplier)x"
                } else {
                    term = "\(finalCoeffString)\(multiplier)x^\(i)"
                }
            }
            equationParts.append(term)
        }

        // Join parts, reversing order, handle signs carefully
         if equationParts.isEmpty { return "0" } // Handle y = 0 case
         let reversedParts = equationParts.reversed()
         var equation = reversedParts.first!
         for part in reversedParts.dropFirst() {
             if part.starts(with: "-") {
                 equation += " - \(part.dropFirst())" // " - " followed by positive part
             } else {
                 equation += " + \(part)"
             }
         }

         // Wrap the whole polynomial in parentheses if it contains + or - for safety
         if equation.contains("+") || equation.contains(" - ") {
             return "(\(equation))"
         } else {
             return equation
         }
    }


    static func formatScalarForParsing(_ value: Double, with settings: UserSettings) -> String {
        // Use high precision, then rely on Double parsing robustness
        let precision = 17 // Sufficient for Double precision
        var formattedString = String(format: "%.\(precision)g", value)

        // Ensure scientific notation uses 'e' and has '+' for positive exponent
        if formattedString.lowercased().contains("e") {
            var parts = formattedString.lowercased().components(separatedBy: "e")
            if parts.count == 2 {
                if let exp = Int(parts[1]) {
                    parts[1] = (exp >= 0 ? "+" : "") + String(exp)
                    formattedString = parts.joined(separator: "e")
                }
            }
        }

        // Replace decimal separator *last*
        return formattedString.replacingOccurrences(of: ".", with: settings.decimalSeparator.rawValue)
    }


    private static func formatComplexForParsing(_ value: Complex, with settings: UserSettings) -> String {
        // Format real and imaginary parts for parsing
        let realStr = formatScalarForParsing(value.real, with: settings)
        let imagStr = formatScalarForParsing(value.imaginary, with: settings)

        // Handle zero cases simply
         if value.imaginary == 0 { return realStr }
         if value.real == 0 { return "\(imagStr)i" } // Assume imagStr includes sign if negative

        // Build the string: (real +/- imag*i)
        let sign = value.imaginary < 0 ? "" : "+" // Imaginary part includes its own sign
        // Always wrap in parentheses for parsing safety
        return "(\(realStr)\(sign)\(imagStr)i)"
    }


    private static func formatPolarForParsing(_ value: Complex, with settings: UserSettings) -> String {
        // Use ∠ symbol for parsing polar form
        let magnitude = value.abs()
        // Always use degrees for parsing polar form for consistency
        let angleDegrees = value.argument() * (180.0 / .pi)
        return "\(formatScalarForParsing(magnitude, with: settings))∠\(formatScalarForParsing(angleDegrees, with: settings))"
    }
}
