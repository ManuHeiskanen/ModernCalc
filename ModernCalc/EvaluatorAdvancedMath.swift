import Foundation

/// This extension contains the evaluation logic for advanced mathematical operations,
/// including calculus (derivatives, integrals, gradients) and plotting.
extension Evaluator {

    // MARK: - Calculus Functions
    
    func calculateNthDerivative(bodyNode: ExpressionNode, varName: String, at a: Double, order n: Int, variables: inout [String: MathValue], functions: inout [String: FunctionDefinitionNode], angleMode: AngleMode) throws -> Double {
        if n == 1 {
            let (valPlus, _) = try evaluateWithTempVar(node: bodyNode, varName: varName, varValue: .dimensionless(a + h), variables: &variables, functions: &functions, angleMode: angleMode)
            let (valMinus, _) = try evaluateWithTempVar(node: bodyNode, varName: varName, varValue: .dimensionless(a - h), variables: &variables, functions: &functions, angleMode: angleMode)
            
            let scalarPlus = try valPlus.asScalar()
            let scalarMinus = try valMinus.asScalar()
            
            return (scalarPlus - scalarMinus) / (2 * h)
        }
        
        let f_prime_at_a_plus_h = try calculateNthDerivative(bodyNode: bodyNode, varName: varName, at: a + h, order: n - 1, variables: &variables, functions: &functions, angleMode: angleMode)
        let f_prime_at_a_minus_h = try calculateNthDerivative(bodyNode: bodyNode, varName: varName, at: a - h, order: n - 1, variables: &variables, functions: &functions, angleMode: angleMode)
        
        return (f_prime_at_a_plus_h - f_prime_at_a_minus_h) / (2 * h)
    }

    func evaluateGradFunction(_ node: FunctionCallNode, variables: inout [String: MathValue], functions: inout [String: FunctionDefinitionNode], angleMode: AngleMode) throws -> (result: MathValue, usedAngle: Bool) {
        guard node.arguments.count == 2 else { throw MathError.incorrectArgumentCount(function: "grad", expected: "2", found: node.arguments.count) }

        guard let funcNameNode = node.arguments[0] as? ConstantNode else { throw MathError.typeMismatch(expected: "User function name for gradient", found: node.arguments[0].description) }
        let funcName = funcNameNode.name

        guard let userFunction = functions[funcName] else { throw MathError.unknownFunction(name: funcName) }
        let varNames = userFunction.parameterNames
        guard !varNames.isEmpty else { throw MathError.unsupportedOperation(op: "grad", typeA: "function with no parameters", typeB: nil) }

        let (pointValue, pointUsedAngle) = try _evaluateSingle(node: node.arguments[1], variables: &variables, functions: &functions, angleMode: angleMode)
        guard case .vector(let pointVector) = pointValue else { throw MathError.typeMismatch(expected: "Vector for gradient point", found: pointValue.typeName) }
        guard pointVector.dimension == varNames.count else { throw MathError.dimensionMismatch(reason: "Point dimension (\(pointVector.dimension)) must match number of function variables (\(varNames.count)).") }

        let capturedVariables = variables
        let capturedFunctions = functions
        
        var partialDerivatives: [Double] = []
        var overallUsedAngle = pointUsedAngle
        
        for (i,_) in varNames.enumerated() {
            let pointForThisVar = pointVector.values[i]
            let f: (Double) throws -> (Double, Bool) = { x_perturbed in
                var tempVars = capturedVariables
                for (j, otherVarName) in varNames.enumerated() {
                    tempVars[otherVarName] = .dimensionless((j == i) ? x_perturbed : pointVector.values[j])
                }
                var tempFuncs = capturedFunctions
                let (result, usedAngle) = try self._evaluateSingle(node: userFunction.body, variables: &tempVars, functions: &tempFuncs, angleMode: angleMode)
                let scalarResult = try result.asScalar()
                return (scalarResult, usedAngle)
            }
            
            let (valPlus, usedAnglePlus) = try f(pointForThisVar + h)
            let (valMinus, usedAngleMinus) = try f(pointForThisVar - h)
            overallUsedAngle = overallUsedAngle || usedAnglePlus || usedAngleMinus
            
            let derivative = (valPlus - valMinus) / (2 * h)
            partialDerivatives.append(derivative)
        }
        return (.vector(Vector(values: partialDerivatives)), overallUsedAngle)
    }

    func adaptiveSimpson(f: (Double) throws -> Double, a: Double, b: Double, tolerance: Double) throws -> Double {
        let c = (a + b) / 2.0; let h = b - a
        let fa = try f(a); let fb = try f(b); let fc = try f(c)
        let s = (h / 6.0) * (fa + 4.0 * fc + fb)
        return try _adaptiveSimpsonRecursive(f: f, a: a, b: b, fa: fa, fb: fb, fc: fc, whole: s, tolerance: tolerance)
    }

    private func _adaptiveSimpsonRecursive(f: (Double) throws -> Double, a: Double, b: Double, fa: Double, fb: Double, fc: Double, whole: Double, tolerance: Double) throws -> Double {
        let c = (a + b) / 2.0; let d = (a + c) / 2.0; let e = (c + b) / 2.0
        let fd = try f(d); let fe = try f(e)
        let left = (c - a) / 6.0 * (fa + 4.0 * fd + fc)
        let right = (b - c) / 6.0 * (fc + 4.0 * fe + fb)
        if abs(left + right - whole) <= 15.0 * tolerance { return left + right + (left + right - whole) / 15.0 }
        let leftHalf = try _adaptiveSimpsonRecursive(f: f, a: a, b: c, fa: fa, fb: fc, fc: fd, whole: left, tolerance: tolerance / 2.0)
        let rightHalf = try _adaptiveSimpsonRecursive(f: f, a: c, b: b, fa: fc, fb: fb, fc: fe, whole: right, tolerance: tolerance / 2.0)
        return leftHalf + rightHalf
    }
    
    // MARK: - Plotting Functions
    
    func findUndeclaredVariables(in node: ExpressionNode, declaredVariables: Set<String>, declaredFunctions: Set<String>) -> Set<String> {
        var found = Set<String>()
        let knownConstants = Set(Evaluator.constants.keys).union(Set(Evaluator.siPrefixes.keys)).union(["i", "j", "k"])

        func traverse(node: ExpressionNode, localScope: Set<String>) {
            switch node {
            case let constantNode as ConstantNode:
                if !declaredVariables.contains(constantNode.name) && !knownConstants.contains(constantNode.name) && !localScope.contains(constantNode.name) {
                    found.insert(constantNode.name)
                }
            case let funcDefNode as FunctionDefinitionNode: traverse(node: funcDefNode.body, localScope: localScope.union(funcDefNode.parameterNames))
            case let integralNode as IntegralNode:
                let newLocalScope = localScope.union([integralNode.variable.name])
                traverse(node: integralNode.body, localScope: newLocalScope); traverse(node: integralNode.lowerBound, localScope: localScope); traverse(node: integralNode.upperBound, localScope: localScope)
            case let derivativeNode as DerivativeNode:
                var newLocalScope = localScope
                if let variable = derivativeNode.variable { newLocalScope.formUnion([variable.name]) }
                traverse(node: derivativeNode.body, localScope: newLocalScope); traverse(node: derivativeNode.point, localScope: localScope); traverse(node: derivativeNode.order, localScope: localScope)
            case let binaryNode as BinaryOpNode: traverse(node: binaryNode.left, localScope: localScope); traverse(node: binaryNode.right, localScope: localScope)
            case let unaryNode as UnaryOpNode: traverse(node: unaryNode.child, localScope: localScope)
            case let postfixNode as PostfixOpNode: traverse(node: postfixNode.child, localScope: localScope)
            case let functionCallNode as FunctionCallNode: functionCallNode.arguments.forEach { traverse(node: $0, localScope: localScope) }
            case let vectorNode as VectorNode: vectorNode.elements.forEach { traverse(node: $0, localScope: localScope) }
            case let matrixNode as MatrixNode: matrixNode.rows.flatMap { $0 }.forEach { traverse(node: $0, localScope: localScope) }
            case let cVectorNode as ComplexVectorNode: cVectorNode.elements.forEach { traverse(node: $0, localScope: localScope) }
            case let cMatrixNode as ComplexMatrixNode: cMatrixNode.rows.flatMap { $0 }.forEach { traverse(node: $0, localScope: localScope) }
            case let assignmentNode as AssignmentNode: traverse(node: assignmentNode.expression, localScope: localScope)
            case let primeNode as PrimeDerivativeNode: traverse(node: primeNode.argument, localScope: localScope)
            case let indexedOpNode as IndexedOperationNode: traverse(node: indexedOpNode.index, localScope: localScope); traverse(node: indexedOpNode.scalar, localScope: localScope)
            default: break
            }
        }
        traverse(node: node, localScope: Set())
        return found
    }

    func evaluateAutoplot(_ node: AutoplotNode, variables: inout [String: MathValue], functions: inout [String: FunctionDefinitionNode]) throws -> MathValue {
        let startTime = CFAbsoluteTimeGetCurrent()
        
        var evaluatedArgs: [MathValue] = []; var canBeVectors = true
        for expr in node.expressions {
            do { var tempVars = variables; let (val, _) = try _evaluateSingle(node: expr, variables: &tempVars, functions: &functions, angleMode: .radians); evaluatedArgs.append(val) }
            catch { canBeVectors = false; break }
        }

        if canBeVectors {
            let vectorsToPlot = evaluatedArgs.compactMap {
                if case .vector(let v) = $0, v.dimension == 2 { return v }
                return nil
            }
            if vectorsToPlot.count == evaluatedArgs.count && !evaluatedArgs.isEmpty {
                var allSeries: [PlotSeries] = []
                for vector in vectorsToPlot { allSeries.append(PlotSeries(name: "v = [\(vector[0]); \(vector[1])]", dataPoints: [DataPoint(x: vector[0], y: vector[1])])) }
                let duration = CFAbsoluteTimeGetCurrent() - startTime
                print("[BENCHMARK] Autoplot (vector) generation time: \(duration * 1000) ms")
                return .plot(PlotData(expression: node.description, series: allSeries, plotType: .vector, explicitYRange: nil, generationTime: duration, xAxisLabel: "X", yAxisLabel: "Y"))
            }
        }

        let numPoints = 200; let defaultRange = -10.0...10.0
        var isParametric = false
        if node.expressions.count == 2 { if node.expressions.contains(where: { $0.description.range(of: #"\bt\b"#, options: .regularExpression) != nil }) { isParametric = true } }
        
        let plotData: PlotData
        if isParametric {
            let xBody = node.expressions[0]; let yBody = node.expressions[1]; let varName = "t"
            
            // --- Dry run to determine units ---
            var tempVarsDryRun = variables
            tempVarsDryRun[varName] = .dimensionless(0)
            let (dryRunX, _) = try _evaluateSingle(node: xBody, variables: &tempVarsDryRun, functions: &functions, angleMode: .radians)
            let (dryRunY, _) = try _evaluateSingle(node: yBody, variables: &tempVarsDryRun, functions: &functions, angleMode: .radians)
            
            let xAxisDimension: UnitDimension = dryRunX.dimensionsIfUnitOrDimensionless
            let yAxisDimension: UnitDimension = dryRunY.dimensionsIfUnitOrDimensionless

            var results = Array<DataPoint?>(repeating: nil, count: numPoints)
            let step = (defaultRange.upperBound - defaultRange.lowerBound) / Double(numPoints - 1)
            let capturedVariables = variables; let capturedFunctions = functions

            DispatchQueue.concurrentPerform(iterations: numPoints) { i in
                let t = defaultRange.lowerBound + Double(i) * step
                var localVars = capturedVariables; var localFuncs = capturedFunctions
                do {
                    let (xValue, _) = try evaluateWithTempVar(node: xBody, varName: varName, varValue: .dimensionless(t), variables: &localVars, functions: &localFuncs, angleMode: .radians)
                    let (yValue, _) = try evaluateWithTempVar(node: yBody, varName: varName, varValue: .dimensionless(t), variables: &localVars, functions: &localFuncs, angleMode: .radians)
                    
                    guard xValue.dimensionsIfUnitOrDimensionless == xAxisDimension,
                          yValue.dimensionsIfUnitOrDimensionless == yAxisDimension else { return }
                    
                    let x_si: Double
                    switch xValue {
                        case .dimensionless(let d): x_si = d
                        case .unitValue(let u): x_si = u.value
                        case .uncertain(let u): x_si = u.value
                        default: return
                    }
                    let y_si: Double
                    switch yValue {
                        case .dimensionless(let d): y_si = d
                        case .unitValue(let u): y_si = u.value
                        case .uncertain(let u): y_si = u.value
                        default: return
                    }

                    if x_si.isFinite, y_si.isFinite {
                        results[i] = DataPoint(x: x_si, y: y_si)
                    }
                } catch { }
            }
            let dataPoints = results.compactMap { $0 }
            if dataPoints.isEmpty { throw MathError.plotError(reason: "Could not generate any data points for the parametric expressions.")}
            
            let seriesName = "(\(DisplayFormatter.formatNodeForLegend(node: xBody)), \(DisplayFormatter.formatNodeForLegend(node: yBody)))"; let plotSeries = PlotSeries(name: seriesName, dataPoints: dataPoints)
            let duration = CFAbsoluteTimeGetCurrent() - startTime
            print("[BENCHMARK] Autoplot (parametric) generation time: \(duration * 1000) ms")

            let xAxisLabel = formatDimensionsForAxis(xAxisDimension, defaultLabel: "x(t)")
            let yAxisLabel = formatDimensionsForAxis(yAxisDimension, defaultLabel: "y(t)")
            
            plotData = PlotData(expression: node.description, series: [plotSeries], plotType: .parametric, explicitYRange: nil, generationTime: duration, xAxisLabel: xAxisLabel, yAxisLabel: yAxisLabel, xAxisDimension: xAxisDimension, yAxisDimension: yAxisDimension)
        } else {
            let declaredVariables = Set(variables.keys); let declaredFunctions = Set(functions.keys); var undeclaredVars = Set<String>()
            for expr in node.expressions { undeclaredVars.formUnion(findUndeclaredVariables(in: expr, declaredVariables: declaredVariables, declaredFunctions: declaredFunctions)) }
            undeclaredVars.remove("t")
            
            let varName: String
            if undeclaredVars.count == 1 { varName = undeclaredVars.first! }
            else if undeclaredVars.isEmpty { varName = "x" }
            else { throw MathError.plotError(reason: "Multiple unknown variables found: [\(undeclaredVars.joined(separator: ", "))].") }

            var allSeries: [PlotSeries] = []
            var yAxisDimension: UnitDimension? = nil

            for body in node.expressions {
                if yAxisDimension == nil {
                     var tempVarsDryRun = variables
                     tempVarsDryRun[varName] = .dimensionless(0)
                     let (dryRunY, _) = try _evaluateSingle(node: body, variables: &tempVarsDryRun, functions: &functions, angleMode: .radians)
                     yAxisDimension = dryRunY.dimensionsIfUnitOrDimensionless
                }

                var results = Array<DataPoint?>(repeating: nil, count: numPoints)
                let step = (defaultRange.upperBound - defaultRange.lowerBound) / Double(numPoints - 1)
                let capturedVariables = variables; let capturedFunctions = functions

                DispatchQueue.concurrentPerform(iterations: numPoints) { i in
                    let x = defaultRange.lowerBound + Double(i) * step
                    var localVars = capturedVariables; var localFuncs = capturedFunctions
                    do {
                        let (yValue, _) = try evaluateWithTempVar(node: body, varName: varName, varValue: .dimensionless(x), variables: &localVars, functions: &localFuncs, angleMode: .radians)
                        
                        guard yValue.dimensionsIfUnitOrDimensionless == yAxisDimension else { return }

                        let y_si: Double
                        switch yValue {
                        case .dimensionless(let d):
                            y_si = d
                        case .unitValue(let u):
                            y_si = u.value
                        case .uncertain(let u):
                            y_si = u.value
                        default:
                            return
                        }

                        if y_si.isFinite {
                            results[i] = DataPoint(x: x, y: y_si)
                        }
                    } catch { }
                }
                let dataPoints = results.compactMap { $0 }
                if !dataPoints.isEmpty { allSeries.append(PlotSeries(name: DisplayFormatter.formatNodeForLegend(node: body), dataPoints: dataPoints)) }
            }
            if allSeries.isEmpty { throw MathError.plotError(reason: "Could not generate any data points for the expression(s).")}
            let duration = CFAbsoluteTimeGetCurrent() - startTime
            print("[BENCHMARK] Autoplot (function) generation time: \(duration * 1000) ms")

            let xAxisLabel = varName
            let yAxisLabel = formatDimensionsForAxis(yAxisDimension ?? [:], defaultLabel: "f(\(varName))")
            
            plotData = PlotData(expression: node.description, series: allSeries, plotType: .line, explicitYRange: nil, generationTime: duration, xAxisLabel: xAxisLabel, yAxisLabel: yAxisLabel, xAxisDimension: [:], yAxisDimension: yAxisDimension)
        }
        return .plot(plotData)
    }
    
    func evaluateScatterplot(_ node: ScatterplotNode, variables: inout [String: MathValue], functions: inout [String: FunctionDefinitionNode]) throws -> MathValue {
        // NOTE: This function remains unit-agnostic for now as vector data types do not intrinsically carry unit information.
        // It will produce a plot with dimensionless axes.
        let startTime = CFAbsoluteTimeGetCurrent()
        let evaluatedArgs = try node.arguments.map { try _evaluateSingle(node: $0, variables: &variables, functions: &functions, angleMode: .radians).result }
        
        let xVector: Vector
        let yVector: Vector
        var fitDegree: Double? = nil
        
        // --- Argument Parsing Logic ---
        switch evaluatedArgs.count {
        case 1:
            guard case .matrix(let matrix) = evaluatedArgs[0] else { throw MathError.typeMismatch(expected: "Matrix", found: evaluatedArgs[0].typeName) }
            guard matrix.columns == 2 else { throw MathError.dimensionMismatch(reason: "Scatterplot matrix must have exactly 2 columns.") }
            guard matrix.rows > 0 else { throw MathError.plotError(reason: "Cannot create a scatterplot from an empty matrix.") }
            var xValues: [Double] = []; var yValues: [Double] = []
            for r in 0..<matrix.rows { xValues.append(matrix[r, 0]); yValues.append(matrix[r, 1]) }
            xVector = Vector(values: xValues); yVector = Vector(values: yValues)

        case 2:
            guard case .vector(let vec1) = evaluatedArgs[0], case .vector(let vec2) = evaluatedArgs[1] else { throw MathError.typeMismatch(expected: "Two Vectors", found: "\(evaluatedArgs[0].typeName), \(evaluatedArgs[1].typeName)") }
            xVector = vec1; yVector = vec2

        case 3:
            guard case .vector(let vec1) = evaluatedArgs[0], case .vector(let vec2) = evaluatedArgs[1] else { throw MathError.typeMismatch(expected: "Vector, Vector, Scalar", found: "\(evaluatedArgs[0].typeName), \(evaluatedArgs[1].typeName), ...") }
            xVector = vec1; yVector = vec2
            fitDegree = try evaluatedArgs[2].asScalar()

        default:
            throw MathError.incorrectArgumentCount(function: "scatterplot", expected: "1 (Matrix), 2 (Vectors), or 3 (Vectors, Degree)", found: evaluatedArgs.count)
        }
        
        guard xVector.dimension == yVector.dimension else { throw MathError.dimensionMismatch(reason: "Vectors for scatterplot must have the same dimension.") }
        guard xVector.dimension > 0 else { throw MathError.plotError(reason: "Cannot create a scatterplot from empty vectors.") }

        let dataPoints = zip(xVector.values, yVector.values).map { DataPoint(x: $0, y: $1) }
        var allSeries = [PlotSeries(name: "Data Points", dataPoints: dataPoints)]

        // --- Fit Generation Logic ---
        if let degree = fitDegree {
            let coeffs = try performPolynomialFit(x: xVector, y: yVector, degree: degree)
            
            let minX = xVector.values.min()!
            let maxX = xVector.values.max()!
            let numFitPoints = 100
            let step = (maxX - minX) / Double(numFitPoints - 1)
            
            let fitPoints = (0..<numFitPoints).map { i -> DataPoint in
                let x = minX + Double(i) * step
                let y = (0...Int(degree)).reduce(0.0) { acc, power in
                    acc + coeffs[power] * pow(x, Double(power))
                }
                return DataPoint(x: x, y: y)
            }
            
            let fitName = (degree == 1) ? "Linear Fit" : "Poly Fit (deg=\(Int(degree)))"
            let equationString = DisplayFormatter.formatPolynomialEquation(coeffs: coeffs)
            allSeries.append(PlotSeries(name: fitName, dataPoints: fitPoints, equation: equationString))
        }
        
        let duration = CFAbsoluteTimeGetCurrent() - startTime
        print("[BENCHMARK] Scatterplot generation time: \(duration * 1000) ms")
        return .plot(PlotData(expression: node.description, series: allSeries, plotType: .scatter, explicitYRange: nil, generationTime: duration))
    }
    
    func evaluatePlot(_ node: PlotNode, variables: inout [String: MathValue], functions: inout [String: FunctionDefinitionNode]) throws -> MathValue {
        let startTime = CFAbsoluteTimeGetCurrent()
        
        // 1. Evaluate ranges, which can now have units.
        let (xMinVal, _) = try _evaluateSingle(node: node.xRange.0, variables: &variables, functions: &functions, angleMode: .radians)
        let (xMaxVal, _) = try _evaluateSingle(node: node.xRange.1, variables: &variables, functions: &functions, angleMode: .radians)
        
        // 2. Extract base values and determine X-axis unit dimension.
        let xMin: UnitValue
        let xMax: UnitValue
        switch (xMinVal, xMaxVal) {
            case (.unitValue(let u1), .unitValue(let u2)):
                guard u1.dimensions == u2.dimensions else { throw MathError.plotError(reason: "Plot range units must be compatible.") }
                xMin = u1; xMax = u2
            case (.dimensionless(let d1), .dimensionless(let d2)):
                xMin = .dimensionless(d1); xMax = .dimensionless(d2)
            case (.unitValue(let u1), .dimensionless(let d2)):
                guard u1.dimensions.isEmpty else { throw MathError.plotError(reason: "Plot range units must be compatible.") }
                xMin = u1; xMax = .dimensionless(d2)
            case (.dimensionless(let d1), .unitValue(let u2)):
                guard u2.dimensions.isEmpty else { throw MathError.plotError(reason: "Plot range units must be compatible.") }
                xMin = .dimensionless(d1); xMax = u2
            default:
                throw MathError.plotError(reason: "Incompatible plot range types.")
        }
        
        let xAxisDimension = xMin.dimensions
        guard xMin.value < xMax.value else { throw MathError.plotError(reason: "Plot range min must be less than max.") }
        
        var explicitYRange: (min: Double, max: Double)? = nil
        if let yRangeNodes = node.yRange {
             let (yMinVal, _) = try _evaluateSingle(node: yRangeNodes.0, variables: &variables, functions: &functions, angleMode: .radians)
             let (yMaxVal, _) = try _evaluateSingle(node: yRangeNodes.1, variables: &variables, functions: &functions, angleMode: .radians)
             let yMin = try yMinVal.asScalar()
             let yMax = try yMaxVal.asScalar()
             guard yMin < yMax else { throw MathError.plotError(reason: "Y-axis range min must be less than max.") }
            explicitYRange = (yMin, yMax)
        }

        let numPoints = 200; let varName = node.variable.name; let isParametric = node.expressions.count == 2 && varName == "t"

        let plotData: PlotData
        if isParametric {
            let xBody = node.expressions[0]; let yBody = node.expressions[1]
            
            // --- Dry run to determine units ---
            var tempVarsDryRun = variables
            tempVarsDryRun[varName] = .unitValue(xMin)
            let (dryRunX, _) = try _evaluateSingle(node: xBody, variables: &tempVarsDryRun, functions: &functions, angleMode: .radians)
            let (dryRunY, _) = try _evaluateSingle(node: yBody, variables: &tempVarsDryRun, functions: &functions, angleMode: .radians)
            
            let parametricXAxisDimension = dryRunX.dimensionsIfUnitOrDimensionless
            let parametricYAxisDimension = dryRunY.dimensionsIfUnitOrDimensionless

            var results = Array<DataPoint?>(repeating: nil, count: numPoints)
            let step = (xMax.value - xMin.value) / Double(numPoints - 1)
            let capturedVariables = variables; let capturedFunctions = functions
            
            DispatchQueue.concurrentPerform(iterations: numPoints) { i in
                let t_si = xMin.value + Double(i) * step
                let tVar = MathValue.unitValue(UnitValue(value: t_si, dimensions: xAxisDimension))
                var localVars = capturedVariables; var localFuncs = capturedFunctions
                do {
                    let (xValue, _) = try evaluateWithTempVar(node: xBody, varName: varName, varValue: tVar, variables: &localVars, functions: &localFuncs, angleMode: .radians)
                    let (yValue, _) = try evaluateWithTempVar(node: yBody, varName: varName, varValue: tVar, variables: &localVars, functions: &localFuncs, angleMode: .radians)
                    
                    guard xValue.dimensionsIfUnitOrDimensionless == parametricXAxisDimension,
                          yValue.dimensionsIfUnitOrDimensionless == parametricYAxisDimension else { return }
                    
                    let x_si: Double
                    switch xValue {
                        case .dimensionless(let d): x_si = d
                        case .unitValue(let u): x_si = u.value
                        case .uncertain(let u): x_si = u.value
                        default: return
                    }
                    let y_si: Double
                    switch yValue {
                        case .dimensionless(let d): y_si = d
                        case .unitValue(let u): y_si = u.value
                        case .uncertain(let u): y_si = u.value
                        default: return
                    }

                    if x_si.isFinite, y_si.isFinite {
                        results[i] = DataPoint(x: x_si, y: y_si)
                    }
                } catch {}
            }
            
            let dataPoints = results.compactMap { $0 }
            if dataPoints.isEmpty { throw MathError.plotError(reason: "Could not generate any data points for the parametric expressions.")}
            let seriesName = "(\(DisplayFormatter.formatNodeForLegend(node: xBody)), \(DisplayFormatter.formatNodeForLegend(node: yBody)))"; let plotSeries = PlotSeries(name: seriesName, dataPoints: dataPoints)
            let duration = CFAbsoluteTimeGetCurrent() - startTime
            print("[BENCHMARK] Plot (parametric) generation time: \(duration * 1000) ms")
            
            let xAxisLabel = formatDimensionsForAxis(parametricXAxisDimension, defaultLabel: "x(\(varName))")
            let yAxisLabel = formatDimensionsForAxis(parametricYAxisDimension, defaultLabel: "y(\(varName))")
            
            plotData = PlotData(expression: node.description, series: [plotSeries], plotType: .parametric, explicitYRange: explicitYRange, generationTime: duration, xAxisLabel: xAxisLabel, yAxisLabel: yAxisLabel, xAxisDimension: parametricXAxisDimension, yAxisDimension: parametricYAxisDimension)
        } else {
            // --- Dry run to determine Y-axis unit ---
            let dryRunX = MathValue.unitValue(xMin)
            var tempVarsDryRun = variables
            tempVarsDryRun[varName] = dryRunX
            let (dryRunY, _) = try _evaluateSingle(node: node.expressions.first!, variables: &tempVarsDryRun, functions: &functions, angleMode: .radians)
            let yAxisDimension = dryRunY.dimensionsIfUnitOrDimensionless

            var allSeries: [PlotSeries] = []
            for body in node.expressions {
                var results = Array<DataPoint?>(repeating: nil, count: numPoints)
                let step = (xMax.value - xMin.value) / Double(numPoints - 1)
                let capturedVariables = variables; let capturedFunctions = functions

                DispatchQueue.concurrentPerform(iterations: numPoints) { i in
                    let x_si = xMin.value + Double(i) * step
                    let xVar = MathValue.unitValue(UnitValue(value: x_si, dimensions: xAxisDimension))
                    var localVars = capturedVariables; var localFuncs = capturedFunctions
                    do {
                        let (yValue, _) = try evaluateWithTempVar(node: body, varName: varName, varValue: xVar, variables: &localVars, functions: &localFuncs, angleMode: .radians)
                        
                        guard yValue.dimensionsIfUnitOrDimensionless == yAxisDimension else { return }
                        
                        let y_si: Double
                        switch yValue {
                        case .dimensionless(let d):
                            y_si = d
                        case .unitValue(let u):
                            y_si = u.value
                        case .uncertain(let u):
                            y_si = u.value
                        default:
                            return
                        }

                        if y_si.isFinite {
                            results[i] = DataPoint(x: x_si, y: y_si)
                        }
                    } catch {}
                }
                
                let dataPoints = results.compactMap { $0 }
                if !dataPoints.isEmpty { allSeries.append(PlotSeries(name: DisplayFormatter.formatNodeForLegend(node: body), dataPoints: dataPoints)) }
            }
            if allSeries.isEmpty { throw MathError.plotError(reason: "Could not generate any data points for the expression(s).")}
            let duration = CFAbsoluteTimeGetCurrent() - startTime
            print("[BENCHMARK] Plot (function) generation time: \(duration * 1000) ms")

            let xAxisLabel = formatDimensionsForAxis(xAxisDimension, defaultLabel: varName)
            let yAxisLabel = formatDimensionsForAxis(yAxisDimension, defaultLabel: "f(\(varName))")

            plotData = PlotData(expression: node.description, series: allSeries, plotType: .line, explicitYRange: explicitYRange, generationTime: duration, xAxisLabel: xAxisLabel, yAxisLabel: yAxisLabel, xAxisDimension: xAxisDimension, yAxisDimension: yAxisDimension)
        }
        return .plot(plotData)
    }
    
    // MARK: - Equation Solving
    
    func evaluateSolve(_ node: SolveNode, variables: inout [String: MathValue], functions: inout [String: FunctionDefinitionNode], angleMode: AngleMode) throws -> (result: MathValue, usedAngle: Bool) {
        let varName = node.variable.name
        
        let functionBody: ExpressionNode
        if let binaryNode = node.equation as? BinaryOpNode, binaryNode.op.rawValue == "==" {
            functionBody = BinaryOpNode(op: Token(type: .op("-"), rawValue: "-"), left: binaryNode.left, right: binaryNode.right)
        } else {
            functionBody = node.equation
        }

        let capturedVariables = variables
        let capturedFunctions = functions
        var usedAngle = false
        var roots = Set<Double>()
        let precision = 1e-9

        // Unit-aware path: Check for a guess with units.
        if let guessNode = node.guess {
            let (guessValue, guessUsedAngle) = try _evaluateSingle(node: guessNode, variables: &variables, functions: &functions, angleMode: angleMode)
            usedAngle = usedAngle || guessUsedAngle
            
            if case .unitValue(let guessUnit) = guessValue {
                let varDimensions = guessUnit.dimensions
                
                let f: (Double) throws -> Double = { x in
                    var tempVars = capturedVariables
                    var tempFuncs = capturedFunctions
                    tempVars[varName] = .unitValue(UnitValue(value: x, dimensions: varDimensions))
                    let (result, f_usedAngle) = try self._evaluateSingle(node: functionBody, variables: &tempVars, functions: &tempFuncs, angleMode: angleMode)
                    usedAngle = usedAngle || f_usedAngle
                    
                    guard case .unitValue(let resultUnit) = result else {
                        throw MathError.typeMismatch(expected: "Equation to resolve to a value with units", found: result.typeName)
                    }
                    return resultUnit.value
                }
                
                let searchRange = (guessUnit.value - 100.0)...(guessUnit.value + 100.0)
                try findRoots(in: [searchRange], for: f, storingIn: &roots, precision: precision)

                let finalRoots = Array(roots).sorted().map { MathValue.unitValue(UnitValue(value: $0, dimensions: varDimensions)) }
                return (result: .roots(finalRoots), usedAngle: usedAngle)
            }
        }
        
        // Path for dimensionless variables, even in equations with units.
        let f: (Double) throws -> Double = { x in
            var tempVars = capturedVariables
            var tempFuncs = capturedFunctions
            tempVars[varName] = .dimensionless(x)
            let (result, f_usedAngle) = try self._evaluateSingle(node: functionBody, variables: &tempVars, functions: &tempFuncs, angleMode: angleMode)
            usedAngle = usedAngle || f_usedAngle
            
            switch result {
            case .dimensionless(let d):
                return d
            case .unitValue(let u):
                return u.value
            case .uncertain(let u):
                return u.value
            default:
                throw MathError.dimensionMismatch(reason: "Solver expression must resolve to a dimensioned or dimensionless value.")
            }
        }
        
        let searchRanges: [ClosedRange<Double>]
        if let guessNode = node.guess {
            let (guessValue, guessUsedAngle) = try _evaluateSingle(node: guessNode, variables: &variables, functions: &functions, angleMode: angleMode)
            usedAngle = usedAngle || guessUsedAngle
            let guessScalar = try guessValue.asScalar()
            searchRanges = [(guessScalar - 100.0)...(guessScalar + 100.0)]
        } else {
            if angleMode == .radians {
                // Use a much narrower search range in radians mode to avoid performance issues with periodic functions.
                searchRanges = [-20...20]
            } else {
                // Keep the wide search range for degrees mode and non-trig functions.
                searchRanges = [-1000...(-100), -100...100, 100...1000]
            }
        }
        
        try findRoots(in: searchRanges, for: f, storingIn: &roots, precision: precision)
        
        let finalRoots = Array(roots).sorted().map { MathValue.dimensionless($0) }
        return (result: .roots(finalRoots), usedAngle: usedAngle)
    }

    /// Helper function to scan ranges for roots using Brent's method.
    private func findRoots(in ranges: [ClosedRange<Double>], for f: (Double) throws -> Double, storingIn roots: inout Set<Double>, precision: Double) throws {
        let stepsPerRange = 5000

        for range in ranges {
            let stepSize = (range.upperBound - range.lowerBound) / Double(stepsPerRange)
            var lastY: Double? = nil
            var lastX: Double? = nil

            for i in 0...stepsPerRange {
                let x = range.lowerBound + Double(i) * stepSize
                do {
                    let y = try f(x)
                    if y.isInfinite || y.isNaN {
                        lastY = nil; lastX = nil; continue
                    }
                    
                    if abs(y) < precision {
                        let roundedRoot = (x * 1/precision).rounded() * precision
                        roots.insert(roundedRoot)
                    }

                    if let prevY = lastY, let prevX = lastX {
                        if prevY * y < 0 {
                            if let root = try Solver.brent(f: f, x1: prevX, x2: x) {
                                let roundedRoot = (root * 1/precision).rounded() * precision
                                roots.insert(roundedRoot)
                            }
                        }
                    }
                    lastY = y
                    lastX = x
                } catch let error as MathError {
                    switch error {
                    case .divisionByZero:
                        lastY = nil
                        lastX = nil
                    case .unsupportedOperation(_, let typeA, _) where typeA.contains("negative number"):
                        lastY = nil
                        lastX = nil
                    case .typeMismatch(_, let found) where found.contains("Negative number"):
                        lastY = nil
                        lastX = nil
                    default:
                        throw error
                    }
                }
            }
        }
    }
    
    // --- NEW: A map of common derived units to their symbols ---
    private static let commonDerivedUnits: [UnitDimension: String] = [
        // SI Base & Derived
        [.kilogram: 1, .meter: 1, .second: -2]: "N",
        [.kilogram: 1, .meter: 2, .second: -2]: "J",
        [.kilogram: 1, .meter: 2, .second: -3]: "W",
        [.kilogram: 1, .meter: -1, .second: -2]: "Pa",
        [.second: -1]: "Hz",
        [.second: 1, .ampere: 1]: "C",
        [.kilogram: 1, .meter: 2, .second: -3, .ampere: -1]: "V",
        [.kilogram: 1, .meter: 2, .second: -3, .ampere: -2]: "Ohm",
        [.kilogram: -1, .meter: -2, .second: 4, .ampere: 2]: "F",
        [.kilogram: 1, .meter: 2, .second: -2, .ampere: -1]: "Wb",
        [.kilogram: 1, .meter: 2, .second: -2, .ampere: -2]: "H",
        [.kilogram: 1, .second: -2, .ampere: -1]: "T",
    ]
    
    // --- MODIFIED: This function now checks for common derived units ---
    private func formatDimensionsForAxis(_ dimensions: UnitDimension, defaultLabel: String) -> String {
        if dimensions.isEmpty {
            return defaultLabel
        }
        
        // --- NEW: Check for a matching derived unit first ---
        if let unitSymbol = Self.commonDerivedUnits[dimensions] {
            return "\(defaultLabel) [\(unitSymbol)]"
        }
        
        // --- Fallback to original logic if no simple derived unit is found ---
        let positiveDims = dimensions.filter { $0.value > 0 }.sorted { $0.key.rawValue < $1.key.rawValue }
        let negativeDims = dimensions.filter { $0.value < 0 }.sorted { $0.key.rawValue < $1.key.rawValue }

        let formatPart = { (dims: [(key: BaseUnit, value: Int)]) -> String in
            dims.map { (unit, exponent) -> String in
                let symbol = UnitStore.baseUnitSymbols[unit] ?? unit.rawValue
                let exponentStr = abs(exponent) == 1 ? "" : "^\(abs(exponent))"
                return "\(symbol)\(exponentStr)"
            }.joined(separator: "·")
        }

        let numerator = formatPart(positiveDims)
        let denominator = formatPart(negativeDims)

        if numerator.isEmpty && denominator.isEmpty {
            return defaultLabel
        } else if !denominator.isEmpty {
            let denStr = denominator.contains("·") ? "(\(denominator))" : denominator
            if numerator.isEmpty {
                return "\(defaultLabel) [1/\(denStr)]"
            } else {
                let numStr = numerator.contains("·") ? "(\(numerator))" : numerator
                return "\(defaultLabel) [\(numStr)/\(denStr)]"
            }
        } else {
            return "\(defaultLabel) [\(numerator)]"
        }
    }
}


/// Solves a system of linear equations Ax = b using Gaussian elimination with partial pivoting.
func solveLinearSystem(A: Matrix, b: Vector) throws -> Vector {
    guard A.rows == A.columns else { throw MathError.dimensionMismatch(reason: "Matrix A must be square for linsolve.") }
    let n = A.rows
    guard b.dimension == n else { throw MathError.dimensionMismatch(reason: "Dimension of vector b must match the rows of matrix A.") }

    var augmentedMatrix: [[Double]] = []
    for i in 0..<n {
        var row = (0..<n).map { A[i, $0] }
        row.append(b[i])
        augmentedMatrix.append(row)
    }

    // Forward elimination with partial pivoting
    for i in 0..<n {
        var maxRow = i
        for k in (i + 1)..<n {
            if abs(augmentedMatrix[k][i]) > abs(augmentedMatrix[maxRow][i]) {
                maxRow = k
            }
        }
        augmentedMatrix.swapAt(i, maxRow)

        guard abs(augmentedMatrix[i][i]) > 1e-12 else {
            throw MathError.unsupportedOperation(op: "linsolve", typeA: "Matrix is singular or nearly singular.", typeB: nil)
        }

        for k in (i + 1)..<n {
            let factor = augmentedMatrix[k][i] / augmentedMatrix[i][i]
            augmentedMatrix[k][i] = 0
            for j in (i + 1)...n {
                augmentedMatrix[k][j] -= factor * augmentedMatrix[i][j]
            }
        }
    }

    // Back substitution
    var x = [Double](repeating: 0, count: n)
    for i in (0..<n).reversed() {
        var sum = 0.0
        for j in (i + 1)..<n {
            sum += augmentedMatrix[i][j] * x[j]
        }
        x[i] = (augmentedMatrix[i][n] - sum) / augmentedMatrix[i][i]
    }

    return Vector(values: x)
}

/// Performs a polynomial regression using the method of least squares.
func performPolynomialFit(x: Vector, y: Vector, degree: Double) throws -> Vector {
    guard degree >= 1 && degree.truncatingRemainder(dividingBy: 1) == 0 else {
        throw MathError.unsupportedOperation(op: "polyfit", typeA: "degree must be an integer >= 1", typeB: nil)
    }
    guard x.dimension == y.dimension else {
        throw MathError.dimensionMismatch(reason: "x and y vectors must have the same number of elements.")
    }
    let numPoints = x.dimension
    let degreeInt = Int(degree)
    guard numPoints > degreeInt else {
        throw MathError.unsupportedOperation(op: "polyfit", typeA: "number of data points must be greater than the polynomial degree", typeB: nil)
    }
    
    // Construct the Vandermonde matrix X
    var xMatrixValues = [Double](repeating: 0, count: numPoints * (degreeInt + 1))
    for i in 0..<numPoints {
        for j in 0...degreeInt {
            xMatrixValues[i * (degreeInt + 1) + j] = pow(x[i], Double(j))
        }
    }
    let xMatrix = Matrix(values: xMatrixValues, rows: numPoints, columns: degreeInt + 1)
    
    // Solve the normal equation: (X^T * X) * c = X^T * y
    let xT = xMatrix.transpose()
    let aMatrix = try xT * xMatrix
    let bVector = try xT * y
    
    let coefficients = try solveLinearSystem(A: aMatrix, b: bVector)
    return coefficients
}

private extension MathValue {
    var dimensionsIfUnitOrDimensionless: UnitDimension {
        switch self {
        case .unitValue(let u):
            return u.dimensions
        case .dimensionless:
            return [:]
        default:
            // This case should ideally not be hit in a plotting context,
            // as we expect plottable values.
            return [:]
        }
    }
}
