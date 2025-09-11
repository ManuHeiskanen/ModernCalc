//
//  CalculatorViewModel.swift
//  ModernCalc
//
//  Created by Manu Heiskanen on 28.8.25.
//

import Foundation
import Combine
import SwiftUI

@MainActor
class CalculatorViewModel: ObservableObject {

    @Published var rawExpression: String = ""
    @Published var history: [Calculation] = []
    @Published var liveHelpText: String = ""
    @Published var liveErrorText: String = ""
    @Published var liveLaTeXPreview: String = ""
    @Published var previewText: String = ""
    @Published var variables: [String: MathValue] = [:]
    @Published var functions: [String: FunctionDefinitionNode] = [:]
    @Published var isTallExpression: Bool = false
    
    @Published var angleMode: AngleMode = .degrees {
        didSet { saveState() }
    }
    @Published var userFunctionDefinitions: [String: String] = [:]
    @Published var cursorPosition = NSRange()
    
    // For managing plot windows state
    @Published var plotViewModels: [PlotViewModel] = []
    @Published var plotToShow: PlotData.ID? = nil
    
    private var settings: UserSettings
    private let evaluator = Evaluator()
    private var lastSuccessfulValue: MathValue?
    private var lastUsedAngleFlag: Bool = false
    private var cancellables = Set<AnyCancellable>()
    private let navigationManager = NavigationManager()
    private let ansVariable = "ans"
    
    // --- CHANGE: Added properties to cache the last calculated state ---
    private var lastCalculatedExpression: String?
    private var lastCalculatedAngleMode: AngleMode?
    
    let siPrefixes: Set<String> = [
        "yotta", "zetta", "exa", "peta", "tera", "giga", "mega", "kilo", "hecto", "deca", "deci",
        "centi", "milli", "micro", "nano", "pico", "femto", "atto", "zepto", "yocto"
    ]
    
    let operatorSymbols: [MathSymbol]
    let greekSymbols: [MathSymbol]
    let constantSymbols: [MathSymbol]
    
    let builtinFunctions: [BuiltinFunction] = [
        .init(name: "abs", signature: "abs(value)", description: "Calculates the absolute value or magnitude."),
        .init(name: "acos", signature: "acos(value)", description: "Calculates the inverse cosine (arccos)."),
        .init(name: "acosh", signature: "acosh(value)", description: "Calculates the inverse hyperbolic cosine."),
        .init(name: "angle", signature: "angle(vectorA, vectorB)", description: "Calculates the angle between two vectors."),
        .init(name: "area_circle", signature: "area_circle(r)", description: "Area of a circle."),
        .init(name: "area_rect", signature: "area_rect(w, h)", description: "Area of a rectangle."),
        .init(name: "area_tri", signature: "area_tri(b, h)", description: "Area of a triangle."),
        .init(name: "arg", signature: "arg(complex)", description: "Calculates the argument (phase) of a complex number."),
        .init(name: "asin", signature: "asin(value)", description: "Calculates the inverse sine (arcsin)."),
        .init(name: "asinh", signature: "asinh(value)", description: "Calculates the inverse hyperbolic sine."),
        .init(name: "atan", signature: "atan(value)", description: "Calculates the inverse tangent (arctan)."),
        .init(name: "atan2", signature: "atan2(y, x)", description: "Calculates the inverse tangent considering the quadrant."),
        .init(name: "atanh", signature: "atanh(value)", description: "Calculates the inverse hyperbolic tangent."),
        .init(name: "autoplot", signature: "autoplot(y(x)) or autoplot(vector)", description: "Automatically plots functions or 2D vectors. Use 'x' for functions, 't' for parametric, or pass vectors directly."),
        .init(name: "avg", signature: "avg(a, b, ...)", description: "Calculates the average of a list of numbers or a vector/matrix."),
        .init(name: "ceil", signature: "ceil(number)", description: "Rounds a number up to the nearest integer."),
        .init(name: "circum_circle", signature: "circum_circle(r)", description: "Circumference of a circle."),
        .init(name: "cmatrix", signature: "cmatrix(a, b ; c, d ; ...)", description: "Forms a complex matrix of given real or imaginary numbers."),
        .init(name: "conj", signature: "conj(complex)", description: "Calculates the complex conjugate."),
        .init(name: "cos", signature: "cos(angle)", description: "Calculates the cosine of an angle."),
        .init(name: "cosh", signature: "cosh(value)", description: "Calculates the hyperbolic cosine."),
        .init(name: "cross", signature: "cross(vectorA, vectorB)", description: "Calculates the cross product of two 3D vectors."),
        .init(name: "cvector", signature: "cvector(a ; b ; c ; ...)", description: "Forms a complex vector of given real or imaginary numbers."),
        .init(name: "derivative", signature: "derivative(expr, var, point, [order])", description: "Finds the instantaneous rate of change (slope). Can calculate higher-order derivatives (e.g., order 2 for concavity)."),
        .init(name: "det", signature: "det(matrix)", description: "Calculates the determinant of a square matrix."),
        .init(name: "dot", signature: "dot(vectorA, vectorB)", description: "Calculates the dot product of two vectors (real or complex)."),
        .init(name: "fact", signature: "fact(integer)", description: "Calculates the factorial of a non-negative integer."),
        .init(name: "factor", signature: "factor(integer)", description: "Returns a vector containing the prime factors of an integer."),
        .init(name: "floor", signature: "floor(number)", description: "Rounds a number down to the nearest integer."),
        .init(name: "gcd", signature: "gcd(a, b)", description: "Finds the greatest common divisor of two integers."),
        .init(name: "grad", signature: "grad(f, pointVector)", description: "Calculates the gradient of a multivariable function 'f' at a specific point."),
        .init(name: "hypot", signature: "hypot(sideA, sideB)", description: "Calculates the hypotenuse of a right triangle."),
        .init(name: "imag", signature: "imag(complex)", description: "Extracts the imaginary part of a complex number."),
        .init(name: "integral", signature: "integral(expr, var, from, to)", description: "Calculates the total area under a function's curve between two points."),
        .init(name: "inv", signature: "inv(matrix)", description: "Calculates the inverse of a square matrix."),
        .init(name: "isprime", signature: "isprime(integer)", description: "Checks if an integer is a prime number. Returns 1 for true, 0 for false."),
        .init(name: "lcm", signature: "lcm(a, b)", description: "Finds the least common multiple of two integers."),
        .init(name: "lg", signature: "lg(number)", description: "Alias for the common (base-10) logarithm."),
        .init(name: "linreg", signature: "linreg(xValues, yValues)", description: "Performs a linear regression and returns the slope and y-intercept."),
        .init(name: "linsolve", signature: "linsolve(A, b)", description: "Solves a system of linear equations Ax = b for x, where A is a matrix and b is a vector."),
        .init(name: "linspace", signature: "linspace(start, end, count)", description: "Creates a vector with 'count' evenly spaced values from 'start' to 'end'."),
        .init(name: "ln", signature: "ln(number)", description: "Calculates the natural (base-e) logarithm."),
        .init(name: "log", signature: "log(number)", description: "Calculates the common (base-10) logarithm."),
        .init(name: "matrix", signature: "matrix(a, b ; c, d ; ...)", description: "Forms a matrix of given real numbers."),
        .init(name: "max", signature: "max(a, b, ...)", description: "Finds the maximum value in a list of numbers or a vector/matrix."),
        .init(name: "median", signature: "median(a, b, ...)", description: "Finds the median of a list of numbers or a vector/matrix."),
        .init(name: "min", signature: "min(a, b, ...)", description: "Finds the minimum value in a list of numbers or a vector/matrix."),
        .init(name: "mod", signature: "mod(a, b)", description: "Calculates the mathematical modulo (remainder of division)."),
        .init(name: "nCr", signature: "nCr(n, k)", description: "Calculates the number of combinations."),
        .init(name: "nPr", signature: "nPr(n, k)", description: "Calculates the number of permutations."),
        .init(name: "plot", signature: "plot(expr, var, x_min, x_max, [y_min, y_max])", description: "Plots expressions over a specified range with optional y-axis limits."),
        .init(name: "polar", signature: "polar(complex)", description: "Converts a complex number to its polar form (R ∠ θ)."),
        .init(name: "random", signature: "random([max], [min, max], [min, max, count])", description: "Generates random numbers or a vector of random integers."),
        .init(name: "randm", signature: "randm(rows, cols)", description: "Creates a matrix with the specified dimensions, filled with random numbers between 0 and 1."),
        .init(name: "randv", signature: "randv(size)", description: "Creates a vector of the specified size, filled with random numbers between 0 and 1."),
        .init(name: "range", signature: "range(start, end, [step])", description: "Creates a vector from 'start' to 'end' with an optional 'step' (default is 1)."),
        .init(name: "real", signature: "real(complex)", description: "Extracts the real part of a complex number."),
        .init(name: "root", signature: "root(number, degree)", description: "Calculates the nth root of a number."),
        .init(name: "round", signature: "round(number)", description: "Rounds a number to the nearest integer."),
        .init(name: "scatterplot", signature: "scatterplot(x_values, y_values)", description: "Creates a scatter plot from two vectors of x and y coordinates, or a single two-column matrix."),
        .init(name: "side", signature: "side(hyp, sideA)", description: "Calculates the missing side of a right triangle."),
        .init(name: "sin", signature: "sin(angle)", description: "Calculates the sine of an angle."),
        .init(name: "sinh", signature: "sinh(value)", description: "Calculates the hyperbolic sine."),
        .init(name: "sqrt", signature: "sqrt(number)", description: "Calculates the square root. Handles complex numbers."),
        .init(name: "stddev", signature: "stddev(a, b, ...)", description: "Calculates the sample standard deviation."),
        .init(name: "stddevp", signature: "stddevp(a, b, ...)", description: "Calculates the population standard deviation."),
        .init(name: "sum", signature: "sum(a, b, ...)", description: "Calculates the sum of a list of numbers or a vector/matrix."),
        .init(name: "tan", signature: "tan(angle)", description: "Calculates the tangent of an angle."),
        .init(name: "tanh", signature: "tanh(value)", description: "Calculates the hyperbolic tangent."),
        .init(name: "trace", signature: "trace(matrix)", description: "Calculates the trace of a square matrix (sum of diagonal elements)."),
        .init(name: "transpose", signature: "transpose(matrix)", description: "Transposes a matrix (rows become columns). For complex matrices, this is not the conjugate transpose."),
        .init(name: "unit", signature: "unit(vector)", description: "Returns the unit vector (vector with magnitude 1)."),
        .init(name: "variance", signature: "variance(a, b, ...)", description: "Calculates the sample variance."),
        .init(name: "vector", signature: "vector(a ; b ; c ; ...)", description: "Forms a vector of given real numbers."),
        .init(name: "vol_cone", signature: "vol_cone(r, h)", description: "Volume of a cone."),
        .init(name: "vol_cube", signature: "vol_cube(s)", description: "Volume of a cube."),
        .init(name: "vol_cylinder", signature: "vol_cylinder(r, h)", description: "Volume of a cylinder."),
        .init(name: "vol_sphere", signature: "vol_sphere(r)", description: "Volume of a sphere.")
    ]
    
    let physicalConstants: [PhysicalConstant] = [
        .init(symbol: "atm", name: "Standard atmosphere", value: 101325),
        .init(symbol: "b", name: "Wien's displacement", value: 2.897771955e-3),
        .init(symbol: "c", name: "Speed of light", value: 299792458),
        .init(symbol: "e", name: "Euler's number", value: M_E),
        .init(symbol: "e0", name: "Elementary charge", value: 1.602176634e-19),
        .init(symbol: "F", name: "Faraday constant", value: 96485.33212),
        .init(symbol: "g", name: "Standard gravity", value: 9.80665),
        .init(symbol: "G", name: "Gravitational constant", value: 6.67430e-11),
        .init(symbol: "h", name: "Planck constant", value: 6.62607015e-34),
        .init(symbol: "ħ", name: "Reduced Planck constant", value: 1.054571817e-34),
        .init(symbol: "kB", name: "Boltzmann constant", value: 1.380649e-23),
        .init(symbol: "me", name: "Electron mass", value: 9.1093837015e-31),
        .init(symbol: "mn", name: "Neutron mass", value: 1.67492749804e-27),
        .init(symbol: "mp", name: "Proton mass", value: 1.67262192369e-27),
        .init(symbol: "NA", name: "Avogadro constant", value: 6.02214076e23),
        .init(symbol: "R", name: "Gas constant", value: 8.314462618),
        .init(symbol: "Rinf", name: "Rydberg constant", value: 10973731.568160),
        .init(symbol: "Vm", name: "Molar volume (STP)", value: 22.41396954e-3),
        .init(symbol: "ε0", name: "Vacuum permittivity", value: 8.8541878128e-12),
        .init(symbol: "μ0", name: "Vacuum permeability", value: 1.25663706212e-6),
        .init(symbol: "π", name: "Pi", value: Double.pi),
        .init(symbol: "σ", name: "Stefan-Boltzmann", value: 5.670374419e-8)
    ]

    let helpTopics: [HelpTopic] = [
        .init(title: "Quick Start", content: "Enter a mathematical expression to see a live result. Press **Enter** to add it to your history. Use the `ans` variable to reference the last result. Use the arrow keys to navigate your history and reuse previous equations or results."),
        .init(title: "Variables & Functions", content: "Assign a variable using `:=`, like `x := 5*2`. Variables are saved automatically. \nDefine custom functions with parameters, like `f(x, y) := x^2 + y^2`. You can then call them like any built-in function: `f(3, 4)`."),
        .init(title: "Operators", content: "Supports standard operators `+ - * / ^ %`. For element-wise vector/matrix operations, use `.*` and `./`. You can modify a single vector element using operators like `.=@` (set), `.+@` (add to), etc., with the syntax `vector_expression .op@ (index, value)`. The `!` operator calculates factorial, and `'` transposes a matrix. For complex matrices, `'` performs the conjugate transpose."),
        .init(title: "Data Types", content: "**Complex Numbers:** Use `i` for the imaginary unit (e.g., `3 + 4i`). \n**Vectors:** Create with `vector(1; 2; 3)`. \n**Matrices:** Create with `matrix(1, 2; 3, 4)`, using commas for columns and semicolons for rows. \n**Polar Form:** Enter complex numbers with `R∠θ` (e.g., `5∠53.13` in degree mode)."),
        .init(title: "Linear Algebra", content: "Solve systems of linear equations of the form `Ax = b` with `linsolve(A, b)`. Standard matrix operations like inverse (`inv`), determinant (`det`), and trace (`trace`) are also available."),
        .init(title: "Plotting", content: "Create 2D plots for functions or vectors. \n- **Automatic Plotting (`autoplot`):** For quick graphs. `autoplot(sin(x))` plots a function. `autoplot(vector(3;4), vector(-1;2))` plots 2D vectors from the origin. `autoplot(cos(t), sin(t))` creates a parametric plot. \n- **Manual Plotting (`plot`):** For detailed control. `plot(x^2, x, -5, 5)` plots `x^2` for `x` from -5 to 5. You can optionally set y-axis limits: `plot(x^2, x, -5, 5, 0, 25)`. \n- **Scatter Plots (`scatterplot`):** Visualize data points with `scatterplot(x_vector, y_vector)` or `scatterplot(2_column_matrix)`. \nClicking a plot in the history will reopen its window."),
        .init(title: "Calculus", content: "Calculate derivatives with `derivative(expression, variable, point, [order])`. You can also use the shorthand `derivative(f, point)` for a pre-defined single-variable function `f`. \nCalculate definite integrals with `integral(expression, variable, from, to)`. \nCalculate the gradient of a multi-variable function `g` with `grad(g, vector(x_point, y_point, ...))`. The function must be pre-defined."),
        .init(title: "Statistics & Random Data", content: "Perform statistical analysis with functions like `sum`, `avg`, `stddev`, `variance`, and `linreg(x, y)`. Generate datasets using `range`, `linspace`, or the versatile `random()` function. You can also create random vectors and matrices with `randv(size)` and `randm(rows, cols)`. Number theory functions like `isprime`, `factor`, `gcd`, and `lcm` are also available.")
    ]


    init(settings: UserSettings) {
        self.settings = settings
        self.operatorSymbols = [
            .init(symbol: "±", name: "Plus-Minus"), .init(symbol: "∠", name: "Angle"), .init(symbol: "√", name: "Square Root", insertionText: "√("),
            .init(symbol: "×", name: "Multiply"), .init(symbol: "÷", name: "Divide"), .init(symbol: "^", name: "Power"),
            .init(symbol: "!", name: "Factorial"),
            .init(symbol: "'", name: "Transpose"), .init(symbol: ".*", name: "Element-wise Multiply"), .init(symbol: "./", name: "Element-wise Divide"),
            .init(symbol: ".=@", name: "Set Element", insertionText: ".=@(index, value)"),
            .init(symbol: ".+@", name: "Add to Element", insertionText: ".+@(index, value)"),
            .init(symbol: ".-@", name: "Subtract from Element", insertionText: ".-@(index, value)"),
            .init(symbol: ".*@", name: "Multiply Element", insertionText: ".*@(index, value)"),
            .init(symbol: "./@", name: "Divide Element", insertionText: "./@(index, value)")
        ]
        self.greekSymbols = [
            .init(symbol: "α", name: "Alpha"), .init(symbol: "Α", name: "Alpha"), .init(symbol: "β", name: "Beta"), .init(symbol: "Β", name: "Beta"),
            .init(symbol: "γ", name: "Gamma"), .init(symbol: "Γ", name: "Gamma"), .init(symbol: "δ", name: "Delta"), .init(symbol: "Δ", name: "Delta"),
            .init(symbol: "ε", name: "Epsilon"), .init(symbol: "Ε", name: "Epsilon"), .init(symbol: "ζ", name: "Zeta"), .init(symbol: "Ζ", name: "Zeta"),
            .init(symbol: "η", name: "Eta"), .init(symbol: "Η", name: "Eta"), .init(symbol: "θ", name: "Theta"), .init(symbol: "Θ", name: "Theta"),
            .init(symbol: "ι", name: "Iota"), .init(symbol: "Ι", name: "Iota"), .init(symbol: "κ", name: "Kappa"), .init(symbol: "Κ", name: "Kappa"),
            .init(symbol: "λ", name: "Lambda"), .init(symbol: "Λ", name: "Lambda"), .init(symbol: "μ", name: "Mu"), .init(symbol: "Μ", name: "Mu"),
            .init(symbol: "ν", name: "Nu"), .init(symbol: "Ν", name: "Nu"), .init(symbol: "ξ", name: "Xi"), .init(symbol: "Ξ", name: "Xi"),
            .init(symbol: "ο", name: "Omicron"), .init(symbol: "Ο", name: "Omicron"), .init(symbol: "ρ", name: "Rho"), .init(symbol: "Ρ", name: "Rho"),
            .init(symbol: "σ", name: "Sigma"), .init(symbol: "Σ", name: "Sigma"), .init(symbol: "τ", name: "Tau"), .init(symbol: "Τ", name: "Tau"),
            .init(symbol: "υ", name: "Upsilon"), .init(symbol: "Υ", name: "Upsilon"), .init(symbol: "φ", name: "Phi"), .init(symbol: "Φ", name: "Phi"),
            .init(symbol: "χ", name: "Chi"), .init(symbol: "Χ", name: "Chi"), .init(symbol: "ψ", name: "Psi"), .init(symbol: "Ψ", name: "Psi"),
            .init(symbol: "ω", name: "Omega"), .init(symbol: "Ω", name: "Omega")
        ]
        self.constantSymbols = physicalConstants.map { .init(symbol: $0.symbol, name: $0.name, insertionText: $0.symbol) }
        
        // --- CHANGE: Modified publisher logic to avoid wasteful calculations ---
        Publishers.CombineLatest3($rawExpression, $cursorPosition, $angleMode)
            .debounce(for: .milliseconds(50), scheduler: RunLoop.main)
            .sink { [weak self] (expression, position, angle) in
                guard let self = self else { return }

                guard !expression.trimmingCharacters(in: .whitespaces).isEmpty else {
                    self.lastSuccessfulValue = nil
                    self.liveHelpText = ""
                    self.liveErrorText = ""
                    self.liveLaTeXPreview = ""
                    self.lastCalculatedExpression = nil
                    self.lastCalculatedAngleMode = nil
                    return
                }
                
                // If expression and angle are unchanged, only update contextual help and stop.
                // This prevents re-calculating when only the cursor position changes (e.g., arrow keys).
                if expression == self.lastCalculatedExpression && angle == self.lastCalculatedAngleMode {
                    self.liveHelpText = self.getContextualHelp(expression: expression, cursor: position) ?? ""
                    return
                }
                
                Task {
                    await self.calculate(expression: expression, cursor: position)
                    // Cache the expression and angle mode that were just calculated.
                    self.lastCalculatedExpression = expression
                    self.lastCalculatedAngleMode = angle
                }
            }
            .store(in: &cancellables)
            
        loadState()
    }
    
    func addPlotViewModel(for plotData: PlotData) {
        if !plotViewModels.contains(where: { $0.plotData.id == plotData.id }) {
            let newPlotViewModel = PlotViewModel(plotData: plotData)
            plotViewModels.append(newPlotViewModel)
        }
    }

    func requestOpenPlotWindow(for plotData: PlotData) {
        addPlotViewModel(for: plotData)
        plotToShow = plotData.id
    }

    func closePlotWindow(id: PlotData.ID?) {
        plotViewModels.removeAll { $0.plotData.id == id }
    }

    private func getContextualHelp(expression: String, cursor: NSRange) -> String? {
        guard cursor.location <= expression.utf16.count else { return nil }
        
        let startIndex = expression.startIndex
        let cursorIndex = expression.index(startIndex, offsetBy: cursor.location, limitedBy: expression.endIndex) ?? expression.endIndex
        let textBeforeCursor = expression[..<cursorIndex]

        var openParenCount = 0
        var lastOpenParenIndex: String.Index?
        
        for index in textBeforeCursor.indices.reversed() {
            let char = textBeforeCursor[index]
            if char == ")" {
                openParenCount += 1
            } else if char == "(" {
                if openParenCount == 0 {
                    lastOpenParenIndex = index
                    break
                } else {
                    openParenCount -= 1
                }
            }
        }
        
        guard let parenIndex = lastOpenParenIndex else { return nil }
        
        let textBeforeParen = textBeforeCursor[..<parenIndex]
        
        let pattern = "\\b([a-zA-Z_][a-zA-Z0-9_]*)$"
        if let range = textBeforeParen.range(of: pattern, options: .regularExpression),
           let function = builtinFunctions.first(where: { $0.name == textBeforeParen[range] }) {
            return "\(function.signature)\n\(function.description)"
        }
        
        return nil
    }

    private func calculate(expression: String, cursor: NSRange) async {
        let helpText = getContextualHelp(expression: expression, cursor: cursor)

        let callsTallFunction = expression.contains("vector(") || expression.contains("matrix(") || expression.contains("range(") || expression.contains("linspace(")
        let parts = expression.components(separatedBy: CharacterSet(charactersIn: "+-()"))
        let hasNestedFraction = parts.contains { $0.filter { $0 == "/" }.count >= 2 }
        let newIsTallExpression = callsTallFunction || hasNestedFraction

        var tempVars = self.variables
        var tempFuncs = self.functions
        
        var finalLiveHelpText: String = ""
        var finalLiveErrorText: String = ""
        let finalLiveLaTeXPreview: String
        
        do {
            let lexer = Lexer(input: expression, decimalSeparator: settings.decimalSeparator)
            let tokens = lexer.tokenize()
            let parser = Parser(tokens: tokens)
            let expressionTree = try parser.parse()
            let expressionLaTeX = LaTeXEngine.formatNode(expressionTree, evaluator: self.evaluator, settings: self.settings)

            let (value, usedAngle) = try evaluator.evaluate(node: expressionTree, variables: &tempVars, functions: &tempFuncs, angleMode: self.angleMode)
            
            self.lastSuccessfulValue = value
            self.lastUsedAngleFlag = usedAngle
            self.variables = tempVars
            self.functions = tempFuncs
            
            let isSimpleVariableDefinition = expressionTree is AssignmentNode && ((expressionTree as! AssignmentNode).expression is NumberNode || (expressionTree as! AssignmentNode).expression is UnaryOpNode)
            
            if case .plot = value {
                finalLiveLaTeXPreview = expressionLaTeX
            } else if case .functionDefinition = value {
                finalLiveLaTeXPreview = expressionLaTeX
            } else if isSimpleVariableDefinition {
                finalLiveLaTeXPreview = expressionLaTeX
            } else {
                let resultLaTeX: String
                let maxLivePreviewRows = 50

                var isResultTooLargeForPreview = false
                if case .vector(let v) = value, v.dimension > maxLivePreviewRows { isResultTooLargeForPreview = true }
                else if case .matrix(let m) = value, m.rows > maxLivePreviewRows { isResultTooLargeForPreview = true }
                else if case .complexVector(let cv) = value, cv.dimension > maxLivePreviewRows { isResultTooLargeForPreview = true }
                else if case .complexMatrix(let cm) = value, cm.rows > maxLivePreviewRows { isResultTooLargeForPreview = true }

                if isResultTooLargeForPreview {
                    switch value {
                    case .vector(let v): resultLaTeX = "\\text{\(v.dimension)-element Vector}"
                    case .matrix(let m): resultLaTeX = "\\text{\(m.rows)x\(m.columns) Matrix}"
                    case .complexVector(let cv): resultLaTeX = "\\text{\(cv.dimension)-element Complex Vector}"
                    case .complexMatrix(let cm): resultLaTeX = "\\text{\(cm.rows)x\(cm.columns) Complex Matrix}"
                    default: resultLaTeX = LaTeXEngine.formatMathValue(value, angleMode: self.angleMode, settings: self.settings)
                    }
                } else {
                    if self.settings.enableLiveRounding {
                        let liveSettings = UserSettings(); liveSettings.displayMode = .fixed
                        liveSettings.fixedDecimalPlaces = self.settings.livePreviewDecimalPlaces; liveSettings.decimalSeparator = self.settings.decimalSeparator
                        resultLaTeX = LaTeXEngine.formatMathValue(value, angleMode: self.angleMode, settings: liveSettings)
                    } else {
                        resultLaTeX = LaTeXEngine.formatMathValue(value, angleMode: self.angleMode, settings: self.settings)
                    }
                }
                finalLiveLaTeXPreview = "\(expressionLaTeX) = \(resultLaTeX)"
            }
            
            finalLiveHelpText = helpText ?? ""

        } catch let error {
            self.lastSuccessfulValue = nil
            
            let errorMessage = (error as? CustomStringConvertible)?.description ?? "An unknown error occurred."
            finalLiveHelpText = helpText ?? ""
            finalLiveErrorText = errorMessage
            finalLiveLaTeXPreview = LaTeXEngine.formatExpression(expression, evaluator: self.evaluator, settings: self.settings)
        }
        
        self.liveHelpText = finalLiveHelpText
        self.liveErrorText = finalLiveErrorText
        self.liveLaTeXPreview = finalLiveLaTeXPreview
        
        if self.isTallExpression != newIsTallExpression {
            self.isTallExpression = newIsTallExpression
        }
    }
    
    func commitCalculation() -> PlotData? {
        // If a history item is currently selected via keyboard navigation...
        if let selectedId = navigationManager.selectedHistoryId, let selectedItem = history.first(where: { $0.id == selectedId }) {

            // ACTION 1: Check for a plot. This is a special action that doesn't insert text.
            if case .plot(let plotData) = selectedItem.result {
                resetNavigation()
                requestOpenPlotWindow(for: plotData)
                return nil // Action is complete.
            }

            // ACTION 2: For everything else, get the text from the preview and insert it.
            // This ensures that what the user sees highlighted is what gets inserted.
            let textToInsert = self.previewText
            resetNavigation()
            
            if !textToInsert.isEmpty {
                self.insertTextAtCursor(textToInsert)
            }
            
            return nil // Action is complete.

        } else {
            // If no history item is selected, proceed with committing the new expression from the input field.
            guard !rawExpression.isEmpty, let valueToCommit = lastSuccessfulValue else { return nil }
            
            var plotDataToReturn: PlotData?
            let calcType: CalculationType
            
            if case .plot(let plotData) = valueToCommit {
                calcType = .plot
                addPlotViewModel(for: plotData)
                plotDataToReturn = plotData
            } else if valueToCommit.typeName == "FunctionDefinition" {
                calcType = .functionDefinition
            } else if rawExpression.contains(":=") {
                calcType = .variableAssignment
            } else {
                calcType = .evaluation
            }

            if calcType != .functionDefinition { self.variables[self.ansVariable] = valueToCommit }
            if calcType == .variableAssignment { saveState() }
            else if calcType == .functionDefinition, case .functionDefinition(let name) = valueToCommit { userFunctionDefinitions[name] = rawExpression; saveState() }
            
            let newCalculation = Calculation(expression: rawExpression, result: valueToCommit, type: calcType, usedAngleSensitiveFunction: self.lastUsedAngleFlag, angleMode: self.angleMode)
            
            Task {
                self.history.append(newCalculation)
                self.rawExpression = ""
            }
            return plotDataToReturn
        }
    }

    func handleKeyPress(keys: Set<KeyEquivalent>) -> Bool {
        if let selectedText = navigationManager.handleKeyPress(keys: keys, history: history, viewModel: self) {
            Task {
                self.previewText = selectedText
            }
            return true
        } else {
            Task {
                self.previewText = ""
            }
            return false
        }
    }

    func resetNavigation() {
        navigationManager.resetSelection()
        Task {
            self.previewText = ""
        }
    }

    var selectedHistoryId: UUID? { navigationManager.selectedHistoryId }
    var selectedHistoryPart: SelectionPart { navigationManager.selectedPart }
    var sortedVariables: [(String, MathValue)] { variables.sorted { $0.key < $1.key } }
    var sortedFunctions: [(String, FunctionDefinitionNode)] { functions.sorted { $0.key < $1.key } }
    
    func deleteVariable(name: String) { variables.removeValue(forKey: name); saveState() }
    func deleteFunction(name: String) { functions.removeValue(forKey: name); userFunctionDefinitions.removeValue(forKey: name); saveState() }
    
    func insertTextAtCursor(_ textToInsert: String) {
        guard let range = Range(cursorPosition, in: rawExpression) else {
            rawExpression += textToInsert; let newLocation = rawExpression.utf16.count; cursorPosition = NSRange(location: newLocation, length: 0)
            return
        }
        rawExpression.replaceSubrange(range, with: textToInsert)
        let newLocation = cursorPosition.location + textToInsert.utf16.count; cursorPosition = NSRange(location: newLocation, length: 0)
    }

    private func saveState() {
        do {
            let variablesData = try JSONEncoder().encode(variables); let functionsData = try JSONEncoder().encode(userFunctionDefinitions)
            UserDefaults.standard.set(variablesData, forKey: "userVariables"); UserDefaults.standard.set(functionsData, forKey: "userFunctionDefinitions")
            UserDefaults.standard.set(angleMode.rawValue, forKey: "angleModeSetting")
        } catch { print("Error saving state: \(error.localizedDescription)") }
    }

    private func loadState() {
        do {
            if let variablesData = UserDefaults.standard.data(forKey: "userVariables") {
                let decodedVars = try JSONDecoder().decode([String: MathValue].self, from: variablesData)
                self.variables = decodedVars.filter { $0.key != ansVariable }
            }
            if let functionsData = UserDefaults.standard.data(forKey: "userFunctionDefinitions") {
                self.userFunctionDefinitions = try JSONDecoder().decode([String: String].self, from: functionsData)
                rebuildFunctionsFromDefinitions()
            }
        } catch {
            print("Error loading variable/function state: \(error.localizedDescription)"); self.variables = [:]; self.userFunctionDefinitions = [:]
        }
        if let savedAngleMode = UserDefaults.standard.string(forKey: "angleModeSetting") { self.angleMode = AngleMode(rawValue: savedAngleMode) ?? .degrees }
    }
    
    private func rebuildFunctionsFromDefinitions() {
        let definitionsToRebuild = self.userFunctionDefinitions; guard !definitionsToRebuild.isEmpty else { return }
        var tempVars = self.variables; var tempFuncs: [String: FunctionDefinitionNode] = [:]

        for (_, definitionString) in definitionsToRebuild {
            do {
                let lexer = Lexer(input: definitionString, decimalSeparator: settings.decimalSeparator)
                let tokens = lexer.tokenize(); let parser = Parser(tokens: tokens); let expressionTree = try parser.parse()
                _ = try evaluator.evaluate(node: expressionTree, variables: &tempVars, functions: &tempFuncs, angleMode: self.angleMode)
            } catch { print("Error rebuilding function '\(definitionString)': \(error)") }
        }
        self.functions = tempFuncs; self.variables = tempVars; self.liveHelpText = ""; self.liveErrorText = ""
    }
    
    func formatCalculationAsLaTeX(_ calculation: Calculation) -> String {
        return LaTeXEngine.format(calculation: calculation, evaluator: self.evaluator, angleMode: self.angleMode, settings: self.settings)
    }

    func formatForHistory(_ value: MathValue) -> String {
        switch value {
        case .scalar(let d): return formatScalarForDisplay(d); case .complex(let c): return formatComplexForDisplay(c); case .vector(let v): return formatVectorForDisplay(v)
        case .matrix(let m): return formatMatrixForDisplay(m); case .tuple(let t): return t.map { formatForHistory($0) }.joined(separator: " OR ")
        case .complexVector(let cv): return formatComplexVectorForDisplay(cv); case .complexMatrix(let cm): return formatComplexMatrixForDisplay(cm)
        case .functionDefinition: return ""; case .polar(let p): return formatPolarForDisplay(p); case .regressionResult(let s, let i): return "m = \(formatScalarForDisplay(s)), b = \(formatScalarForDisplay(i))"
        case .plot(let plotData): return "Plot: \(plotData.expression)"
        }
    }
    
    func formatForParsing(_ value: MathValue) -> String {
        switch value {
        case .scalar(let d): return formatScalarForParsing(d); case .complex(let c): return formatComplexForParsing(c)
        case .vector(let v): return "vector(\(v.values.map { formatScalarForParsing($0) }.joined(separator: ";")))"
        case .matrix(let m): return "matrix(\((0..<m.rows).map { r in (0..<m.columns).map { c in formatScalarForParsing(m[r, c]) }.joined(separator: ",") }.joined(separator: ";")))"
        case .tuple(let t): return t.map { formatForParsing($0) }.first ?? ""
        case .complexVector(let cv): return "cvector(\(cv.values.map { formatForParsing(.complex($0)) }.joined(separator: ";")))"
        case .complexMatrix(let cm): return "cmatrix(\((0..<cm.rows).map { r in (0..<cm.columns).map { c in formatForParsing(.complex(cm[r, c])) }.joined(separator: ",") }.joined(separator: ";")))"
        case .functionDefinition: return ""; case .polar(let p): return formatPolarForParsing(p); case .regressionResult: return ""
        case .plot(let plotData): return "autoplot(\(plotData.expression))"
        }
    }

    func formatScalarForDisplay(_ value: Double) -> String {
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
    
    private func formatComplexForDisplay(_ value: Complex) -> String {
        if value.real != 0 && value.imaginary != 0 { return "\(formatScalarForDisplay(value.real)) \(value.imaginary < 0 ? "-" : "+") \(formatScalarForDisplay(abs(value.imaginary)))i" }
        else if value.real != 0 { return formatScalarForDisplay(value.real) }
        else if value.imaginary != 0 { return "\(formatScalarForDisplay(value.imaginary))i" }
        else { return "0" }
    }
    
    private func formatPolarForDisplay(_ value: Complex) -> String {
        let magnitude = value.abs(); let angle = value.argument()
        if self.angleMode == .degrees { let angleDegrees = angle * (180.0 / .pi); return "\(formatScalarForDisplay(magnitude)) ∠ \(formatScalarForDisplay(angleDegrees))°" }
        else { return "\(formatScalarForDisplay(magnitude)) ∠ \(formatScalarForDisplay(angle)) rad" }
    }

    private func formatVectorForDisplay(_ vector: Vector) -> String {
        let maxDisplayRows = 10
        if vector.dimension <= maxDisplayRows {
            return (0..<vector.dimension).map { "[ \(formatScalarForDisplay(vector[$0])) ]" }.joined(separator: "\n")
        } else {
            var lines: [String] = []
            let headCount = 5
            let tailCount = 4
            for i in 0..<headCount {
                lines.append("[ \(formatScalarForDisplay(vector[i])) ]")
            }
            lines.append("... (\(vector.dimension - (headCount + tailCount)) more items) ...")
            for i in (vector.dimension - tailCount)..<vector.dimension {
                lines.append("[ \(formatScalarForDisplay(vector[i])) ]")
            }
            return lines.joined(separator: "\n")
        }
    }
    
    private func formatMatrixForDisplay(_ matrix: Matrix) -> String {
        let maxDisplayRows = 10
        if matrix.rows == 0 || matrix.columns == 0 { return "[]" }

        func formatRow(_ r: Int, columnWidths: [Int]) -> String {
            let rowContent = (0..<matrix.columns).map { c in
                let formattedNumber = formatScalarForDisplay(matrix[r, c])
                let padding = String(repeating: " ", count: columnWidths[c] - formattedNumber.count)
                return padding + formattedNumber
            }.joined(separator: "  ")
            return "[ \(rowContent) ]"
        }

        var columnWidths = [Int](repeating: 0, count: matrix.columns)
        for c in 0..<matrix.columns {
            var maxWidth = 0
            for r in 0..<matrix.rows {
                let formattedNumber = formatScalarForDisplay(matrix[r, c])
                if formattedNumber.count > maxWidth { maxWidth = formattedNumber.count }
            }
            columnWidths[c] = maxWidth
        }

        if matrix.rows <= maxDisplayRows {
            return (0..<matrix.rows).map { r in formatRow(r, columnWidths: columnWidths) }.joined(separator: "\n")
        } else {
            var lines: [String] = []
            let headCount = 5
            let tailCount = 4
            for r in 0..<headCount {
                lines.append(formatRow(r, columnWidths: columnWidths))
            }
            lines.append("... (\(matrix.rows - (headCount + tailCount)) more rows) ...")
            for r in (matrix.rows - tailCount)..<matrix.rows {
                lines.append(formatRow(r, columnWidths: columnWidths))
            }
            return lines.joined(separator: "\n")
        }
    }
    
    private func formatComplexVectorForDisplay(_ vector: ComplexVector) -> String {
        let maxDisplayRows = 10
        if vector.dimension <= maxDisplayRows {
            return (0..<vector.dimension).map { "[ \(formatComplexForDisplay(vector[$0])) ]" }.joined(separator: "\n")
        } else {
            var lines: [String] = []
            let headCount = 5
            let tailCount = 4
            for i in 0..<headCount {
                lines.append("[ \(formatComplexForDisplay(vector[i])) ]")
            }
            lines.append("... (\(vector.dimension - tailCount)) more items) ...")
            for i in (vector.dimension - tailCount)..<vector.dimension {
                lines.append("[ \(formatComplexForDisplay(vector[i])) ]")
            }
            return lines.joined(separator: "\n")
        }
    }
    
    private func formatComplexMatrixForDisplay(_ matrix: ComplexMatrix) -> String {
        let maxDisplayRows = 10
        if matrix.rows == 0 || matrix.columns == 0 { return "[]" }
        
        func formatRow(_ r: Int, columnWidths: [Int]) -> String {
            let rowContent = (0..<matrix.columns).map { c in
                let formattedNumber = "(\(formatComplexForDisplay(matrix[r, c])))"
                let padding = String(repeating: " ", count: columnWidths[c] - formattedNumber.count)
                return padding + formattedNumber
            }.joined(separator: "  ")
            return "[ \(rowContent) ]"
        }

        var columnWidths = [Int](repeating: 0, count: matrix.columns)
        for c in 0..<matrix.columns {
            var maxWidth = 0
            for r in 0..<matrix.rows {
                let formattedNumber = "(\(formatComplexForDisplay(matrix[r, c])))"
                if formattedNumber.count > maxWidth { maxWidth = formattedNumber.count }
            }
            columnWidths[c] = maxWidth
        }

        if matrix.rows <= maxDisplayRows {
            return (0..<matrix.rows).map { r in formatRow(r, columnWidths: columnWidths) }.joined(separator: "\n")
        } else {
            var lines: [String] = []
            let headCount = 5
            let tailCount = 4
            for r in 0..<headCount {
                lines.append(formatRow(r, columnWidths: columnWidths))
            }
            lines.append("... (\(matrix.rows - (headCount + tailCount)) more rows) ...")
            for r in (matrix.rows - tailCount)..<matrix.rows {
                lines.append(formatRow(r, columnWidths: columnWidths))
            }
            return lines.joined(separator: "\n")
        }
    }
    
    private func formatScalarForParsing(_ value: Double) -> String {
        let stringValue = String(value)
        if stringValue.contains("e") { return "(\(stringValue.replacingOccurrences(of: "e", with: "*10^")))" }
        return stringValue.replacingOccurrences(of: ",", with: ".")
    }
    
    private func formatComplexForParsing(_ value: Complex) -> String {
        let sign = value.imaginary < 0 ? "" : "+"; return "(\(formatScalarForParsing(value.real))\(sign)\(formatScalarForParsing(value.imaginary))i)"
    }
    
    private func formatPolarForParsing(_ value: Complex) -> String {
        let magnitude = value.abs(); let angleDegrees = value.argument() * (180.0 / .pi)
        return "\(formatScalarForParsing(magnitude))∠\(formatScalarForParsing(angleDegrees))"
    }
}
