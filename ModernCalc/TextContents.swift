//
//  TextContents.swift
//  ModernCalc
//
//  Created by Manu Heiskanen on 8.9.2025.
//

import Foundation

/// A centralized repository for static application content like function definitions,
/// constants, and help topics.
struct TextContents {
    
    static let builtinFunctions: [BuiltinFunction] = [
        .init(name: "abs", signature: "abs(value)", description: "Calculates the absolute value or magnitude. For square matrices, it returns the determinant."),
        .init(name: "acos", signature: "acos(value)", description: "Calculates the inverse cosine (arccos)."),
        .init(name: "acosh", signature: "acosh(value)", description: "Calculates the inverse hyperbolic cosine."),
        .init(name: "angle", signature: "angle(vectorA, vectorB)", description: "Calculates the angle between two vectors."),
        .init(name: "area_circle", signature: "area_circle(r)", description: "Area of a circle."),
        .init(name: "area_rect", signature: "area_rect(w, h)", description: "Area of a rectangle."),
        .init(name: "area_tri", signature: "area_tri(b, h)", description: "Area of a triangle."),
        .init(name: "areaplot", signature: "areaplot(f(x), g(x), x, min, max)", description: "Plots and calculates the area between f(x) and g(x) from min to max. If only f(x) is provided, it calculates the area between the function and the x-axis."),
        .init(name: "arg", signature: "arg(complex)", description: "Calculates the argument (phase) of a complex number."),
        .init(name: "asin", signature: "asin(value)", description: "Calculates the inverse sine (arcsin)."),
        .init(name: "asinh", signature: "asinh(value)", description: "Calculates the inverse hyperbolic sine."),
        .init(name: "atan", signature: "atan(value)", description: "Calculates the inverse tangent (arctan)."),
        .init(name: "atan2", signature: "atan2(y, x)", description: "Calculates the inverse tangent considering the quadrant."),
        .init(name: "atanh", signature: "atanh(value)", description: "Calculates the inverse hyperbolic tangent."),
        .init(name: "autoplot", signature: "autoplot(y(x)) or autoplot(vector)", description: "Automatically plots functions or 2D vectors. Use 'x' for functions, 't' for parametric, or pass vectors directly."),
        .init(name: "avg", signature: "avg(a, b, ...)", description: "Calculates the average of a list of numbers or a vector/matrix."),
        .init(name: "binomdist", signature: "binomdist(k, n, p)", description: "Calculates the binomial distribution probability."),
        .init(name: "ceil", signature: "ceil(number)", description: "Rounds a number up to the nearest integer."),
        .init(name: "circum_circle", signature: "circum_circle(r)", description: "Circumference of a circle."),
        .init(name: "cmatrix", signature: "cmatrix(a, b ; c, d ; ...)", description: "Forms a complex matrix of given real or imaginary numbers."),
        .init(name: "conj", signature: "conj(complex)", description: "Calculates the complex conjugate."),
        .init(name: "corr", signature: "corr(vectorX, vectorY)", description: "Calculates the Pearson correlation coefficient between two datasets."),
        .init(name: "cos", signature: "cos(angle)", description: "Calculates the cosine of an angle."),
        .init(name: "cosh", signature: "cosh(value)", description: "Calculates the hyperbolic cosine."),
        .init(name: "count", signature: "count(data, value)", description: "Counts occurrences of a specific value in a vector or matrix."),
        .init(name: "countabove", signature: "countabove(data, threshold)", description: "Counts elements greater than a specific value."),
        .init(name: "countbelow", signature: "countbelow(data, threshold)", description: "Counts elements less than a specific value."),
        .init(name: "cov", signature: "cov(vectorX, vectorY)", description: "Calculates the sample covariance between two datasets."),
        .init(name: "ctranspose", signature: "ctranspose(matrix)", description: "Calculates the conjugate transpose (Hermitian transpose) of a complex matrix or vector. Equivalent to the \"'\" operator."),
        .init(name: "cross", signature: "cross(vectorA, vectorB)", description: "Calculates the cross product of two 3D vectors. **Example:** 'cross(vector(1;0;0), vector(0;1;0))' -> returns 'vector(0;0;1)'"),
        .init(name: "cvector", signature: "cvector(a ; b ; c ; ...)", description: "Forms a complex vector of given real or imaginary numbers."),
        .init(name: "derivative", signature: "derivative(expr, var, point, [order])", description: "Finds the instantaneous rate of change (slope). Can calculate higher-order derivatives (e.g., order 2 for concavity)."),
        .init(name: "det", signature: "det(matrix)", description: "Calculates the determinant of a square matrix."),
        .init(name: "diag", signature: "diag(matrix or vector or a, b, ...)", description: "If given a single vector, creates a diagonal matrix. If given a single matrix, extracts its main diagonal. If given multiple scalars, creates a diagonal matrix from them."),
        .init(name: "dot", signature: "dot(vectorA, vectorB)", description: "Calculates the dot product of two vectors (real or complex). Also accepts N-by-1 or 1-by-N matrices as vector inputs. **Example:** 'dot(vector(1;2;3), vector(4;5;6))'"),
        .init(name: "eig", signature: "eig(matrix)", description: "Calculates the eigenvalues and eigenvectors of a square matrix. Returns the eigenvector matrix (V) and a diagonal matrix of eigenvalues (D)."),
        .init(name: "eye", signature: "eye(n)", description: "Creates an n x n identity matrix."),
        .init(name: "fact", signature: "fact(integer)", description: "Calculates the factorial of a non-negative integer."),
        .init(name: "factor", signature: "factor(integer)", description: "Returns a vector containing the prime factors of an integer."),
        .init(name: "fft", signature: "fft(vector)", description: "Calculates the Fast Fourier Transform of a real or complex vector. The vector size must be a power of 2."),
        .init(name: "find", signature: "find(data, value)", description: "Returns a vector of 1-based indices for all occurrences of a value."),
        .init(name: "floor", signature: "floor(number)", description: "Rounds a number down to the nearest integer."),
        .init(name: "gcd", signature: "gcd(a, b, ...)", description: "Finds the greatest common divisor of a list of integers or a vector/matrix. Also performs element-wise GCD between two arguments."),
        .init(name: "geomean", signature: "geomean(a, b, ...)", description: "Calculates the geometric mean of a list of non-negative numbers or a vector/matrix."),
        .init(name: "getcolumn", signature: "getcolumn(matrix, index)", description: "Extracts a column from a matrix as a vector. Note: Index is 1-based."),
        .init(name: "getrow", signature: "getrow(matrix, index)", description: "Extracts a row from a matrix as a vector. Note: Index is 1-based."),
        .init(name: "grad", signature: "grad(f, pointVector)", description: "Calculates the gradient of a multivariable function 'f' at a specific point."),
        .init(name: "harmean", signature: "harmean(a, b, ...)", description: "Calculates the harmonic mean of a list of non-zero numbers or a vector/matrix."),
        .init(name: "hypot", signature: "hypot(sideA, sideB)", description: "Calculates the hypotenuse of a right triangle."),
        .init(name: "if", signature: "if(condition, true_val, false_val)", description: "Returns true_val if condition is non-zero, otherwise returns false_val. Works also when plotting. The types and units of true_val and false_val must be compatible. **Example:** 'if(1 > 0, 10.m, 20.m)'"),
        .init(name: "ifft", signature: "ifft(vector)", description: "Calculates the Inverse Fast Fourier Transform of a complex vector. The vector size must be a power of 2."),
        .init(name: "imag", signature: "imag(complex)", description: "Extracts the imaginary part of a complex number."),
        .init(name: "impedance", signature: "impedance(f, component)", description: "Calculates impedance Z for a component (R, L, or C) at a given frequency f. Component type is inferred from its unit (Ω, H, F). Returns a complex value."),
        .init(name: "importcsv", signature: "importcsv()", description: "Opens a file dialog to import data from a CSV file."),
        .init(name: "integral", signature: "integral(expr, var, from, to)", description: "Calculates the total area under a function's curve between two points."),
        .init(name: "inv", signature: "inv(matrix)", description: "Calculates the inverse of a square matrix."),
        .init(name: "iqr", signature: "iqr(data)", description: "Calculates the interquartile range (Q3 - Q1) of a dataset."),
        .init(name: "isprime", signature: "isprime(integer)", description: "Checks if an integer is a prime number. Returns 1 for true, 0 for false."),
        .init(name: "lcm", signature: "lcm(a, b, ...)", description: "Finds the least common multiple of a list of integers or a vector/matrix. Also performs element-wise LCM between two arguments."),
        .init(name: "lg", signature: "lg(number)", description: "Alias for the common (base-10) logarithm."),
        .init(name: "linreg", signature: "linreg(xValues, yValues)", description: "Performs a linear regression and returns the slope and y-intercept. **Example:** 'linreg(vector(1;2;3), vector(2; 3.9; 6.1))'"),
        .init(name: "linsolve", signature: "linsolve(A, b)", description: "Solves a system of linear equations Ax = b for x, where A is a matrix and b is a vector."),
        .init(name: "linspace", signature: "linspace(start, end, count)", description: "Creates a vector with 'count' evenly spaced values from 'start' to 'end'."),
        .init(name: "ln", signature: "ln(number)", description: "Calculates the natural (base-e) logarithm."),
        .init(name: "log", signature: "log(number) or log(base, number)", description: "Calculates the common (base-10) or custom base logarithm."),
        .init(name: "matrix", signature: "matrix(a, b ; c, d ; ...)", description: "Forms a matrix from numeric values. The function is unit-aware, and all elements must have compatible units."),
        .init(name: "max", signature: "max(a, b, ...)", description: "Finds the maximum value in a list of numbers or a vector/matrix."),
        .init(name: "mean", signature: "mean(a, b, ...)", description: "Calculates the arithmetic mean (average) of a list of numbers or a vector/matrix."),
        .init(name: "median", signature: "median(a, b, ...)", description: "Finds the median of a list of numbers or a vector/matrix."),
        .init(name: "min", signature: "min(a, b, ...)", description: "Finds the minimum value in a list of numbers or a vector/matrix."),
        .init(name: "mod", signature: "mod(a, b)", description: "Calculates the mathematical modulo (remainder of division)."),
        .init(name: "mode", signature: "mode(a, b, ...)", description: "Finds the most frequent value(s) in a list of numbers or a vector/matrix."),
        .init(name: "nCr", signature: "nCr(n, k)", description: "Calculates the number of combinations."),
        .init(name: "norm", signature: "norm(vector or matrix)", description: "Calculates the magnitude (Euclidean or L2 norm) of a vector or the Frobenius norm of a matrix."),
        .init(name: "nPr", signature: "nPr(n, k)", description: "Calculates the number of permutations."),
        .init(name: "nsolve", signature: "nsolve(equation, var, [guess])", description: "Alias for solve. Numerically finds real root(s) of an equation."),
        .init(name: "normdist", signature: "normdist(x, mean, stddev)", description: "Calculates the normal distribution probability density."),
        .init(name: "ones", signature: "ones(rows, [cols])", description: "Creates a vector or matrix filled with ones."),
        .init(name: "ode45", signature: "ode45(f, t_span, y0)", description: "Solves a system of ordinary differential equations dy/dt = f(t, y). 'f' is a user function that takes (t, y), 't_span' is a vector '[t_start, t_end]', and 'y0' is the initial state vector. Returns the time and state solution."),
            .init(name: "ones", signature: "ones(rows, [cols])", description: "Creates a vector or matrix filled with ones."),
        .init(name: "percentile", signature: "percentile(data, p)", description: "Calculates the p-th percentile of a dataset (e.g., percentile(data, 75))."),
        .init(name: "plot", signature: "plot(expr, var, x_min, x_max, [y_min, y_max])", description: "Plots expressions over a specified range with optional y-axis limits."),
        .init(name: "polar", signature: "polar(complex)", description: "Converts a complex number to its polar form (R ∠ θ)."),
        .init(name: "polyfit", signature: "polyfit(x_vec, y_vec, degree)", description: "Fits a polynomial of the given degree to the data points and returns the unit-aware coefficients. **Example:** 'polyfit(x_data, y_data, 2)' -> returns quadratic fit coefficients."),
        .init(name: "powerspectrum", signature: "powerspectrum(data, Fs)", description: "Calculates and plots the power spectrum of a signal. 'data' is the time-domain vector and 'Fs' is the sampling rate in Hz."),
        .init(name: "quartile", signature: "quartile(data, q)", description: "Calculates the q-th quartile (1, 2, or 3) of a dataset."),
        .init(name: "random", signature: "random([max], [min, max], [min, max, count])", description: "Generates random numbers. 'random()' gives a float in [0,1]. 'random(max)' gives an integer in [1, max]. 'random(min, max)' gives an integer in [min, max]. 'random(min, max, count)' returns a vector of integers."),
        .init(name: "randm", signature: "randm(rows, cols)", description: "Creates a matrix with the specified dimensions, filled with random numbers between 0 and 1."),
        .init(name: "randv", signature: "randv(size)", description: "Creates a column vector of the specified size, filled with random numbers between 0 and 1."),
        .init(name: "range", signature: "range(start, end, [step])", description: "Creates a vector from 'start' to 'end' with an optional 'step' (default is 1)."),
        .init(name: "rank", signature: "rank(matrix)", description: "Calculates the rank of a matrix."),
        .init(name: "real", signature: "real(complex)", description: "Extracts the real part of a complex number."),
        .init(name: "rmse", signature: "rmse(vectorA, vectorB)", description: "Calculates the root-mean-square error between two datasets."),
        .init(name: "rmsd", signature: "rmsd(vectorA, vectorB)", description: "Alias for rmse. Calculates the root-mean-square deviation."),
        .init(name: "root", signature: "root(number, degree)", description: "Calculates the nth root of a number."),
        .init(name: "round", signature: "round(number)", description: "Rounds a number to the nearest integer."),
        .init(name: "scatterplot", signature: "scatterplot(x, y, [degree])", description: "Creates a scatter plot. Optionally fits a polynomial of the given degree (e.g., 1 for linear).\n**Example:** 'scatterplot(vector(1;2;3), vector(2;5;6), 1)'"),
        .init(name: "side", signature: "side(hyp, sideA)", description: "Calculates the missing side of a right triangle."),
        .init(name: "sin", signature: "sin(angle)", description: "Calculates the sine of an angle."),
        .init(name: "sinh", signature: "sinh(value)", description: "Calculates the hyperbolic sine."),
        .init(name: "solve", signature: "solve(equation, var, [guess])", description: "Numerically finds real root(s) of an equation for a variable. Searches a wide range for solutions. Providing a guess will focus the search around that value.\n**Example:** 'solve(x^2 == 9, x)'\n**With Units (Guess Required):** 'solve(F/2.kg == 9.8.m/.s^2, F, 1.N)'"),
        .init(name: "sort", signature: "sort(vector, [\"asc\" or \"desc\"])", description: "Sorts a vector in ascending (default) or descending order."),
        .init(name: "sqrt", signature: "sqrt(number)", description: "Calculates the square root. Handles complex numbers."),
        .init(name: "stddev", signature: "stddev(a, b, ...)", description: "Calculates the sample standard deviation."),
        .init(name: "stddevp", signature: "stddevp(a, b, ...)", description: "Calculates the population standard deviation."),
        .init(name: "sum", signature: "sum(a, b, ...)", description: "Calculates the sum of a list of numbers or a vector/matrix."),
        .init(name: "tan", signature: "tan(angle)", description: "Calculates the tangent of an angle."),
        .init(name: "tanh", signature: "tanh(value)", description: "Calculates the hyperbolic tangent."),
        .init(name: "trace", signature: "trace(matrix)", description: "Calculates the trace of a square matrix (sum of diagonal elements)."),
        .init(name: "transpose", signature: "transpose(matrix)", description: "Transposes a matrix (rows become columns). For complex matrices, this performs a simple transpose without conjugation. For the conjugate transpose, use the 'ctranspose' function or the \"'\" operator."),
        .init(name: "uncert", signature: "uncert(value, name: value, ...)", description: "Creates a value with uncertainty. Use named arguments like 'random', 'resolution', 'accuracy'. Calculations propagate errors automatically.\n**Example:** 'g := uncert(9.8.m/.s^2, random: 0.1.m/.s^2)'"),
        .init(name: "unique", signature: "unique(data)", description: "Returns a vector of the unique elements from a vector or matrix."),
        .init(name: "unit", signature: "unit(vector)", description: "Returns the unit vector (vector with magnitude 1)."),
        .init(name: "variance", signature: "variance(a, b, ...)", description: "Calculates the sample variance."),
        .init(name: "vector", signature: "vector(a, b, ...) or vector(a; b; ...)", description: "Creates a vector. Use commas for a row vector (e.g., 'vector(1,2,3)') or semicolons for a column vector (e.g., 'vector(1;2;3)'). The function is unit-aware."),
        .init(name: "vol_cone", signature: "vol_cone(r, h)", description: "Volume of a cone for radius r and height h."),
        .init(name: "vol_cube", signature: "vol_cube(s)", description: "Volume of a cube for side length s."),
        .init(name: "vol_cylinder", signature: "vol_cylinder(r, h)", description: "Volume of a cylinder for radius r and height h."),
        .init(name: "vol_sphere", signature: "vol_sphere(r)", description: "Volume of a sphere for radius r."),
        .init(name: "zeros", signature: "zeros(rows, [cols])", description: "Creates a vector or matrix filled with zeros."),
        
        .init(name: "sec", signature: "sec(angle)", description: "Calculates the secant."),
        .init(name: "csc", signature: "csc(angle)", description: "Calculates the cosecant."),
        .init(name: "cot", signature: "cot(angle)", description: "Calculates the cotangent."),
        .init(name: "asec", signature: "asec(value)", description: "Calculates the inverse secant."),
        .init(name: "acsc", signature: "acsc(value)", description: "Calculates the inverse cosecant."),
        .init(name: "acot", signature: "acot(value)", description: "Calculates the inverse cotangent."),
        .init(name: "sech", signature: "sech(value)", description: "Calculates the hyperbolic secant."),
        .init(name: "csch", signature: "csch(value)", description: "Calculates the hyperbolic cosecant."),
        .init(name: "coth", signature: "coth(value)", description: "Calculates the hyperbolic cotangent."),
        .init(name: "asech", signature: "asech(value)", description: "Calculates the inverse hyperbolic secant."),
        .init(name: "acsch", signature: "acsch(value)", description: "Calculates the inverse hyperbolic cosecant."),
        .init(name: "acoth", signature: "acoth(value)", description: "Calculates the inverse hyperbolic cotangent."),
        .init(name: "deg2rad", signature: "deg2rad(degrees)", description: "Converts an angle from degrees to radians."),
        .init(name: "rad2deg", signature: "rad2deg(radians)", description: "Converts an angle from radians to degrees."),
        .init(name: "sign", signature: "sign(value)", description: "Returns -1 for negative, 1 for positive, 0 for zero.")
    ]
    
    static let physicalConstants: [PhysicalConstant] = [
        .init(symbol: "amu", name: "Atomic Mass Unit", value: 1.66053906660e-27),
        .init(symbol: "atm", name: "Standard atmosphere", value: 101325),
        .init(symbol: "b", name: "Wien's displacement", value: 2.897771955e-3),
        .init(symbol: "c", name: "Speed of light", value: 299792458),
        .init(symbol: "c0", name: "Speed of sound (STP)", value: 343),
        .init(symbol: "e0", name: "Elementary charge", value: 1.602176634e-19),
        .init(symbol: "g", name: "Standard gravity", value: 9.80665),
        .init(symbol: "G", name: "Gravitational constant", value: 6.67430e-11),
        .init(symbol: "h", name: "Planck constant", value: 6.62607015e-34),
        .init(symbol: "hbar", name: "Reduced Planck constant", value: 1.054571817e-34),
        .init(symbol: "kB", name: "Boltzmann constant", value: 1.380649e-23),
        .init(symbol: "me", name: "Electron mass", value: 9.1093837015e-31),
        .init(symbol: "mn", name: "Neutron mass", value: 1.67492749804e-27),
        .init(symbol: "mp", name: "Proton mass", value: 1.67262192369e-27),
        .init(symbol: "ME", name: "Earth mass", value: 5.972e24),
        .init(symbol: "NA", name: "Avogadros constant", value: 6.02214076e23),
        .init(symbol: "R", name: "Gas constant", value: 8.314462618),
        .init(symbol: "Rinf", name: "Rydberg constant", value: 10973731.568160),
        .init(symbol: "RE", name: "Earth radius", value: 6.371e6),
        .init(symbol: "Vm", name: "Molar volume (STP)", value: 22.41396954e-3),
        .init(symbol: "ε0", name: "Vacuum permittivity", value: 8.8541878128e-12),
        .init(symbol: "μ0", name: "Vacuum permeability", value: 1.25663706212e-6),
        .init(symbol: "μB", name: "Bohr magneton", value: 9.2740100783e-24),
        .init(symbol: "μN", name: "Nuclear magneton", value: 5.0507837461e-27),
        .init(symbol: "π", name: "Pi", value: Double.pi),
        .init(symbol: "σ", name: "Stefan-Boltzmann", value: 5.670374419e-8)
    ]

    static let helpTopics: [HelpTopic] = [
        .init(title: "Syntax Guide", content: """
        A comprehensive guide to the ModernCalc syntax.

        **Variables & Functions**
        - **Assignment:** Use `:=` to assign variables and define functions.
          - `radius := 5.m`
          - `area(r) := pi * r^2`
        - **Last Answer:** Use the special variable `ans` to reference the result of the last calculation.

        **Data Types**
        - **Vectors:** Use `vector()` with semicolons for column vectors or commas for row vectors.
          - `vector(1; 2; 3)`
          - `matrix(4, 5, 6)` (row vector is a 1xN matrix)
        - **Matrices:** Use `matrix()` with commas for columns and semicolons for rows.
          - `matrix(1, 2; 3, 4)`
        - **Complex Numbers:** Use `i` for the imaginary unit.
          - `3 + 4i`
          - `5∠53.13` (polar form, respects DEG/RAD mode)

        **Units**
        - **Attaching Units:** Attach a unit to a number using a dot (`.`).
          - `10.kg`
          - `25.s`
        - **Compound Units:** Create compound units using standard operators. The dot (`.`) has high precedence and binds the unit to the number before other operations occur.
          - `9.81.m/.s^2` (Parsed as `(9.81 * .m) / (.s^2)`)
          - `100.kW*.h`
        - **Unit Conversion:** Use the `in` operator to convert between compatible dimensions.
          - `10.m/.s in .km/.h`
          - `1.acre in .m^2`

        **Operators**
        - **Standard:** `+ - * / ^ %`
        - **Element-wise:** For vectors/matrices, use `.*` and `./` for element-by-element operations.
        - **Factorial & Transpose:** Use postfix operators `!` and `'`.
          - `5!`
          - `my_matrix'`
        - **Element Modification:** Use `@` operators to change a single vector element. For example, `v .=@(2, 99)` sets the 2nd element of vector `v` to 99.
        - **Comparisons:** `==, !=, >, <, >=, <=` return `1.0` for true and `0.0` for false.
        - **Logical:** `&&` (AND), `||` (OR), `~` (NOT).
        """),
        .init(title: "Quick Start", content: "Enter a mathematical expression to see a live result. Press **Enter** to add it to your history. Use the `ans` variable to reference the last result. Use the arrow keys or your mouse to navigate your history and reuse previous equations, results or plots."),
        .init(title: "Variables & Functions", content: "Assign a variable using `:=`, like `x := 5*2`. Variables are saved automatically. \nDefine custom functions with parameters, like `f(x, y) := x^2 + y^2`. You can then call them like any built-in function: `f(3, 4)`."),
        .init(title: "Units & Conversions", content: "Attach units to numbers using a dot, like `20.m` or `9.8.m/.s^2`. You can convert between compatible units using the `in` operator, for example: `1.year in .day`."),
        .init(title: "Uncertainty", content: "Create a value with uncertainty using `uncert(value, random: 0.1, resolution: 0.05)`. Calculations will propagate errors according to standard rules: random (statistical) errors are combined in quadrature, and systematic errors (from resolution and accuracy) are also combined in quadrature.\n- **value**: The nominal value of the measurement.\n- **random** (or **r**): (Type A) The statistical uncertainty, like a standard deviation.\n- **resolution** (or **res**): (Type B) Systematic uncertainty from instrument resolution.\n- **accuracy** (or **a**): (Type B) Systematic uncertainty from instrument accuracy."),
        .init(title: "Operators", content: "Supports standard operators `+ - * / ^ %` as well as logical and comparison operators like `&&`, `||`, `~`, `>`, `==`, etc., which return 1.0 for true and 0.0 for false. For element-wise vector/matrix operations, use `.*` and `./`. You can modify a single vector element using operators like `.=@` (set), `.+@` (add to), etc., with the syntax `vector_expression .op@ (index, value)`. The `!` operator calculates factorial, and `'` transposes a matrix. For complex matrices, `'` performs the conjugate transpose."),
        .init(title: "Data Types", content: "**Complex Numbers:** Use `i` for the imaginary unit (e.g., `3 + 4i`). \n**Vectors:** Create with `vector(1; 2; 3)`. \n**Matrices:** Create with `matrix(1, 2; 3, 4)`, using commas for columns and semicolons for rows. \n**Polar Form:** Enter complex numbers with `R∠θ` (e.g., `5∠53.13` in degree mode)."),
        .init(title: "Linear Algebra", content: "Solve systems of linear equations of the form `Ax = b` with `linsolve(A, b)`. Standard matrix operations like inverse (`inv`), determinant (`det`), trace (`trace`), and rank (`rank`) are also available. Create identity, zero, or one matrices with `eye`, `zeros`, and `ones`."),
        .init(title: "Plotting & Data Analysis", content: "**Function Plotting:** Use `autoplot(sin(x))` for quick graphs, or `plot(expr, var, x_min, x_max)` for detailed control. \n**Scatter Plots:** Visualize data with `scatterplot(x_vector, y_vector)`. You can add an optional third argument for the degree of a polynomial fit, e.g., `scatterplot(x, y, 1)` for a linear fit or `scatterplot(x, y, 2)` for a quadratic fit. \n**Regression:** Use `polyfit(x_vector, y_vector, degree)` to get the coefficients of a best-fit polynomial."),
        .init(title: "Data Querying", content: "Ask questions about your data with functions like `count`, `countabove`, `countbelow`, and `find`. For example, `count(my_data, 5)` finds the number of 5s, and `find(my_data, 5)` returns their indices."),
        .init(title: "CSV Import & Data Handling", content: "Use the `importcsv()` command or the \".csv\" button to open a file dialog. The interactive window allows you to select which rows and columns to import, and then assign the selected data to a matrix variable. \nOnce your data is in a matrix, you can extract a specific column or row into a vector using the `getcolumn(matrix, index)` and `getrow(matrix, index)` functions. **Note:** All indices are 1-based."),
        .init(title: "Calculus", content: "Calculate derivatives with `derivative(...)` using the central finite difference method. \nCalculate definite integrals with `integral(...)` using adaptive Simpson's method, which is efficient for smooth functions.\nCalculate the gradient of a multi-variable function `g` with `grad(g, ...)`."),
        .init(title: "Equation Solving", content: """
        The `solve` command numerically finds real roots for an equation. A "root" is a value of a variable that makes the equation true.

        **Syntax:** `solve(equation, variable, [optional guess])`

        - **equation:** The equation to solve. It must contain a comparison, usually `==`.
        - **variable:** The variable you want to solve for.
        - **guess (optional):** A number to start the search near. This is useful for equations with multiple solutions or to speed up the search.

        **How It Works**
        The solver works by finding where the expression `left_side - right_side` equals zero. It automatically searches a wide range of numbers (-1000 to 1000) to find all possible solutions. If you provide a guess, it will focus its search in a smaller range around that number.

        **Basic Example:**
        To solve $x^2 = 9$:
        `solve(x^2 == 9, x)`
        *Result: -3, 3*

        **Advanced Example:**
        Find where a sine wave intersects a line:
        `solve(sin(x) == x/2, x)`
        *Result: -1.895, 0, 1.895*

        If you only wanted the positive root, you could provide a guess:
        `solve(sin(x) == x/2, x, 2)`
        *Result: 1.895*

        **Solving with Units:**
        To solve an equation with units, you **must provide a guess** with the correct units.
        `solve(F/2.kg == 9.8.m/.s^2, F, 1.N)`
        *Result: 19.6 N*

        **Troubleshooting:**
        - **No solutions found:** The equation may not have any real roots, or the roots may be outside the default search range. Try providing a guess or plotting the function to visualize where the roots might be.
        - **Slow performance:** For very complex equations, the search can be slow. Providing a guess will significantly speed up the process.
        """),
        .init(title: "Statistics & Number Theory", content: "Perform statistical analysis with functions like `sum`, `avg`, `stddev`, `variance`, `median`, `percentile`, `quartile`, `iqr`, `corr`, and `sort`. Generate datasets using `range` or `linspace`. Number theory functions like `isprime`, `factor`, `gcd`, and `lcm` are also available.")
    ]
    
    static let siPrefixes: Set<String> = [
        "yotta", "zetta", "exa", "peta", "tera", "giga", "mega", "kilo", "hecto", "deca", "deci",
        "centi", "milli", "micro", "nano", "pico", "femto", "atto", "zepto", "yocto"
    ]
    
    static let operatorSymbols: [MathSymbol] = [
        .init(symbol: "±", name: "Plus-Minus"), .init(symbol: "∠", name: "Angle"), .init(symbol: "√", name: "Square Root", insertionText: "√("),
        .init(symbol: "×", name: "Multiply"), .init(symbol: "÷", name: "Divide"), .init(symbol: "^", name: "Power"),
        .init(symbol: "!", name: "Factorial"),
        .init(symbol: "'", name: "Transpose"), .init(symbol: ".*", name: "Element-wise Multiply"), .init(symbol: "./", name: "Element-wise Divide"),
        .init(symbol: ".=@", name: "Set Element", insertionText: ".=@(index, value)"),
        .init(symbol: ".+@", name: "Add to Element", insertionText: ".+@(index, value)"),
        .init(symbol: ".-@", name: "Subtract from Element", insertionText: ".-@(index, value)"),
        .init(symbol: ".*@", name: "Multiply Element", insertionText: ".*@(index, value)"),
        .init(symbol: "./@", name: "Divide Element", insertionText: "./@(index, value)"),
        // Comparison and Logical Operators
        .init(symbol: "==", name: "Equal to"), .init(symbol: "!=", name: "Not Equal to"), .init(symbol: ">", name: "Greater Than"),
        .init(symbol: "<", name: "Less Than"), .init(symbol: ">=", name: "Greater Than or Equal to"), .init(symbol: "<=", name: "Less Than or Equal to"),
        .init(symbol: "&&", name: "Logical AND"), .init(symbol: "||", name: "Logical OR"), .init(symbol: "~", name: "Logical NOT")
    ]
    
    static let greekSymbols: [MathSymbol] = [
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
}
