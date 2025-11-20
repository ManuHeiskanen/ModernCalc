# ModernCalc
![Demo Image](https://github.com/MHeis22/ModernCalc/blob/main/ModernCalc/Assets.xcassets/DemoImg.png)


**ModernCalc** is an intuitive and powerful mathematical environment for **macOS**, designed for students, engineers, and scientists.  

By blending the simplicity of a text-based interface with a robust evaluation engine that understands physical units, ModernCalc streamlines complex calculations, data analysis, and visualization.

---

## Why ModernCalc?

### Think on the Page  

Get instant feedback with a live-updating LaTeX preview of your expression and its result. The text-based workflow lets you build calculations naturally, without ever leaving the keyboard.

### Calculate with Confidence  

Go beyond basic arithmetic. ModernCalc's powerful engine handles variables, complex numbers, vectors, matrices, calculus, and a comprehensive unit system with dimensional analysis. Errors in unit conversion are a thing of the past.

### Propagate Uncertainty  

Define values with experimental uncertainty using the `uncert()` function. All subsequent calculations will automatically and correctly propagate both random and systematic errors.

### Visualize Instantly  

Turn data and functions into insight. Create function plots, parametric equations, and scatter plots with polynomial fitting using simple commands. Each plot opens in a dedicated, interactive window.

### Analyze Real Data  

Import data directly from `.csv` files into matrix variables. Preview your data, select columns, fix delimiters, and start your analysis in seconds.

---

## Core Features

### Fundamental Workflow

- **Live LaTeX Preview**: See your math beautifully rendered as you type.  

- **Calculation History**: Access, modify, and reuse previous calculations with arrow-key navigation.  

- **Variable & Function Support**: Assign values to variables (`x := 5.kg`) and define your own custom, multi-variable functions (`f(x) := x^2`).  

### Advanced Data Types

- **Complex Numbers**: Full support for complex arithmetic, including polar form (`5∠53.13`).  

- **Vectors & Matrices**: `vector(1; 2; 3) + vector(4; 5; 6)`.  

- **Physical Units & Constants**: A comprehensive unit system prevents errors.  

Examples:

`2.5.m + 50.cm -> 3 m`
`9.81.m/.s^2 * 10.kg -> 98.1 N`
`1.acre in .m^2 -> 4046.86 m^2`

- **Uncertainty Propagation**: Handle experimental data with confidence.  

Examples:

`v := uncert(9.8.m/.s, random: 0.2.m/.s)`
`d := uncert(2.5.m, accuracy: 0.1.m)`
`d/v -> (0.26 ± 0.01) s`

---

## Powerful Computation

### Calculus & Analysis

- **Derivatives**: `derivative(x^3, x, 2)`  

- **Integrals**: `integral(sin(x), x, 0, pi)`  

- **Gradients**: `grad(f, vector(1,2))`  

### Equation Solving

Numerically find real roots for equations, with or without units:  

`solve(x^2 == 9, x)`
`solve(P/1.m^2 == 1.atm, P, 1.N)`

### Statistics

A rich library of functions including `mean`, `stddev`, `corr`, `polyfit`, and more.

---

## Data Analysis & Visualization

### Flexible Plotting Engine

`plot(sin(x), x, -pi, pi)`
`autoplot(x^2, -x^2)`
`scatterplot(my_data_x, my_data_y, 1) (with linear fit)`

### Interactive CSV Importer

- Use the `file` button or `importcsv()` to launch the importer.  

- Visually select columns and rows to import as a matrix.  

---

## How to Download and Install

1. Go to the **ModernCalc Releases** page.  

2. Download the latest `.zip` file from the list of releases.  

3. Unzip the downloaded file.  

4. Drag **ModernCalc.app** into your **Applications** folder.  

5. On first launch, you may need to right-click the app and select **Open** from the context menu to bypass macOS security warnings. This only needs to be done once.  

---

**ModernCalc** is the perfect tool for anyone who wants to **calculate, analyze, and visualize**—all within a single, elegant application.


