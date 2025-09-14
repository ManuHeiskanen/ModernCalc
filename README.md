# ModernCalc

ModernCalc is not just another calculator; it's a powerful and intuitive mathematical environment for macOS designed for students, engineers, and scientists. By blending the simplicity of a text-based interface with a robust evaluation engine, ModernCalc streamlines complex calculations, data analysis, and visualization.

*"Future Image here"*

## Why ModernCalc?

- **Live & Intuitive**: Get instant feedback with a live-updating LaTeX preview of your expression and its result as you type. No more guessing if you entered the formula correctly.  
- **Powerful Math Engine**: Go beyond basic arithmetic. ModernCalc supports variables, complex numbers, vectors, matrices, derivatives, integrals, and a comprehensive unit system.  
- **Built-in Plotting**: Visualize your functions and data effortlessly. Create function plots, parametric equations, and scatter plots with polynomial fitting—all with simple commands that open in a dedicated, interactive plot window.  
- **Data-Aware**: Import data directly from .csv files into matrix variables. Preview your data, select columns, change delimiters, and start your analysis in seconds.  
- **A Familiar Feel**: Keep your hands on the keyboard. The persistent history, variable assignments (`x := 5`), and function definitions (`f(x) := x^2`) create a workflow that feels like a classic command-line math tool, but with the beauty and usability of a modern Mac app.  

## Core Features

- **Real-time LaTeX Preview**: See your math beautifully rendered on the fly.  
- **Calculation History**: Easily access, modify, and reuse previous calculations with arrow-key navigation.  
- **Variable & Function Support**: Assign values to variables and define your own custom functions.  

`radius:=2.5`
`area(r):=pi*r^2`
`area(radius)`

- **Advanced Data Types**: Native support for:  
- **Complex Numbers**: `(2 + 3i) * (1 - i)`  
- **Vectors & Matrices**: `[1; 2; 3] + [4; 5; 6]`  
- **Physical Units & Constants**:
  `2.5.m + 50.cm -> 3.m`
  `6.ft in .cm -> 182.88 cm`

- **Calculus & Analysis**:  
- Derivatives: `derivative(x^3, x, 2)`  
- Integrals: `integral(sin(x), x, 0, pi)`  
- Polynomial Fitting: `polyfit(x_data, y_data, 2)`  

- **Powerful Plotting Engine**:  
- Function Plot: `plot(sin(x) + cos(2*x), x=-5..5)`  
- Automatic Plotting: `autoplot(x^2, -x^2)`  
- Scatter Plots from CSV: `scatterplot(my_csv_matrix)`  

- **CSV Data Import**:  
- Use the `.csv` button or type `importcsv()` to launch the importer.  
- Visually select columns and rows to import as a matrix.  

---

## How to Download and Install

1. Go to the [ModernCalc Releases page](https://github.com/ManuHeiskanen/ModernCalc/releases).  
2. Download the latest `.zip` file from the list of releases.  
3. Extract the downloaded `.zip` file.  
4. Inside, you will find the **ModernCalc.app** file.  
5. Drag and drop **ModernCalc.app** into your **Applications** folder.  
6. Open it like any other macOS application. If/when macOS shows a security warning, do what the "info" section on your device says to do. This will only need to be done once.

---

ModernCalc is the perfect tool for anyone who wants to calculate, analyze, and visualize, all within a single, elegant application.
