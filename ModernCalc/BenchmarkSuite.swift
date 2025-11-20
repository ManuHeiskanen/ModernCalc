//
//  BenchmarkSuite.swift
//  ModernCalc
//
//  Created by Manu Heiskanen on 20.11.2025.
//


import Foundation

class BenchmarkSuite {
    
    /// Runs all registered benchmarks with multiple iterations and averaging.
    static func runAll() async {
        print("----------- STARTING BENCHMARKS (Average of 5 runs) -----------")
        
        await measure(name: "Parsing", runs: 5) {
            await benchmarkParser()
        }
        
        await measure(name: "Evaluation Loop (Tree-Walk)", runs: 5) {
            await benchmarkEvaluatorLoop()
        }
        
        await measure(name: "Matrix Multiplication (100x100)", runs: 5) {
            await benchmarkMatrixOperations()
        }
        
        await measure(name: "Large Vector Ops (1,000,000 elements)", runs: 5) {
            await benchmarkLargeVectorOps()
        }
        
        
        await measure(name: "Linear System Solve (100x100)", runs: 5) {
            await benchmarkLinearSolve()
        }
        
        await measure(name: "Eigenvalue Decomposition (50x50)", runs: 5) {
            await benchmarkEigenvalue()
        }
        
        await measure(name: "Numerical Integration", runs: 5) {
            await benchmarkIntegration()
        }
        
        
        await measure(name: "FFT (Power Spectrum - 2048 pts)", runs: 5) {
            await benchmarkFFT()
        }
        
        await measure(name: "Matrix Inversion (100x100)", runs: 5) {
            await benchmarkMatrixInversion()
        }
        
        print("---------------------------------------------------------------")
    }
    
    static func measure(name: String, runs: Int, block: () async -> Void) async {
        print("\n[\(name)]")
        var totalTime: Double = 0
        
        // Warmup run
        await block()
        
        for i in 1...runs {
            let start = CFAbsoluteTimeGetCurrent()
            await block()
            let duration = CFAbsoluteTimeGetCurrent() - start
            totalTime += duration
            print("    Run \(i): \(String(format: "%.4f", duration))s")
        }
        
        let avg = totalTime / Double(runs)
        print("    >> Average: \(String(format: "%.4f", avg))s")
    }
    
    // MARK: - Benchmarks
    
    static func benchmarkParser() async {
        let expression = "sin(x) * (x^2 + 2*x + 1) / (cos(x) + 2)"
        let lexer = Lexer(input: expression)
        let tokens = lexer.tokenize()
        
        for _ in 0..<10_000 {
            let parser = Parser(tokens: tokens)
            _ = try? parser.parse()
        }
    }
    
    static func benchmarkEvaluatorLoop() async {
        let expression = "x^2 + sin(x) * cos(x)"
        let lexer = Lexer(input: expression)
        let tokens = lexer.tokenize()
        let parser = Parser(tokens: tokens)
        let node = try! parser.parse()
        
        let evaluator = Evaluator()
        var variables: [String: MathValue] = ["x": .dimensionless(0)]
        var functions: [String: FunctionDefinitionNode] = [:]
        
        for i in 0..<50_000 {
            variables["x"] = .dimensionless(Double(i) * 0.01)
            _ = try? evaluator.evaluate(node: node, variables: &variables, functions: &functions, angleMode: .radians)
        }
    }
    
    static func benchmarkMatrixOperations() async {
        // Increased to 100x100 to really stress the BLAS implementation
        let size = 100
        let values = Array(repeating: 1.5, count: size * size)
        let matrixA = Matrix(values: values, rows: size, columns: size)
        let matrixB = Matrix(values: values, rows: size, columns: size)
        
        let valA = MathValue.matrix(matrixA)
        let valB = MathValue.matrix(matrixB)
        
        let evaluator = Evaluator()
        let opToken = Token(type: .op("*"), rawValue: "*")
        
        for _ in 0..<50 {
            _ = try? evaluator.evaluateBinaryOperation(op: opToken, left: valA, right: valB)
        }
    }
    
    static func benchmarkLargeVectorOps() async {
        // 1 Million elements. vDSP should crush this vs standard map.
        let size = 1_000_000
        let values = Array(repeating: 1.001, count: size)
        let v1 = Vector(values: values)
        let v2 = Vector(values: values)
        
        let valA = MathValue.vector(v1)
        let valB = MathValue.vector(v2)
        let evaluator = Evaluator()
        let opToken = Token(type: .op("+"), rawValue: "+") // Vector Addition
        let opToken2 = Token(type: .op("*"), rawValue: ".*") // Element-wise Mult
        
        for _ in 0..<10 {
            _ = try? evaluator.evaluateBinaryOperation(op: opToken, left: valA, right: valB)
            _ = try? evaluator.evaluateBinaryOperation(op: opToken2, left: valA, right: valB)
        }
    }
    
    static func benchmarkMatrixInversion() async {
        // 100x100 Inversion. Previously impossible/slow. Now fast via LAPACK.
        let size = 100
        var values = [Double]()
        for i in 0..<size {
            for j in 0..<size {
                values.append(i == j ? Double(size) : Double.random(in: 0...1))
            }
        }
        let m = Matrix(values: values, rows: size, columns: size)
        
        for _ in 0..<20 {
            _ = try? m.inverse()
        }
    }

    static func benchmarkLinearSolve() async {
        let size = 100
        var aValues = [Double]()
        for i in 0..<size {
            for j in 0..<size {
                aValues.append(i == j ? Double.random(in: 50...100) : Double.random(in: 0...1))
            }
        }
        
        let matrixA = Matrix(values: aValues, rows: size, columns: size)
        let vectorB = Vector(values: (0..<size).map { _ in Double.random(in: 0...10) })
        
        let evaluator = Evaluator()
        var variables: [String: MathValue] = ["A": .matrix(matrixA), "b": .vector(vectorB)]
        var functions: [String: FunctionDefinitionNode] = [:]
        
        let expression = "linsolve(A, b)"
        let lexer = Lexer(input: expression)
        let tokens = lexer.tokenize()
        let parser = Parser(tokens: tokens)
        let node = try! parser.parse()

        for _ in 0..<50 {
            _ = try? evaluator.evaluate(node: node, variables: &variables, functions: &functions, angleMode: .radians)
        }
    }
    
    static func benchmarkEigenvalue() async {
        let size = 50
        var values = [Double]()
        for _ in 0..<(size*size) { values.append(Double.random(in: -1...1)) }
        let m = Matrix(values: values, rows: size, columns: size)
        
        let evaluator = Evaluator()
        for _ in 0..<20 {
            _ = try? evaluator.performEigenvalueDecomposition(matrix: m)
        }
    }
    
    static func benchmarkIntegration() async {
        let expression = "integral(sin(x) * x^2, x, 0, 10)"
        let lexer = Lexer(input: expression)
        let tokens = lexer.tokenize()
        let parser = Parser(tokens: tokens)
        let node = try! parser.parse()
        
        let evaluator = Evaluator()
        var variables: [String: MathValue] = [:]
        var functions: [String: FunctionDefinitionNode] = [:]
        
        for _ in 0..<100 {
             _ = try? evaluator.evaluate(node: node, variables: &variables, functions: &functions, angleMode: .radians)
        }
    }
    
    static func benchmarkFFT() async {
        let size = 2048
        let vector = Vector(values: (0..<size).map { _ in Double.random(in: -1...1) })
        let evaluator = Evaluator()
        var variables: [String: MathValue] = ["v": .vector(vector)]
        var functions: [String: FunctionDefinitionNode] = [:]
        let expression = "powerspectrum(v, 44100)"
        let lexer = Lexer(input: expression)
        let parser = Parser(tokens: lexer.tokenize())
        let node = try! parser.parse()
        
        for _ in 0..<100 {
            _ = try? evaluator.evaluate(node: node, variables: &variables, functions: &functions, angleMode: .radians)
        }
    }
}
