import Foundation

class BenchmarkSuite {
    
    static func runAll() async {
        print("----------- STARTING BENCHMARKS -----------")
        await benchmarkParser()
        await benchmarkEvaluatorLoop()
        await benchmarkMatrixOperations()
        await benchmarkPlottingSimulation()
        print("-------------------------------------------")
    }
    
    static func benchmarkParser() async {
        print("\n[1] Parsing Benchmark (10,000 iterations)")
        let expression = "sin(x) * (x^2 + 2*x + 1) / (cos(x) + 2)"
        let lexer = Lexer(input: expression)
        let tokens = lexer.tokenize()
        
        let start = CFAbsoluteTimeGetCurrent()
        
        for _ in 0..<10_000 {
            let parser = Parser(tokens: tokens)
            _ = try? parser.parse()
        }
        
        let duration = CFAbsoluteTimeGetCurrent() - start
        print("    Time: \(String(format: "%.4f", duration))s")
    }
    
    static func benchmarkEvaluatorLoop() async {
        print("\n[2] Evaluation Loop (Tree-Walk) (50,000 iterations)")
        // Simulates evaluating a function repeatedly (like in a loop or integral)
        let expression = "x^2 + sin(x) * cos(x)"
        let lexer = Lexer(input: expression)
        let tokens = lexer.tokenize()
        let parser = Parser(tokens: tokens)
        let node = try! parser.parse()
        
        let evaluator = Evaluator()
        var variables: [String: MathValue] = ["x": .dimensionless(0)]
        var functions: [String: FunctionDefinitionNode] = [:]
        
        let start = CFAbsoluteTimeGetCurrent()
        
        for i in 0..<50_000 {
            variables["x"] = .dimensionless(Double(i) * 0.01)
            // We simulate the overhead of passing variables inout
            _ = try? evaluator.evaluate(node: node, variables: &variables, functions: &functions, angleMode: .radians)
        }
        
        let duration = CFAbsoluteTimeGetCurrent() - start
        print("    Time: \(String(format: "%.4f", duration))s")
    }
    
    static func benchmarkMatrixOperations() async {
        print("\n[3] Matrix Multiplication (50x50) (10 iterations)")
        // Creating two 50x50 matrices manually
        let size = 50
        let values = Array(repeating: 1.5, count: size * size)
        let matrixA = Matrix(values: values, rows: size, columns: size)
        let matrixB = Matrix(values: values, rows: size, columns: size)
        
        let valA = MathValue.matrix(matrixA)
        let valB = MathValue.matrix(matrixB)
        
        let evaluator = Evaluator()
        let opToken = Token(type: .op("*"), rawValue: "*")
        
        let start = CFAbsoluteTimeGetCurrent()
        
        for _ in 0..<10 {
            _ = try? evaluator.evaluateBinaryOperation(op: opToken, left: valA, right: valB)
        }
        
        let duration = CFAbsoluteTimeGetCurrent() - start
        print("    Time: \(String(format: "%.4f", duration))s")
    }
    
    static func benchmarkPlottingSimulation() async {
        print("\n[4] Plotting Data Generation Overhead")
        // This tests the specific overhead found in EvaluatorAdvancedMath.swift
        // where variables are copied for every data point in concurrentPerform.
        
        let iterations = 100_000
        let variables: [String: MathValue] = [
            "a": .dimensionless(1), "b": .dimensionless(2),
            "c": .dimensionless(3), "d": .dimensionless(4),
            "x": .dimensionless(0)
        ]
        
        let start = CFAbsoluteTimeGetCurrent()
        
        // Simulating the logic inside evaluatePlot's concurrentPerform
        DispatchQueue.concurrentPerform(iterations: iterations) { i in
            // The critical performance hit in your current code:
            // Copying the dictionary for every single pixel/point
            var localVars = variables
            localVars["x"] = .dimensionless(Double(i))
            
            // Simulate a tiny lookup cost
            _ = localVars["a"]
        }
        
        let duration = CFAbsoluteTimeGetCurrent() - start
        print("    Time: \(String(format: "%.4f", duration))s (Simulating \(iterations) points)")
    }
}