//
//  EvaluatorODESolver.swift
//  ModernCalc
//
//  Created by Manu Heiskanen on 7.10.2025.
//

import Foundation

/// This extension contains the logic for evaluating an `ode45` function call.
extension Evaluator {
    
    /// Evaluates an `ode45` function call node by preparing arguments and calling the numerical solver.
    /// - Parameters:
    ///   - node: The `ODENode` representing the `ode45(...)` call.
    ///   - variables: The current scope of variables.
    ///   - functions: The current scope of user-defined functions.
    ///   - angleMode: The current angle mode for calculations.
    /// - Returns: A `MathValue.odeSolution` containing the time vector and state matrix of the result.
    func evaluateODESolve(_ node: ODENode, variables: inout [String: MathValue], functions: inout [String: FunctionDefinitionNode], angleMode: AngleMode) throws -> MathValue {
        // 1. Argument count is validated by the parser creating the ODENode.

        // 2. Extract the name of the user-defined ODE function.
        let funcName = node.functionName.name

        // 3. Find the user-defined function and validate its signature (must be f(t, y)).
        guard let userFunction = functions[funcName] else {
            throw MathError.unknownFunction(name: funcName)
        }
        guard userFunction.parameterNames.count == 2 else {
            throw MathError.incorrectArgumentCount(function: funcName, expected: "2 (e.g., t, y)", found: userFunction.parameterNames.count)
        }

        // 4. Evaluate the `t_span` and `y0` arguments from the ODENode properties.
        let (tSpanValue, _) = try _evaluateSingle(node: node.timeSpan, variables: &variables, functions: &functions, angleMode: angleMode)
        let tSpanVector = try tSpanValue.asVector(for: "ode45 t_span")
        guard tSpanVector.dimension == 2 else {
            throw MathError.dimensionMismatch(reason: "Time span (t_span) for ode45 must be a 2-element vector [t0, tf].")
        }
        let t0 = tSpanVector[0]
        let tf = tSpanVector[1]

        let (y0Value, _) = try _evaluateSingle(node: node.initialConditions, variables: &variables, functions: &functions, angleMode: angleMode)
        let y0Vector = try y0Value.asVector(for: "ode45 y0")

        // 5. Create a Swift closure that the numerical solver can execute.
        // This closure captures the necessary context and calls the user's function.
        let capturedVariables = variables
        let capturedFunctions = functions
        let f: (Double, Vector) throws -> Vector = { t, y in
            var tempVars = capturedVariables
            // Map the solver's t and y to the user's function parameter names.
            tempVars[userFunction.parameterNames[0]] = .dimensionless(t)
            tempVars[userFunction.parameterNames[1]] = .vector(y)

            var tempFuncs = capturedFunctions
            let (result, _) = try self._evaluateSingle(node: userFunction.body, variables: &tempVars, functions: &tempFuncs, angleMode: angleMode)
            
            let resultVector = try result.asVector(for: "ode45 return value")
            
            // Ensure the function's output dimension matches the input dimension.
            guard resultVector.dimension == y0Vector.dimension else {
                throw MathError.dimensionMismatch(reason: "The function provided to ode45 must return a vector of the same dimension as the initial condition vector (y0). Expected \(y0Vector.dimension), found \(resultVector.dimension).")
            }
            return resultVector
        }

        // 6. Call the solver with the prepared closure and arguments.
        let (time, states) = try Solver.ode45(f: f, t_span: [t0, tf], y0: y0Vector)

        // 7. Return the result wrapped in the new .odeSolution type.
        return .odeSolution(time: time, states: states)
    }
}
