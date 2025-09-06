//
//  Lexer.swift
//  ModernCalc
//
//  Created by Manu Heiskanen on 28.8.2025.
//

import Foundation

enum TokenType: Equatable {
    case number(Double)
    case op(String) // Changed from Character to String
    case paren(Character)
    case bracket(Character)
    case assignment
    case separator(Character) // For , and ;
    case identifier(String)
    case unitVector(Character) // For i', j', k'
    case unknown(Character)
    case invalid(String)
}

struct Token: Equatable {
    let type: TokenType
    let rawValue: String
}

class Lexer {
    private let input: String
    private var currentIndex: String.Index
    private let decimalSeparator: DecimalSeparator

    init(input: String, decimalSeparator: DecimalSeparator = .period) {
        self.input = input
        self.currentIndex = input.startIndex
        self.decimalSeparator = decimalSeparator
    }
    
    func tokenize() -> [Token] {
        var tokens: [Token] = []
        let greekLetters = "αβγδεζηθικλμνξοπρστυφχψωΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡΣΤΥΦΧΨΩ"
        
        while let char = peek() {
            if char.isWhitespace {
                advance()
                continue
            }
            
            // Handle numbers that might start with a decimal separator, e.g., .5
            if char.isNumber || (char == decimalSeparator.character && (peekNext()?.isNumber ?? false)) {
                tokens.append(lexNumber())
                continue
            }
            
            if char.isLetter || greekLetters.contains(char) || char == "_" {
                if char == "j" && peekNext() == "'" {
                    advance(); advance()
                    tokens.append(Token(type: .unitVector("j"), rawValue: "j'"))
                } else if char == "k" && peekNext() == "'" {
                    advance(); advance()
                    tokens.append(Token(type: .unitVector("k"), rawValue: "k'"))
                } else {
                    tokens.append(lexIdentifier())
                }
                continue
            }

            switch char {
            case "+", "-", "%", "^", "=":
                advance()
                tokens.append(Token(type: .op(String(char)), rawValue: String(char)))
            case "*":
                advance()
                tokens.append(Token(type: .op("*"), rawValue: "*"))
            case "/":
                advance()
                tokens.append(Token(type: .op("/"), rawValue: "/"))
            case "×":
                advance()
                tokens.append(Token(type: .op("*"), rawValue: "×"))
            case "÷":
                advance()
                tokens.append(Token(type: .op("/"), rawValue: "÷"))
            case "√":
                advance()
                tokens.append(Token(type: .identifier("sqrt"), rawValue: "√"))
            case "±":
                advance()
                tokens.append(Token(type: .op("±"), rawValue: "±"))
            case "∠":
                advance()
                tokens.append(Token(type: .op("∠"), rawValue: "∠"))
            case "!": // Add factorial operator
                advance()
                tokens.append(Token(type: .op("!"), rawValue: "!"))
            case "'": // Transpose operator
                advance()
                tokens.append(Token(type: .op("'"), rawValue: "'"))
            case ".": // Element-wise operators
                if peekNext() == "*" {
                    advance(); advance()
                    tokens.append(Token(type: .op(".*"), rawValue: ".*"))
                } else if peekNext() == "/" {
                    advance(); advance()
                    tokens.append(Token(type: .op("./"), rawValue: "./"))
                } else {
                    tokens.append(Token(type: .unknown(advance()!), rawValue: "."))
                }
            case "°":
                advance()
                continue
            case "(": tokens.append(Token(type: .paren("("), rawValue: String(advance()!)))
            case ")": tokens.append(Token(type: .paren(")"), rawValue: String(advance()!)))
            case "[": tokens.append(Token(type: .bracket("["), rawValue: String(advance()!)))
            case "]": tokens.append(Token(type: .bracket("]"), rawValue: String(advance()!)))
            case ":":
                if peekNext() == "=" {
                    advance(); advance()
                    tokens.append(Token(type: .assignment, rawValue: ":="))
                } else {
                    tokens.append(Token(type: .unknown(advance()!), rawValue: ":"))
                }
            case ",", ";":
                advance()
                tokens.append(Token(type: .separator(char), rawValue: String(char)))
            default:
                tokens.append(Token(type: .unknown(advance()!), rawValue: String(char)))
            }
        }
        return tokens
    }

    private func lexNumber() -> Token {
        let startIndex = currentIndex
        var hasDecimal = false

        // Handle case where number starts with decimal separator
        if let char = peek(), char == decimalSeparator.character {
            hasDecimal = true
            advance()
        }
        
        while let char = peek() {
            if char.isNumber {
                advance()
            } else if char == decimalSeparator.character && !hasDecimal {
                hasDecimal = true
                advance()
            } else {
                break
            }
        }
        
        let numberString = String(input[startIndex..<currentIndex])
        let sanitizedString = numberString.replacingOccurrences(of: ",", with: ".")
        
        if let value = Double(sanitizedString) {
            return Token(type: .number(value), rawValue: numberString)
        } else {
            return Token(type: .invalid(numberString), rawValue: numberString)
        }
    }
    
    private func lexIdentifier() -> Token {
        let startIndex = currentIndex
        let greekLetters = "αβγδεζηθικλμνξοπρστυφχψωΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡΣΤΥΦΧΨΩ"
        // Removed ' from the valid characters
        while let char = peek(), char.isLetter || greekLetters.contains(char) || char == "_" {
            advance()
        }
        let identifierString = String(input[startIndex..<currentIndex])
        
        if identifierString == "π" {
            return Token(type: .identifier("pi"), rawValue: "π")
        }
        
        return Token(type: .identifier(identifierString), rawValue: identifierString)
    }

    private func peek() -> Character? {
        guard currentIndex < input.endIndex else { return nil }
        return input[currentIndex]
    }

    private func peekNext() -> Character? {
        guard let nextIndex = input.index(currentIndex, offsetBy: 1, limitedBy: input.endIndex) else { return nil }
        guard nextIndex < input.endIndex else { return nil }
        return input[nextIndex]
    }

    @discardableResult
    private func advance() -> Character? {
        guard let char = peek() else { return nil }
        currentIndex = input.index(after: currentIndex)
        return char
    }
}
