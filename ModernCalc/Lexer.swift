//
//  Lexer.swift
//  ModernCalc
//
//  Created by Manu Heiskanen on 28.8.2025.
//

import Foundation

enum TokenType: Equatable {
    case number(Double)
    case complexLiteral(Double)
    case op(Character)
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
    enum DecimalSeparator: Character {
        case period = "."
        case comma = ","
    }

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
        while let char = peek() {
            if char.isWhitespace {
                advance()
                continue
            }

            if char.isNumber {
                tokens.append(lexNumber())
                continue
            }
            
            if char.isLetter || "αβγδθλμπρστω".contains(char) {
                if char == "i" {
                    if peekNext() == "'" {
                        advance()
                        advance()
                        tokens.append(Token(type: .unitVector("i"), rawValue: "i'"))
                    } else {
                        tokens.append(lexIdentifierOrComplex())
                    }
                } else if char == "j" && peekNext() == "'" {
                    advance()
                    advance()
                    tokens.append(Token(type: .unitVector("j"), rawValue: "j'"))
                } else if char == "k" && peekNext() == "'" {
                    advance()
                    advance()
                    tokens.append(Token(type: .unitVector("k"), rawValue: "k'"))
                } else {
                    tokens.append(lexIdentifierOrComplex())
                }
                continue
            }

            switch char {
            case "+", "-", "*", "/", "%", "^", "=":
                advance()
                tokens.append(Token(type: .op(char), rawValue: String(char)))
            // MODIFIED: Handle aliases for operators and functions
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
            case "π":
                advance()
                tokens.append(Token(type: .identifier("pi"), rawValue: "π"))
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

        while let char = peek() {
            if char.isNumber {
                advance()
            } else if char == decimalSeparator.rawValue && !hasDecimal {
                hasDecimal = true
                advance()
            } else {
                break
            }
        }
        
        if let char = peek(), char == "i" {
            let numberString = String(input[startIndex..<currentIndex])
            let sanitizedString = numberString.replacingOccurrences(of: ",", with: ".")
            if let value = Double(sanitizedString) {
                advance()
                return Token(type: .complexLiteral(value), rawValue: numberString + "i")
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
    
    private func lexIdentifierOrComplex() -> Token {
        let startIndex = currentIndex
        while let char = peek(), char.isLetter || "αβγδθλμπρστω".contains(char) {
            advance()
        }
        let identifierString = String(input[startIndex..<currentIndex])
        
        if identifierString == "i" {
            return Token(type: .complexLiteral(1.0), rawValue: "i")
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

