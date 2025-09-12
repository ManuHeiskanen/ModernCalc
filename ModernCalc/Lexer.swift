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
    case string(String) // NEW: For string literals like "asc"
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
        let argumentSeparator = (decimalSeparator == .period) ? Character(",") : Character(".")
        
        while let char = peek() {
            if char.isWhitespace {
                advance()
                continue
            }
            
            if char.isNumber || (char == decimalSeparator.character && (peek(offset: 1)?.isNumber ?? false)) {
                tokens.append(lexNumber())
                continue
            }
            
            if char.isLetter || greekLetters.contains(char) || char == "_" {
                if char == "j" && peek(offset: 1) == "'" {
                    advance(); advance()
                    tokens.append(Token(type: .unitVector("j"), rawValue: "j'"))
                } else if char == "k" && peek(offset: 1) == "'" {
                    advance(); advance()
                    tokens.append(Token(type: .unitVector("k"), rawValue: "k'"))
                } else {
                    tokens.append(lexIdentifier())
                }
                continue
            }

            switch char {
            case "\"":
                tokens.append(lexString())
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
            case "!":
                advance()
                tokens.append(Token(type: .op("!"), rawValue: "!"))
            case "'":
                advance()
                tokens.append(Token(type: .op("'"), rawValue: "'"))
            // MODIFIED: Replaced the ambiguous fallthrough logic with explicit cases for '.' and ','
            case ",":
                if argumentSeparator == "," {
                    advance()
                    tokens.append(Token(type: .separator(","), rawValue: ","))
                } else {
                    // If we are here, comma must be the decimal separator. If it appears alone, it's an error,
                    // because lexNumber should have handled it if it was part of a number.
                    tokens.append(Token(type: .unknown(advance()!), rawValue: ","))
                }
            case ".":
                if argumentSeparator == "." {
                    advance()
                    tokens.append(Token(type: .separator("."), rawValue: "."))
                } else {
                    // Here, period must be the decimal separator. Check for element-wise operators.
                    if peek(offset: 1) == "=" && peek(offset: 2) == "@" {
                        advance(); advance(); advance()
                        tokens.append(Token(type: .op(".=@"), rawValue: ".=@"))
                    } else if peek(offset: 1) == "+" && peek(offset: 2) == "@" {
                        advance(); advance(); advance()
                        tokens.append(Token(type: .op(".+@"), rawValue: ".+@"))
                    } else if peek(offset: 1) == "-" && peek(offset: 2) == "@" {
                        advance(); advance(); advance()
                        tokens.append(Token(type: .op(".-@"), rawValue: ".-@"))
                    } else if peek(offset: 1) == "*" && peek(offset: 2) == "@" {
                        advance(); advance(); advance()
                        tokens.append(Token(type: .op(".*@"), rawValue: ".*@"))
                    } else if peek(offset: 1) == "/" && peek(offset: 2) == "@" {
                        advance(); advance(); advance()
                        tokens.append(Token(type: .op("./@"), rawValue: "./@"))
                    } else if peek(offset: 1) == "*" {
                        advance(); advance()
                        tokens.append(Token(type: .op(".*"), rawValue: ".*"))
                    } else if peek(offset: 1) == "/" {
                        advance(); advance()
                        tokens.append(Token(type: .op("./"), rawValue: "./"))
                    } else {
                        // Period is the decimal separator but wasn't part of a number. Unknown.
                        tokens.append(Token(type: .unknown(advance()!), rawValue: "."))
                    }
                }
            case "°":
                advance()
                continue
            case "(": tokens.append(Token(type: .paren("("), rawValue: String(advance()!)))
            case ")": tokens.append(Token(type: .paren(")"), rawValue: String(advance()!)))
            case "[": tokens.append(Token(type: .bracket("["), rawValue: String(advance()!)))
            case "]": tokens.append(Token(type: .bracket("]"), rawValue: String(advance()!)))
            case ":":
                if peek(offset: 1) == "=" {
                    advance(); advance()
                    tokens.append(Token(type: .assignment, rawValue: ":="))
                } else {
                    tokens.append(Token(type: .unknown(advance()!), rawValue: ":"))
                }
            case ";":
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
            } else if char == decimalSeparator.character && !hasDecimal {
                hasDecimal = true
                advance()
            } else {
                // If the character is not a digit or the valid decimal separator, stop parsing the number.
                break
            }
        }
        
        let numberString = String(input[startIndex..<currentIndex])
        // Standardize the decimal separator to '.' for Double conversion.
        let sanitizedString = numberString.replacingOccurrences(of: String(decimalSeparator.character), with: ".")
        
        if let value = Double(sanitizedString) {
            return Token(type: .number(value), rawValue: numberString)
        } else {
            return Token(type: .invalid(numberString), rawValue: numberString)
        }
    }
    
    private func lexString() -> Token {
        let startIndex = currentIndex
        advance() // Consume the opening quote
        
        var value = ""
        while let char = peek(), char != "\"" {
            advance()
            value.append(char)
        }
        
        if peek() == "\"" {
            advance() // Consume the closing quote
        }
        
        let rawValue = String(input[startIndex..<currentIndex])
        return Token(type: .string(value), rawValue: rawValue)
    }

    private func lexIdentifier() -> Token {
        let startIndex = currentIndex
        let greekLetters = "αβγδεζηθικλμνξοπρστυφχψωΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡΣΤΥΦΧΨΩ"
        while let char = peek(), char.isLetter || char.isNumber || greekLetters.contains(char) || char == "_" {
            advance()
        }
        let identifierString = String(input[startIndex..<currentIndex])
        
        if identifierString == "π" {
            return Token(type: .identifier("pi"), rawValue: "π")
        }
        
        return Token(type: .identifier(identifierString), rawValue: identifierString)
    }

    private func peek(offset: Int = 0) -> Character? {
        guard let targetIndex = input.index(currentIndex, offsetBy: offset, limitedBy: input.endIndex),
              targetIndex < input.endIndex else {
            return nil
        }
        return input[targetIndex]
    }

    @discardableResult
    private func advance() -> Character? {
        guard let char = peek() else { return nil }
        currentIndex = input.index(after: currentIndex)
        return char
    }
}
