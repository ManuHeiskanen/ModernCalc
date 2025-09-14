//
//  Lexer.swift
//  ModernCalc
//
//  Created by Manu Heiskanen on 28.8.2025.
//

import Foundation

enum TokenType: Equatable {
    case number(Double)
    case op(String)
    case paren(Character)
    case bracket(Character)
    case assignment
    case separator(Character)
    case identifier(String)
    case string(String)
    case unit(String)
    case unitVector(Character)
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
    private var lastTokenType: TokenType?

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
                let token = lexNumber()
                tokens.append(token)
                self.lastTokenType = token.type
                continue
            }
            
            if char.isLetter || greekLetters.contains(char) || char == "_" {
                let token: Token
                if char == "j" && peek(offset: 1) == "'" {
                    advance(); advance()
                    token = Token(type: .unitVector("j"), rawValue: "j'")
                } else if char == "k" && peek(offset: 1) == "'" {
                    advance(); advance()
                    token = Token(type: .unitVector("k"), rawValue: "k'")
                } else {
                    token = lexIdentifier()
                }
                tokens.append(token)
                self.lastTokenType = token.type
                continue
            }

            var currentToken: Token?
            switch char {
            case "\"":
                currentToken = lexString()
            case "+", "-", "%", "^", "=":
                advance()
                currentToken = Token(type: .op(String(char)), rawValue: String(char))
            case "*":
                advance()
                currentToken = Token(type: .op("*"), rawValue: "*")
            case "/":
                advance()
                currentToken = Token(type: .op("/"), rawValue: "/")
            case "×":
                advance()
                currentToken = Token(type: .op("*"), rawValue: "×")
            case "÷":
                advance()
                currentToken = Token(type: .op("/"), rawValue: "÷")
            case "√":
                advance()
                currentToken = Token(type: .identifier("sqrt"), rawValue: "√")
            case "±":
                advance()
                currentToken = Token(type: .op("±"), rawValue: "±")
            case "∠":
                advance()
                currentToken = Token(type: .op("∠"), rawValue: "∠")
            case "!":
                advance()
                currentToken = Token(type: .op("!"), rawValue: "!")
            case "'":
                advance()
                currentToken = Token(type: .op("'"), rawValue: "'")
            case ",":
                if argumentSeparator == "," {
                    advance()
                    currentToken = Token(type: .separator(","), rawValue: ",")
                } else {
                    currentToken = Token(type: .unknown(advance()!), rawValue: ",")
                }
            case ".":
                // Try to parse as a unit first. This is more flexible and allows for
                // expressions like "1/.s" (1 / 1 second).
                let (unitSymbol, charsConsumed) = peekAheadForUnit()
                if !unitSymbol.isEmpty {
                    // It's a valid unit symbol.
                    for _ in 0..<charsConsumed { advance() }
                    currentToken = Token(type: .unit(unitSymbol), rawValue: ".\(unitSymbol)")
                } else {
                    // If it's not a unit, it could be a decimal separator for function arguments
                    // or a dot operator (like .* for element-wise multiplication).
                    if argumentSeparator == "." {
                        advance()
                        currentToken = Token(type: .separator("."), rawValue: ".")
                    } else {
                        currentToken = lexDotOperator()
                    }
                }
            case "°":
                advance()
                continue
            case "(": currentToken = Token(type: .paren("("), rawValue: String(advance()!))
            case ")": currentToken = Token(type: .paren(")"), rawValue: String(advance()!))
            case "[": currentToken = Token(type: .bracket("["), rawValue: String(advance()!))
            case "]": currentToken = Token(type: .bracket("]"), rawValue: String(advance()!))
            case ":":
                if peek(offset: 1) == "=" {
                    advance(); advance()
                    currentToken = Token(type: .assignment, rawValue: ":=")
                } else {
                    advance()
                    currentToken = Token(type: .assignment, rawValue: ":")
                }
            case ";":
                advance()
                currentToken = Token(type: .separator(char), rawValue: String(char))
            default:
                currentToken = Token(type: .unknown(advance()!), rawValue: String(char))
            }
            
            if let token = currentToken {
                tokens.append(token)
                self.lastTokenType = token.type
            }
        }
        return tokens
    }
    
    private func lexDotOperator() -> Token {
        if peek(offset: 1) == "=" && peek(offset: 2) == "@" {
            advance(); advance(); advance()
            return Token(type: .op(".=@"), rawValue: ".=@")
        } else if peek(offset: 1) == "+" && peek(offset: 2) == "@" {
            advance(); advance(); advance()
            return Token(type: .op(".+@"), rawValue: ".+@")
        } else if peek(offset: 1) == "-" && peek(offset: 2) == "@" {
            advance(); advance(); advance()
            return Token(type: .op(".-@"), rawValue: ".-@")
        } else if peek(offset: 1) == "*" && peek(offset: 2) == "@" {
            advance(); advance(); advance()
            return Token(type: .op(".*@"), rawValue: ".*@")
        } else if peek(offset: 1) == "/" && peek(offset: 2) == "@" {
            advance(); advance(); advance()
            return Token(type: .op("./@"), rawValue: "./@")
        } else if peek(offset: 1) == "*" {
            advance(); advance()
            return Token(type: .op(".*"), rawValue: ".*")
        } else if peek(offset: 1) == "/" {
            advance(); advance()
            return Token(type: .op("./"), rawValue: "./")
        } else {
            return Token(type: .unknown(advance()!), rawValue: ".")
        }
    }
    
    private func peekAheadForUnit() -> (symbol: String, charsConsumed: Int) {
        var potentialUnit = ""
        var offset = 0
        
        // Start by consuming the dot
        offset += 1
        
        // Skip leading whitespace after the dot
        while let char = peek(offset: offset), char.isWhitespace {
            offset += 1
        }
        
        // Now find the actual unit symbol
        while let char = peek(offset: offset), char.isLetter || char == "μ" {
            potentialUnit.append(char)
            offset += 1
        }
        
        // If we found a valid unit in our store, return it and the total number of characters
        // we looked past (dot + spaces + symbol length).
        if UnitStore.units[potentialUnit] != nil {
            return (potentialUnit, offset)
        }
        
        // Otherwise, it wasn't a valid unit.
        return ("", 0)
    }

    private func lexNumber() -> Token {
        let startIndex = currentIndex
        var hasDecimal = false

        while let char = peek() {
            if char.isNumber {
                advance()
            } else if char == decimalSeparator.character && !hasDecimal {
                // MODIFICATION: Only consume the dot if it's followed by another number.
                // This prevents the lexer from consuming "1." in "1.Hz" as a single number,
                // allowing the dot to be correctly identified as a unit separator.
                if peek(offset: 1)?.isNumber ?? false {
                    hasDecimal = true
                    advance()
                } else {
                    // This dot is likely a unit separator or an operator, so stop parsing the number.
                    break
                }
            } else {
                break
            }
        }
        
        let numberString = String(input[startIndex..<currentIndex])
        let sanitizedString = numberString.replacingOccurrences(of: String(decimalSeparator.character), with: ".")
        
        if let value = Double(sanitizedString) {
            return Token(type: .number(value), rawValue: numberString)
        } else {
            return Token(type: .invalid(numberString), rawValue: numberString)
        }
    }
    
    private func lexString() -> Token {
        let startIndex = currentIndex
        advance()
        var value = ""
        while let char = peek(), char != "\"" {
            advance()
            value.append(char)
        }
        if peek() == "\"" { advance() }
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
        if identifierString == "π" { return Token(type: .identifier("pi"), rawValue: "π") }
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
