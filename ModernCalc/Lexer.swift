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
            case "+", "-", "%", "^", "~": // Added ~
                advance()
                currentToken = Token(type: .op(String(char)), rawValue: String(char))
            case "=":
                advance()
                if peek() == "=" {
                    advance()
                    currentToken = Token(type: .op("=="), rawValue: "==")
                } else {
                    currentToken = Token(type: .op("="), rawValue: "=")
                }
            case "!":
                advance()
                if peek() == "=" {
                    advance()
                    currentToken = Token(type: .op("!="), rawValue: "!=")
                } else {
                    currentToken = Token(type: .op("!"), rawValue: "!")
                }
            case ">":
                advance()
                if peek() == "=" {
                    advance()
                    currentToken = Token(type: .op(">="), rawValue: ">=")
                } else {
                    currentToken = Token(type: .op(">"), rawValue: ">")
                }
            case "<":
                advance()
                if peek() == "=" {
                    advance()
                    currentToken = Token(type: .op("<="), rawValue: "<=")
                } else {
                    currentToken = Token(type: .op("<"), rawValue: "<")
                }
            case "&":
                advance()
                if peek() == "&" {
                    advance()
                    currentToken = Token(type: .op("&&"), rawValue: "&&")
                } else {
                    currentToken = Token(type: .unknown("&"), rawValue: "&")
                }
            case "|":
                advance()
                if peek() == "|" {
                    advance()
                    currentToken = Token(type: .op("||"), rawValue: "||")
                } else {
                    currentToken = Token(type: .unknown("|"), rawValue: "|")
                }
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
                 // CORRECTED LOGIC: A dot introduces a unit *only if* it's followed by a letter.
                 // Otherwise, it must be a dot-operator or a separator.
                if let nextChar = peek(offset: 1), nextChar.isLetter {
                    let (unitSymbol, charsConsumed) = peekAheadForUnit()
                    if !unitSymbol.isEmpty {
                        for _ in 0..<charsConsumed { advance() }
                        currentToken = Token(type: .unit(unitSymbol), rawValue: ".\(unitSymbol)")
                    } else {
                        currentToken = lexDotOperatorOrUnknown()
                    }
                } else {
                    currentToken = lexDotOperatorOrUnknown()
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
                    // --- FIX: A standalone colon is the range operator, not an assignment ---
                    currentToken = Token(type: .op(":"), rawValue: ":")
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
    
    private func lexDotOperatorOrUnknown() -> Token {
        // The dot character '.' is used as an argument separator ONLY when the
        // decimal separator is set to a comma.
        if decimalSeparator == .comma && peek() == "." {
            advance()
            return Token(type: .separator("."), rawValue: ".")
        }
        // In all other cases (when the decimal separator is a period, or it's not a separator),
        // the dot must be part of a dot-operator (like .*, ./, or .=@).
        return lexDotOperator()
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
            // A lone dot (when not a decimal separator for numbers or a separator for commas) is treated as unknown.
            return Token(type: .unknown(advance()!), rawValue: ".")
        }
    }
    
    private func peekAheadForUnit() -> (symbol: String, charsConsumed: Int) {
        var potentialUnit = ""
        var offset = 0
        
        // Consume the dot itself
        offset += 1
        
        // Skip any whitespace between the dot and the unit name
        while let char = peek(offset: offset), char.isWhitespace {
            offset += 1
        }
        
        // Read the potential unit name (letters only)
        while let char = peek(offset: offset), char.isLetter || char == "μ" || char == "Ω" {
            potentialUnit.append(char)
            offset += 1
        }
        
        // Check if the accumulated string is a valid unit.
        if UnitStore.units[potentialUnit] != nil {
            return (potentialUnit, offset)
        }
        
        // If not a valid unit, backtrack and return nothing.
        return ("", 0)
    }

    private func lexNumber() -> Token {
        let startIndex = currentIndex
        var hasDecimal = false

        while let char = peek() {
            if char.isNumber {
                advance()
            } else if char == decimalSeparator.character && !hasDecimal {
                // Look ahead to ensure there's a digit after the decimal.
                // This prevents "5." from being lexed as a number.
                if peek(offset: 1)?.isNumber ?? false {
                    hasDecimal = true
                    advance()
                } else {
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
        if identifierString == "in" {
            return Token(type: .op("in"), rawValue: "in")
        }
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
