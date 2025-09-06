//
//  MathJaxView.swift
//  ModernCalc
//
//  Created by Manu Heiskanen on 6.9.2025.
//

import SwiftUI
import MathJaxSwift

/// A singleton class that manages a single `MathJax` instance, following the
/// library's documentation for best practices on performance and stability.
private class EquationRenderer {
    /// A shared instance to ensure the expensive MathJax engine is initialized only once.
    static let shared = try! EquationRenderer()
    
    private var mathjax: MathJax
    
    // The TeX input processor options - load all available packages for maximum compatibility.
    private let inputOptions = TeXInputProcessorOptions(loadPackages: TeXInputProcessorOptions.Packages.all)
    
    // The SVG output processor options - align the rendered equation to the left.
    private let outputOptions = SVGOutputProcessorOptions(displayAlign: SVGOutputProcessorOptions.DisplayAlignments.left)
    
    // The conversion options - render the equation as a block element with a specific font size.
    private let convOptions = ConversionOptions(display: true, em: 14)
    
    private init() throws {
        // Initialize MathJax, specifying SVG as the preferred (and only) output format.
        mathjax = try MathJax(preferredOutputFormat: .svg)
    }
    
    /// Converts a TeX input string to an SVG string using the pre-configured options.
    /// - Parameter texInput: The LaTeX string to convert.
    /// - Returns: A string containing the SVG representation of the input.
    func convert(_ texInput: String) async throws -> String {
        return try await mathjax.tex2svg(
            texInput,
            conversionOptions: convOptions,
            inputOptions: inputOptions,
            outputOptions: outputOptions
        )
    }
}


/// A fully native SwiftUI view that converts a LaTeX string to an NSImage and displays it.
/// This view does NOT use WebKit.
struct MathJaxView: View {
    
    var latex: String

    @Environment(\.colorScheme) var colorScheme

    // State to hold the final rendered image.
    @State private var renderedImage: NSImage?
    // State for fallback text on error.
    @State private var errorText: String?

    var body: some View {
        ZStack {
            if let renderedImage = renderedImage {
                // Display the rendered NSImage.
                Image(nsImage: renderedImage)
                    .resizable()
                    .scaledToFit()
                    .padding(.vertical, 10)
                    .padding(.horizontal)
            } else if let errorText = errorText {
                // On failure, display the raw LaTeX that caused the error.
                Text(errorText)
                    .foregroundColor(.red)
                    .font(.system(.body, design: .monospaced))
                    .lineLimit(1)
                    .truncationMode(.head)
            } else {
                // Show an empty view while loading or for empty input.
                Color.clear
            }
        }
        // --- CHANGE: Use custom colors for light and dark modes ---
        .foregroundColor(colorScheme == .dark ? Color(red: 222/255, green: 222/255, blue: 221/255) /* #dededd */ : Color(red: 36/255, green: 36/255, blue: 35/255) /* #242423 */)
        .task(id: latex) {
            // This task automatically cancels and restarts whenever the `latex` input changes.
            guard !latex.isEmpty else {
                self.renderedImage = nil
                self.errorText = nil
                return
            }

            do {
                // 1. Convert LaTeX to an SVG string using the shared renderer.
                let svgString = try await EquationRenderer.shared.convert(latex)

                // 2. Convert the SVG string to UTF-8 data.
                guard let svgData = svgString.data(using: .utf8) else {
                    throw ConversionError.encodingFailed
                }

                // 3. Create a native NSImage from the SVG data.
                guard let image = NSImage(data: svgData) else {
                    throw ConversionError.imageCreationFailed
                }
                
                image.isTemplate = true
                
                // 4. Update the UI on the main thread.
                await MainActor.run {
                    self.renderedImage = image
                    self.errorText = nil
                }
            } catch {
                // If any part of the process fails, show the raw LaTeX as a fallback.
                print("MathJax rendering failed for input '\(latex)': \(error)")
                await MainActor.run {
                    self.errorText = latex
                    self.renderedImage = nil
                }
            }
        }
    }
    
    enum ConversionError: Error {
        case encodingFailed
        case imageCreationFailed
    }
}
