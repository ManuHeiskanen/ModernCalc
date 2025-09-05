//
//  MathJaxView.swift
//  ModernCalc
//
//  Created by Manu Heiskanen on 6.9.2025.
//

import SwiftUI
import WebKit

// A view that renders a LaTeX string using MathJax within a WKWebView.
struct MathJaxView: NSViewRepresentable {
    
    // The LaTeX string to be rendered.
    var latex: String

    // The HTML template that loads MathJax and provides a container for the math content.
    private var html: String {
        """
        <!DOCTYPE html>
        <html>
        <head>
            <meta charset="utf-8">
            <meta name="viewport" content="width=device-width, initial-scale=1">
            <script>
                MathJax = {
                    tex: {
                        inlineMath: [['$', '$'], ['\\(', '\\)']]
                    },
                    svg: {
                        fontCache: 'global'
                    },
                    startup: {
                        ready: () => {
                            MathJax.startup.defaultReady();
                            document.getElementById('math').style.visibility = 'visible';
                        }
                    }
                };
            </script>
            <script id="MathJax-script" async src="https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-svg.js"></script>
            <style>
                body {
                    display: flex;
                    justify-content: flex-end; /* Align content to the right */
                    align-items: center;
                    height: 100vh;
                    margin: 0;
                    padding: 0 20px;
                    background-color: transparent;
                    color: white; /* Will be replaced by system color */
                    font-size: 1.4em; /* Match app font size */
                }
                #math {
                    visibility: hidden; /* Hide until MathJax is ready */
                }
            </style>
        </head>
        <body>
            <div id="math"></div>
        </body>
        </html>
        """
    }

    func makeNSView(context: Context) -> WKWebView {
        let webView = WKWebView()
        // Make the web view's background transparent to blend with the app's UI.
        webView.setValue(false, forKey: "drawsBackground")
        webView.loadHTMLString(html, baseURL: nil)
        return webView
    }

    func updateNSView(_ nsView: WKWebView, context: Context) {
        // Use the system's current text color to ensure the math adapts to light/dark mode.
        let textColor = NSColor.textColor.hexString
        
        // A JavaScript function to update the math content and text color.
        // We escape the LaTeX string to ensure it's safely passed to JavaScript.
        let javascript = """
            document.getElementById('math').innerHTML = `$$\(latex.escapedForJavaScript)$$`;
            MathJax.typeset();
            document.body.style.color = '\(textColor)';
        """
        nsView.evaluateJavaScript(javascript)
    }
}

// Helper extension to get the hex string of an NSColor.
extension NSColor {
    var hexString: String {
        guard let rgbColor = usingColorSpace(.sRGB) else { return "#000000" }
        let red = Int(round(rgbColor.redComponent * 255.0))
        let green = Int(round(rgbColor.greenComponent * 255.0))
        let blue = Int(round(rgbColor.blueComponent * 255.0))
        return String(format: "#%02X%02X%02X", red, green, blue)
    }
}

// Helper extension to safely escape strings for JavaScript.
extension String {
    var escapedForJavaScript: String {
        return self.replacingOccurrences(of: "\\", with: "\\\\")
                   .replacingOccurrences(of: "`", with: "\\`")
                   .replacingOccurrences(of: "$", with: "\\$")
    }
}
