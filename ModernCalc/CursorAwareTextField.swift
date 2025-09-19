//
//  CursorAwareTextField.swift
//  ModernCalc
//
//  Created by Manu Heiskanen on 1.9.2025.
//

import SwiftUI

// A custom NSViewRepresentable that wraps NSTextField to provide cursor position awareness.
struct CursorAwareTextField: NSViewRepresentable {
    @Binding var text: String
    @Binding var selectedRange: NSRange
    @Binding var cursorRect: CGRect

    func makeCoordinator() -> Coordinator {
        Coordinator(self)
    }

    func makeNSView(context: Context) -> NSTextField {
        let textField = NSTextField(string: text)
        textField.delegate = context.coordinator
        context.coordinator.setTextField(textField)
        
        textField.isBordered = false
        textField.backgroundColor = .clear
        textField.focusRingType = .none
        textField.font = .monospacedSystemFont(ofSize: 26, weight: .regular)
        return textField
    }

    func updateNSView(_ nsView: NSTextField, context: Context) {
        // The coordinator now handles updates to prevent feedback loops.
        context.coordinator.updateView(text: text, selectedRange: selectedRange)
    }

    class Coordinator: NSObject, NSTextFieldDelegate {
        var parent: CursorAwareTextField
        private weak var textField: NSTextField?
        private var isUpdatingFromSwiftUI = false

        init(_ parent: CursorAwareTextField) {
            self.parent = parent
        }

        func setTextField(_ textField: NSTextField) {
            self.textField = textField
            // Observe frame changes to update cursor rect on window resize/move
            textField.postsFrameChangedNotifications = true
            NotificationCenter.default.addObserver(
                self,
                selector: #selector(viewFrameDidChange(_:)),
                name: NSView.frameDidChangeNotification,
                object: textField
            )
        }
        
        /// Updates the NSTextField from SwiftUI state changes.
        func updateView(text: String, selectedRange: NSRange) {
            guard let textField = self.textField else { return }

            isUpdatingFromSwiftUI = true

            if textField.stringValue != text {
                textField.stringValue = text
            }
            
            if let editor = textField.currentEditor() as? NSTextView, editor.selectedRange != selectedRange {
                editor.selectedRange = selectedRange
                
                DispatchQueue.main.async {
                    self.updateCursorRect()
                }
            }
            
            DispatchQueue.main.async {
                self.isUpdatingFromSwiftUI = false
            }
        }
        
        deinit {
            NotificationCenter.default.removeObserver(self)
        }

        func controlTextDidChange(_ obj: Notification) {
            guard !isUpdatingFromSwiftUI,
                  let textField = obj.object as? NSTextField,
                  let editor = textField.currentEditor() as? NSTextView
            else { return }

            if editor.hasMarkedText() {
                return
            }
            
            parent.text = textField.stringValue
        }
        
        func controlTextDidBeginEditing(_ obj: Notification) {
            NotificationCenter.default.addObserver(
                self,
                selector: #selector(textViewDidChangeSelection(_:)),
                name: NSTextView.didChangeSelectionNotification,
                object: nil
            )
        }
        
        func controlTextDidEndEditing(_ obj: Notification) {
            NotificationCenter.default.removeObserver(
                self,
                name: NSTextView.didChangeSelectionNotification,
                object: nil
            )
        }
        
        @objc func textViewDidChangeSelection(_ notification: Notification) {
            guard !isUpdatingFromSwiftUI,
                  let textView = notification.object as? NSTextView,
                  let ourTextField = self.textField,
                  textView.delegate === ourTextField
            else {
                return
            }

            if textView.hasMarkedText() {
                return
            }
            
            parent.selectedRange = textView.selectedRange
            updateCursorRect()
        }
        
        @objc func viewFrameDidChange(_ notification: Notification) {
            updateCursorRect()
        }
        
        /// --- MODIFIED: A simplified and more direct cursor position calculation ---
        private func updateCursorRect() {
            guard let textField = self.textField,
                  let editor = textField.currentEditor() as? NSTextView else { return }
            
            let range = NSRange(location: editor.selectedRange.location, length: 0)
            
            // 1. Get rect in the text editor's local coordinates.
            let rectInEditor = editor.firstRect(forCharacterRange: range, actualRange: nil)

            // 2. Convert the rect directly from the editor's coordinate system to the text field's system.
            // This is more direct and avoids the buggy intermediate conversion to window coordinates.
            let rectInTextField = editor.convert(rectInEditor, to: textField)
            
            // Update the binding on the main thread to be safe.
            DispatchQueue.main.async {
                self.parent.cursorRect = rectInTextField
            }
        }
    }
}

