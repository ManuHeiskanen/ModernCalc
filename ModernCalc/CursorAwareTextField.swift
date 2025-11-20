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
        textField.textColor = .clear // Hide the real text, but keep the cursor
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
        }
        
        /// Updates the NSTextField from SwiftUI state changes.
        func updateView(text: String, selectedRange: NSRange) {
            guard let textField = self.textField else { return }

            // 1. Lock the delegate: We are changing things manually, so ignore the Delegate noise.
            isUpdatingFromSwiftUI = true

            // 2. Update Text: Only if it actually changed (prevents cursor jumping)
            if textField.stringValue != text {
                textField.stringValue = text
            }

            // 3. Update Cursor: Only if it actually changed
            if let editor = textField.currentEditor(), editor.selectedRange != selectedRange {
                editor.selectedRange = selectedRange
            }
            
            // 4. Unlock the delegate IMMEDIATELY.
            // Do NOT use DispatchQueue.main.async here. It creates a race condition
            // where fast typing is ignored because the flag is still true.
            self.isUpdatingFromSwiftUI = false
        }
        
        deinit {
            NotificationCenter.default.removeObserver(self)
        }

        func controlTextDidChange(_ obj: Notification) {
            // If the view is being updated by SwiftUI, ignore this delegate callback.
            guard !isUpdatingFromSwiftUI,
                  let textField = obj.object as? NSTextField,
                  let editor = textField.currentEditor() as? NSTextView
            else { return }

            // If text is being composed (e.g., after a dead key press),
            // wait until composition is finished before updating the binding.
            if editor.hasMarkedText() {
                return
            }
            
            // UPDATE 1: Update the text binding
            parent.text = textField.stringValue
            
            // UPDATE 2: Update the range binding IMMEDIATELY.
            // This prevents the "De-sync" where SwiftUI has the new text
            // but the old cursor position.
            parent.selectedRange = editor.selectedRange
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
            // If the view is being updated by SwiftUI, ignore this delegate callback.
            guard !isUpdatingFromSwiftUI,
                  let textView = notification.object as? NSTextView,
                  let ourTextField = self.textField,
                  textView.delegate === ourTextField
            else {
                return
            }

            // Also guard selection changes against marked text to avoid conflicts.
            if textView.hasMarkedText() {
                return
            }
            
            // The flag system allows us to update the binding synchronously without warnings.
            parent.selectedRange = textView.selectedRange
        }
    }
}

