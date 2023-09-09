;;;; SPDX-FileCopyrightText: Artyom Bologov
;;;; SPDX-License-Identifier: BSD-3 Clause

(in-package :trivial-toplevel-prompt)

(defun shortest-package-nickname (package)
  (first (sort (cons (package-name package) (package-nicknames package))
               #'< :key #'length)))

(defvar *command-index* -1
  "REPL command index for implementations that don't have it.")

(defun set-toplevel-prompt (prompt-specifier)
  "Redefine REPL prompt to be PROMPT-SPECIFIER.

PROMPT-SPECIFIER can be a
- String, which will be used as a format control.
- Function, which prints the prompt text to the supplied stream.

The arguments for format control or function are:
- Stream to print to. Only relevant for function, not supplied to
  format control.
- Current process/thread name (string).
- Current package name (string).
- Current command number (integer).
- Whether in debugger loop (generalized boolean).
- Debug level (nullable integer).
- Whether in the stepping loop (generalized boolean).
- Whether in the inspect loop (generalized boolean)."
  (let ((prompt-function
          (etypecase prompt-specifier
            (string (lambda (stream process/thread-name package-name
                             command-number debug-p debug-level stepping-p inspect-p)
                      (format
                       stream prompt-specifier
                       process/thread-name package-name
                       command-number debug-p debug-level stepping-p inspect-p)))
            (function prompt-specifier))))
    (declare (ignorable prompt-function))
    #+sbcl
    (setf sb-int:*repl-prompt-fun*
          (lambda (stream)
            (fresh-line stream)
            (funcall prompt-function stream
                     (sb-thread:thread-name sb-thread:*current-thread*)
                     (shortest-package-nickname *package*)
                     (incf *command-index*)
                     (plusp sb-debug::*debug-command-level*) sb-debug::*debug-command-level*
                     (sb-impl::stepping-enabled-p) (not (not (ignore-errors sb-ext::*inspected*))))))
    #+ccl
    (progn
      (fmakunbound 'ccl::print-listener-prompt)
      (setf (fdefinition 'ccl::print-listener-prompt)
            (lambda (stream &optional force)
              (declare (ignore force))
              (fresh-line stream)
              (funcall prompt-function stream
                       (ccl:process-name ccl:*current-process*)
                       (shortest-package-nickname *package*)
                       (incf *command-index*)
                       (plusp ccl::*break-level*) ccl::*break-level*
                       ;; CCL doesnt? support stepping?
                       nil
                       ;; @ is an inspector-specific variable on CCL.
                       ccl::@)
              (force-output stream))))
    #+ecl
    (setf si::*tpl-prompt-hook*
          (lambda ()
            (fresh-line *standard-output*)
            (funcall prompt-function *standard-output*
                     (or #+threads (string (mp:process-name mp:*current-process*)) "REPL")
                     (shortest-package-nickname *package*)
                     (incf *command-index*)
                     (plusp si::*tpl-level*) si::*tpl-level*
                     (plusp si:*step-level*) (plusp si::*inspect-level*))
            (force-output *standard-output*)))
    #+abcl
    (setf *repl-prompt-fun*
          (lambda (stream)
            (fresh-line stream)
            (funcall prompt-function stream
                     (threads:thread-name (threads:current-thread))
                     (shortest-package-nickname *package*)
                     tpl::*cmd-number*
                     (plusp ext:*debug-level*) ext:*debug-level*
                     ;; ABCL has no stepping?
                     nil sys::*inspect-break*)))
    #+clisp
    (progn
      (setf sys::*prompt-start* ""
            sys::*prompt-break* ""
            sys::*prompt-step* ""
            sys::*prompt-finish* "")
      (setf sys::*prompt-body*
            (lambda ()
              (with-output-to-string (stream)
                (funcall prompt-function stream
                         ;; Arbitrary process name, CLISP has no inspectable
                         ;; threads/processes?
                         "REPL" (shortest-package-nickname *package*)
                         (incf sys::*command-index*)
                         (and (sys::break-level) (plusp (sys::break-level))) (sys::break-level)
                         (plusp (sys::step-level)) (not (not sys::*inspect-all*)))))))
    #+allegro
    (setf tpl:*prompt*
          (lambda (stream process-name focused-process-name
                   stepping-p break-level continuable-p inspect-p
                   package command-number)
            (declare (ignore focused-process-name continuable-p))
            (fresh-line stream)
            (funcall prompt-function stream
                     process-name (shortest-package-nickname package)
                     command-number
                     break-level break-level
                     stepping-p inspect-p)))
    #-(or sbcl ccl ecl clisp abcl allegro)
    (warn "Trivial Toplevel Prompt does not support this implementation yet. Help in supporting it!")))
