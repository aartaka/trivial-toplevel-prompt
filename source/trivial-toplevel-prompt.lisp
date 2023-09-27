;;;; SPDX-FileCopyrightText: Artyom Bologov
;;;; SPDX-License-Identifier: BSD-3 Clause

(in-package :trivial-toplevel-prompt)

(defvar *previous-prompting-stack* nil
  "A list of previous prompting states: replaced functions, rebound variables etc.
Allows to restore the previous state of the prompt in `reset-toplevel-prompt'.")

(defun reset-toplevel-prompt ()
  "Return the prompt to the state before the last `set-toplevel-prompt'."
  (if *previous-prompting-stack*
      #+sbcl
      (setf sb-int:*repl-prompt-fun*
            (pop *previous-prompting-stack*))
      #+ccl
      (setf (fdefinition 'ccl::print-listener-prompt)
            (pop *previous-prompting-stack*))
      #+ecl
      (setf si::*tpl-prompt-hook* (pop *previous-prompting-stack*))
      #+clisp
      (destructuring-bind (start body finish break step)
          (pop *previous-prompting-stack*)
        (setf sys::*prompt-start* start
              sys::*prompt-body* body
              sys::*prompt-finish* finish
              sys::*prompt-break* break
              sys::*prompt-step* step))
      #+abcl
      (setf *repl-prompt-fun* (pop *previous-prompting-stack*))
      #+allegro
      #-(or sbcl ccl ecl clisp abcl allegro)
      nil
      (warn "Nothing to reset: no previous state saved."))
  nil)

(defun shortest-package-nickname (package)
  (first (sort (cons (package-name package) (package-nicknames package))
               #'< :key #'length)))

;; FIXME: Remove it and be honest about the absence of history.
(defvar *command-index* -1
  "REPL command index for implementations that don't have it.")

(defun bool (value)
  "Ensure that VALUE is coerced to a strict NIL/T boolean."
  (not (not value)))

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
  ;; FIXME: Why is there debug-p? Isn't debug-level enough? It's a
  ;; nullable integer, after all...
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
    (progn
      (push sb-int:*repl-prompt-fun* *previous-prompting-stack*)
      (setf sb-int:*repl-prompt-fun*
            (lambda (stream)
              (fresh-line stream)
              (funcall prompt-function stream
                       (sb-thread:thread-name sb-thread:*current-thread*)
                       (shortest-package-nickname *package*)
                       (incf *command-index*)
                       (plusp sb-debug::*debug-command-level*) sb-debug::*debug-command-level*
                       (sb-impl::stepping-enabled-p) (true (ignore-errors sb-ext::*inspected*))))))
    #+ccl
    (progn
      (push (fdefinition 'ccl::print-listener-prompt)
            *previous-prompting-stack*)
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
                       (true ccl::@))
              (force-output stream))))
    #+ecl
    (progn
      (push si::*tpl-prompt-hook* *previous-prompting-stack*)
      (setf si::*tpl-prompt-hook*
            (lambda ()
              (fresh-line *standard-output*)
              (funcall prompt-function *standard-output*
                       (or #+threads (string (mp:process-name mp:*current-process*)) "REPL")
                       (shortest-package-nickname *package*)
                       (incf *command-index*)
                       (plusp si::*tpl-level*) si::*tpl-level*
                       (plusp si:*step-level*) (plusp si::*inspect-level*))
              (force-output *standard-output*))))
    #+abcl
    (progn
      (push *repl-prompt-fun* *previous-prompting-stack*)
      (setf *repl-prompt-fun*
            (lambda (stream)
              (fresh-line stream)
              (funcall prompt-function stream
                       (threads:thread-name (threads:current-thread))
                       (shortest-package-nickname *package*)
                       tpl::*cmd-number*
                       (plusp ext:*debug-level*) ext:*debug-level*
                       ;; ABCL has no stepping?
                       nil sys::*inspect-break*))))
    #+clisp
    (progn
      (push (list sys::*prompt-start* sys::*prompt-body* sys::*prompt-finish*
                  sys::*prompt-break* sys::*prompt-step*)
            *previous-prompting-stack*)
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
                         (plusp (sys::step-level)) (true sys::*inspect-all*))))))
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
