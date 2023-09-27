;;;; SPDX-FileCopyrightText: Artyom Bologov
;;;; SPDX-License-Identifier: BSD-3 Clause

(uiop:define-package :trivial-toplevel-prompt
  (:use :common-lisp)
  (:export #:set-toplevel-prompt #:reset-toplevel-prompt)
  (:documentation "`trivial-toplevel-prompt' is a library to customize REPL prompts.

The main entry point is `set-toplevel-prompt', which redefines
implementation-specific prompts to follow a certain format.

`reset-toplevel-prompt' is an undo operation that restored the prompt
to the state before the last `set-toplevel-prompt' was invoked."))
(in-package :trivial-toplevel-prompt)

(defvar *previous-prompting-stack* nil
  "A list of previous prompting states: replaced functions, rebound variables etc.
Allows to restore the previous state of the prompt in `reset-toplevel-prompt'.")

(defun reset-toplevel-prompt ()
  "Return the prompt to the state before the last `set-toplevel-prompt'."
  (if *previous-prompting-stack*
      #+sbcl
      (sb-ext:without-package-locks
	(destructuring-bind (repl-fun debug-prompt)
	    (pop *previous-prompting-stack*)
	  (setf sb-int:*repl-prompt-fun* repl-fun
		(fdefinition 'sb-debug::debug-prompt) debug-prompt)))
      #+ccl
      (progn
	(fmakunbound 'ccl::print-listener-prompt)
	(setf (fdefinition 'ccl::print-listener-prompt)
	      (pop *previous-prompting-stack*)))
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
      (setf tpl::*repl-prompt-fun* (pop *previous-prompting-stack*))
      #+allegro
      (setf tpl:*prompt* (pop *previous-prompting-stack*))
      #-(or sbcl ccl ecl clisp abcl allegro)
      nil
      (warn "Nothing to reset: no previous state saved."))
  nil)

(defun shortest-package-nickname (package)
  (first (sort (cons (package-name package) (package-nicknames package))
               #'< :key #'length)))

(defun true (value)
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
- Current command number (nullable integer).
  - NIL if there's no command history for the implementation.
- Debug level (nullable integer).
  - NIL if not in the debugger or unknown.
- Whether in the stepping loop (boolean).
- Whether in the inspect loop (boolean)."
  (let ((prompt-function
          (etypecase prompt-specifier
            (string (lambda (stream process/thread-name package-name
                             command-number debug-level stepping-p inspect-p)
                      (format
                       stream prompt-specifier
                       process/thread-name package-name
                       command-number debug-level stepping-p inspect-p)))
            (function prompt-specifier))))
    (declare (ignorable prompt-function))
    #+sbcl
    (progn
      (push (list sb-int:*repl-prompt-fun* (fdefinition 'sb-debug::debug-prompt))
	    *previous-prompting-stack*)
      (flet ((call (stream)
	       (fresh-line stream)
	       (funcall prompt-function stream
			(sb-thread:thread-name sb-thread:*current-thread*)
			(shortest-package-nickname *package*)
			;; FIXME: No history on SBCL. Maybe use frame
			;; number for history, at least in debugger?
			nil
			(when (plusp sb-debug::*debug-command-level*)
			  sb-debug::*debug-command-level*)
			(sb-impl::stepping-enabled-p) (true (ignore-errors sb-ext::*inspected*)))))
	(sb-ext:without-package-locks
	  (setf (fdefinition 'sb-debug::debug-prompt)
		(lambda (stream)
		  (sb-thread::get-foreground)
		  (call stream))
		sb-int:*repl-prompt-fun*
		(lambda (stream)
		  (call stream))))))
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
                       nil ;; No history on CCL.
                       (when (plusp ccl::*break-level*)
			 ccl::*break-level*)
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
                       nil ;; History not available
                       (when (plusp si::*tpl-level*)
			 si::*tpl-level*)
                       (plusp si:*step-level*) (plusp si::*inspect-level*))
              (force-output *standard-output*))))
    #+abcl
    (progn
      (push tpl::*repl-prompt-fun* *previous-prompting-stack*)
      (setf tpl::*repl-prompt-fun*
            (lambda (stream)
              (fresh-line stream)
              (funcall prompt-function stream
                       (threads:thread-name (threads:current-thread))
                       (shortest-package-nickname *package*)
                       tpl::*cmd-number*
                       (when (plusp ext:*debug-level*)
			 ext:*debug-level*)
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
                         (when (and (sys::break-level) (plusp (sys::break-level)))
			   (sys::break-level))
                         (plusp (sys::step-level)) (true sys::*inspect-all*))))))
    #+allegro
    (progn
      (push tpl:*prompt* *previous-prompting-stack*)
      (setf tpl:*prompt*
            (lambda (stream process-name focused-process-name
                     stepping-p break-level continuable-p inspect-p
                     package command-number)
              (declare (ignore focused-process-name continuable-p))
              (fresh-line stream)
              (funcall prompt-function stream
                       process-name (shortest-package-nickname package)
                       command-number
                       break-level
                       stepping-p inspect-p))))
    #-(or sbcl ccl ecl clisp abcl allegro)
    (warn "Trivial Toplevel Prompt does not support this implementation yet. Help in supporting it!")))
