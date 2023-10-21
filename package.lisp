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

#+sbcl
(when (ignore-errors (find-package :sb-aclrepl))
  (pushnew :sb-aclrepl *features*))
