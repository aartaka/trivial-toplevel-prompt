;;;; SPDX-FileCopyrightText: Artyom Bologov
;;;; SPDX-License-Identifier: BSD-3 Clause

(uiop:define-package :trivial-toplevel-prompt
  (:use :common-lisp)
  (:export #:set-toplevel-prompt)
  (:documentation "`trivial-toplevel-prompt' is a library to customize REPL prompts.

The main entry point is `set-toplevel-prompt', which redefines
implementation-specific prompts to follow a certain format."))
