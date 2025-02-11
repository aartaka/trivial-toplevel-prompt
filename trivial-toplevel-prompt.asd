;;;; SPDX-FileCopyrightText: Artyom Bologov
;;;; SPDX-License-Identifier: BSD-3 Clause

(asdf:defsystem "trivial-toplevel-prompt"
  :description "Portability library to customize REPL prompts."
  :author "Artyom Bologov"
  :homepage "https://github.com/aartaka/trivial-toplevel-prompt"
  :bug-tracker "https://github.com/aartaka/trivial-toplevel-prompt/issues"
  :source-control (:git "https://github.com/aartaka/trivial-toplevel-prompt.git")
  :license  "BSD-3 Clause"
  :version "0.1.1"
  :serial t
  :components ((:file "package")
               (:file "prompt")))
