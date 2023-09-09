;;;; SPDX-FileCopyrightText: Artyom Bologov
;;;; SPDX-License-Identifier: BSD-3 Clause

;;; Commentary:
;;
;; GNU Guix development package.  To build and install, clone this repository,
;; switch directory to here and run:
;;
;;   guix package --install-from-file=guix.scm
;;
;; To use as the basis for a development environment, run:
;;
;;   guix shell --container -D -f guix.scm
;;
;; Replace --container by --pure if you still want ASDF to see external
;; libraries in ~/common-lisp, etc.
;;
;;; Code:

(use-modules (guix packages)
             ((guix licenses) #:prefix license:)
             (guix gexp)
             (guix git-download)
             (guix build-system asdf)
             (gnu packages)
             (gnu packages lisp)
             (gnu packages lisp-check)
             (gnu packages lisp-xyz))

(define-public sbcl-trivial-toplevel-prompt
  (package
   (name "sbcl-trivial-toplevel-prompt")
   (version "0.0.1")
   (source
    (local-file (dirname (current-filename)) #:recursive? #t)
    ;;;; Or this, in case of contributing to Guix.
    ;; (origin
    ;;   (method git-fetch)
    ;;   (uri (git-reference
    ;;         (url "https://github.com/aartaka/trivial-toplevel-prompt")
    ;;         (commit version)))
    ;;   (file-name (git-file-name "cl-trivial-toplevel-prompt" version))
    ;;   (sha256
    ;;    (base32
    ;;     "SPECIFY-HASH")))
    )
   (build-system asdf-build-system/sbcl)
   ;; We use `cl-*' inputs and not `sbcl-*' ones so that CCL users can also use
   ;; this Guix manifests.
   ;;
   ;; Another reason is to not fail when an input dependency is found in
   ;; ~/common-lisp, which would trigger a rebuild of the SBCL input in the
   ;; store, which is read-only and would thus fail.
   ;;
   ;; The official Guix package should use `sbcl-*' inputs though.
   (native-inputs (list cl-lisp-unit2 sbcl))
   (synopsis
    "Portability library to customize CL implementations' REPL prompt.")
   (home-page "https://github.com/aartaka/trivial-toplevel-prompt")
   (description "Trivial Toplevel Prompt allows to customize Lisp REPL prompt.

The only function that Trivial Toplevel Prompt has is
@code{set-toplevel-prompt}, which allows to redefine the
implementation-specific prompt printers.

Currently works on SBCL, CCL, ECL, ABCL, CLISP, and Allegro CL.")
   (license license:bsd-3)))

(define-public cl-trivial-toplevel-prompt
  (sbcl-package->cl-source-package sbcl-trivial-toplevel-prompt))

(define-public ecl-trivial-toplevel-prompt
  (sbcl-package->ecl-package sbcl-trivial-toplevel-prompt))

cl-trivial-toplevel-prompt
