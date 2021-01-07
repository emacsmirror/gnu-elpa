;;; gnu-elpa-utils.el --- Helper functions for `gnu-elpa'  -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2021  Free Software Foundation, Inc.

;; Author: Stefan Monnier <monnier@iro.umontreal.ca>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'gnu-elpa-features)

;;;###autoload (eval-after-load 'package
;;;###autoload   '(unless (assoc "gnu" package-archives)
;;;###autoload      (push '("gnu" . "https://elpa.gnu.org/packages/")
;;;###autoload            package-archives)))

;;;###autoload ;; Skip load-source-file-function which would slow us down by
;;;###autoload ;; a factor 2 (this assumes we were careful to save this file
;;;###autoload ;; so it doesn't need any decoding).
;;;###autoload (let ((load-source-file-function nil))
;;;###autoload   (require 'gnu-elpa-features nil 'noerror))

(defvar gnu-elpa--debug nil)

(defun gnu-elpa--message (n &rest args)
  (when (and gnu-elpa--debug
             (or (not (numberp gnu-elpa--debug))
                 (>= gnu-elpa--debug n)))
    (apply #'message args)))

(defun gnu-elpa--f-of-af (af)
  "Return the function to which an autoload-form belongs."
  (catch 'found
    (mapatoms (lambda (f)
                (if (eq (symbol-function f) af)
                    (throw 'found f))))))

(defun gnu-elpa--autoloaded-sym-p (f)
  (and (symbolp f) (autoloadp (symbol-function f))))

(defun gnu-elpa--autoloaded-function ()
  "Return the pair (SYMBOL . CAUSE) of the autoload cause.
SYMBOL is the function/macro that is being autoloaded, and CAUSE
is what appears to have triggered the autoload."
  (let* ((bt (backtrace-frames))
         (adl-function nil)
         ;; (bt-stash bt)
         (cause nil)
         (trigger-function nil))
    ;; (message "Backtrace frames: %S" bt)
    (while bt
      (pcase (pop bt)
        ;; The "normal" case: a funcall to the autoloaded function!
        (`(,_ ,(and f (pred gnu-elpa--autoloaded-sym-p)) . ,_)
         ;; A call to an autoloaded function.  That must be us!
         (setq trigger-function f)
         (setq bt nil))
        ;; Autoloading can come from C for `call-interactively',
        ;; `interactive-form', `macroexpand', `func-arity', or by keymap lookup
        ;; if it's an autoloaded keymap!
        (`(,_ ,(and c (or 'call-interactively 'interactive-form
                          'func-arity))
              (,(and f (pred gnu-elpa--autoloaded-sym-p)) . ,_) . ,_)
         (setq adl-function f)
         (setq cause c))
        (`(,_ macroexpand
              ((,(and f (pred gnu-elpa--autoloaded-sym-p)) . ,_) . ,_) . ,_)
         ;; FIXME: Actually, for `macroexpand', I can't see a "simple" way to
         ;; reliably find the macro that's being autoloaded from the backtrace,
         ;; since `f' may be a macro which macroexpanded to a call to the macro
         ;; we're trying to autoload :-(
         (setq adl-function f)
         (setq cause 'macroexpand))
        (`(,_ autoload-do-load (,af . ,(or `(,f . ,_) (let f nil))) . ,_)
         ;; This catches cases like `M-x sml-mode RET' where `command-execute'
         ;; calls `autoload-do-load' before actually calling the function.
         (setq adl-function
               (if (null f)
                   ;; Some callers of `autoload-do-load'
                   ;; (e.g. `describe-function') don't specify the second arg.
                   (gnu-elpa--f-of-af af)
                 f)))
        ((and `(,_ ,f ,args . ,_)
              (guard (and (null cause) adl-function)))
         (setq cause (cons f args)))
        (bf (gnu-elpa--message 2 "Backtrace frame: %S" bf))
        ))
    (cond
     (trigger-function
      (unless (eq trigger-function adl-function)
        (gnu-elpa--message
         1 "Inconsistency between funcall(%S) and autoload-do-load(%S)"
         trigger-function adl-function))
      (cons trigger-function 'funcall))
     (adl-function
      (cons adl-function cause))
     (t (error "Can't find the autoload call!")))))

(defun gnu-elpa--package (func)
  "Return the package that provides function FUNC."
  (let ((thepkg nil))
    (pcase-dolist (`(,prefix . ,pkg) gnu-elpa--autoloads-table)
      (if (string-prefix-p prefix (symbol-name func)) (setq thepkg pkg)))
    thepkg))

(defun gnu-elpa--perform-autoload ()
  "Prompt to install the package that provides the currently autoloaded function.
The relevant function is found by walking the stack until we find a function.
Presumes we're in the process of calling an autoloaded function that's not
yet loaded."
  (pcase-let*
      ((`(,f . ,c) (gnu-elpa--autoloaded-function))
       (pkg (gnu-elpa--package f)))
    (gnu-elpa--message 1 "Autoloading %S because of %S" f c)
    (gnu-elpa--maybe-install pkg f)))

(defun gnu-elpa--maybe-install (pkg f)
  ;; FIXME: This prompt is too dry, we should popup a little buffer
  ;; explaining a bit more what's going on with a short description of
  ;; the package.
  ;; FIXME: We should ask "yes/notnow/never"!
  (unless (yes-or-no-p (format "Function %S was called: Install package %s? "
                               f pkg))
    ;; FIXME: If "never" we should record this info somewhere
    ;; and then avoid reinstalling the corresponding autoloads
    ;; at the next start.
    ;; FIXME: Remove the corresponding autoloads for the current session!
    ;; FIXME: Rather than just "Abort" try and behave better in cases
    ;; such as when sql.el calls `sqlind-minor-mode'.
    (error "Abort!"))
  ;; FIXME: These two initializations should be performed by
  ;; `package-install'!
  (unless (bound-and-true-p package--initialized) (package-initialize t))
  (unless package-archive-contents (package-refresh-contents))
  ;; FIXME: Is `package-install' really sufficient to load the proper function?
  (package-install (intern pkg)))

(provide 'gnu-elpa-utils)
;;; gnu-elpa-utils.el ends here
