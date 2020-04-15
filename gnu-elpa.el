;;; gnu-elpa.el --- Advertize GNU ELPA packages  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Free Software Foundation, Inc.

;; Author: Stefan Monnier <monnier@iro.umontreal.ca>
;; Keywords:
;; Version: 0

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

;; This package adds placeholders for the features defined by GNU ELPA packages
;; such that the users are informed about the existence of those features and
;; so they can easily install the relevant package on the spot.

;;;; FIXME/TODO:

;; - Allow packages more control over what is auto-predefined.
;; - Allow more than `auto-mode-alist' and `autoload's, e.g. allow
;;   new menu entries.
;; - Merge with `gnu-elpa-keyring-update'?

;;; Code:

;;;###autoload (unless (assoc "gnu" package-archives)
;;;###autoload   (push '("gnu" . "https://elpa.gnu.org/packages/")
;;;###autoload         package-archives))

;; This file is not meant to be `require'd but to be loaded in response
;; to calling a function (i.e. via autoload) and it will find the package
;; that provides this function and suggest installing the package.
;;(provide 'gnu-elpa)

(let* ((bt (backtrace-frames))
       (bt-stash bt)
       (trigger-function nil))
  (while bt
    (pcase-let ((`(\_ ,f . ,_) (pop bt)))
      (when (and (symbolp f) (autoloadp (indirect-function f)))
        (setq trigger-function f)
        (setq bt nil))))
  (unless trigger-function
    (error "Can't find the autoload call!"))
  (message "Autoloading %S" trigger-function))

;;; gnu-elpa.el ends here
