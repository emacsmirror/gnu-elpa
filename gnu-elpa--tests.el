;;; gnu-elpa--tests.el --- Tests for the `gnu-elpa' package  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Free Software Foundation, Inc.

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

;;

;;; Code:

(require 'cl-lib)
(require 'gnu-elpa-utils)

(ert-deftest gnu-elpa--describe-function ()
  (cl-letf (((symbol-function #'yes-or-no-p) #'ignore)
            ((symbol-function #'gnu-elpa--maybe-install)
             (lambda (pkg f) (defalias f (lambda (&rest _) "Dummy doc")))))
    (let ((s (describe-function 'sml-run)))
      (should (stringp s)))))

(ert-deftest gnu-elpa--command-execute ()
  (defvar gnu-elpa--test)
  (cl-letf (((symbol-function #'yes-or-no-p) #'ignore)
            ((symbol-function #'gnu-elpa--maybe-install)
             (lambda (pkg f) (defalias f (lambda (&rest _) "Dummy doc"
                                      (interactive)
                                      (setq-local gnu-elpa--test 42))))))
    (setq gnu-elpa--test 1)
    (should (command-execute 'sml-mode))
    (should (equal gnu-elpa--test 42))))

(provide 'gnu-elpa--tests)
;;; gnu-elpa--tests.el ends here
