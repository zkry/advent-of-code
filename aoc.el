;;; aoc.el --- AoC Helper Functions -*- lexical-binding: t -*-

;; Author: Zachary Romero
;; Maintainer: Zachary Romero
;; Version: 0.1.0
;; Package-Requires: ()
;; Homepage: homepage
;; Keywords: keywords


;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
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

;; List of AOC Helpers

;;; Code:

(require 'subr-x)

(defun aoc-lines (text)
  "Return each line of TEXT."
  (split-string (string-trim-right text "\n") "\n"))

(defun aoc-ints (text)
  "Extract all of the integers from TEXT."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (let ((matches))
      (while (re-search-forward "[[:digit:]]+" (point-max) t)
        (push (string-to-number (match-string 0)) matches))
      (reverse matches))))

(defun pp-hash (table)
  (let ((data (nthcdr 2 (nbutlast
                         (split-string (pp-to-string table) "[()]")
                         2))))
    (concat "(" (car data) ")")))

(defun aocp- (x &optional form)
  (with-current-buffer (get-buffer-create "*aoc-output*")
    (goto-char (point-min))
    (insert ">>>>>>>>>>" (current-time-string) "\n" )
    (when form
      (insert (pp-to-string form) "\n"))
    (if (hash-table-p x)
        (insert (yaml-encode x))
      (insert (pp-to-string x)))
    (insert "\n\n"))
  x)

(defmacro aocp (form)
  `(aocp- ,form (quote ,form)))

(provide 'aoc)

;;; aoc.el ends here
