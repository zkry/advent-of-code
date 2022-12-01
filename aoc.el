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

(defun aoc-read (text)
  (read (format "(%s)" text)))

(defun aoc-chargrid (text)
  (apply #'vector (split-string text "\n")))

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

(defun aoc-groups (text)
  "Return groups of TEXT."
  (split-string (string-trim-right text "\n") "\n\n"))

(defun aoc-groups-of (text subdiv-fn)
  "Return groups of TEXT parsed by SUBDIV-FN."
  (let ((groups (split-string (string-trim-right text "\n") "\n\n")))
    (seq-map subdiv-fn groups)))

(defun aoc--parse-line (line regexp conversions)
  "Extract REGEXP matche on LINE, converting them with CONVERSIONS."
  (let ((fields))
    (when (string-match regexp line)
      (dotimes (i (length conversions))
        (let ((conv-fn (nth i conversions))
              (matched-val (match-string (1+ i) line)))
          (when matched-val
            (when conv-fn
              (setq matched-val (funcall conv-fn matched-val)))
            (push matched-val fields))))
      (reverse fields))))

(defun aoc-parsed-lines (text regexp &rest conversions)
  "Parse TEXT according to REGEXP with CONVERSIONS."
  (let ((lines (aoc-lines text)))
    (seq-map (lambda (line)
               (let ((parse (aoc--parse-line line regexp conversions)))
                 (unless parse
                   (error "parse failed: %s" line))
                 parse))
             lines)))

(provide 'aoc)

;;; aoc.el ends here
