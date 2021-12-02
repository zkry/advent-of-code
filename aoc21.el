;;; aoc21.el --- Solutions to Advent of Code 2021 -*- lexical-binding: t -*-

;; Author: Zachary Romero
;; Maintainer: Zachary Romero
;; Version: 0.1.0
;; Package-Requires: ()
;; Homepage: https://github.com/zkry/advent-of-code-2021
;; Keywords: 


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

;; This file contains solutions to advent of code problems.

;;; Code:

(require 'f)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Utility Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun aoc21--puzzle-file-name-for-day (d)
  "Return the name of puzzle file for a given day's D puzzle."
  (format "puzzle%d.txt" d))

(defun aoc21--read-file-for-day (d)
  "Return the contents of a file for a given day's D puzzle."
  (f-read (aoc21--puzzle-file-name-for-day d)))

(defun aoc21--read-number-list (fn)
  "Read a list of numbers is integers from file named FN."
  (let* ((f-contents (f-read fn))
         (lines (split-string f-contents)))
    (seq-map (lambda (l) (read l)) lines)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Day 1 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun aoc21-day-1-1 ()
  "Count how many times the values increase."
  (let* ((fn (aoc21--puzzle-file-name-for-day 1))
         (data (aoc21--read-number-list fn))
         (inc-ct 0))
    (seq-mapn (lambda (a b)
                (if (> b a)
                    (setq inc-ct (1+ inc-ct))
                  nil))
              data
              (cdr data))
    inc-ct))

(defun aoc21-day-1-2 ()
  "Count how many times the sum of 3 consecutive values increase."
  (let* ((fn (aoc21--puzzle-file-name-for-day 1))
         (data (aoc21--read-number-list fn))
         (inc-ct 0)
         (sliding-window (seq-mapn #'+ data (cdr data) (cddr data))))
    (seq-mapn (lambda (a b)
                (if (> b a)
                    (setq inc-ct (1+ inc-ct))
                  nil))
              sliding-window
              (cdr sliding-window))
    inc-ct))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Day 2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun aoc21--day-2-read-commands ()
  "Read a list of 'forward 1' style commands."
  (let* ((f-contents (aoc21--read-file-for-day 2))
         (lines (seq-filter (lambda (s) (not (string-blank-p s)))
                            (split-string f-contents "\n"))))
    (seq-map (lambda (l)
               (let ((parts (split-string l)))
                 (setcar (cdr parts) (read (cadr parts)))
                 parts))
             lines)))

(defun aoc21--day-2-1 ()
  "Calculate the product of horizontal and depth of the submarine."
  (let* ((horiz 0)
         (depth 0)
         (cmds (aoc21--day-2-read-commands)))
    (dolist (cmd cmds)
      (pcase cmd
        (`("forward" ,amount) (setq horiz (+ horiz amount)))
        (`("up" ,amount)      (setq depth (- depth amount)))
        (`("down" ,amount)    (setq depth (+ depth amount)))))
    (* horiz depth)))

(defun aoc21--day-2-2 ()
  "Calculate the product of horizontal and depth of the submarine using aim."
  (let* ((aim 0)
         (horiz 0)
         (depth 0)
         (cmds (aoc21--day-2-read-commands)))
    (dolist (cmd cmds)
      (pcase cmd
        (`("forward" ,amount)
         (setq horiz (+ horiz amount))
         (setq depth (+ depth (* aim amount))))
        (`("up" ,amount)      (setq aim (- aim amount)))
        (`("down" ,amount)    (setq aim (+ aim amount)))))
    (* horiz depth)))

(provide 'aoc21)

;;; aoc21.el ends here
