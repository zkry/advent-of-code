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

(defun aoc21--read-lines (fn)
  "Return lines read from file FN."
  (let* ((f-contents (f-read fn)))
    (split-string f-contents "\n")))

(defun aoc21--read-variable-list (fn)
  "Read a list of space separated items read via Lisp reader from file FN."
  (let* ((f-contents (f-read fn))
         (lines (seq-filter (lambda (s) (not (string-blank-p s)))
                            (split-string f-contents "\n"))))
    (seq-map (lambda (l)
               (let ((parts (split-string l)))
                 (mapcar #'read parts)))
             lines)))

(defun aoc21--read-char-lists (fn)
  "Return lines read from file FN."
  (let* ((f-contents (f-read fn)))
    (seq-map #'string-to-list (split-string f-contents))))

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

(defun aoc21-day-2-1 ()
  "Calculate the product of horizontal and depth of the submarine."
  (let* ((horiz 0)
         (depth 0)
         (cmds (aoc21--read-variable-list "puzzle2.txt")))
    (dolist (cmd cmds)
      (pcase cmd
        (`(forward ,amount) (setq horiz (+ horiz amount)))
        (`(up ,amount)      (setq depth (- depth amount)))
        (`(down ,amount)    (setq depth (+ depth amount)))))
    (* horiz depth)))

(defun aoc21-day-2-2 ()
  "Calculate the product of horizontal and depth of the submarine using aim."
  (let* ((aim 0)
         (horiz 0)
         (depth 0)
         (cmds (aoc21--read-variable-list "puzzle2.txt")))
    (dolist (cmd cmds)
      (pcase cmd
        (`(forward ,amount) (setq horiz (+ horiz amount))
                            (setq depth (+ depth (* aim amount))))
        (`(up ,amount)      (setq aim (- aim amount)))
        (`(down ,amount)    (setq aim (+ aim amount)))))
    (* horiz depth)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Day 3 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun aoc21--bit-char-not (c)
  "Apply the not operation to a bit character C."
  (if (= c ?0) ?1 ?0))

(defun aoc21--common-bit-at-pos (data n tie-val)
  "For DATA, determine the most common bit at pos N returning TIE-VAL if counts equal."
  (let ((one-ct 0)
        (zero-ct 0))
    (seq-map (lambda (d)
               (if (eql (nth n d) ?1)
                   (setq one-ct (1+ one-ct))
                 (setq zero-ct (1+ zero-ct))))
             data)
    (cond
     ((> one-ct zero-ct) ?1)
     ((= one-ct zero-ct) tie-val)
     (t                  ?0))))

(defun aoc21--common-number-generator (data &optional inverse-p)
  "Find the common bits for each position in DATA, inversing result if INVERSE-P is not-nil."
  (let ((result '()))
    (dotimes (bit-pos (length (car data)))
      (let ((common-bit (aoc21--common-bit-at-pos data bit-pos ?_)))
        (when inverse-p
          (setq common-bit (aoc21--bit-char-not common-bit)))
        (setq result (append result (list common-bit)))))
    (read (concat "#b" result))))

(defun aoc21-day-3-1 ()
  "Solution for day 3-1."
  (let* ((data (aoc21--read-char-lists "puzzle3.txt")))
    (* (aoc21--common-number-generator data) (aoc21--common-number-generator data t))))

(defun aoc21--filter-for-bit (data n bit)
  "Filter DATA where the bit at position N is BIT."
  (seq-filter (lambda (d) (eql (nth n (string-to-list d)) bit)) data))

(defun aoc21--oxygen/co2-gen-rating (data &optional co2-p)
  "Run the oxygen algorithm on DATA, switching to CO2 algorithm if CO2-P is not-nil."
  (catch 'result
    (dotimes (n (length (car data)))
      (when (= 1 (length data))
        (throw 'result (read (concat "#b" (car data)))))
      (let ((bit (aoc21--common-bit-at-pos data n ?1)))
        (when co2-p (setq bit (aoc21--bit-char-not bit)))
        (setq data (aoc21--filter-for-bit data n bit))))))

(defun aoc21-day-3-2 ()
  "Solution for day 3-2."
  (let* ((data (aoc21--read-char-lists "puzzle3.txt")))
    (* (aoc21--oxygen/co2-gen-rating data) (aoc21--oxygen/co2-gen-rating data t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Day 4 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun aoc21--board-idx (board row col)
  (aref board (+ (* row 5) col)))

(defun aoc21--calc-score (board called-nums number)
  (let ((total 0))
    (dotimes (row 5)
      (dotimes (col 5)
        (when (not (gethash (aoc21--board-idx board row col) called-nums nil))
          (setq total (+ total (aoc21--board-idx board row col))))))
    (* total number)))

(defun aoc21--win-p (board called-nums)
  (cl-labels ((checked-p (row col) (gethash (aoc21--board-idx board row col) called-nums nil)))
    (or
     (cl-loop for row from 0 to 4
              thereis (cl-loop for col from 0 to 4
                               always (checked-p row col)))
     (cl-loop for col from 0 to 4
              thereis (cl-loop for row from 0 to 4
                               always (checked-p row col)))
     (cl-loop for row from 0 to 4
              for col from 0 to 4
              always (checked-p row col))
     (cl-loop for row from 0 to 4
              for col downfrom 4 to 0
              always (checked-p row col)))))

(defun aoc21-day-4-1 ()
  "Solve day 4-1 problem."
  (catch 'solution
    (let* ((lines (aoc21--read-lines "puzzle4.txt"))
           (numbers (seq-map #'read (split-string (car lines) ",")))
           (file-contents (f-read "puzzle4.txt"))
           (boards (seq-map (lambda (board-txt)
                              (apply #'vector (seq-map #'read (split-string board-txt))))
                            (cdr (split-string file-contents "\n\n"))))
           (called-nums (make-hash-table)))
      (dolist (number numbers)
        (puthash number t called-nums)
        (dolist (board boards)
          (when (aoc21--win-p board called-nums)
            (throw 'solution (aoc21--calc-score board called-nums number)))))
      (error "no solution found"))))

(defun aoc21-day-4-2 ()
  "Solve day 4-1 problem."
  (catch 'solution
    (let* ((lines (aoc21--read-lines "puzzle4.txt"))
           (numbers (seq-map #'read (split-string (car lines) ",")))
           (file-contents (f-read "puzzle4.txt"))
           (boards (seq-map (lambda (board-txt)
                              (apply #'vector (seq-map #'read (split-string board-txt))))
                            (cdr (split-string file-contents "\n\n"))))
           (board-ct (length boards))
           (called-nums (make-hash-table)))
      (dolist (number numbers)
        (puthash number t called-nums)
        (dolist (board boards)
          (when (aoc21--win-p board called-nums)
            (setq board-ct (1- board-ct))
            (when (= 0 board-ct)
              (throw 'solution (aoc21--calc-score board called-nums number)))
            (fillarray board -1)))))))

(provide 'aoc21)

;;; aoc21.el ends here
