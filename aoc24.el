;;; aoc24.el --- Advent of Code 2024 -*- lexical-binding: t -*-

;;; Commentary:

;; Solutions for Advent of Code 2024

;;; Code:

(require 'aoc)
(require 'dash)



;;; Day 2

(defun aoc24-day2-safe-p (level &optional no-expand)
  (let ((diffs (--map (- (car it) (cadr it)) (-partition-in-steps 2 1 level))))
    (or (and (or (--every (> 0 it) diffs) (--every (< 0 it) diffs))
             (--every (<= (abs it) 3) diffs))
        (and (not no-expand)
             (--some (aoc24-day2-safe-p it t)
                     (--map (-remove-at-indices (list it) level) (number-sequence 0 (1- (length level)))))))))

(defun aoc24-day2 ()
  (->> (f-read "./inputs/2024/day2.txt")
       (aoc-lines)
       (seq-map #'aoc-ints)
       (-count #'aoc24-day2-safe-p)))


;;; Day 3

(defconst aoc24-day3-rx
  (rx (or "do()"
          "don't()"
          (seq "mul(" (group (+ digit)) "," (group (+ digit)) ")"))))

(defun aoc24-day3 (&optional part-2)
  (with-temp-buffer
    (insert-file-contents "./inputs/2024/day3.txt")
    (goto-char (point-min))
    (let ((enabled t)
          (sum 0))
      (while (search-forward-regexp aoc24-day3-rx nil t)
        (cond
         ((equal (match-string 0) "do()")
          (setq enabled t))
         ((equal (match-string 0) "don't()")
          (when part-2 (setq enabled nil)))
         (t (when enabled
              (let ((a (string-to-number (match-string 1)))
                    (b (string-to-number (match-string 2))))
                (cl-incf sum (* a b)))))))
      sum)))
