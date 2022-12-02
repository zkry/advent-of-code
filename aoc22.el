;;; aoc22.el --- Advent of Code 2022 -*- lexical-binding: t -*-

;;; Commentary:

;; Solutions for Advent of Code 2022

;;; Code:

(require 'dash)
(require 'aoc)

;; Day 1.1
(->> (f-read "puzzle1.txt")
     (aoc-groups)
     (seq-map 'aoc-ints)
     (seq-map '-sum)
     (apply 'max))

;; Day 1.2
(->> (f-read "puzzle1.txt")
     (aoc-groups)
     (seq-map 'aoc-ints)
     (seq-map '-sum)
     (-sort '>)
     (-take 3)
     (-sum))


;; Day 2.1
(defun aoc22-day2 (score1 score2)
  (-let* ((data (aoc-read-lines (f-read "puzzle2.txt")))
          (score 0))
    (pcase-dolist (`(,it ,me) data)
      (cl-incf score (+ (ht-get score1 me) (ht-get* score2 it me))))
    score))

(defconst aoc22-day2-part1 (list (ht ('X 1) ('Y 2) ('Z 3))
                                 (ht ('A (ht ('X 3) ('Y 6) ('Z 0)))
                                     ('B (ht ('X 0) ('Y 3) ('Z 6)))
                                     ('C (ht ('X 6) ('Y 0) ('Z 3))))))
(apply #'aoc22-day2 aoc22-day2-part1)


;; Day 2.2
(defconst aoc22-day2-part2 (list (ht ('X 0) ('Y 3) ('Z 6))
                                 (ht ('A (ht ('X 3) ('Y 1) ('Z 2)))
                                     ('B (ht ('X 1) ('Y 2) ('Z 3)))
                                     ('C (ht ('X 2) ('Y 3) ('Z 1))))))
(apply #'aoc22-day2 aoc22-day2-part2)


(provide 'aoc22)

;;; aoc22.el ends here
