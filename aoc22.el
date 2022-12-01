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

(provide 'aoc22)

;;; aoc22.el ends here
