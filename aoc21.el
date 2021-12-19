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
(require 'aoc)

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Day 5 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun aoc21-day-5 (&optional part-2)
  (let* ((data (seq-map #'aoc-ints (aoc-lines (f-read "puzzle5.txt"))))
         (points (make-hash-table :test 'equal)))
    (dolist (pt data)
      (pcase-let ((`(,x1 ,y1 ,x2 ,y2) pt))
        (cond
         ((= x1 x2)
          (cl-loop for y downfrom y1 to y2
                   do (inchash (cons x1 y) points))
          (cl-loop for y from y1 to y2
                   do (inchash (cons x1 y) points)))
         ((= y1 y2)
          (cl-loop for x downfrom x1 to x2
                   do (inchash (cons x y1) points))
          (cl-loop for x from x1 to x2
                   do (inchash (cons x y1) points)))
         (t (when part-2
              (cl-loop for x from x1 to x2
                       for y from y1 to y2
                       do (inchash (cons x y) points))
              (cl-loop for x downfrom x1 to x2
                       for y from y1 to y2
                       do (inchash (cons x y) points))
              (cl-loop for x from x1 to x2
                       for y downfrom y1 to y2
                       do (inchash (cons x y) points))
              (cl-loop for x downfrom x1 to x2
                       for y downfrom y1 to y2
                       do (inchash (cons x y) points)))))))
    (cl-loop with total = 0
             for v being the hash-values of points
             when (> v 1)
             do (cl-incf total)
             finally return total)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Day 6 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun aoc21-day-6-1 (&optional part-2)
  (let* ((db (make-hash-table :test 'equal)))
    (dotimes (day 256)
      (let ((new-fish-ct 0)
            (old-fish '()))
        (dolist (fish data)
          (let ((new-ct fish))
            (if (= 0 new-ct)
                (progn
                  (setq new-ct 6)
                  (cl-incf new-fish-ct))
              (setq new-ct (1- new-ct)))
            (setq old-fish (cons new-ct old-fish))))
        (setq data (append (make-list new-fish-ct 8) old-fish))))
    (length data)))

(defun aoc21-day-6-2 ()
  (let* ((data (aoc-ints (f-read "puzzle6.txt")))
         (db (make-hash-table :test 'equal)))
    (dolist (fish data)
      (inchash fish db))
    (dotimes (day 256)
      (let ((new-db (make-hash-table :test 'equal))
            (new-ct 0))
        (maphash (lambda (day ct)
                   (if (= 0 day)
                       (progn
                         (setq new-ct ct)
                         (puthash 6 (+ ct (gethash 6 new-db 0)) new-db))
                     (puthash (1- day) (+ ct (gethash (1- day) new-db 0)) new-db)))
                 db)
        (inchash 8 new-db new-ct)
        (setq db new-db)))
    (cl-loop with total = 0
             for ct being the hash-values of db
             do (setq total (+ total ct))
             finally return total)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Day 7 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun aoc21--day-7-cost (x)
  (/ (* x (+ x 1)) 2))

(defun aoc21-day-7-1 ()
  (let* ((data (aoc-ints (f-read "puzzle7.txt"))))
    (cl-loop for i from 1 to 1000
             minimize (cl-loop for d in data
                               sum (abs (- i d))))))

(defun aoc21-day-7-2 ()
  (let* ((data (aoc-ints (f-read "puzzle7.txt"))))
    (cl-loop for i from 1 to 1000
             minimize (cl-loop for d in data
                               sum (aoc21--day-7-cost (abs (- i d)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Day 8 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun aoc21--day-8-narrow-down-segments (str seg-filters db)
  "Narrow down digit STR to be SEG-FILTERS in DB."
  (cl-loop for c across str
           do (puthash c (cl-intersection seg-filters (gethash c db '())) db)))

(defun aoc-21--day-8-set-chars (c num db)
  "Express fact that char C is the segment NUM in DB."
  (cl-loop for i across "abcdefg"
           when (not (equal i c))
           do (puthash i (remove num (gethash i db '())) db))
  (puthash c (list num) db))

(defun aoc21-determine-mapping (segments)
  "Return a map of char to segment from list of SEGMENTS."
  (let ((db (make-hash-table :test 'equal))
        (one-letters "")
        (seven-letters "")
        (six-letters '()))
    ;; Initialize segments: express the fact that every char
    ;; could be any segment.  Segments are as follows:
    ;;      00
    ;;     1  2
    ;;      33
    ;;     4  5
    ;;      66
    (cl-loop for i across "abcdefg"
             do (puthash i '(0 1 2 3 4 5 6) db))

    ;; Count the number of times each letter occurs. Based on these
    ;; counts the following facts appear:
    ;; - the letter that occurs 6 timse is segment 1
    ;; - the letter that occurs 4 timse is segment 4
    ;; - the letter that occurs 9 timse is segment 5
    (let ((freqs (make-hash-table :test 'equal)))
      (cl-loop for c across (string-join segments "")
               do (inchash c freqs))
      (cl-loop for k being the hash-keys of freqs using (hash-values v)
               do (cond
                   ((= v 6) (aoc-21--day-8-set-chars k 1 db))
                   ((= v 9) (aoc-21--day-8-set-chars k 5 db))
                   ((= v 4) (aoc-21--day-8-set-chars k 4 db)))))    

    ;; narrow down the segments based on the characters in the digit.
    ;; For example, given a digit of length two, we know that its segments
    ;; must be either 2 or 5.
    (dolist (s segments)
      (pcase (length s)
                  (2
                   (progn 
                     (aoc21--day-8-narrow-down-segments s '(2 5) db)
                     (setq one-letters s)))
                  (4 (aoc21--day-8-narrow-down-segments s '(1 2 3 5) db))
                  (3 (aoc21--day-8-narrow-down-segments s '(0 2 5) db)
                     (setq seven-letters s))
                  (7 (aoc21--day-8-narrow-down-segments s '(0 1 2 3 4 5 6) db))
                  (6 (setq six-letters (cl-adjoin s six-letters)))))

    ;; Special deduction: we can deduce segment 0 automatically from one and seven. 
    (let ((zero-letter (cl-set-difference (seq-into seven-letters 'list) (seq-into one-letters 'list))))
      (puthash (car zero-letter) '(0) db)
      (cl-loop for i across "abcdefg"
               when (not (equal i (car zero-letter)))
               do (puthash i (remove 0 (gethash i db '())) db)))

    ;; Iterate through the numbers we definitely know, eliminating
    ;; options of other numbers that we know they can't be.
    (while (not (cl-loop for v being the hash-values of db
                         always (= 1 (length v))))
      (cl-loop for k being the hash-keys of db using (hash-values v)
               when (= (length v) 1)
               do (cl-loop for i across "abcdefg"
                           when (not (equal i k))
                           do (puthash i (remove (car v) (gethash i db '())) db))))
    db))

(defun aoc21-get-digit (segs)
  "Return whcih digit representation is the set of SEGS."
  (cond
   ((not (cl-set-exclusive-or segs '(0 1 2 4 5 6))) "0")
   ((not (cl-set-exclusive-or segs '(2 5))) "1")
   ((not (cl-set-exclusive-or segs '(0 2 3 4 6))) "2")
   ((not (cl-set-exclusive-or segs '(0 2 3 5 6))) "3")
   ((not (cl-set-exclusive-or segs '(1 2 3 5))) "4")
   ((not (cl-set-exclusive-or segs '(0 1 3 5 6))) "5")
   ((not (cl-set-exclusive-or segs '(0 1 3 4 5 6))) "6")
   ((not (cl-set-exclusive-or segs '(0 2 5))) "7")
   ((not (cl-set-exclusive-or segs '(0 1 2 3 4 5 6))) "8")
   ((not (cl-set-exclusive-or segs '(0 1 2 3 5 6))) "9")))

(defun aoc21-day-8 ()
  (interactive)
  (let* ((data (seq-map (lambda (l) (seq-map (lambda (x) (split-string x))
                                             (split-string l "|")))
                        (aoc-lines (f-read "puzzle8.txt"))))
         (sum 0))
    (dolist (d data)
      (let* ((sequences (car d))
             (nums (cadr d))
             (mapping (aoc21-determine-mapping sequences))
             (digits '()))
        (dolist (n nums)
          (let ((seg-set (seq-map (lambda (x) (car (gethash x mapping))) (seq-into n 'list))))
            (setq digits (append digits (list (aoc21-get-digit seg-set))))))
        (setq sum (+ sum (read (string-join digits ""))))))
    sum))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Day 9 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun aaref (array &rest idxs)
  (while idxs
    (setq array (aref array (car idxs)))
    (setq idxs (cdr idxs)))
  array)

(defun aoc-num-grid (text)
  "Split TEXT as a vector of vectors."
  (vconcat
   (seq-map (lambda (line) (vconcat
                            (seq-map (lambda (x) (read (char-to-string x)))
                                     (string-to-vector line))))
            (split-string (string-trim-right text "\n") "\n"))))

(defun aoc21-day-9-1 ()
  (let* ((input (aoc-num-grid (f-read "puzzle9.txt"))) ;
         (row-ct (length input))
         (col-ct (length (aref input 0)))
         (sum 0)
         (low-pts '()))
    (dotimes (r row-ct)
      (dotimes (c col-ct)
        (let ((v (aaref input r c))
              (up (or (and (<= 0 (1- r)) (aaref input (1- r) c)) 10))
              (left (or (and (<= 0 (1- c)) (aaref input r (1- c))) 10))
              (down (or (and (< (1+ r) row-ct) (aaref input (1+ r) c)) 10))
              (right (or (and (< (1+ c) col-ct) (aaref input r (1+ c))) 10)))
          (when (and (< v up)
                     (< v down)
                     (< v left)
                     (< v right))
            (setq low-pts (cl-adjoin (cons r c) low-pts))
            (setq sum (+ sum v 1))))))
    low-pts))

(defun aoc21--day-9-basin-size (row col data)
  (if (or (< row 0) (< col 0) (>= row (length data)) (>= col (length (aref data 0)))
          (= (aaref data row col) -1)
          (= (aaref data row col) 9))
      0
    (setf (aref (aref data row) col) -1)
    (+ 1
       (aoc21--day-9-basin-size (1- row) col data)
       (aoc21--day-9-basin-size (1+ row) col data)
       (aoc21--day-9-basin-size row (1- col) data)
       (aoc21--day-9-basin-size row (1+ col) data))))

(defun aoc21-day-9-2 ()
  (let* ((input (aoc-num-grid (f-read "puzzle9.txt"))) ;
         (row-ct (length input))
         (col-ct (length (aref input 0)))
         (sum 0)
         (low-pts '())
         (sizes '()))
    (dotimes (r row-ct)
      (dotimes (c col-ct)
        (let ((v (aaref input r c))
              (up (or (and (<= 0 (1- r)) (aaref input (1- r) c)) 10))
              (left (or (and (<= 0 (1- c)) (aaref input r (1- c))) 10))
              (down (or (and (< (1+ r) row-ct) (aaref input (1+ r) c)) 10))
              (right (or (and (< (1+ c) col-ct) (aaref input r (1+ c))) 10)))
          (when (and (< v up)
                     (< v down)
                     (< v left)
                     (< v right))
            (setq low-pts (cl-adjoin (cons r c) low-pts))))))
    (dolist (pt low-pts)
      (let* ((row (car pt))
             (col (cdr pt))
             (size (aoc21--day-9-basin-size row col input)))
        (setq sizes (cons size sizes))))
    (apply '* (seq-take (sort sizes '>) 3))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Day 10 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun aoc21-day-10-1 ()
  (let* ((data (aoc-lines (f-read "puzzle10.txt")))
         (score 0))
    (dolist (line data)
      (let ((parens))
        (cl-loop for c across line
                 do (pcase c
                      (?\( (push 'round parens))
                      (?\[ (push 'square parens))
                      (?\{ (push 'curly parens))
                      (?\< (push 'angle parens))
                      (?\) (let ((elt (pop parens)))
                             (when (not (eql 'round elt))
                               (setq score (+ score 3))
                               (return))))
                      (?\] (let ((elt (pop parens)))
                             (when (not (eql 'square elt))
                               (setq score (+ score 57))
                               (return))))
                      (?\} (let ((elt (pop parens)))
                             (when (not (eql 'curly elt))
                               (setq score (+ score 1197))
                               (return))))
                      (?\> (let ((elt (pop parens)))
                             (when (not (eql 'angle elt))
                               (setq score (+ score 25137))
                               (return))))))))
    score))

(defun aoc21--day-10-calculate-completion-pts (parens)
  (let ((total 0))
    (dolist (paren parens)
      (setq total (* 5 total))
      (pcase paren
        ('round (setq total (+ total 1)))
        ('square (setq total (+ total 2)))
        ('curly (setq total (+ total 3)))
        ('angle (setq total (+ total 4)))))
    total))

(defun aoc21-day-10-2 ()
  (let* ((data (aoc-lines (f-read "puzzle10.txt")))
         (scores '()))
    (dolist (line data)
      (let ((parens))
        (cl-loop for c across line
                 do (pcase c
                      (?\( (push 'round parens))
                      (?\[ (push 'square parens))
                      (?\{ (push 'curly parens))
                      (?\< (push 'angle parens))
                      (?\) (let ((elt (pop parens)))
                             (when (not (eql 'round elt))
                               (return))))
                      (?\] (let ((elt (pop parens)))
                             (when (not (eql 'square elt))
                               (return))))
                      (?\} (let ((elt (pop parens)))
                             (when (not (eql 'curly elt))
                               (return))))
                      (?\> (let ((elt (pop parens)))
                             (when (not (eql 'angle elt))
                               (return)))))
                 finally do (push (aoc21--day-10-calculate-completion-pts (aocp parens)) scores))))
    (nth (/ (length scores) 2) (sort scores '<))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Day 11 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun aoc21-day-11--energize-neighbors (data r c)
  (cl-loop for dr from -1 to 1
           do (cl-loop for dc from -1 to 1
                       when (and (not (and (zerop dr) (zerop dc)))
                                 (>= 9 (+ r dr) 0)
                                 (>= 9(+ c dc) 0))
                       do (progn
                            (aocp (cons dr dc))
                            (cl-incf (aref (aref data (+ r dr)) (+ c dc))))))
  data)

(defun aoc21-day-11-1 ()
  (let* ((data (aoc-num-grid (f-read "puzzle13.txt"))) ;; 
         (flash-ct 0)
         (debug 0))
    (dotimes (_ 100)
      (dotimes (r 10)
        (dotimes (c 10)
          (cl-incf (aref (aref data r) c))))
      (let ((flashes '()))
        (catch 'done
          (while t
            (let ((prev-ct (length flashes)))
             (dotimes (r 10)
               (dotimes (c 10)
                 (when (and (> (aaref data r c) 9) (not (member (list r c) flashes)))
                   (aoc21-day-10--energize-neighbors data r c)
                   (setq flashes (cl-adjoin (list r c) flashes :test 'equal)))))
             (when (not (> (length flashes) prev-ct))
               (throw 'done nil)))))
        (dolist (flash flashes)
          (pcase-let ((`(,r ,c) flash))
            (when (> (aaref data r c) 9)
              (setf (aref (aref data r) c) 0)
              (cl-incf flash-ct))))))
    flash-ct))

(defun aoc21-day-11-2 ()
  (let* ((data (aoc-num-grid (f-read "puzzle13.txt"))) ;; 
         (step 0)
         (debug 0))
    (catch 'all-flash
      (while t
        (cl-incf step)
        (dotimes (r 10)
          (dotimes (c 10)
            (cl-incf (aref (aref data r) c))))
        (let ((flashes '()))
          (catch 'done
            (while t
              (let ((prev-ct (length flashes)))
                (dotimes (r 10)
                  (dotimes (c 10)
                    (when (and (> (aaref data r c) 9) (not (member (list r c) flashes)))
                      (aoc21-day-10--energize-neighbors data r c)
                      (setq flashes (cl-adjoin (list r c) flashes :test 'equal)))))
                (when (not (> (length flashes) prev-ct))
                  (throw 'done nil)))))
          (when (= 100 (length flashes))
            (throw 'all-flash step))
          (dolist (flash flashes)
            (pcase-let ((`(,r ,c) flash))
              (when (> (aaref data r c) 9)
                (setf (aref (aref data r) c) 0)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Day 12 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun aoc21-day-12-small-cave-p (sym)
  (equal (symbol-name sym) (downcase (symbol-name sym))))

(defvar aoc21-day-12-paths nil)

(defun aoc21-day-12-traverse (at paths path &optional no-small-cave)
  (if (eql at 'end)
      (setq aoc21-day-12-paths (append aoc21-day-12-paths (list path)))
    (let ((to-paths (seq-filter (lambda (p)
                                  (and
                                   (not (eql 'start (cadr p)))
                                   (or (not (aoc21-day-12-small-cave-p (cadr p)))
                                       no-small-cave
                                       (not (memq (cadr p) path)))
                                   (eql (car p) at)))
                                paths)))
      (dolist (p to-paths)
        (pcase-let ((`(,at ,cave) p))
          (let ((small-cave-twice-p (and (aoc21-day-12-small-cave-p cave)
                                         (memq cave path))))
            (aoc21-day-12-traverse cave paths (append path (list cave))
                                   (if small-cave-twice-p nil no-small-cave))))))))

(defun aoc21-day-12-2 ()
  (let* ((data (aoc-parsed-lines (f-read "puzzle12.txt") "\\([^-]+\\)-\\([^-]+\\)" 'read 'read)))
    (setq aoc21-day-12-paths nil)
    (setq data (append data (seq-map (lambda (x) (list (cadr x) (car x))) data)))
    (aoc21-day-12-traverse 'start data '(start) t)
    (aocp (length aoc21-day-12-paths))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Day 13 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun aoc21-day-13-fold-vertical (fold-x pts)
  (delete-dups
   (seq-map (lambda (pt)
              (pcase-let ((`(,x ,y) pt))
                (if (> x fold-x)
                    (list (- fold-x (- x fold-x)) y)
                  (list x y))))
            pts)))

(defun aoc21-day-13-fold-horizontal (fold-y pts)
  (delete-dups
   (seq-map (lambda (pt)
              (pcase-let ((`(,x ,y) pt))
                (if (> y fold-y)
                    (list x (- fold-y (- y fold-y)))
                  (list x y))))
            pts)))

(defun aoc21-day-13-1 ()
  (let* ((data (aoc-groups (f-read "puzzle13.txt")))
         (pts (seq-map #'aoc-ints (aoc-lines (nth 0 data))))
         (folds (aoc-parsed-lines (nth 1 data) "fold along \\(.\\)=\\(.*\\)" 'read 'read)))
    (length (aoc21-day-13-fold-vertical 655 pts))))

(defun aoc21-day-13-2 ()
  (let* ((data (aoc-groups (f-read "puzzle13.txt")))
         (pts (seq-map #'aoc-ints (aoc-lines (nth 0 data))))
         (folds (aoc-parsed-lines (nth 1 data) "fold along \\(.\\)=\\(.*\\)" 'read 'read)))
    (dolist (fold folds)
      (pcase fold
        (`(x ,amt) (setq pts (aoc21-day-13-fold-vertical amt pts)))
        (`(y ,amt) (setq pts (aoc21-day-13-fold-horizontal amt pts)))))
    (with-current-buffer (get-buffer-create "*aoc-answer*")
      (erase-buffer)
      (dotimes (row 100)
        (dotimes (col 100)
          (if (member (list row col) pts)
              (insert "#")
            (insert " ")))
        (insert "\n")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Day 14 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun aoc21-day-14-1 ()
  (let* ((groups (aoc-groups (f-read "puzzle14.txt"))) ;; 
         (start-list (seq-map (lambda (x) (read (char-to-string x))) (car groups)))
         (first-sym (car start-list))
         (last-sym (last start-list))
         (rule-list (aoc-parsed-lines (cadr groups) "\\(.\\)\\(.\\) -> \\(.\\)" #'read #'read #'read))
         (rules (seq-map (lambda (l) (cons (list (car l) (cadr l)) (caddr l))) rule-list))
         (start (make-hash-table :test 'equal)))
    (seq-mapn (lambda (a b)
                (inchash (list a b) start))
              start-list (cdr start-list))
    (dotimes (n 40)
      (let ((next (make-hash-table :test 'equal)))
        (maphash (lambda (polymer ct)
                   (let* ((left (car polymer))
                          (right (cadr polymer))
                          (match (cdr (assoc (list left right) rules))))
                     (inchash (list left match) next ct)
                     (inchash (list match right) next ct)))
                 start)
        (setq start next)))
    (let ((counts (make-hash-table)))
      (maphash (lambda (polymer ct)
                 (let ((left (car polymer))
                       (right (cadr polymer)))
                   (inchash left counts ct)
                   (inchash right counts ct)))
               start)
      (inchash first-sym counts)
      (inchash last-sym counts)
      (maphash (lambda (k ct)
                 (puthash k (/ ct 2) counts))
               counts)
      (let ((min 99999999999999)
            (max 0))
        (maphash (lambda (k v)
                   (when (> v max)
                     (setq max v))
                   (when (< v min)
                     (setq min v)))
                 counts)
        (- max min)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Day 14 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun aoc-21-traverse (board r c cost seen)
  (if (or (< r 0) (< c 0) (> r 99) (> c 99) (> cost 390) (member (cons r c) seen))
      999999
    (let ((new-cost (+ cost (aaref board r c)))
          (new-seen (cons (cons r c) seen)))
      (if (and (= 99 r) (= 99 c))
          (aocp new-cost)
        (min (aoc-21-traverse board (1+ r) c new-cost new-seen)
             (aoc-21-traverse board r (1+ c) new-cost new-seen)
             (aoc-21-traverse board (1- r) c new-cost new-seen)
             (aoc-21-traverse board r (1- c) new-cost new-seen))))))

(defun aoc-21-day-14-1 ()
  (let* ((data (aoc-grid (f-read "puzzle15.txt"))))
    (aoc-21-traverse data 0 0 0 '())))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Day 15 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'heap)

(defun aoc-day-15-cmp-heap (a b)
  (< (car a) (car b)))

(defun aoc-day-15-next-item (heap visited)
  (catch 'done
    (while t
      (let* ((next (heap-delete-root heap))
             (row (cadr next))
             (col (caddr next)))
        (when (not (gethash (list row col) visited))
          (throw 'done next))))))

(defun aoc-day-15 ()
  (let* ((size 100)
         (data (aoc-input))
         (weights (make-hash-table :test 'equal))
         (distances (make-hash-table :test 'equal))
         (visited (make-hash-table :test 'equal))
         (heap (make-heap 'aoc-day-15-cmp-heap)))
    (dotimes (r (* size 5))
      (dotimes (c (* size 5))
        (let* ((rinc (/ r size))
               (cinc (/ c size))
               (val (1+ (mod (1- (+ (aaref data (mod r size) (mod c size)) rinc cinc)) 9)))
               (val (if (= val 0) 1 val)))
          (puthash (list r c) val weights))))
    (dotimes (r (* size 5))
      (dotimes (c (* size 5))
        (puthash (list r c) 9999999 distances)))
    (puthash (list 0 0) 0 distances)
    (heap-add heap '(0 0 0))
    (catch 'done
      (while t
        (cl-incf debug)
        (aocp debug)
        (let* ((next (aoc-day-15-next-item heap visited))
               (at-row (cadr next))
               (at-col (caddr next))
               (at-weight (car next)))
          (puthash (list at-row at-col) at-weight visited)
          (when (and (= at-row (1- (* size 5))) (= at-col (1- (* 5 size))))
            (throw 'done (aocp at-weight)))
          (dolist (diff '((-1 0) (1 0) (0 -1) (0 1)))
            (pcase-let* ((`(,dr ,dc) diff)
                         (row (+ at-row dr))
                         (col (+ at-col dc)))
              (let ((d (gethash (list row col) distances))
                    (w (gethash (list row col) weights)))
                (when (and d (not (gethash (list row col) visited)))
                  (heap-add heap (list (min d (+ w at-weight)) row col))
                  (puthash (list row col) (min d (+ w at-weight)) distances))))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Day 16 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar aoc-21-version-sum nil)
(defvar aoc-21-position nil)
(defvar aoc-21-data nil)

(defun aoc-21-read-bits (ct)
  (let ((s ""))
    (dotimes (n ct)
      (setq s (concat s (format "%d" (aref aoc-21-data (+ aoc-21-position n))))))
    (cl-incf aoc-21-position ct)
    (read (concat "#b" s))))

(defun aoc-21-grab-bits (ct)
  (let ((bits '()))
    (dotimes (n ct)
      (push (aref aoc-21-data (+ aoc-21-position n)) bits))
    (cl-incf aoc-21-position ct)
    (reverse bits)))

(defun aoc-21-binary-to-decimal (digits)
  (let ((num ""))
    (dolist (d digits)
      (setq num (concat num (format "%d" d))))
    (read (concat "#b" num))))

(defun aoc-21-hex-to-binary (h)
  (vconcat (seq-mapcat (lambda (hd)
                         (pcase hd
                           (?0 '(0 0 0 0)) (?1 '(0 0 0 1)) (?2 '(0 0 1 0)) (?3 '(0 0 1 1)) (?4 '(0 1 0 0))
                           (?5 '(0 1 0 1)) (?6 '(0 1 1 0)) (?7 '(0 1 1 1)) (?8 '(1 0 0 0))
                           (?9 '(1 0 0 1)) (?A '(1 0 1 0)) (?B '(1 0 1 1)) (?C '(1 1 0 0))
                           (?D '(1 1 0 1)) (?E '(1 1 1 0)) (?F '(1 1 1 1)) ))
                       (string-to-list h))))

(defun aoc-21-parse-version ()
  (aoc-21-read-bits 3))

(defun aoc-21-parse-packet-type ()
  (aoc-21-read-bits 3))

(defun aoc-21-parse-literal-packet ()
  (let ((number '()))
    (catch 'done
      (while t
        (let ((bits (aoc-21-grab-bits 5)))
          (setq number (append number (cdr bits)))
          (when (= 0 (car bits))
            (throw 'done number)))))
    (aoc-21-binary-to-decimal number)))

(defun aoc-21-parse-operator-contents--length (len)
  (let ((children '())
        (target-pos (+ aoc-21-position len)))
    (while (< aoc-21-position target-pos)
      (push (aoc-21-parse-packet) children))
    (reverse children)))

(defun aoc-21-parse-operator-contents--count (ct)
  (let ((children '()))
    (dotimes (i ct)
      (push (aoc-21-parse-packet) children))
    (reverse children)))

(defun aoc-21-parse-operator-contents ()
  (let ((length-type (car (aoc-21-grab-bits 1))))
    (if (= length-type 0)
        (let ((length (aoc-21-read-bits 15)))
          (aoc-21-parse-operator-contents--length length))
      (let ((sub-ct (aoc-21-read-bits 11)))
        (aoc-21-parse-operator-contents--count sub-ct)))))

(defun aoc-21-parse-packet ()
  (let ((version (aoc-21-parse-version))
        (type (aoc-21-parse-packet-type)))
    (cl-incf aoc-21-version-sum version)
    (if (= type 4)
        (aoc-21-parse-literal-packet)
      (let ((children (aoc-21-parse-operator-contents)))
        (cons type children)))))

(defun aoc-21-eval-expr (expr)
  (if (not (listp expr))
      expr
    (let ((op (car expr))
          (args (seq-map #'aoc-21-eval-expr (cdr expr))))
      (pcase op
        (0 (apply '+ args))
        (1 (apply '* args))
        (2 (apply 'min args))
        (3 (apply 'max args))
        (5 (if (apply '> args) 1 0))
        (6 (if (apply '< args) 1 0))
        (7 (if (apply '= args) 1 0))))))

(defun aoc-21-day-16 ()
  (let ((puzzle-input (f-read "puzzle16.txt")))
    (setq aoc-21-data (aoc-21-hex-to-binary puzzle-input))
    (setq aoc-21-position 0)
    (setq aoc-21-version-sum 0)
    (let ((parse (aoc-21-parse-packet)))
      (aocp (aoc-21-eval-expr parse)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Day 17 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun aoc-21-day-17-in-range (x y)
  (and (<= 206 x 250)
       (<= -105 y -57)))

(defun aoc-21-day-17-overshot-p (x y)
  (or (> x 250)
      (< y -105)))

(defun aoc-21-day-17-simulate-shot (vx vy)
  (let ((x 0)
        (y 0)
        (max-height 0))
    (while (not (or (aoc-21-day-17-in-range x y)
                    (aoc-21-day-17-overshot-p x y)))
      (setq x (+ x vx))
      (setq y (+ y vy))
      (when (> vx 0)
        (setq vx (- vx 1)))
      (setq vy (- vy 1))
      (when (> y max-height)
        (setq max-height y)))
    (if (aoc-21-day-17-in-range x y)
        (aocp max-height)
      nil)))

(defun aoc-21-day-17-day-1 ()
  (aocp (cl-loop for vx from 19 to 22
                 maximize (cl-loop for vy from 0 to 1000
                                   maximize (aoc-21-day-17-simulate-shot vx vy)))))

(defun aoc-21-day-17-day-2 ()
  (let ((ct 0))
    (aocp (cl-loop for vx from 10 to 250
                   do (cl-loop for vy from 105 to 1000
                               do (when (aoc-21-day-17-simulate-shot vx vy)
                                    (cl-incf ct)))))
    (aocp ct)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; day 18 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun aoc21-day-18-explode (n pos)
  (with-current-buffer (get-buffer-create "*aoc-test*")
    (erase-buffer)
    (insert n)
    (goto-char pos)
    (search-forward-regexp "\\([[:digit:]]+\\)")
    (let ((l (read (match-string 0))))
      (search-forward-regexp "\\([[:digit:]]+\\)")
      (let ((r (read (match-string 0))))
        (goto-char pos)
        (while (not (looking-at "]"))
          (delete-forward-char 1))
        (delete-forward-char 1)
        (insert "0")
        (when (search-forward-regexp "\\([[:digit:]]+\\)" nil t)
          (let ((n (read (match-string 0))))
            (while (looking-back "[[:digit:]]")
              (delete-forward-char -1))
            (insert (format "%d" (+ n r)))))
        (goto-char pos)
        
        (when (search-backward-regexp "\\([[:digit:]]+\\)" nil t)
          (let ((n (thing-at-point 'number)))
            (while (looking-at "[[:digit:]]")
              (delete-forward-char 1))
            (while (looking-back "[[:digit:]]")
              (delete-forward-char -1))
            (insert (format "%d" (+ n l)))))))
    (buffer-string)))

(defun aoc21-day-18-scan-explodes (n)
  (with-temp-buffer
    (insert n)
    (goto-char (point-min))
    (catch 'done
      (let ((depth 0))
        (while (not (eobp))
          (when (looking-at "\\[")
            (setq depth (1+ depth)))
          (when (looking-at "\\]")
            (setq depth (1- depth)))
          (when (= depth 5)
            (throw 'done
                   (aoc21-day-18-explode n (point))))
          (forward-char 1)))
      nil)))

(defun aoc21-day-18-scan-split (n)
  (with-temp-buffer
    (insert n)
    (goto-char (point-min))
    (when (search-forward-regexp "\\([[:digit:]][[:digit:]]+\\)" nil t)
      (let ((number (read (match-string 0))))
        (while (looking-back "[[:digit:]]")
          (delete-forward-char -1))
        (insert (format "[%d,%d]" (/ number 2)
                        (+ (/ number 2)
                           (if (oddp number) 1 0))))
        (buffer-string)))))

(defun aoc21-day-18-add (n1 n2)
  (let ((s (concat "[" n1 "," n2 "]"))
        (cont t))
    (aocp s)
    (while cont
      (setq cont nil)
      (let ((expl (aoc21-day-18-scan-explodes s)))
        (when expl
          (setq cont t)
          (setq s expl)
          (aocp expl))
        (when (not expl)
          (let ((split (aoc21-day-18-scan-split s)))
            (when split
              (setq cont t)
              (setq s split)
              (aocp split))))))
    s))

(defun aoc21-day-18-magnitude* (x)
  (if (consp x)
      (let ((l (aoc21-day-18-magnitude* (car x)))
            (r (aoc21-day-18-magnitude* (cdr x))))
        (if (not r)
            l
          (+ (* 3 l) (* 2 r))))
    x))

(defun aoc21-day-18-magnitude (s)
  (with-temp-buffer
    (insert s)
    (replace-string "[" "(" nil (point-min) (point-max))
    (replace-string "]" ")" nil (point-min) (point-max))
    (replace-string "," " . " nil (point-min) (point-max))
    (aoc21-day-18-magnitude*
     (read (buffer-string)))))

(defun aoc21-day-18-part-1 ()
  (let ((lines (aoc-lines (f-read "puzzle18.txt"))))
    (let ((s (car lines)))
      (dolist (line (cdr lines))
        (setq s (aoc21-day-18-add s line))
        (aocp s))
      (aoc21-day-18-magnitude s))))

(defun aoc21-day-18-part-2 ()
  (let ((lines (aoc-lines (f-read "puzzle18.txt")))
        (max 0))
    (dolist (l1 lines)
      (dolist (l2 lines)
        (when (not (equal l1 l2))
          (let ((magnitude (aoc21-day-18-magnitude (aoc21-day-18-add l1 l2))))
            (when (> magnitude max)
              (setq max magnitude))))))
    max))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Day 19 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defconst aoc21-axis-of-rotation
  '((x y z)   (-x -y z) (-y x z) (y -x z)
    (-z y x)  (z -y x)  (y z x) (-y -z x)
    (-x y -z) (x -y -z) (y x -z) (-y -x z)
    (z y -x)  (-z -y -x) (-y z -x) (y -z -x)
    (x z -y)  (-x -z -y) (-z x -y) (z -x -y)
    (x -z y)  (-x z y) (z x y) (-z -x y)))

(defun aoc21-eval-axis (axis x y z)
  (mapcar
   (lambda (sym)
     (cond
      ((eql sym 'x) x)
      ((eql sym '-x) (- x))
      ((eql sym 'y) y)
      ((eql sym '-y) (- y))
      ((eql sym 'z) z)
      ((eql sym '-z) (- z))))
   axis))

(defun aoc21-permutate-points (axis pts)
  (seq-map (lambda (pt)
             (let ((x (car pt))
                   (y (cadr pt))
                   (z (caddr pt)))
               (aoc21-eval-axis axis x y z)))
           pts))

(defun aoc21-diff-pts (a b)
  (let ((x1 (car a))
        (x2 (car b))
        (y1 (cadr a))
        (y2 (cadr b))
        (z1 (caddr a))
        (z2 (caddr b)))
    (list (- x1 x2)
          (- y1 y2)
          (- z1 z2))))

(defun aoc21-translate-point (pt offset)
  (pcase-let ((`(,dx ,dy ,dz) offset))
    (pcase-let ((`(,x ,y ,z) pt))
      (list (+ x dx)
            (+ y dy)
            (+ z dz)))))

(defun aoc21-consolidate-points (pts1 pts2)
  (catch 'done 
    (dolist (axis aoc21-axis-of-rotation)
      (let ((pts2v (aoc21-permutate-points axis pts2)))
        (dolist (pt2 pts2v) ; for each point in the variant
          (dolist (pt1 pts1) ; for each point in pts 1
            (let* ((diff (aoc21-diff-pts pt1 pt2))
                   (pts2voff (seq-map (lambda (pt) (aoc21-translate-point pt diff)) pts2v))
                   (intersection (cl-intersection pts1 pts2voff :test 'equal)))
              (when (>= (length intersection) 12)
                (throw 'done (list axis diff))))))))))

(defun aoc21-day-19-parse-input ()
  (aoc-groups-of
   (f-read "puzzle19.txt")
   (lambda (group)
     (let* ((lines (aoc-lines group))
            (first-line (car lines))
            (rest-lines (cdr lines)))
       (cons
        (car (aoc-ints first-line))
        (seq-map (lambda (nums) (seq-map #'read (split-string nums ","))) rest-lines))))))

(defun aoc21-day-19-find-translations ()
  (let ((puzzle (aoc21-day-19-parse-input))
        (translations '()))
    (dolist (scanner-1 puzzle)
      (let ((idx-1 (car scanner-1))
            (pts-1 (cdr scanner-1)))
        (dolist (scanner-2 puzzle)
          (let ((idx-2 (car scanner-2))
                (pts-2 (cdr scanner-2)))
            (when (not (> idx-2 idx-1))
              (let ((join (aoc21-consolidate-points pts-1 pts-2)))
                (when join
                  (setq translations (cons (list idx-1 idx-2 join) translations)))))))))))

(defconst aoc21-translations (aoc21-day-19-find-translations))

(defun aoc21-day-19-input-hash (&optional part-2)
  (let ((puzzle (aoc21-day-19-parse-input))
        (hash (make-hash-table :test 'equal)))
    (dolist (p puzzle)
      (let ((idx (car p))
            (pts (cdr p)))
        (puthash idx pts hash)))
    (when part-2
      (dotimes (idx 34)
        (puthash idx (cons '(0.25 0.25 0.25) (gethash idx hash)) hash)))
    hash))

(defconst aoc21-combined-data (aoc21-day-19-part-1))

(defun aoc21-day-19-part-1 (&optional part-2)
  (let ((data (aoc21-day-19-input-hash)))
    (dotimes (_ 30)
      (dolist (diff aoc21-translations)
        (pcase-let ((`(,idx-a ,idx-b (,axis ,diff)) diff))
          (let* ((pts-a (gethash idx-a data))
                 (pts-b (gethash idx-b data))
                 (pts-b-rotate (aoc21-permutate-points axis pts-b))
                 (pts-b-moved (seq-map (lambda (pt) (aoc21-translate-point pt diff)) pts-b-rotate))
                 (union-pts (cl-union pts-a pts-b-moved :test 'equal)))
            (puthash idx-a union-pts data)))))
    data))

(defconst aoc21-day-19-data (aoc21-day-19-part-1 t))

(defun aoc21-day-19-part-2 ()
  (let ((max 0)
      (scanner-pts (let ((pts (gethash 0 aoc21-day-19-data)))
                     (seq-map (lambda (pt) (seq-map #'round pt))
                              (seq-filter (lambda (pt) (not (integerp (car pt)))) pts)))))
  (dolist (pt1 scanner-pts)
    (dolist (pt2 scanner-pts)
      (when (not (equal pt1 pt2))
        (let ((m-dist (aoc21-manhattan-dist pt1 pt2)))
          (when (> m-dist max)
            (setq max m-dist))))))
  max))

(defun aoc21-manhattan-dist (pt1 pt2)
  (pcase-let ((`(,x1 ,y1 ,z1) pt1))
    (pcase-let ((`(,x2 ,y2 ,z2) pt2))
      (+ (abs (- x1 x2))
         (abs (- y1 y2))
         (abs (- z1 z2))))))

(provide 'aoc21)

;;; aoc21.el ends here
