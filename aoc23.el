(require 'dash)

;;; Day 1

(defconst aoc-day1-input (f-read "inputs/day1.txt"))

(defconst aoc-day1-numbers
  '(("one" . "1") ("two" . "2") ("three" . "3")
    ("four" . "4") ("five" . "5") ("six" . "6")
    ("seven" . "7") ("eight" . "8") ("nine" . "9")))

(defun aoc-day1-filter-numbers (line)
  "Remove all non-number characters from LINE."
  (concat (seq-filter (lambda (char) (<= ?0 char ?9)) line)))

(defun aoc-day1-first-and-last (line)
  "Remove all inner characters from LINE."
  (concat (list (aref line 0) (aref line (1- (length line))))))

(defun aoc-day1 (&optional replace-text-digits)
  (let ((input aoc-day1-input))
    (when replace-text-digits
      (pcase-dolist (`(,name . ,digit) aoc-day1-numbers)
        (setq input (string-replace name (concat name digit name) input))))
    (-as-> input in
           (string-lines in)
           (seq-map #'aoc-day1-filter-numbers in)
           (seq-map #'aoc-day1-first-and-last in)
           (seq-map #'string-to-number in)
           (seq-reduce #'+ in 0))))


;;; Day 2

(defconst aoc-day2-config
  '((red . 12)
    (green . 13)
    (blue . 14)))

(defun aoc-max-elts (game)
  (-max-by

   ))


(defun aoc-day2-parse-line (line)
  (let* ((all-bag (nth 1 (string-split line ":" t)))
         (draws (seq-map (lambda (draw)
                           (seq-map (lambda (part)
                                      (pcase-let* ((`(,ct ,color) (string-split (string-trim part) " ")))
                                        (cons (string-to-number ct)
                                              (intern color))))
                                    (string-split draw "," t)))
                         (string-split all-bag ";" t))))
    draws))

(defconst aoc-day2-input (seq-map #'aoc-day2-parse-line (string-lines (f-read "inputs/day2.txt"))))

(defun aoc-day2-possible (game)
  (catch 'done
    (dolist (draw game)
      (pcase-dolist (`(,ct . ,color) draw)
        (let ((limit (alist-get color aoc-day2-config)))
          (when (> ct limit)
            (throw 'done nil)))))
    t))

(defun aoc-day2-1 ()
  (let* ((sum 0))
    (seq-map-indexed
     (lambda (game idx)
       (when (aoc-day2-possible game)
         (cl-incf sum (1+ idx))))
     aoc-day2-input)
    sum))

(defun aoc-day2-power (game)
  (let ((res (ht ('red 0) ('green 0) ('blue 0))))
    (dolist (draw game)
      (pcase-dolist (`(,ct . ,color) draw)
        (when (> ct (ht-get res color 0))
          (ht-set res color ct))))
    (* (ht-get res 'red)
       (ht-get res 'green)
       (ht-get res 'blue))))

(defun aoc-day2-2 ()
  (let* ((sum 0))
    (seq-map-indexed
     (lambda (game idx)
       (cl-incf sum (aoc-day2-power game)))
     aoc-day2-input)
    sum))

;;; Day 3

(defconst aoc-day3-input (f-read "inputs/day3.txt"))

(defun aoc-day3-1 ()
  (let* ((sum 0)
         (gear-sum 0)
         (adj-poss (ht-create))
         (numbers (ht-create))
         (number-poss (ht-create)))
    ;; create data structures containing reference data
    (with-temp-buffer
      (insert-file-contents "inputs/day3.txt")
      (goto-char (point-min))
      (while (not (eobp))
        (let* ((at-char (char-after (point)))
               (line-num (line-number-at-pos))
               (col-num (current-column)))
          (pcase at-char
            (?. (ignore))
            (?\n (ignore))
            ((pred cl-digit-char-p)
             (let ((start (point)))
               (skip-chars-forward "0123456789")
               (let ((number (string-to-number (buffer-substring-no-properties start (point)))))
                 (ht-set numbers `(,line-num ,col-num) number)
                 (cl-loop for c from col-num to (+ col-num (- (point) start 1)) do
                          (ht-set number-poss `(,line-num ,c) number)))
               (backward-char)))
            (_ (ht-set adj-poss `(,line-num ,col-num) at-char ))))
        (forward-char)))
    ;; part1 - sum numbers that have 2 adjacent symbols
    (maphash
     (pcase-lambda (`(,line ,col) num)
       (let ((num-width (length (number-to-string num)))
             (adjacent nil))
         (cl-loop for l from (1- line) to (1+ line) do
                  (cl-loop for c from (1- col) to (+ col num-width) do
                           (progn
                             (when (ht-get adj-poss `(,l ,c))
                               (setq adjacent t)))))
         (when adjacent
           (cl-incf sum num))))
     numbers)
    ;; part2 - sum product of nubmers that make up a gear
    (maphash
     (pcase-lamda (`(,line ,col) sym)
       (when (= sym ?*)
         (let ((adj-nums '()))
           (cl-loop for l from (1- line) to (1+ line) do
                    (cl-loop for c from (1- col) to (1+ col) do
                             (let* ((num (ht-get number-poss `(,l ,c))))
                               (when (and num (not (member num adj-nums)))
                                 (push num adj-nums)))))
           (message "adj-nums: %s" adj-nums)
           (when (= (length adj-nums) 2)
             (cl-incf gear-sum (apply #'* adj-nums))))))
     adj-poss)
    (list sum gear-sum)))

;;; Day 4

(defconst aoc-day4-input (f-read "inputs/day4.txt"))

(defun aoc-day4-parse-input (text)
  (let* ((lines (string-lines text)))
    (seq-map
     (lambda (line)
       (let* ((parts (string-split line " | " t))
              (winning-numbers (cdr (aoc-ints (car parts))))
              (card-numbers (aoc-ints (cadr parts))))
         (list winning-numbers card-numbers)))
     lines)))

(defun aoc-day4-card-to-pt1-pts (card)
  (pcase-let ((score 0)
              (`(,winning-numbers ,card-numbers) card))
    (dolist (card-number card-numbers)
      (when (member card-number winning-numbers)
        (if (= 0 score)
            (setq score 1)
          (setq score (* score 2)))))
    score))

(defun aoc-day4-1 ()
  (let* ((data (aoc-day4-parse-input aoc-day4-input)))
    (-sum (-map #'aoc-day4-card-to-pt1-pts data))))

(defun aoc-day4-card-score (card)
  (pcase-let ((score 0)
              (`(,winning-numbers ,card-numbers) card))
    (dolist (card-number card-numbers)
      (when (member card-number winning-numbers)
        (cl-incf score)))
    score))

(defvar aoc-day4-cache nil)

(defun aoc-day4-final-card-score (scores)
  (if (not scores)
      0
    (if-let ((score (ht-get aoc-day4-cache scores)))
        score
      (let* ((top (car scores))
             (copy-scores (-sum (-map #'aoc-day4-final-card-score (-take top (-tails (cdr scores))))))
             (total-score (1+ copy-scores)))
        (ht-set aoc-day4-cache scores total-score)
        total-score))))

(defun aoc-day4-2 ()
  (let* ((aoc-day4-cache (ht-create))
         (data (aoc-day4-parse-input aoc-day4-input))
         (scores (-map #'aoc-day4-card-score data)))
    (-sum (-map #'aoc-day4-final-card-score (-tails scores)))))


;;; day 5

(load-file "inputs/day5.txt")  ;; puzzle text edited to elisp code

(defun aoc-day5-to-mapping (range-list)
  (let ((res (ht-create)))
    (pcase-dolist (`(,dest ,src ,len) range-list)
      (let* ((diff (- dest src))
             (source-range (cons src
                                 (+ src len -1))))
        (ht-set res source-range diff)))
    res))

(defun aoc-day5-map (source range-map)
  (catch 'done
    (maphash
     (lambda (range map-amt)
       (when (range-member-p source range)
         (throw 'done (+ source map-amt))))
     range-map)
    source))

(defun aoc-day5-1 ()
  (let* ((stages (-map #'aoc-day5-to-mapping aoc-day5-mapping-lists)))
    (apply #'min
           (--map
            (-reduce-from #'aoc-day5-map it stages)
            aoc-day5-seeds))))

(defun aoc-day5-seeds-to-ranges (seeds)
  (-map
   (pcase-lambda (`(,start ,length))
     (cons start (+ start length -1)))
   (-partition 2 seeds)))

(defun aoc-day5-seeds-to-ranges-2 (seeds)
  (-reduce-from
   (pcase-lambda (acc `(,start ,length))
     (range-concat acc (cons start (+ start length -1))))
   '()
   (-partition 2 seeds)))

(defun aoc-day5-map-ranges (source-ranges mapping)
  (setq source-ranges (range-sort-normalize source-ranges))
  (let ((res '()))
    (maphash
     (lambda (map-range map-amt)
       (let ((overlap (range-intersection-simple source-ranges map-range)))
         (when overlap
           (aocp (list map-range map-amt overlap (range-shift map-amt overlap)))
           (setq res (range-concat res (range-shift map-amt overlap)))
           (setq source-ranges (range-difference source-ranges overlap)))))
     mapping)
    (range-concat res source-ranges)))

(defun aoc-day5-2 ()
  (let* ((seed-ranges (range-sort-normalize (aoc-day5-seeds-to-ranges-2 aoc-day5-seeds)))
         (stages (-map #'aoc-day5-to-mapping aoc-day5-mapping-lists)))
    (apply #'min
     (-map (lambda (x) (if (consp x) (car x) x))
           (-reduce-from
            #'aoc-day5-map-ranges
            seed-ranges
            stages)))))

(provide 'oac)
;;; aoc.el ends here
