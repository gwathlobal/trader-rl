(in-package :trader-rl)

;; the year has four months, 30 days each
(defparameter *month-names* (list "Helmet's Harvest" "New Life" "Moving Stars" "Windy Whispers"))

(defun show-date-time (wtime)
  (multiple-value-bind (year r) (truncate wtime 120)
    (multiple-value-bind (month day) (truncate r 30)
      (format nil "~A of ~A, year ~A" (1+ day) (nth month *month-names*) (1+ year)))))
