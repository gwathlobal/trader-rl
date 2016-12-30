(in-package :trader-rl)

;; the year has four months, 30 days each
(defparameter *month-names* (list "Helmet's Harvest" "New Life" "Moving Stars" "Windy Whispers"))

(defconstant +days-in-month+ 30)
(defconstant +months-in-year+ 4)

(defun show-date-time (wtime)
  (multiple-value-bind (year r) (truncate wtime (* +days-in-month+ +months-in-year+))
    (multiple-value-bind (month day) (truncate r +days-in-month+)
      (format nil "~A of ~A, year ~A" (1+ day) (nth month *month-names*) (1+ year)))))
