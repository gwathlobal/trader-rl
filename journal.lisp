(in-package :trader-rl)

;; a journal is ((<date> <importance> <string> <id>) ...)
;; where
;; <id> - id of the entry, all entries across the whole world must have unique ids
;; <date> - wtime when the entry was made
;; <importance> - parameter to define how difficult it is to forget the entry
;; <string> - the text of the entry 

(defconstant +journal-importance-low+ 0)
(defconstant +journal-importance-high+ 1)

(defparameter *journal-entry-id* 0)

(defun add-to-journal (old-journal-list &key date importance string)
  (pushnew (list date importance string *journal-entry-id*) old-journal-list)
  (incf *journal-entry-id*)
  old-journal-list)  

(defun remove-n-from-journal (old-journal-list n)
  (let ((new-journal-list))
    (setf new-journal-list (remove (nth n old-journal-list) old-journal-list))
    new-journal-list))

(defun remove-from-journal (old-journal-list journal-entry)
  (let ((new-journal-list))
    (setf new-journal-list (remove journal-entry old-journal-list))
    new-journal-list))

(defun get-n-journal-entry (old-journal-list n)
  (nth n old-journal-list))

(defun get-n-journal-entry-date (old-journal-list n)
  (first (nth n old-journal-list)))

(defun get-n-journal-entry-importance (old-journal-list n)
  (second (nth n old-journal-list)))

(defun get-n-journal-entry-string (old-journal-list n)
  (third (nth n old-journal-list)))

(defun get-n-journal-entry-id (old-journal-list n)
  (fourth (nth n old-journal-list)))

(defun get-journal-entry-date (journal-entry)
  (first journal-entry))

(defun get-journal-entry-importance (journal-entry)
  (second journal-entry))

(defun get-journal-entry-string (journal-entry)
  (third journal-entry))

(defun get-journal-entry-id (journal-entry)
  (fourth journal-entry))

(defun show-journal-entry (journal-entry)
  (format nil "~35@A - ~A" (show-date-time (get-journal-entry-date journal-entry)) (get-journal-entry-string journal-entry)))

(defun merge-journals (journal-1 journal-2)
  (let ((new-journal (copy-list journal-1)))
    (dolist (entry journal-2)
      (unless (find entry new-journal)
        (pushnew entry new-journal)))
    (stable-sort new-journal #'> :key #'get-journal-entry-id)))
