(in-package :trader-rl)

;; ==============================

(defclass quest-type ()
  ((id :initform nil :accessor id)
   
   ;; deprecated
   (intro-str :initarg :intro-str :accessor intro-str)
   (quest-item-id :initarg :quest-item-id :accessor quest-item-id)
   (quest-item-num :initarg :quest-item-num :accessor quest-item-num)
   
   ;; new
   
   ))

(defun set-quest-type-by-id (quest-type-id quest-type)
  (setf (id quest-type) quest-type-id)
  (setf (gethash quest-type-id *quest-types*) quest-type))

(defun get-quest-type-by-id (quest-type-id)
  (gethash quest-type-id *quest-types*))

(defclass quest-type-delivery (quest-type)
  ())
