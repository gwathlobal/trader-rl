(in-package :trader-rl)

(defconstant +quest-stage-new+ 0)
(defconstant +quest-stage-accepted+ 1)
(defconstant +quest-stage-completed+ 2)
(defconstant +quest-stage-failed+ 3)

(defparameter *quest-stage-names* (list "New" "Accepted" "Completed" "Failed"))

(defclass quest ()
  ((id :initform nil :accessor id)
   (quest-type-id :initarg :quest-type-id :accessor quest-type-id)
   (stage :initform +quest-stage-new+ :initarg :stage :accessor stage)
   (descr :initarg :descr :accessor descr)
   (complete-descr :initarg :complete-descr :accessor complete-descr)
   (quester-id :initform nil :initarg :quester-id :accessor quester-id)
   (event-id :initform nil :initarg :event-id :accessor event-id)
   (date :initform 0 :initarg :date :accessor date)
   (reward :initform 0 :initarg :reward :accessor reward)
   (move-to-market :initform t :initarg :move-to-market :accessor move-to-market)
   (on-check-complete :initform nil :accessor on-check-complete)
   (on-complete :initform nil :accessor on-complete)))

(defmethod initialize-instance :after ((quest quest) &key)
  (setf (id quest) (find-free-id *quests*))
  (setf (gethash (id quest) *quests*) quest)
  )

(defun add-to-quests (old-quest-list quest)
  (pushnew (id quest) old-quest-list)
  old-quest-list)

(defun remove-from-quests (old-quest-list quest-id)
  (let ((new-quest-list))
    (setf new-quest-list (remove quest-id old-quest-list))
    new-quest-list))

(defun get-quest-in-list (quest-list quest-id)
  (find quest-id quest-list))
  
(defun get-n-quest-in-list (quest-list n)
  (nth n quest-list))

(defun get-quest-by-id (quest-id)
  (gethash quest-id *quests*))

(defun remove-quest-from-world (quest-id)
  (remhash quest-id *quests*))

(defun show-quest-entry (quest-id)
  (let ((quest (get-quest-by-id quest-id)))
    (format nil "~35@A - ~10@A - ~A" (show-date-time (date quest)) (nth (stage quest) *quest-stage-names*) (descr quest))))

(defun generate-quest-journal (quest-list)
  (let ((quest-journal nil))
    (dolist (quest-id quest-list)
      (setf quest-journal (append quest-journal (list (show-quest-entry quest-id)))))
    quest-journal))

(defclass quest-delivery (quest)
  ((giver-id :initarg :giver-id :accessor giver-id)
   (item-type-id :initarg :item-type-id :accessor item-type-id)
   (qty :initarg :qty :accessor qty)
   (dst-id :initarg :dst-id :accessor dst-id)))

(defmethod initialize-instance :after ((quest quest-delivery) &key)
  (setf (descr quest) (format nil "~A wants you to deliver ~A ~A to ~A." 
                              (ruler-name (get-realm-by-id (giver-id quest))) 
                              (qty quest) 
                              (name (get-item-type-by-id (item-type-id quest))) 
                              (name (get-settlement-by-id (dst-id quest)))))
  
  (setf (complete-descr quest) (format nil "Deliver ~A ~A to ~A" 
                                       (qty quest) 
                                       (name (get-item-type-by-id (item-type-id quest))) 
                                       (name (get-settlement-by-id (dst-id quest)))))

  (setf (on-check-complete quest) #'(lambda (quest)
                                      (let ((quester (get-trader-by-id (quester-id quest))))
                                        (if (and (= (stage quest) +quest-stage-accepted+)
                                                 (>= (get-item-inv (item-type-id quest) (inv quester)) (qty quest))
                                                 (= (dst-id quest) (current-settlement-id quester)))
                                          t
                                          nil))))

  (setf (on-complete quest) #'(lambda (quest)
                                (let ((quester (get-trader-by-id (quester-id quest))))
                                  (remove-from-inv (item-type-id quest) (inv quester) (qty quest))
                                  (when (move-to-market quest)
                                    (add-to-inv (item-type-id quest) (market (get-settlement-by-id (dst-id quest))) (qty quest)))
                                  (incf (money quester) (reward quest))
                                  (setf (stage quest) +quest-stage-completed+)
                                  (incf (ruler-favor (get-realm-by-id (giver-id quest))) (1+ (random 2)))
                                  )))
  )

