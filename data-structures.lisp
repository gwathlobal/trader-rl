(in-package :trader-rl)

(defclass link ()
  ((id :initform nil :initarg :id :accessor id) 
   (dst-id :initform nil :initarg :dst-id :accessor dst-id) ;; id of dst settlement
   (days :initform nil :initarg :days :accessor days)))

(defmethod initialize-instance :after ((link link) &key)
  (setf (id link) (find-free-id *links*))
  (setf (gethash (id link) *links*) link)
  )

;; ==============================

(defclass settlement ()
  ((id :initform nil :initarg :id :accessor id)
   (x :initform 0 :initarg :x :accessor x)
   (y :initform 0 :initarg :y :accessor y)
   (name :initform "Unnamed settlement" :initarg :name :accessor name)
   (race-type :initform (random (length *race-names*)) :initarg :race-type :accessor race-type)
   
   (links :initform nil :initarg :links :accessor links)
   ;; type - (link-id link-id ...)
   
   (market :initform (make-hash-table) :accessor market)

   (features :initform () :initarg :features :accessor features) ;; list of +feature-type-...+ constants

   (events :initform (make-hash-table) :accessor events)

   (journal :initform () :initarg :journal :accessor journal) ;; see journal.lisp
   
   (realm-id :initform nil :initarg :realm-id :accessor realm-id)
   (settlement-type :initform 0 :initarg :settlement-type :accessor settlement-type)
   (settlement-size :initform 0 :initarg :settlement-size :accessor settlement-size)
   
   (base-demand-supply :initform (make-hash-table) :accessor base-demand-supply)
   (cur-demand-supply :initform (make-hash-table) :accessor cur-demand-supply)
  
   ))

(defmethod initialize-instance :after ((settlement settlement) &key)
  (setf (id settlement) (find-free-id *settlements*))
  (setf (gethash (id settlement) *settlements*) settlement))

(defun initialize-features (settlement)
  (set-settlement-feature settlement +feature-type-population+)
  (cond
    ((= (settlement-type settlement) +settlement-type-agriculture+)
     (set-settlement-feature settlement +feature-type-farm+)
     )
    ((= (settlement-type settlement) +settlement-type-mining+)
     (set-settlement-feature settlement +feature-type-mine+)
     )
    ((= (settlement-type settlement) +settlement-type-industry+)
     (set-settlement-feature settlement +feature-type-craftshop+)
     (set-settlement-feature settlement +feature-type-jewelshop+)
     (set-settlement-feature settlement +feature-type-weaponshop+)
     )
    ((= (settlement-type settlement) +settlement-type-sprawling+)
     (set-settlement-feature settlement +feature-type-palace+)
     )
    ))

(defun initialize-demand-supply (settlement)
  (loop
    for item-type being the hash-value in *item-types*
    do
       (setf (gethash (id item-type) (base-demand-supply settlement)) (cons 0 0)))
  )

(defun initialize-current-demand-supply (settlement)
  (loop
    for item-type being the hash-value in *item-types*
    do
       (setf (gethash (id item-type) (cur-demand-supply settlement)) (cons (car (gethash (id item-type) (base-demand-supply settlement)))
                                                                           (cdr (gethash (id item-type) (base-demand-supply settlement)))))))

(defun get-settlement-cur-demand (settlement item-type-id)
  (car (gethash item-type-id (cur-demand-supply settlement))))

(defun get-settlement-cur-supply (settlement item-type-id)
  (cdr (gethash item-type-id (cur-demand-supply settlement))))

(defun get-settlement-base-demand (settlement item-type-id)
  (car (gethash item-type-id (base-demand-supply settlement))))

(defun get-settlement-base-supply (settlement item-type-id)
  (cdr (gethash item-type-id (base-demand-supply settlement))))

(defun set-settlement-cur-demand (settlement item-type-id value)
  (setf (car (gethash item-type-id (cur-demand-supply settlement))) value))

(defun set-settlement-cur-supply (settlement item-type-id value)
  (setf (cdr (gethash item-type-id (cur-demand-supply settlement))) value))

(defun set-settlement-base-demand (settlement item-type-id value)
  (setf (car (gethash item-type-id (base-demand-supply settlement))) value))

(defun set-settlement-base-supply (settlement item-type-id value)
  (setf (cdr (gethash item-type-id (base-demand-supply settlement))) value))

(defun set-settlement-base-demand-supply (settlement item-type-id &key (demand 0) (supply 0))
  (setf (gethash item-type-id (base-demand-supply settlement)) (cons demand supply)))

(defun mod-settlement-base-demand-supply (settlement item-type-id &key (demand 0) (supply 0))
  (setf (gethash item-type-id (base-demand-supply settlement)) 
        (cons (+ (get-settlement-base-demand settlement item-type-id) demand) 
              (+ (get-settlement-base-supply settlement item-type-id) supply))))

(defun get-settlement-buy-price (settlement item-type-id)
  (let ((x (1+ (/ (get-settlement-cur-demand settlement item-type-id) 10)))
        (result))
    (setf result (round (* x (base-price (get-item-type-by-id item-type-id)))))
    result))

(defun get-settlement-sell-price (settlement item-type-id)
  (let ((x (1+ (/ (get-settlement-cur-supply settlement item-type-id) 10)))
        (result))
    (setf result (round (* x (base-price (get-item-type-by-id item-type-id)))))
    result))

(defun get-settlement-link (settlement n)
  (gethash (nth n (links settlement)) *links*))

(defun get-settlement-feature (settlement feature-type-id)
  (find feature-type-id (features settlement)))

(defun set-settlement-feature (settlement feature-type-id)
  (unless (get-settlement-feature settlement feature-type-id)
    (pushnew feature-type-id (features settlement))
    ;; invoke on-add function if available
    (when (on-add (get-feature-type-by-id feature-type-id))
      (funcall (on-add (get-feature-type-by-id feature-type-id))
               (get-feature-type-by-id feature-type-id)
               settlement))))

(defun get-settlement-realm (settlement)
  (gethash (realm-id settlement) *realms*))

(defun set-settlement-realm (settlement realm)
  (setf (realm-id settlement) (id realm)))

(defun get-settlement-size-name (settlement)
  (nth (settlement-size settlement) *settlement-size-names*))

(defun get-settlement-type-adj (settlement)
  (nth (settlement-type settlement) *settlement-type-names*))

(defun get-settlement-descr-for-player (settlement)
  (let ((str (create-string)))
    (format str "You are in the ~A ~A of ~A~%" (get-settlement-type-adj settlement) (get-settlement-size-name settlement) (name settlement))
    (format str "It is a ~(~A~) ~A.~%" (nth (race-type settlement) *race-names*) (get-settlement-size-name settlement))
    
    (when (features settlement)
      (format str "~A~%" (show-settlement-features settlement)))

    (format str "~A" (show-settlement-events settlement))
    
    (format str "~%It is day ~A today.~%" (show-date-time (wtime *world*)))
    (format str "~%")
    str))

;; ==============================


(defclass trader ()
  ((id :initform nil :initarg :id :accessor id)
   (name :initform "Anonymous" :initarg :name :accessor name)
   (action-done :initform 1 :accessor action-done) ;; fixnum
   (current-settlement-id :initform nil :initarg :current-settlement-id :accessor current-settlement-id)
   (inv :initform (make-hash-table) :accessor inv)
   (money :initform 0 :initarg :money :accessor money)
   (journal :initform () :initarg :journal :accessor journal)
   ))

(defmethod initialize-instance :after ((trader trader) &key)
  (setf (id trader) (find-free-id *traders*))
  (setf (gethash (id trader) *traders*) trader)
  )

;; ==============================

(defclass item-type ()
  ((id :initform nil :accessor id)
   (name :initform "Item" :initarg :name :accessor name)
   (descr :initform "Description" :initarg :descr :accessor descr)
   (base-price :initform 0 :initarg :base-price :accessor base-price)))

(defun set-item-type-by-id (item-type-id item-type)
  (setf (id item-type) item-type-id)
  (setf (gethash item-type-id *item-types*) item-type))

(defun get-item-type-by-id (item-type-id)
  (gethash item-type-id *item-types*))

;; ==============================

(defclass world ()
  ((wtime :initform 0 :initarg :wtime :accessor wtime)))

;; ==============================

(defclass realm ()
  ((id :initform nil :accessor id)
   (ruler-race :initarg :ruler-race :accessor ruler-race)
   (ruler-name :initarg :ruler-name :accessor ruler-name)
   (ruler-male :initarg :ruler-male :accessor ruler-male)
   (ruler-title :initarg :ruler-title :accessor ruler-title)
   (ruler-favor :initform 0 :initarg :ruler-favor :accessor ruler-favor)
   (audience :initform nil :accessor audience)
   (cur-quest :initform nil :initarg :cur-quest :accessor cur-quest)
   (quest-timer :initform 0 :accessor quest-timer)
   ))

(defmethod initialize-instance :after ((realm realm) &key)
  (setf (id realm) (find-free-id *realms*))
  (setf (gethash (id realm) *realms*) realm)

  (setf (ruler-title realm) (nth (random (length *ruler-title-names*)) *ruler-title-names*))
  )

(defclass realm-human (realm)
  ())

(defmethod initialize-instance :after ((realm realm-human) &key)
  (if (zerop (random 2))
    (progn
      (setf (ruler-male realm) t)
      (setf (ruler-name realm) (nth (random (length *ruler-human-male-names*)) *ruler-human-male-names*)))
    (progn
      (setf (ruler-male realm) nil)
      (setf (ruler-name realm) (nth (random (length *ruler-human-female-names*)) *ruler-human-female-names*)))))

(defclass realm-tachidi (realm)
  ())

(defmethod initialize-instance :after ((realm realm-tachidi) &key)
  (setf (ruler-male realm) nil)
  (setf (ruler-name realm) (format nil "~A'~A'~A" 
                                   (nth (random (length *ruler-tachidi-first-names*)) *ruler-tachidi-first-names*) 
                                   (nth (random (length *ruler-tachidi-second-names*)) *ruler-tachidi-second-names*)
                                   (nth (random (length *ruler-tachidi-third-names*)) *ruler-tachidi-third-names*)
                                   )))

(defclass realm-gremlin (realm)
  ())

(defmethod initialize-instance :after ((realm realm-gremlin) &key)
  (if (zerop (random 2))
    (progn
      (setf (ruler-male realm) t)
      (setf (ruler-name realm) (nth (random (length *ruler-gremlin-male-names*)) *ruler-gremlin-male-names*)))
    (progn
      (setf (ruler-male realm) nil)
      (setf (ruler-name realm) (nth (random (length *ruler-gremlin-female-names*)) *ruler-gremlin-female-names*)))))

(defclass realm-saurian (realm)
  ())

(defmethod initialize-instance :after ((realm realm-saurian) &key)
  (setf (ruler-male realm) t)
  (setf (ruler-name realm) (format nil "~A'~A" (nth (random (length *ruler-saurian-first-names*)) *ruler-saurian-first-names*) (nth (random (length *ruler-saurian-last-names*)) *ruler-saurian-last-names*))))

;; ==============================

(defclass quest-type ()
  ((id :initform nil :accessor id)
   (intro-str :initarg :intro-str :accessor intro-str)
   (quest-item-id :initarg :quest-item-id :accessor quest-item-id)
   (quest-item-num :initarg :quest-item-num :accessor quest-item-num)))

(defun set-quest-type-by-id (quest-type-id quest-type)
  (setf (id quest-type) quest-type-id)
  (setf (gethash quest-type-id *quest-types*) quest-type))

(defun get-quest-type-by-id (quest-type-id)
  (gethash quest-type-id *quest-types*))

;; ==============================

(defclass feature-type ()
  ((id :initform nil :accessor id)
   (name :initarg :name :accessor name)
   (hidden :initform nil :initarg :hidden :accessor hidden)
   (on-tick :initform nil :initarg :on-tick :accessor on-tick)
   (on-add :initform nil :initarg :on-add :accessor on-add)
   ))

(defun set-feature-type-by-id (feature-type-id feature-type)
  (setf (id feature-type) feature-type-id)
  (setf (gethash feature-type-id *feature-types*) feature-type))

(defun get-feature-type-by-id (feature-type-id)
  (gethash feature-type-id *feature-types*))

;; ==============================

(defclass event-type ()
  ((id :initform nil :accessor id)
   (descr :initform "" :initarg :descr :accessor descr)
   (max-stage :initform 5 :initarg :max-stage :accessor max-stage)
   (on-tick :initform nil :initarg :on-tick :accessor on-tick) ;; when the event is active, invoke every turn
   (on-show :initform nil :initarg :on-show :accessor on-show) ;; invoke when the list of events is being displayed to determine how to show the event description
   (on-rand :initform nil :initarg :on-rand :accessor on-rand) ;; invoke when a random event is triggered to check if the conditions are applicable
   (on-rotate :initform nil :initarg :on-rotate :accessor on-rotate) ;; if set, the event type becomes non-random; the function invokes every turn to check if the conditions are met and the event should apply 
   ))

(defun set-event-type-by-id (event-type-id event-type)
  (setf (id event-type) event-type-id)
  (setf (gethash event-type-id *event-types*) event-type)
  
  ;; if the event has on-rotate function defined, it should be also added to the rotational events table, otherwise add to random events table 
  (if (on-rotate event-type)
    (setf (gethash event-type-id *event-rotate-types*) event-type)
    (unless (find (id event-type) *event-random-types*)
      (pushnew (id event-type) *event-random-types*)
      ))
  )

(defun get-event-type-by-id (event-type-id)
  (gethash event-type-id *event-types*))

(defclass event ()
  ((id :initform nil :accessor id)
   (event-type-id :initarg :event-type-id :accessor event-type-id)
   (stage :initform 0 :initarg :stage :accessor stage)
   ))

(defmethod initialize-instance :after ((event event) &key)
  (setf (id event) (find-free-id *events*))
  (setf (gethash (id event) *events*) event)
  )

(defmethod descr ((event event))
  (descr (get-event-type-by-id (event-type-id event))))

(defmethod on-show ((event event))
  (on-show (get-event-type-by-id (event-type-id event))))

(defmethod on-tick ((event event))
  (on-tick (get-event-type-by-id (event-type-id event))))

(defmethod on-rand ((event event))
  (on-rand (get-event-type-by-id (event-type-id event))))

(defmethod on-rotate ((event event))
  (on-rotate (get-event-type-by-id (event-type-id event))))

(defmethod max-stage ((event event))
  (max-stage (get-event-type-by-id (event-type-id event))))

(defun get-event-settlement (settlement event-type-id)
  (gethash (gethash event-type-id (events settlement)) *events*))

(defun add-event-settlement (settlement event)
  ;;(format t "GET-EVENT-SETTLEMENT ~A~%" (get-event-settlement settlement (event-type-id event)))
  (if (get-event-settlement settlement (event-type-id event))
    nil
    (setf (gethash (event-type-id event) (events settlement)) (id event))))

(defun remove-event-settlement (settlement event-type-id)
  (when (get-event-settlement settlement event-type-id)
    (remhash (id (get-event-settlement settlement event-type-id)) *events*)
    (remhash event-type-id (events settlement))))

;; ======================================

(defun find-free-id (hash-table)
  (do ((id 0 (+ id 1)))
      ((eql (gethash id hash-table) nil) id)))

(defun get-settlement-by-id (settlement-id)
  (gethash settlement-id *settlements*))

(defun get-settlement-info-brief (settlement)
  (let ((str (make-array (list 0) :element-type 'character :adjustable t :fill-pointer t)))
    (format str "")
    (format str "~A (" (name settlement))
    (loop 
      for link-id in (links settlement)
      with link = nil
      do
         (setf link (gethash link-id *links*))
         (format str "-> (~A, days: ~A) " (name (gethash (dst-id link) *settlements*)) (days link)))
    (format str ")")
    ;;(format t "~A~%" str)
    str))

(defun show-game-info ()
  (let ((str (make-array (list 0) :element-type 'character :adjustable t :fill-pointer t)))
    (format str "")
    (loop
      for settlement being the hash-values in *settlements*
      do
         (format str "~A~%" (get-settlement-info-brief settlement)))
    ;;(format t "~A~%" *settlements*)
    str
    ))
 
(defun move-trader-to-settlement (trader new-settlement days)
  (format t "MOVE-TRADER-TO-SETTLEMENT: ~A (ID: ~A) FROM ~A (ID: ~A) TO ~A (ID: ~A)~%" 
          (name trader) (id trader) 
          (name (get-settlement-by-id (current-settlement-id trader))) (current-settlement-id trader) 
          (name new-settlement) (id new-settlement))
  
  (setf (journal trader) (add-to-journal (journal trader) :date (wtime *world*) :importance +journal-importance-low+ 
                                                          :string (format nil "We departed from ~A and travelled to ~A." 
                                                                          (name (get-settlement-by-id (current-settlement-id trader))) 
                                                                          (name new-settlement))))
  (setf (current-settlement-id trader) (id new-settlement))
  (decf (action-done trader) days))

(defun show-item-line (item-type-id qty)
  (let ((str (make-array (list 0) :element-type 'character :adjustable t :fill-pointer t)))
    (if (> qty 1)
      (format str "~A x~A" (name (get-item-type-by-id item-type-id)) qty)
      (format str "~A" (name (get-item-type-by-id item-type-id))))
    str
    ))

(defun show-item-all (item-type-id qty)
  (let ((str (make-array (list 0) :element-type 'character :adjustable t :fill-pointer t)))
    (format str "~A.~%~A~%" (name (get-item-type-by-id item-type-id)) (descr (get-item-type-by-id item-type-id)))
    (when (> qty 1)
      (format str "Quantity: ~A" qty))
    (format str "~%")
    str
    ))

(defun show-settlement-features (settlement)
  (let ((str (create-string)) (feature))
    (loop 
      for i from 0 below (length (features settlement)) 
      do
         (setf feature (get-feature-type-by-id (nth i (features settlement))))
         ;;(format t "FEATURE-ID ~A~%" (nth i (features settlement)))
         (unless (hidden feature)
           (unless (zerop i)
             (format str ","))
           (format str " ~A" (name feature))
           ))
    (setf str (format nil "This ~(~A~) has the following features:~A" (get-settlement-size-name settlement) str))))

(defun show-settlement-events (settlement)
  (let ((str (create-string)) (event) (start t))
    
    (loop 
      for event-type-id being the hash-key in (events settlement) 
      do
         (when start
           (setf start nil)
           (format str "~%"))
         (setf event (get-event-settlement settlement event-type-id))
         ;;(format t "EVENT-ID ~A~%" (descr event))
         
         (if (on-show event)
           (format str "~A" (funcall (on-show event) event settlement))
           (format str "~A~%" (descr event)))
         )
    str))

(defun get-sell-price (item-type-id settlement)
  (get-settlement-sell-price settlement item-type-id))

(defun get-buy-price (item-type-id settlement)
  (get-settlement-buy-price settlement item-type-id))

