(in-package :trader-rl)

(defun make-world-turn ()
  (loop
    for settlement being the hash-value in *settlements*
    do
       (adjust-settlement-features settlement)
       (adjust-settlement-prices settlement)
       (adjust-settlement-events settlement)
       (adjust-settlement-journal settlement)
    )

  (loop
    for realm being the hash-value in *realms*
    do
       (adjust-realm realm)
       (adjust-realm-quests realm)
    )

  (loop
    for trader being the hash-value in *traders*
    do
       (adjust-trader-journal trader)
       (adjust-trader-quests trader)
    )
  
  (incf (wtime *world*)))

(defun adjust-settlement-prices (settlement)
  (loop
    for item-type-id being the hash-key in (cur-demand-supply settlement)
    with base-demand = 0
    with base-supply = 0
    with cur-demand = 0
    with cur-supply = 0
    do
       (setf base-demand (get-settlement-base-demand settlement item-type-id))
       (setf base-supply (get-settlement-base-supply settlement item-type-id))
       (setf cur-demand (get-settlement-cur-demand settlement item-type-id))
       (setf cur-supply (get-settlement-cur-supply settlement item-type-id))
       
       ;;(format t "~%ADJUST-SETTLEMENT-PRICES: ~A, ~A, DELTA-DEMAND ~A, DELTA-SUPPLY ~A~%" (name settlement) (name (get-item-type-by-id item-type-id)) (- base-demand cur-demand) (- base-supply cur-supply))

       (cond
         ((> (- base-demand cur-demand) 0)
          (when (zerop (random 5))
            (set-settlement-cur-demand settlement item-type-id (1+ cur-demand)))
          )
         ((< (- base-demand cur-demand) 0) 
          (when (zerop (random 5))
            (set-settlement-cur-demand settlement item-type-id (1- cur-demand)))))
       (cond
         ((> (- base-supply cur-supply) 0)
          (when (zerop (random 5))
            (set-settlement-cur-supply settlement item-type-id (1+ cur-supply)))
          )
         ((< (- base-supply cur-supply) 0) 
          (when (zerop (random 5))
            (set-settlement-cur-supply settlement item-type-id (1- cur-supply)))))
       ))

(defun adjust-realm (realm)
  (incf (quest-timer realm))
  (when (> (quest-timer realm) 30)
    (setf (cur-quest realm) (random (hash-table-count *quest-types*)))
    (setf (cur-quest realm) +quest-type-money-donation+)
    (setf (quest-timer realm) 0))
  (when (and (zerop (random 100)) (> (ruler-favor realm) 0))
    (decf (ruler-favor realm)))
  (setf (audience realm) nil)
  )

(defun adjust-settlement-events (settlement)
  ;; add a new random event
  ;; in case of 1 in 40 chance 
  (when (zerop (random 40))
    (let ((event-type nil)
          (passed nil)
          (applicable-events ()))

      ;; create a list of applicable random events
      (dolist (event-type-id *event-random-types*)
        (setf event-type (get-event-type-by-id event-type-id))
        
        ;; if the event has no on-rand function - it always applies
        ;; if the event has the on-rand function, check if the conditions are met
        (setf passed nil)
        (unless (on-rand event-type)
          (setf passed t))
        (when (and (on-rand event-type)
                   (funcall (on-rand event-type) event-type settlement))
          (setf passed t))
        
        (when passed
          (pushnew event-type-id applicable-events)))
      
      ;; select a random event
      (setf event-type (get-event-type-by-id (nth (random (length applicable-events)) applicable-events)))
      
      (format t "SETTLEMENT ~A, EVENT-RANDOM-TYPES ~A, EVENT-NAME ~A, GET-EVENT ~A~%" (name settlement) applicable-events (descr event-type) (get-event-settlement settlement (id event-type)))

      ;; if there is no similair event, add the event
      (unless (get-event-settlement settlement (id event-type)))
        (add-event-settlement settlement (make-instance 'event :event-type-id (id event-type)))))

  ;; check all rotational events to see if they are applicable
  (loop
    for event-type being the hash-value in *event-rotate-types*
    do
       ;;(format t "EVENT ROTATE, SETTLEMENT ~A, PASSED ~A, GET-EVENT ~A~%" (name settlement) (funcall (on-rotate event-type) event-type settlement) (get-event-settlement settlement (id event-type)))

       (when (and (funcall (on-rotate event-type) event-type settlement)
                  (not (get-event-settlement settlement (id event-type))))
         (add-event-settlement settlement (make-instance 'event :event-type-id (id event-type)))))
  
  ;; process existing events
  (loop 
    for event-type-id being the hash-key in (events settlement)
    with event = nil
    do
       
       (setf event (get-event-settlement settlement event-type-id))
       
       ;; if the event-type has its own on-tick function, call it and let it do the removal 
       ;; otherwise tick and remove event explicitly
       (if (on-tick event)
         (funcall (on-tick event) event settlement)
         (progn
           (incf (stage event))
           (when (> (stage event) (max-stage event))
             (remove-event-settlement settlement (event-type-id event)))
           ))
    ))


(defun adjust-settlement-features (settlement)
  (loop 
      for feature-type-id in (features settlement)
      with feature = nil
      do
         (setf feature (get-feature-type-by-id feature-type-id))
                  
         (when (on-tick feature)
           (funcall (on-tick feature) feature settlement)
           )
         ))

(defun adjust-settlement-journal (settlement)
  (setf (journal settlement) (remove-if #'(lambda (entry)
                                            (if (> (- (wtime *world*) (get-journal-entry-date entry))
                                                   (* (1+ (get-journal-entry-importance entry)) 
                                                      (* +days-in-month+ +months-in-year+)))
                                              t
                                              nil)) 
                                        (journal settlement)))
  )

(defun adjust-trader-journal (trader)
  (setf (journal trader) (merge-journals (journal trader) 
                                         (journal (get-settlement-by-id (current-settlement-id trader)))))
  )

(defun adjust-trader-quests (trader)
  (loop 
    for quest-id in (quests trader)
    with quest = nil
    do
       (setf quest (get-quest-by-id quest-id))
       (when (or (= (stage quest) +quest-stage-completed+)
                 (= (stage quest) +quest-stage-failed+))
         (setf (quests trader) (remove-from-quests (quests trader) quest-id))
         (remove-quest-from-world quest-id))
    )
  )

(defun adjust-realm-quests (realm)
  (loop 
    for quest-id in (quests realm)
    with quest = nil
    do
       (setf quest (get-quest-by-id quest-id))
       (when (and (= (stage quest) +quest-stage-new+)
                  (eq (get-event-by-id (event-id quest)) nil))
         (setf (quests realm) (remove-from-quests (quests realm) quest-id))
         (remove-quest-from-world quest-id))
    ))
