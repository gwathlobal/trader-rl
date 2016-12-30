(in-package :trader-rl)

(defun show-cur-time ()
  (format nil "~A" (wtime *world*)))

(defun make-world-turn ()
  (loop
    for settlement being the hash-value in *settlements*
    do
       (adjust-settlement-features settlement)
       (adjust-settlement-prices settlement)
       (adjust-settlement-events settlement)
    )

  (loop
    for realm being the hash-value in *realms*
    do
       (adjust-realm realm)
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
  ;; add a new event
  (when (zerop (random 40))
    (add-event-settlement settlement (make-instance 'event :event-type-id (random (hash-table-count *event-types*)))))

  ;; process existing events
  (loop 
      for event-type-id being the hash-key in (events settlement)
      with event = nil
      do
         
         (setf event (get-event-settlement settlement event-type-id))
                  
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
