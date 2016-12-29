(in-package :trader-rl)

(defun show-cur-time ()
  (format nil "~A" (wtime *world*)))

(defun make-world-turn ()
  (loop
    for settlement being the hash-value in *settlements*
    do
       (adjust-settlement-items settlement)
       (adjust-settlement-prices settlement)
    )

  (loop
    for realm being the hash-value in *realms*
    do
       (adjust-realm realm)
    )
  (incf (wtime *world*)))

(defun adjust-settlement-items (settlement)
  (let ((items-produce (items-produce settlement))
        (items-consume (items-consume settlement))
        )
    (format t "~%ADJUST-SETTLEMENT-ITEMS: ~A~%" (name settlement))
    ;; set up production and consumptions for the settlement
    
    ;; iterate through the production to add items to the marketplace
    (loop
      for prod-cons in items-produce
      with qty-value = 0
      with item-type-id = 0
      do
         (setf item-type-id (car prod-cons))
         (setf qty-value (if (> (cdr prod-cons) 0)
                           (cdr prod-cons)
                           0))
         (unless (zerop qty-value)
           (add-to-inv item-type-id (market settlement) qty-value))
      )
    ;; iterate through the consumptions to remove items from the marketplace
    (loop
      for cons-cons in items-consume
      with qty-value = 0
      with item-type-id = 0
      do
         (setf item-type-id (car cons-cons))
         (setf qty-value (if (> (cdr cons-cons) 0)
                           (cdr cons-cons)
                           0))
         (unless (zerop qty-value)
           (remove-from-inv item-type-id (market settlement) qty-value)
           ))
      )
    
    )

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
       
       ;;(format t "~%ADJUST-SETTLEMENT-PRICES: ~A, DELTA-DEMAND ~A, DELTA-SUPPLY ~A~%" (name settlement) (- base-demand cur-demand) (- base-supply cur-supply))

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
