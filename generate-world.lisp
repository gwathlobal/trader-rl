(in-package :trader-rl)

(defparameter *max-x-world* 20)
(defparameter *max-y-world* 20)

(defun generate-world (settlements traders &optional (max-settlements 5))
  (declare (ignore traders))
  (let ((pc-trader nil) (edges nil)
        (realm-human)
        (realm-tachidi)
        (realm-saurian)
        (realm-gremlin))
    ;; adding pc trader
    (setf pc-trader (make-instance 'trader :name "Player" :money 50000))
    (setf *player* pc-trader)
    
    ;; creating edges for the connected graph of the settlements
    (loop
      for i from 0 below max-settlements
      do
         ;; make a list of edges of type ((src-node-id dst-node-id) ...)
         ;; always connect to the from the first node to the next node
         (when (< (1+ i) max-settlements)
           (setf edges (append edges (list (list i (1+ i))))))
         ;; 30% chance to connect to any next node
         (when (and (< (1+ i) max-settlements) (< (random 100) 30))
           (setf edges (append edges (list (list i (+ i 1 (random (- (1- max-settlements) i))))))))
         ;; 30% chance to connect to any previous node
         (when (and (> i 0) (< (random 100) 30))
           (setf edges (append edges (list (list i (random i))))))
      )
    
    (setf realm-human (make-instance 'realm-human :ruler-race +race-type-human+))
    (setf realm-tachidi (make-instance 'realm-tachidi :ruler-race +race-type-tachidi+))
    (setf realm-saurian (make-instance 'realm-saurian :ruler-race +race-type-saurian+))
    (setf realm-gremlin (make-instance 'realm-gremlin :ruler-race +race-type-gremlin+))
    
    ;; adding settlements to world
    (loop 
      for i from 0 below max-settlements
      with settlement = nil
      with cur-settlement-race = 0
      with settlement-name-list-human = (copy-list *settlement-human-names*)
      with settlement-name-list-tachidi = (copy-list *settlement-tachidi-names*)
      with settlement-name-list-saurian = (copy-list *settlement-saurian-names*)
      with settlement-name-list-gremlin = (copy-list *settlement-gremlin-names*)
      with settlement-name-n = nil
      with settlement-name = nil
      with x = 0
      with y = 0
      with min-distance = 4
      with realm-id = nil
      do 
         (setf cur-settlement-race (random 4))
          ;; make sure all randomly picked names are unique (by removing the picked name from the pool of names) 
         (cond
           ((= cur-settlement-race +race-type-human+) 
            (setf settlement-name-n (random (length settlement-name-list-human)))
            (setf settlement-name (nth settlement-name-n settlement-name-list-human))
            (setf settlement-name-list-human (remove (nth settlement-name-n settlement-name-list-human) settlement-name-list-human))
            (setf realm-id (id realm-human))
            )
           ((= cur-settlement-race +race-type-tachidi+)
            (setf settlement-name-n (random (length settlement-name-list-tachidi)))
            (setf settlement-name (nth settlement-name-n settlement-name-list-tachidi))
            (setf settlement-name-list-tachidi (remove (nth settlement-name-n settlement-name-list-tachidi) settlement-name-list-tachidi))
            (setf realm-id (id realm-tachidi))
            )
           ((= cur-settlement-race +race-type-gremlin+)
            (setf settlement-name-n (random (length settlement-name-list-gremlin)))
            (setf settlement-name (nth settlement-name-n settlement-name-list-gremlin))
            (setf settlement-name-list-gremlin (remove (nth settlement-name-n settlement-name-list-gremlin) settlement-name-list-gremlin))
            (setf realm-id (id realm-gremlin))
            )
           ((= cur-settlement-race +race-type-saurian+)
            (setf settlement-name-n (random (length settlement-name-list-saurian)))
            (setf settlement-name (nth settlement-name-n settlement-name-list-saurian))
            (setf settlement-name-list-saurian (remove (nth settlement-name-n settlement-name-list-saurian) settlement-name-list-saurian))
            (setf realm-id (id realm-saurian))
            ))
        
         ;;(format t "~%SETTLEMENT-NAME ~A, CUR-SETTLEMENT-RACE ~A, REALM-ID ~A~%")
         
         ;; find such location that the distance from all other already created locations is more than min-distance
         (loop
           with free-place-found = nil
           until free-place-found
           do
              (setf x (random *max-x-world*))
              (setf y (random *max-y-world*))
              (setf free-place-found t)
              (loop
                for i-settlement being the hash-value in settlements
                do
                   (when (< (get-distance x y (x i-settlement) (y i-settlement)) min-distance)
                     (setf free-place-found nil)
                     (loop-finish)
                     )))
           
         ;; create and add settlement to the world
         (setf settlement (make-instance 'settlement :name settlement-name :x x :y y :settlement-size (random (1- (length *settlement-size-names*)))))

         ;; make the last city the capital
         (when (= i (1- max-settlements))
           (setf (settlement-size settlement) +settlement-size-city+))
         
         ;; adjust settlement type based on size
         (cond
           ((= (settlement-size settlement) +settlement-size-village+)
            (if (zerop (random 2))
              (setf (settlement-type settlement) +settlement-type-agriculture+)
              (setf (settlement-type settlement) +settlement-type-mining+)))
           ((= (settlement-size settlement) +settlement-size-town+)
            (setf (settlement-type settlement) +settlement-type-industry+))
           ((= (settlement-size settlement) +settlement-size-city+)
            (setf (settlement-type settlement) +settlement-type-sprawling+)
            ))

         ;; set realm
         (setf (realm-id settlement) realm-id)
         (setf (race-type settlement) cur-settlement-race)
         
         
         (initialize-demand-supply settlement)
         (initialize-features settlement)
         (initialize-current-demand-supply settlement)

         ;;(add-event-settlement settlement (make-instance 'event :event-type-id +event-type-famine+))
         
         ;; create 3 random items in the settlement market
         (add-to-inv (random 9) (market settlement) (+ 5 (random 10)))
         (add-to-inv (random 9) (market settlement) (+ 5 (random 10)))
         (add-to-inv (random 9) (market settlement) (+ 5 (random 10)))
         (add-to-inv  +item-type-food+ (market settlement) (+ 100 (random 100)))
      
         ;;(format t "S = ~A ~A ~%" (id settlement) (name settlement))
      )

    ;; adding links to each settlement
    (loop
      for edge in edges
      do
         ;;(format t "SRC NAME ~A~%" (name (gethash (first edge) settlements)))
         (make-link (gethash (first edge) settlements) (gethash (second edge) settlements))   
      )
 
    (setf (current-settlement-id *player*) 0)
    ;;(loop
    ;;  for i being the hash-value in *links*
    ;;  do
    ;;     (format t "LINK id: ~A, dst id: ~A (~A)~%" (id i) (dst-id i) (name (gethash (dst-id i) *settlements*))))
    ;;(format t "LINK LENGTH ~A~%" (hash-table-count *links*))
    
    nil))

(defun add-link-to-list (link-list link)
  (append link-list (list (id link))))

(defun find-link-by-dst (src-settlement dst-settlement)
  (loop 
    for link-id in (links src-settlement)
    with link = nil
    do
       (setf link (gethash link-id *links*))
       
       (when (= (dst-id link) (id dst-settlement)) (return-from find-link-by-dst link))
    )
  nil)

(defun make-link (settlement-1 settlement-2)
  (let ((days (truncate (get-distance (x settlement-1) (y settlement-1) (x settlement-2) (y settlement-2)))))
    
    (unless (find-link-by-dst settlement-1 settlement-2)
      (setf (links settlement-1) (add-link-to-list (links settlement-1) (make-instance  'link :dst-id (id settlement-2) :days days))))
      
    (unless (find-link-by-dst settlement-2 settlement-1)
      (setf (links settlement-2) (add-link-to-list (links settlement-2) (make-instance 'link :dst-id (id settlement-1) :days days))))
    
    ))

(defun get-distance (x1 y1 x2 y2)
  (sqrt (+ (expt (- x1 x2) 2) (expt (- y1 y2) 2))))

