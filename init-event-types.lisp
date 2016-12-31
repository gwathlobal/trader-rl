(in-package :trader-rl)

;;=========================================
;;
;; EVENT TYPES
;;
;;=========================================


(set-event-type-by-id +event-type-famine+ (make-instance 'event-type :descr "Famine is raging in this settlement!" :max-stage 7
                                                                     :on-show #'(lambda (event settlement)
                                                                                  (declare (ignore settlement))
                                                                                  (let ((str (create-string "")))
                                                                                    (if (< (stage event) 15)
                                                                                      (format str "~A~%" (descr event))
                                                                                      (format str "This settlement has recently suffered from famine.~%"))
                                                                                    str)
                                                                                  )
                                                                     :on-tick #'(lambda (event settlement)
                                                                                  (format t "ON-TICK ~A ~A~%" (name settlement) (descr event))
                                                                                  
                                                                                  (when (zerop (stage event))
                                                                                    (setf (journal settlement) 
                                                                                          (add-to-journal (journal settlement) :date (wtime *world*) :importance +journal-importance-high+ 
                                                                                                                               :string (format nil "The ~(~A~) of ~A was struck by famine!" 
                                                                                                                                               (get-settlement-size-name settlement) 
                                                                                                                                               (name settlement))))
                                                                                    ;; add a quest to the realm ruler to deliver food to this settlement
                                                                                    (let ((quest))
                                                                                      (setf quest (make-instance 'quest-delivery 
                                                                                                                 :quest-type-id +quest-type-delivery+
                                                                                                                 :giver-id (id (get-settlement-realm settlement))
                                                                                                                 :item-type-id +item-type-food+
                                                                                                                 :qty 300
                                                                                                                 :reward 1500
                                                                                                                 :dst-id (id settlement)
                                                                                                                 :event-id (id event)
                                                                                                                 :date (wtime *world*)))
                                                                                      
                                                                                      (setf (quests (get-settlement-realm settlement)) 
                                                                                            (add-to-quests (quests (get-settlement-realm settlement)) quest))
                                                                                      )
                                                                                    )
                                                                                  
                                                                                  (incf (stage event))
                                                                                  
                                                                                  (when (<= (stage event) (- (max-stage event) 3))
                                                                                    (remove-from-inv +item-type-food+ (market settlement) 50)
                                                                                    (set-settlement-cur-demand settlement +item-type-food+ 15)
                                                                                    (set-settlement-cur-supply settlement +item-type-food+ 14))
                                                                                  
                                                                                  (when (> (stage event) (- (max-stage event) 3))
                                                                                    (set-settlement-cur-demand settlement +item-type-food+ 5)
                                                                                    (set-settlement-cur-supply settlement +item-type-food+ 4))
                                                                                  
                                                                                  ;; by default lambda does not seem to create a block, thus you need this to return from it
                                                                                  (block nil
                                                                                    (when (> (stage event) (max-stage event))
                                                                                      (remove-event-settlement settlement (event-type-id event))
                                                                                      (return))
                                                                                    
                                                                                    (when (> (get-item-inv +item-type-food+ (market settlement)) 300)
                                                                                      (remove-event-settlement settlement (event-type-id event))
                                                                                      (set-settlement-cur-demand settlement +item-type-food+ 5)
                                                                                      (set-settlement-cur-supply settlement +item-type-food+ 4)
                                                                                      (return)))
                                                                                  
                                                                                  )
                                                                     :on-rotate #'(lambda (event-type settlement)
                                                                                    (declare (ignore event-type))
                                                                                    (if (<= (get-item-inv +item-type-food+ (market settlement)) 20)
                                                                                      t
                                                                                      nil))))

(set-event-type-by-id +event-type-harvest+ (make-instance 'event-type :descr "This settlement was blessed with plentiful harvest!" :max-stage 5
                                                                      :on-tick #'(lambda (event settlement)
                                                                                   (format t "ON-TICK ~A ~A~%" (name settlement) (descr event))
                                                                                   
                                                                                   (when (zerop (stage event))
                                                                                     (setf (journal settlement) 
                                                                                           (add-to-journal (journal settlement) :date (wtime *world*) :importance +journal-importance-low+ 
                                                                                                                                :string (format nil "The ~(~A~) of ~A was blessed with plentiful harvest." 
                                                                                                                                                (get-settlement-size-name settlement) 
                                                                                                                                                (name settlement)))))
                                                                                   (incf (stage event))
                                                                                   
                                                                                   (produce-settlement-item +item-type-food+ settlement 50)
                                                                                   (produce-settlement-item +item-type-wood+ settlement 10)
                                                                                   (set-settlement-cur-demand settlement +item-type-food+ -8)
                                                                                   (set-settlement-cur-supply settlement +item-type-food+ -9)
                                                                                   (set-settlement-cur-demand settlement +item-type-wood+ -5)
                                                                                   (set-settlement-cur-supply settlement +item-type-wood+ -6)
                                                                                   
                                                                                   (when (> (stage event) (max-stage event))
                                                                                     (remove-event-settlement settlement (event-type-id event)))
                                                                                   )
                                                                      :on-rand #'(lambda (event-type settlement)
                                                                                   (declare (ignore event-type))
                                                                                   (if (get-settlement-feature settlement +feature-type-farm+)
                                                                                     t
                                                                                     nil
                                                                                     )
                                                                                   )))

(set-event-type-by-id +event-type-ore-deposit+ (make-instance 'event-type :descr "The miners have found a rich ore vein!" :max-stage 5
                                                                      :on-tick #'(lambda (event settlement)
                                                                                   (format t "ON-TICK ~A ~A~%" (name settlement) (descr event))

                                                                                   (when (zerop (stage event))
                                                                                     (setf (journal settlement) 
                                                                                           (add-to-journal (journal settlement) :date (wtime *world*) :importance +journal-importance-low+ 
                                                                                                                                :string (format nil "The miners of the ~(~A~) of ~A found a rich ore vein." 
                                                                                                                                                (get-settlement-size-name settlement) 
                                                                                                                                                (name settlement)))))
                                                                                   (incf (stage event))
                                                                                   
                                                                                   (produce-settlement-item +item-type-ore+ settlement 50)
                                                                                   (set-settlement-cur-demand settlement +item-type-ore+ -6)
                                                                                   (set-settlement-cur-supply settlement +item-type-ore+ -7)
                                                                                   
                                                                                   (when (> (stage event) (max-stage event))
                                                                                     (remove-event-settlement settlement (event-type-id event)))
                                                                                   )
                                                                      :on-rand #'(lambda (event-type settlement)
                                                                                   (declare (ignore event-type))
                                                                                   (if (get-settlement-feature settlement +feature-type-mine+)
                                                                                     t
                                                                                     nil
                                                                                     )
                                                                                   )))

(set-event-type-by-id +event-type-gem-deposit+ (make-instance 'event-type :descr "The miners have found a gem deposit!" :max-stage 5
                                                                      :on-tick #'(lambda (event settlement)
                                                                                   (format t "ON-TICK ~A ~A~%" (name settlement) (descr event))

                                                                                   (when (zerop (stage event))
                                                                                     (setf (journal settlement) 
                                                                                           (add-to-journal (journal settlement) :date (wtime *world*) :importance +journal-importance-low+ 
                                                                                                                                :string (format nil "The miners of the ~(~A~) of ~A found a gem deposit." 
                                                                                                                                                (get-settlement-size-name settlement) 
                                                                                                                                                (name settlement)))))

                                                                                   (incf (stage event))
                                                                                   
                                                                                   (produce-settlement-item +item-type-gems+ settlement 20)
                                                                                   (set-settlement-cur-demand settlement +item-type-gems+ -5)
                                                                                   (set-settlement-cur-supply settlement +item-type-gems+ -6)
                                                                                   
                                                                                   (when (> (stage event) (max-stage event))
                                                                                     (remove-event-settlement settlement (event-type-id event)))
                                                                                   )
                                                                      :on-rand #'(lambda (event-type settlement)
                                                                                   (declare (ignore event-type))
                                                                                   (if (get-settlement-feature settlement +feature-type-mine+)
                                                                                     t
                                                                                     nil
                                                                                     )
                                                                                   )))

(set-event-type-by-id +event-type-fire+ (make-instance 'event-type :descr "Fire is raging through the settlement!" :max-stage 1
                                                                   :on-tick #'(lambda (event settlement)
                                                                                   (format t "ON-TICK ~A ~A~%" (name settlement) (descr event))

                                                                                   (when (zerop (stage event))
                                                                                     (setf (journal settlement) 
                                                                                           (add-to-journal (journal settlement) :date (wtime *world*) :importance +journal-importance-high+ 
                                                                                                                                :string (format nil "Fire raged through the ~(~A~) of ~A." 
                                                                                                                                                (get-settlement-size-name settlement) 
                                                                                                                                                (name settlement))))
                                                                                     ;; destroy all warehouses
                                                                                     (remove-settlement-feature settlement +feature-type-small-warehouse+)
                                                                                     (remove-settlement-feature settlement +feature-type-med-warehouse+)
                                                                                     (remove-settlement-feature settlement +feature-type-large-warehouse+)

                                                                                     ;; destroy goods which quantity exceeds the new maximum settlement capacity
                                                                                     (loop
                                                                                       for item-type-id being the hash-key in (market settlement) using (hash-value item-qty) 
                                                                                       do
                                                                                          (when (> item-qty (get-settlement-max-capacity settlement))
                                                                                            (remove-from-inv item-type-id (market settlement) (- item-qty (get-settlement-max-capacity settlement)))
                                                                                            )
                                                                                       )
                                                                                     )

                                                                                   (incf (stage event))
                                                                                   
                                                                                   (when (> (stage event) (max-stage event))
                                                                                     (remove-event-settlement settlement (event-type-id event)))
                                                                                   )
                                                                      :on-rand #'(lambda (event-type settlement)
                                                                                   (declare (ignore event-type))
                                                                                   (if (or (get-settlement-feature settlement +feature-type-small-warehouse+)
                                                                                           (get-settlement-feature settlement +feature-type-med-warehouse+)
                                                                                           (get-settlement-feature settlement +feature-type-large-warehouse+))
                                                                                     t
                                                                                     nil
                                                                                     )
                                                                                   )))

(set-event-type-by-id +event-type-build-warehouse-1+ (make-instance 'event-type :descr "The settlement mayor decided to build a small warehouse." :max-stage 1
                                                                                :on-tick #'(lambda (event settlement)
                                                                                             (format t "ON-TICK ~A ~A~%" (name settlement) (descr event))

                                                                                             (when (zerop (stage event))
                                                                                               (setf (journal settlement) 
                                                                                                     (add-to-journal (journal settlement) :date (wtime *world*) :importance +journal-importance-low+ 
                                                                                                                                          :string (format nil "People of ~A built a small warehouse." 
                                                                                                                                                          (name settlement))))
                                                                                               ;; build a small warehouses
                                                                                               (set-settlement-feature settlement +feature-type-small-warehouse+)
                                                                                               )

                                                                                             (incf (stage event))
                                                                                             
                                                                                             (when (> (stage event) (max-stage event))
                                                                                               (remove-event-settlement settlement (event-type-id event)))
                                                                                             )
                                                                                :on-rand #'(lambda (event-type settlement)
                                                                                             (declare (ignore event-type))
                                                                                             (if (or (get-settlement-feature settlement +feature-type-small-warehouse+)
                                                                                                     (get-settlement-feature settlement +feature-type-med-warehouse+)
                                                                                                     (get-settlement-feature settlement +feature-type-large-warehouse+))
                                                                                               nil
                                                                                               t
                                                                                               )
                                                                                             )))

(set-event-type-by-id +event-type-build-warehouse-2+ (make-instance 'event-type :descr "The settlement mayor decided to build a medium warehouse." :max-stage 1
                                                                                :on-tick #'(lambda (event settlement)
                                                                                             (format t "ON-TICK ~A ~A~%" (name settlement) (descr event))

                                                                                             (when (zerop (stage event))
                                                                                               (setf (journal settlement) 
                                                                                                     (add-to-journal (journal settlement) :date (wtime *world*) :importance +journal-importance-low+ 
                                                                                                                                          :string (format nil "People of ~A built a medium warehouse." 
                                                                                                                                                          (name settlement))))
                                                                                               ;; build a medium warehouses
                                                                                               (remove-settlement-feature settlement +feature-type-small-warehouse+)
                                                                                               (set-settlement-feature settlement +feature-type-med-warehouse+)
                                                                                               )

                                                                                             (incf (stage event))
                                                                                             
                                                                                             (when (> (stage event) (max-stage event))
                                                                                               (remove-event-settlement settlement (event-type-id event)))
                                                                                             )
                                                                                :on-rand #'(lambda (event-type settlement)
                                                                                             (declare (ignore event-type))
                                                                                             (if (get-settlement-feature settlement +feature-type-small-warehouse+)
                                                                                               t
                                                                                               nil
                                                                                               )
                                                                                             )))

(set-event-type-by-id +event-type-build-warehouse-3+ (make-instance 'event-type :descr "The settlement mayor decided to build a large warehouse." :max-stage 1
                                                                                :on-tick #'(lambda (event settlement)
                                                                                             (format t "ON-TICK ~A ~A~%" (name settlement) (descr event))

                                                                                             (when (zerop (stage event))
                                                                                               (setf (journal settlement) 
                                                                                                     (add-to-journal (journal settlement) :date (wtime *world*) :importance +journal-importance-low+ 
                                                                                                                                          :string (format nil "People of ~A built a large warehouse." 
                                                                                                                                                          (name settlement))))
                                                                                               ;; build a large warehouses
                                                                                               (remove-settlement-feature settlement +feature-type-med-warehouse+)
                                                                                               (set-settlement-feature settlement +feature-type-large-warehouse+)
                                                                                               )

                                                                                             (incf (stage event))
                                                                                             
                                                                                             (when (> (stage event) (max-stage event))
                                                                                               (remove-event-settlement settlement (event-type-id event)))
                                                                                             )
                                                                                :on-rand #'(lambda (event-type settlement)
                                                                                             (declare (ignore event-type))
                                                                                             (if (get-settlement-feature settlement +feature-type-med-warehouse+)
                                                                                               t
                                                                                               nil
                                                                                               )
                                                                                             )))

(set-event-type-by-id +event-type-rot+ (make-instance 'event-type :descr "Mushrooms in storage are suffering from a strange rot. People are troubled." :max-stage 5
                                                                                :on-tick #'(lambda (event settlement)
                                                                                             (format t "ON-TICK ~A ~A~%" (name settlement) (descr event))

                                                                                             (when (zerop (stage event))
                                                                                               (setf (journal settlement) 
                                                                                                     (add-to-journal (journal settlement) :date (wtime *world*) :importance +journal-importance-low+ 
                                                                                                                                          :string (format nil "Mushroom storage of ~A suffered from a strange rot." 
                                                                                                                                                          (name settlement))))
                                                                                               ;; build a large warehouses
                                                                                               )

                                                                                             (incf (stage event))
                                                                                             
                                                                                             (remove-from-inv +item-type-food+ (market settlement) 40)
                                                                                             (remove-from-inv +item-type-wood+ (market settlement) 20)
                                                                                             
                                                                                             (when (> (stage event) (max-stage event))
                                                                                               (remove-event-settlement settlement (event-type-id event)))
                                                                                             )
                                                                                ))
