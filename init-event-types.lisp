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
                                                                                   (incf (stage event))
                                                                                   
                                                                                   (add-to-inv +item-type-food+ (market settlement) 50)
                                                                                   (add-to-inv +item-type-wood+ (market settlement) 10)
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
                                                                                   (incf (stage event))
                                                                                   
                                                                                   (add-to-inv +item-type-ore+ (market settlement) 50)
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
                                                                                   (incf (stage event))
                                                                                   
                                                                                   (add-to-inv +item-type-gems+ (market settlement) 20)
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
