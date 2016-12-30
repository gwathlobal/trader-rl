(in-package :trader-rl)

;;=========================================
;;
;; FEATURE TYPES
;;
;;=========================================

(set-feature-type-by-id +feature-type-palace+ (make-instance 'feature-type :name "Palace" :hidden nil
                                                                           :on-tick #'(lambda (feature settlement)
                                                                                        (declare (ignore feature))
                                                                                        ;; palace consumes food, weapons, gems and jewelry
                                                                                        (remove-from-inv +item-type-food+ (market settlement) (random 7))
                                                                                        (remove-from-inv +item-type-gems+ (market settlement) (1+ (random 3)))
                                                                                        (remove-from-inv +item-type-jewelry+ (market settlement) (+ 3 (random 3)))
                                                                                        (remove-from-inv +item-type-weapons+ (market settlement) (+ 2 (random 2)))
                                                                                        ;; palace terraces may produce food
                                                                                        (add-to-inv +item-type-food+ (market settlement) (1+ (random 7)))
                                                                                        )
                                                                           :on-add #'(lambda (feature settlement)
                                                                                       (declare (ignore feature))
                                                                                       ;; palace creates demand for gems and jewelry
                                                                                       (mod-settlement-base-demand-supply settlement +item-type-gems+ :demand 1 :supply 0)
                                                                                       (mod-settlement-base-demand-supply settlement +item-type-jewelry+ :demand 2 :supply 1)
                                                                                       )))
(set-feature-type-by-id +feature-type-farm+ (make-instance 'feature-type :name "Farm" :hidden nil
                                                           :on-tick #'(lambda (feature settlement)
                                                                        (declare (ignore feature))
                                                                        ;; farm produces food & wood
                                                                        (add-to-inv +item-type-food+ (market settlement) (+ 9 (random 6) (random 6) (random 6)))
                                                                        (add-to-inv +item-type-wood+ (market settlement) (+ 3 (random 6)))
                                                                        ;; farm consumes tools
                                                                        (remove-from-inv +item-type-tools+ (market settlement) (random 6))
                                                                        )
                                                           :on-add #'(lambda (feature settlement)
                                                                       (declare (ignore feature))
                                                                       ;; farm creates demand for tools & supply for food and wood
                                                                       (mod-settlement-base-demand-supply settlement +item-type-food+ :demand -2 :supply -3)
                                                                       (mod-settlement-base-demand-supply settlement +item-type-wood+ :demand -2 :supply -3)
                                                                       (mod-settlement-base-demand-supply settlement +item-type-tools+ :demand 2 :supply 1)
                                                                       )))
(set-feature-type-by-id +feature-type-craftshop+ (make-instance 'feature-type :name "Craftsman shop" :hidden nil
                                                                :on-tick #'(lambda (feature settlement)
                                                                             (declare (ignore feature))
                                                                             ;; craftsman shop produces furniture & crafts
                                                                             (add-to-inv +item-type-furniture+ (market settlement) (+ 6 (random 6)))
                                                                             (add-to-inv +item-type-crafts+ (market settlement) (+ 6 (random 6)))
                                                                             ;; craftsman shop consumes wood
                                                                             (remove-from-inv +item-type-wood+ (market settlement) (+ 6 (random 6)))
                                                                             )
                                                                :on-add #'(lambda (feature settlement)
                                                                            (declare (ignore feature))
                                                                            ;; craftsman shop creates demand for wood & supply for furniture and crafts
                                                                            (mod-settlement-base-demand-supply settlement +item-type-wood+ :demand 2 :supply 1)
                                                                            (mod-settlement-base-demand-supply settlement +item-type-furniture+ :demand -2 :supply -3)
                                                                            (mod-settlement-base-demand-supply settlement +item-type-crafts+ :demand -2 :supply -3)
                                                                            )))
(set-feature-type-by-id +feature-type-jewelshop+ (make-instance 'feature-type :name "Jeweller" :hidden nil
                                                                :on-tick #'(lambda (feature settlement)
                                                                             (declare (ignore feature))
                                                                             ;; jeweller produces jewelry
                                                                             (add-to-inv +item-type-jewelry+ (market settlement) (+ 1 (random 3)))
                                                                             ;; jeweller consumes gems
                                                                             (remove-from-inv +item-type-gems+ (market settlement) (+ 3 (random 6)))
                                                                             )
                                                                :on-add #'(lambda (feature settlement)
                                                                            (declare (ignore feature))
                                                                            ;; jeweller creates demand for gems & supply for jewelry
                                                                            (mod-settlement-base-demand-supply settlement +item-type-jewelry+ :demand 0 :supply -1)
                                                                            (mod-settlement-base-demand-supply settlement +item-type-gems+ :demand 2 :supply 1)
                                                                            )))
(set-feature-type-by-id +feature-type-weaponshop+ (make-instance 'feature-type :name "Blacksmith" :hidden nil
                                                                 :on-tick #'(lambda (feature settlement)
                                                                             (declare (ignore feature))
                                                                             ;; blacksmith produces tools & weapons
                                                                             (add-to-inv +item-type-tools+ (market settlement) (+ 6 (random 6)))
                                                                             (add-to-inv +item-type-weapons+ (market settlement) (+ 3 (random 6)))
                                                                             ;; blacksmith consumes ore
                                                                             (remove-from-inv +item-type-ore+ (market settlement) (+ 9 (random 6) (random 6)))
                                                                             )
                                                                 :on-add #'(lambda (feature settlement)
                                                                             (declare (ignore feature))
                                                                             ;; blacksmith creates demand for ore & supply for tools and weapons
                                                                             (mod-settlement-base-demand-supply settlement +item-type-tools+ :demand -2 :supply -3)
                                                                             (mod-settlement-base-demand-supply settlement +item-type-weapons+ :demand -1 :supply -2)
                                                                             (mod-settlement-base-demand-supply settlement +item-type-ore+ :demand 2 :supply 1)
                                                                             )))
(set-feature-type-by-id +feature-type-mine+ (make-instance 'feature-type :name "Mine" :hidden nil
                                                           :on-tick #'(lambda (feature settlement)
                                                                        (declare (ignore feature))
                                                                        ;; mine produces ore & gems
                                                                        (add-to-inv +item-type-ore+ (market settlement) (+ 9 (random 6) (random 6) (random 6)))
                                                                        (add-to-inv +item-type-gems+ (market settlement) (+ 3 (random 6)))
                                                                        ;; mine consumes tools
                                                                        (remove-from-inv +item-type-tools+ (market settlement) (+ 3 (random 6)))
                                                                        )
                                                           :on-add #'(lambda (feature settlement)
                                                                       (declare (ignore feature))
                                                                       ;; mine creates demand for tools & supply for gems and ore
                                                                       (mod-settlement-base-demand-supply settlement +item-type-ore+ :demand -2 :supply -3)
                                                                       (mod-settlement-base-demand-supply settlement +item-type-gems+ :demand -2 :supply -3)
                                                                       (mod-settlement-base-demand-supply settlement +item-type-tools+ :demand 2 :supply 1)
                                                                       )))
(set-feature-type-by-id +feature-type-population+ (make-instance 'feature-type :name "Population" :hidden t
                                                                               :on-tick #'(lambda (feature settlement)
                                                                                            (declare (ignore feature))
                                                                                            ;; population consumes and produces food depending on the settlement size
                                                                                            ;; population consumes crafts and furniture
                                                                                            (cond
                                                                                              ((= (settlement-size settlement) +settlement-size-village+)
                                                                                               (add-to-inv +item-type-food+ (market settlement) (+ 7 (random 7) (random 7)))
                                                                                               (remove-from-inv +item-type-food+ (market settlement) (+ 4 (random 7) (random 7)))
                                                                                               (remove-from-inv +item-type-crafts+ (market settlement) (random 4))
                                                                                               (remove-from-inv +item-type-furniture+ (market settlement) (random 3))
                                                                                               )
                                                                                              ((= (settlement-size settlement) +settlement-size-town+)
                                                                                               (add-to-inv +item-type-food+ (market settlement) (+ 4 (random 7) (random 7)))
                                                                                               (remove-from-inv +item-type-food+ (market settlement) (+ 3 (random 7) (random 7)))
                                                                                               (remove-from-inv +item-type-crafts+ (market settlement) (+ 6 (random 6)))
                                                                                               (remove-from-inv +item-type-furniture+ (market settlement) (+ 2 (random 6)))
                                                                                               )
                                                                                              ((= (settlement-type settlement) +settlement-type-sprawling+)
                                                                                               (add-to-inv +item-type-food+ (market settlement) (+ 2 (random 7) (random 7) (random 7)))
                                                                                               (remove-from-inv +item-type-food+ (market settlement) (+ 1 (random 7) (random 7) (random 7)))
                                                                                               (remove-from-inv +item-type-crafts+ (market settlement) (+ 6 (random 6)))
                                                                                               (remove-from-inv +item-type-furniture+ (market settlement) (+ 6 (random 6)))
                                                                                               )
                                                                                              )
                                                                                            )
                                                                               :on-add #'(lambda (feature settlement)
                                                                                           (declare (ignore feature))
                                                                                           ;; population creates demand for food, crafts & furniture depending on the settlement size
                                                                                           (cond
                                                                                              ((= (settlement-size settlement) +settlement-size-village+)
                                                                                               (if (= (settlement-type settlement) +settlement-type-agriculture+)
                                                                                                 (mod-settlement-base-demand-supply settlement +item-type-food+ :demand -2 :supply -3)
                                                                                                 (mod-settlement-base-demand-supply settlement +item-type-food+ :demand 1 :supply 0))
                                                                                               (mod-settlement-base-demand-supply settlement +item-type-crafts+ :demand 1 :supply 0)
                                                                                               (mod-settlement-base-demand-supply settlement +item-type-furniture+ :demand 1 :supply 0)
                                                                                               )
                                                                                              ((= (settlement-size settlement) +settlement-size-town+)
                                                                                               (mod-settlement-base-demand-supply settlement +item-type-food+ :demand 1 :supply 0)
                                                                                               (mod-settlement-base-demand-supply settlement +item-type-crafts+ :demand 1 :supply 0)
                                                                                               (mod-settlement-base-demand-supply settlement +item-type-furniture+ :demand 1 :supply 0)
                                                                                               )
                                                                                              ((= (settlement-type settlement) +settlement-type-sprawling+)
                                                                                               (mod-settlement-base-demand-supply settlement +item-type-food+ :demand 2 :supply 1)
                                                                                               (mod-settlement-base-demand-supply settlement +item-type-crafts+ :demand 2 :supply 1)
                                                                                               (mod-settlement-base-demand-supply settlement +item-type-furniture+ :demand 2 :supply 1)
                                                                                               )
                                                                                              )
                                                                                           )))

