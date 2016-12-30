(in-package :trader-rl)

(set-item-type-by-id +item-type-food+ (make-instance 'item-type :name "Edible mushrooms" :descr "In the underground world of Arq, mushrooms are grown for food." :base-price 5))
(set-item-type-by-id +item-type-ore+ (make-instance 'item-type :name "Ore" :descr "Ore is mined to be smelted into different metals." :base-price 7))
(set-item-type-by-id +item-type-tools+ (make-instance 'item-type :name "Tools" :descr "Everybody needs tools for their trade." :base-price 15))
(set-item-type-by-id +item-type-wood+ (make-instance 'item-type :name "Wood" :descr "The world of Arq sees no sun, but its mushrooms can grow as big as any tree." :base-price 8))
(set-item-type-by-id +item-type-furniture+ (make-instance 'item-type :name "Furniture" :descr "Ever tried to sleep on the cave floor? Nasty experience!" :base-price 20))
(set-item-type-by-id +item-type-weapons+ (make-instance 'item-type :name "Weapons & Armor" :descr "War. War never changes, even underground." :base-price 30))
(set-item-type-by-id +item-type-crafts+ (make-instance 'item-type :name "Crafts" :descr "Mainly figurines, toys, instruments and mugs." :base-price 14))
(set-item-type-by-id +item-type-gems+ (make-instance 'item-type :name "Gems" :descr "Sometimes miners find these while drilling through the rock." :base-price 50))
(set-item-type-by-id +item-type-jewelry+ (make-instance 'item-type :name "Jewelry" :descr "Afordable obly by the richest." :base-price 100))


(set-quest-type-by-id +quest-type-money-donation+ (make-instance 'quest-type :intro-str "Our realm requires a sizable donation of gold to further our wealth and prosperity. You are required to present us with 2000 gold." 
                                                                             :quest-item-id +quest-item-money+
                                                                             :quest-item-num 2000))

(set-quest-type-by-id +quest-type-weapons-donation+ (make-instance 'quest-type :intro-str "Forgotten beasts threaten peace of our realm. To arm new recruits we need 50 sets of weapons & armor." 
                                                                              :quest-item-id +item-type-weapons+
                                                                              :quest-item-num 50))

(set-quest-type-by-id +quest-type-gems-donation+ (make-instance 'quest-type :intro-str "Our crafters require gems to master their skills. 30 gems will do." 
                                                                            :quest-item-id +item-type-gems+
                                                                            :quest-item-num 30))

(set-quest-type-by-id +quest-type-jewelry-donation+ (make-instance 'quest-type :intro-str "We need rings and necklaces to show everyone the superiority of our realm. 20 items of jewelry will be sufficient." 
                                                                               :quest-item-id +item-type-jewelry+
                                                                               :quest-item-num 20))

(set-quest-type-by-id +quest-type-food-donation+ (make-instance 'quest-type :intro-str "Famine often plagues our realm. We need 500 barrels of mushrooms to feed the starving." 
                                                                            :quest-item-id +item-type-food+
                                                                            :quest-item-num 500))

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

;;=========================================
;;
;; EVENT TYPES
;;
;;=========================================


(set-event-type-by-id +event-type-famine+ (make-instance 'event-type :descr "Famine is raging in this settlement!" :max-stage 17
                                                                     :on-show #'(lambda (event settlement)
                                                                                  (declare (ignore settlement))
                                                                                  (let ((str (create-string "")))
                                                                                    (if (< (stage event) 15)
                                                                                      (format str "~A~%" (descr event))
                                                                                      (format str "This settlement has recently suffered from famine.~%"))
                                                                                    str)
                                                                                  )
                                                                     :on-tick #'(lambda (event settlement)
                                                                                  (format t "ON-TICK ~A~%" (descr event))
                                                                                  (incf (stage event))
                                                                                  (set-settlement-cur-demand settlement +item-type-food+ 15)
                                                                                  (set-settlement-cur-supply settlement +item-type-food+ 15)
                                                                                  (when (> (stage event) (- (max-stage event) 3))
                                                                                    (set-settlement-cur-demand settlement +item-type-food+ 5)
                                                                                    (set-settlement-cur-supply settlement +item-type-food+ 5))
                                                                                  (when (> (stage event) (max-stage event))
                                                                                    (remove-event-settlement settlement (event-type-id event)))
                                                                                  )))
