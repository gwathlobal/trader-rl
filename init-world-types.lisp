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


(set-feature-type-by-id +feature-type-palace+ (make-instance 'feature-type :name "Palace"))


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
