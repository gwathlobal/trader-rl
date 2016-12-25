(in-package :trader-rl)

(defclass link ()
  ((id :initform nil :initarg :id :accessor id) 
   (dst-id :initform nil :initarg :dst-id :accessor dst-id) ;; id of dst settlement
   (days :initform nil :initarg :days :accessor days)))

(defmethod initialize-instance :after ((link link) &key)
  (setf (id link) (find-free-id *links*))
  (setf (gethash (id link) *links*) link)
  )

(defclass settlement ()
  ((id :initform nil :initarg :id :accessor id)
   (x :initform 0 :initarg :x :accessor x)
   (y :initform 0 :initarg :y :accessor y)
   (name :initform "Unnamed settlement" :initarg :name :accessor name)
   (links :initform nil :initarg :links :accessor links)
   ;; type - (link-id link-id ...)
   ;;(shops :initform nil :initarg :shops :accessor shops)
   ;; type - (shop-id shop-id ...)

   (market :initform (make-hash-table) :accessor market)

   (palace-id :initform nil :initarg :palace-id :accessor palace-id)
   (settlement-type :initform 0 :initarg :settlement-type :accessor settlement-type)
   (settlement-size :initform 0 :initarg :settlement-size :accessor settlement-size)
   
   (items-produce :initform () :accessor items-produce)
   (items-consume :initform () :accessor items-consume)
  
   (base-demand-supply :initform (make-hash-table) :accessor base-demand-supply)
   (cur-demand-supply :initform (make-hash-table) :accessor cur-demand-supply)
  
   ))

(defmethod initialize-instance :after ((settlement settlement) &key)
  (setf (id settlement) (find-free-id *settlements*))
  (setf (gethash (id settlement) *settlements*) settlement))

(defun initialize-produce-consume (settlement)
  (let ((items-produce nil)
        (items-consume nil)
        )
    (cond
      ((= (settlement-type settlement) +settlement-type-agriculture+)
       (pushnew (cons +item-type-food+ (+ 9 (random 6) (random 6) (random 6))) items-produce)
       (pushnew (cons +item-type-wood+ (+ 3 (random 6))) items-produce)
       (pushnew (cons +item-type-tools+ (random 6)) items-consume)
       (pushnew (cons +item-type-furniture+ (random 3)) items-consume)
       (pushnew (cons +item-type-crafts+ (random 4)) items-consume)
       )
      ((= (settlement-type settlement) +settlement-type-mining+)
       (pushnew (cons +item-type-food+ (- 6 (random 7) (random 7))) items-consume)
       (pushnew (cons +item-type-food+ (- 4 (random 7) (random 7))) items-produce)
       (pushnew (cons +item-type-ore+ (+ 9 (random 6) (random 6) (random 6))) items-produce)
       (pushnew (cons +item-type-gems+ (+ 3 (random 6))) items-produce)
       (pushnew (cons +item-type-tools+ (+ 3 (random 6))) items-consume)
       (pushnew (cons +item-type-furniture+ (random 3)) items-consume)
       (pushnew (cons +item-type-crafts+ (random 4)) items-consume)
       )
      ((= (settlement-type settlement) +settlement-type-industry+)
       (pushnew (cons +item-type-food+ (- 3 (random 7) (random 7))) items-consume)
       (pushnew (cons +item-type-food+ (- 2 (random 7) (random 7))) items-produce)
       (pushnew (cons +item-type-ore+ (+ 9 (random 6) (random 6))) items-consume)
       (pushnew (cons +item-type-gems+ (+ 3 (random 6))) items-consume)
       (pushnew (cons +item-type-tools+ (+ 6 (random 6))) items-produce)
       (pushnew (cons +item-type-furniture+ (+ 6 (random 6))) items-produce)
       (pushnew (cons +item-type-crafts+ (+ 6 (random 6))) items-consume)
       (pushnew (cons +item-type-wood+ (+ 6 (random 6))) items-consume)
       (pushnew (cons +item-type-jewelry+ (+ 1 (random 3))) items-produce)
       (pushnew (cons +item-type-weapons+ (+ 3 (random 6))) items-produce)
       )
      ((= (settlement-type settlement) +settlement-type-sprawling+)
       (pushnew (cons +item-type-food+ (- 1 (random 7) (random 7) (random 7))) items-consume)
       (pushnew (cons +item-type-food+ (- 2 (random 7) (random 7) (random 7))) items-produce)
       (pushnew (cons +item-type-ore+ (+ 2 (random 6))) items-consume)
       (pushnew (cons +item-type-gems+ (+ 1 (random 3))) items-consume)
       (pushnew (cons +item-type-tools+ (+ 3 (random 6))) items-consume)
       (pushnew (cons +item-type-furniture+ (+ 6 (random 6))) items-consume)
       (pushnew (cons +item-type-crafts+ (+ 6 (random 6))) items-consume)
       (pushnew (cons +item-type-wood+ (+ 3 (random 6))) items-consume)
       (pushnew (cons +item-type-jewelry+ (+ 3 (random 3))) items-consume)
       (pushnew (cons +item-type-weapons+ (+ 6 (random 6))) items-consume)
       )
      )
    (setf (items-produce settlement) items-produce)
    (setf (items-consume settlement) items-consume)))

(defun initialize-demand-supply (settlement)
  (loop
    for item-type being the hash-value in *item-types*
    do
       (setf (gethash (id item-type) (base-demand-supply settlement)) (cons 0 0)))
  (cond
    ((= (settlement-type settlement) +settlement-type-agriculture+)
     (setf (gethash +item-type-food+ (base-demand-supply settlement)) (cons -2 -1))
     (setf (gethash +item-type-wood+ (base-demand-supply settlement)) (cons -2 -1))
     (setf (gethash +item-type-tools+ (base-demand-supply settlement)) (cons 2 1))
     (setf (gethash +item-type-furniture+ (base-demand-supply settlement)) (cons 1 0))
     (setf (gethash +item-type-crafts+ (base-demand-supply settlement)) (cons 1 0)))
    ((= (settlement-type settlement) +settlement-type-mining+)    
     (setf (gethash +item-type-ore+ (base-demand-supply settlement)) (cons -2 -1))
     (setf (gethash +item-type-gems+ (base-demand-supply settlement)) (cons -2 -1))
     (setf (gethash +item-type-tools+ (base-demand-supply settlement)) (cons 2 1))
     (setf (gethash +item-type-furniture+ (base-demand-supply settlement)) (cons 1 0))
     (setf (gethash +item-type-crafts+ (base-demand-supply settlement)) (cons 1 0)))
    ((= (settlement-type settlement) +settlement-type-industry+)
     (setf (gethash +item-type-food+ (base-demand-supply settlement)) (cons 1 0))
     (setf (gethash +item-type-wood+ (base-demand-supply settlement)) (cons 2 1))
     (setf (gethash +item-type-ore+ (base-demand-supply settlement)) (cons 2 1))
     (setf (gethash +item-type-gems+ (base-demand-supply settlement)) (cons 2 1))
     (setf (gethash +item-type-tools+ (base-demand-supply settlement)) (cons -2 -1))
     (setf (gethash +item-type-furniture+ (base-demand-supply settlement)) (cons -2 -1))
     (setf (gethash +item-type-crafts+ (base-demand-supply settlement)) (cons -2 -1))
     (setf (gethash +item-type-jewelry+ (base-demand-supply settlement)) (cons -1 0))
     (setf (gethash +item-type-weapons+ (base-demand-supply settlement)) (cons -1 0))
     )
    ((= (settlement-type settlement) +settlement-type-sprawling+)
     (setf (gethash +item-type-food+ (base-demand-supply settlement)) (cons 2 1))
     (setf (gethash +item-type-food+ (base-demand-supply settlement)) (cons 2 1))
     (setf (gethash +item-type-gems+ (base-demand-supply settlement)) (cons 1 0))
     (setf (gethash +item-type-tools+ (base-demand-supply settlement)) (cons 1 0))
     (setf (gethash +item-type-furniture+ (base-demand-supply settlement)) (cons 2 1))
     (setf (gethash +item-type-crafts+ (base-demand-supply settlement)) (cons 2 1))
     (setf (gethash +item-type-jewelry+ (base-demand-supply settlement)) (cons 2 1))
     (setf (gethash +item-type-weapons+ (base-demand-supply settlement)) (cons 2 1))
     )
    )
  (loop
    for item-type being the hash-value in *item-types*
    do
       (setf (gethash (id item-type) (cur-demand-supply settlement)) (cons (car (gethash (id item-type) (base-demand-supply settlement)))
                                                                           (cdr (gethash (id item-type) (base-demand-supply settlement))))))
  )

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

;;(defun get-settlement-shop (settlement n)
;;  (gethash (nth n (shops settlement)) *shops*))

;;(defun get-first-settlement-shop (settlement)
;;  (when (shops settlement)
;;    (gethash (first (shops settlement)) *shops*)))

(defun get-settlement-palace (settlement)
  (gethash (palace-id settlement) *palaces*))

(defun set-settlement-palace (settlement palace)
  (setf (palace-id settlement) (id palace)))

(defun get-settlement-size-name (settlement)
  (nth (settlement-size settlement) *settlement-size-names*))

(defun get-settlement-type-adj (settlement)
  (nth (settlement-type settlement) *settlement-type-names*))

(defun get-settlement-descr-for-player (settlement)
  (let ((str (create-string)))
    (format str "You are in the ~A ~A of ~A~%" (get-settlement-type-adj settlement) (get-settlement-size-name settlement) (name settlement))
    ;;(when (get-first-settlement-shop settlement)
    ;; (format str "There is a ~A in the town.~%" (name (get-first-settlement-shop settlement))))
    (format str "It is day ~A today.~%" (show-cur-time))
    (format str "~%")
    str))

(defclass trader ()
  ((id :initform nil :initarg :id :accessor id)
   (name :initform "Anonymous" :initarg :name :accessor name)
   (action-done :initform 1 :accessor action-done) ;; fixnum
   (current-settlement-id :initform nil :initarg :current-settlement-id :accessor current-settlement-id)
   (inv :initform (make-hash-table) :accessor inv)
   (money :initform 0 :initarg :money :accessor money)
   ))

(defmethod initialize-instance :after ((trader trader) &key)
  (setf (id trader) (find-free-id *traders*))
  (setf (gethash (id trader) *traders*) trader)
  )


;;(defclass item ()
;;  ((id :initform nil :initarg :id :accessor id)
;;   (item-type :initarg :item-type :accessor item-type)
;;   (qty :initform 0 :initarg :qty :accessor qty)
;;   ))

;;(defmethod initialize-instance :after ((item item) &key)
;;  (setf (id item) (find-free-id *items*))
;;  (setf (gethash (id item) *items*) item)
;;  )

;;(defmethod name ((item item))
;;  (name (get-item-type-by-id (item-type item))))

;;(defmethod descr ((item item))
;;  (descr (get-item-type-by-id (item-type item))))

;;(defun get-item-by-id (item-id)
;;  (gethash item-id *items*))

;;(defun rem-item-from-world (item)
;;  (remhash (id item) *items*))

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

(defclass world ()
  ((wtime :initform 0 :initarg :wtime :accessor wtime)))

(defclass palace ()
  ((id :initform nil :accessor id)
   (ruler-name :initarg :ruler-name :accessor ruler-name)
   (ruler-male :initarg :ruler-male :accessor ruler-male)
   (ruler-title :initarg :ruler-title :accessor ruler-title)
   (ruler-favor :initform 0 :initarg :ruler-favor :accessor ruler-favor)
   (audience :initform nil :accessor audience)
   (cur-quest :initform nil :initarg :cur-quest :accessor cur-quest)
   (quest-timer :initform 0 :accessor quest-timer)
   ))

(defmethod initialize-instance :after ((palace palace) &key)
  (setf (id palace) (find-free-id *palaces*))
  (setf (gethash (id palace) *palaces*) palace)

  (if (zerop (random 2))
    (progn
      (setf (ruler-male palace) t)
      (setf (ruler-name palace) (nth (random (length *ruler-male-names*)) *ruler-male-names*)))
    (progn
      (setf (ruler-male palace) nil)
      (setf (ruler-name palace) (nth (random (length *ruler-female-names*)) *ruler-female-names*))))
  
  (setf (ruler-title palace) (nth (random (length *ruler-title-names*)) *ruler-title-names*))
  )

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

(defun get-sell-price (item-type-id settlement)
  (get-settlement-sell-price settlement item-type-id))

(defun get-buy-price (item-type-id settlement)
  (get-settlement-buy-price settlement item-type-id))

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