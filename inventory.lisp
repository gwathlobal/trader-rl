(in-package :trader-rl)

(defun get-item-inv (item-type-id inv)
  (if (gethash item-type-id inv)
    (gethash item-type-id inv)
    0))

(defun add-to-inv (item-type-id inv qty)
  (when (> qty 0)
    (if (gethash item-type-id inv) 
      (incf (gethash item-type-id inv) qty)
      (setf (gethash item-type-id inv) qty)))
  )

(defun remove-from-inv (item-type-id inv qty)
  (cond
    ((<= qty 0) nil)
    ((>= qty (get-item-inv item-type-id inv))
     (remhash item-type-id inv))
    ((< qty (get-item-inv item-type-id inv))
     (decf (gethash item-type-id inv) qty))
     ))

(defun inv-as-list (inv)
  (loop
    for item-type-id being the hash-keys in inv using (hash-value qty)
    with item-list = nil
    finally (return item-list)
    do
       (setf item-list (append item-list (list (cons item-type-id qty))))
       ))

(defun buy-item (item-type-id buyer settlement &optional (qty (get-item-inv item-type-id (market settlement))))
  
  (when (> (* (get-buy-price item-type-id settlement) qty)
           (money buyer))
    (return-from buy-item nil))
  
  (let ((old-qty (get-item-inv item-type-id (market settlement)))
        (new-qty))
   
   
    (decf (money buyer) (* (get-buy-price item-type-id settlement) qty))
    (remove-from-inv item-type-id (market settlement) qty)
    (add-to-inv item-type-id (inv buyer) qty)
    (setf new-qty (get-item-inv item-type-id (market settlement)))
    
    (adjust-price settlement item-type-id old-qty new-qty))
  t)
 

(defun sell-item (item-type-id seller settlement &optional (qty (get-item-inv item-type-id (inv seller))))
  (let ((old-qty (get-item-inv item-type-id (inv seller)))
        (new-qty))
        
    (incf (money seller) (* (get-sell-price item-type-id settlement) qty))
    (remove-from-inv item-type-id (inv seller) qty)
    (add-to-inv item-type-id (market settlement) qty)
    (setf new-qty (get-item-inv item-type-id (inv seller)))
    (adjust-price settlement item-type-id old-qty new-qty))
  t)

(defun adjust-price (settlement item-type-id old-qty new-qty)
  (if (< (- new-qty old-qty) 0)
    (progn
      (format t "BUY~%")
      (set-settlement-cur-demand settlement item-type-id (1+ (get-settlement-cur-demand settlement item-type-id)))
      ;(when (zerop new-qty) (setf new-qty 1))
      ;(when (zerop old-qty) (setf old-qty 1))
      ;(set-settlement-buy-price settlement item-type-id (ceiling (/ (* old-qty (get-settlement-buy-price settlement item-type-id)) new-qty)))
      ;(set-settlement-sell-price settlement item-type-id (round (/ (* old-qty (get-settlement-buy-price settlement item-type-id)) (+ (get-settlement-max-reserve-by-id settlement item-type-id) new-qty))))
      )
    (progn
      (format t "SELL OLD: ~A NEW: ~A~%" old-qty new-qty)
      (set-settlement-cur-supply settlement item-type-id (1- (get-settlement-cur-supply settlement item-type-id)))
      ;(when (zerop new-qty) (setf new-qty 1))
      ;(when (zerop old-qty) (setf old-qty 1))
      ;(set-settlement-sell-price settlement item-type-id (ceiling (/ (* old-qty (get-settlement-sell-price settlement item-type-id)) new-qty)))
      ;(set-settlement-buy-price settlement item-type-id (truncate (/ (* old-qty (get-settlement-buy-price settlement item-type-id)) (* 1 new-qty))))
      )
    )
  (format t "~A, BUY ~A, SELL ~A~%" (name (get-item-type-by-id item-type-id)) (get-settlement-cur-demand settlement item-type-id) (get-settlement-cur-supply settlement item-type-id))
  )
