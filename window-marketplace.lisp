(in-package :trader-rl)

(defconstant +win-mode-buy+ 0)
(defconstant +win-mode-sell+ 1)

(defconstant +win-qty-none+ 0)
(defconstant +win-qty-input+ 1)
(defconstant +win-qty-no-money+ 2)

(defclass marketplace-window (window)
  ((cur-sel :initform 0 :accessor cur-sel)
   (cur-mode :initform +win-mode-buy+ :accessor cur-mode)
   (cur-qty-mode :initform +win-qty-none+ :accessor cur-qty-mode)
   (input :initform (make-array (list 1) :element-type 'character :adjustable t :fill-pointer t :initial-contents "0") :accessor input)
   (item :initform nil :accessor item)
   (item-list :initform nil :accessor item-list)))

(defmethod generate-win-actions ((win marketplace-window))
  (setf (win-actions win) nil)
  (let ((player-settlement (get-settlement-by-id (current-settlement-id *player*)))
        )
    (if (= (cur-mode win) +win-mode-buy+)
      (progn
        (setf (item-list win) (inv-as-list (market player-settlement)))
        (loop
          for item-type-id being the hash-keys in (market player-settlement) using (hash-value qty)
          do
             (setf (win-actions win) (append (win-actions win) (list (cons (format nil "(~A$/~A$) ~A" 
                                                                                   (get-buy-price item-type-id player-settlement)
                                                                                   (* (get-buy-price item-type-id player-settlement) qty)
                                                                                   (show-item-line item-type-id qty))  
                                                                           #'(lambda (n) 
                                                                               (unless (buy-item (car (nth n (item-list win))) *player* player-settlement)
                                                                                   (setf (cur-qty-mode win) +win-qty-no-money+))
                                                                               (generate-win-actions win)
                                                                               )))))
          ))
      (progn
        (setf (item-list win) (inv-as-list (inv *player*)))
        
        (loop
          for item-type-id being the hash-keys in (inv *player*) using (hash-value qty)
          do
             (setf (win-actions win) (append (win-actions win) (list (cons (format nil "(~A$/~A$) ~A"
                                                                                   (get-sell-price item-type-id player-settlement)
                                                                                   (* (get-sell-price item-type-id player-settlement) qty)
                                                                                   (show-item-line item-type-id qty))  
                                                                           #'(lambda (n) 
                                                                               (sell-item (car (nth n (item-list win))) *player* player-settlement)
                                                                               (generate-win-actions win)
                                                                               )))))
          ))
      ))
  
  (setf (win-actions win) (append (win-actions win) (list (cons "Back"  #'(lambda (n) (declare (ignore n)) (setf *current-window* (return-to win)))))))
  )

(defmethod make-output ((win marketplace-window))
  ;; fill with black background
  (sdl:fill-surface sdl:*black*)
  
  (let ((str (create-string)) (cur-line 0) (player-settlement (get-settlement-by-id (current-settlement-id *player*)))
        )
    
    (setf (cur-sel win) (adjust-selection-list (cur-sel win) (length (win-actions win))))
    
    ;; output city info
    (format str "You are in the marketplace of ~A now.~%It is day ~A today. You have ~A gold.~%~%" (name player-settlement) (show-date-time (wtime *world*)) (money *player*))

    (if (= (cur-mode win) +win-mode-buy+)
      (progn
        (unless (zerop (get-item-inv (car (nth (cur-sel win) (item-list win))) (market player-settlement))) 
          
          (format str "~ACurrent buy price is ~A$ (~A$ for all).~%" 
                  (show-item-all (car (nth (cur-sel win) (item-list win)))(cdr (nth (cur-sel win) (item-list win))))
                  (get-buy-price (car (nth (cur-sel win) (item-list win))) player-settlement)
                  (* (get-buy-price (car (nth (cur-sel win) (item-list win))) player-settlement) (cdr (nth (cur-sel win) (item-list win)))))))
      (progn
        (unless (zerop (get-item-inv (car (nth (cur-sel win) (item-list win))) (inv *player*)))
          (format str "~ACurrent sell price is ~A$ (~A$ for all).~%" 
                  (show-item-all (car (nth (cur-sel win) (item-list win)))(cdr (nth (cur-sel win) (item-list win))))
                  (get-sell-price (car (nth (cur-sel win) (item-list win))) player-settlement)
                  (* (get-sell-price (car (nth (cur-sel win) (item-list win))) player-settlement) (cdr (nth (cur-sel win) (item-list win))))))))
    
    (format str "~%")
    
    (sdl:with-default-font ((sdl:initialise-default-font sdl:*font-6x13*))
      (setf cur-line (write-text str (sdl:rectangle :x 1 :y 1 :w 800 :h 600))))
    
    ;; output available commands 
    
    
    (let ((cur-str) (lst (make-list 0)) (color-list (make-list 0)))
      (setf cur-str (cur-sel win))
      (dotimes (i (length (win-actions win)))
        
        (setf lst (append lst (list (car (nth i (win-actions win))))))
        

        (if (= i cur-str)  
	  (setf color-list (append color-list (list sdl:*yellow*)))
	  (setf color-list (append color-list (list sdl:*white*)))))
      
      (draw-selection-list lst cur-str 10 0 (+ 0 (* cur-line 13)) color-list))

    ;; output help prompt
    (sdl:with-default-font ((sdl:initialise-default-font sdl:*font-6x13*))
      (cond 
        ((= (cur-qty-mode win) +win-qty-input+)
         (write-text (format nil "Enter the quantity to ~A (0-~A): ~A" 
                             (if (= (cur-mode win) +win-mode-buy+) "buy" "sell")
                             (cdr (item win))
                             (input win)) 
                     (sdl:rectangle :x 0 :y (+ 0 (* cur-line 13) (* 10 13)) :w 800 :h 20)))
        ((= (cur-qty-mode win) +win-qty-no-money+)
         (write-text (format nil "You do not have enough money! [Esc] Back")
                     (sdl:rectangle :x 0 :y (+ 0 (* cur-line 13) (* 10 13)) :w 800 :h 20)))
        ((= (cur-qty-mode win) +win-qty-none+)
         (write-text (format nil "[Enter] ~A  [Ctrl+Enter] ~A  [Tab] Change to ~A  [Esc] Back" 
                             (if (= (cur-mode win) +win-mode-buy+) "Buy" "Sell")
                             (if (= (cur-mode win) +win-mode-buy+) "Buy N" "Sell N")
                             (if (= (cur-mode win) +win-mode-buy+) "sell mode" "buy mode")) 
                     (sdl:rectangle :x 0 :y (+ 0 (* cur-line 13) (* 10 13)) :w 800 :h 20)))))
    
    )
  (sdl:update-display))

(defmethod run-window ((win marketplace-window))
  (tagbody
     (sdl:with-events ()
       (:quit-event () (funcall (quit-func win)) t)
       (:key-down-event (:key key :mod mod :unicode unicode)
                        (cond
                          ;; in qty entry mode
                          ((= (cur-qty-mode win) +win-qty-input+)
                           (cond
                             ((sdl:key= key :sdl-key-escape)
                              (setf (cur-qty-mode win) +win-qty-none+))
                             ((sdl:key= key :sdl-key-backspace)
                              (unless (zerop (fill-pointer (input win)))
                                (decf (fill-pointer (input win)))))
                             ((sdl:key= key :sdl-key-return)
                              (let ((i (parse-integer (input win) :junk-allowed t)))
                                (when (and (not (eql i nil)))
                                  (cond 
                                    ((> i (cdr (item win)))
                                     (setf (input win) (make-array (list (length (format nil "~A" (cdr (item win))))) :element-type 'character :adjustable t :fill-pointer t :initial-contents (format nil "~A" (cdr (item win))))))
                                    ((< i 0)
                                     (setf (input win) (make-array (list 1) :element-type 'character :adjustable t :fill-pointer t :initial-contents "0")))
                                    (t
                                     (setf (cur-qty-mode win) +win-qty-none+)
                                     (if (= (cur-mode win) +win-mode-buy+) 
                                       (progn
                                         (unless (buy-item (car (item win)) *player* (get-settlement-by-id (current-settlement-id *player*)) i)
                                           (setf (cur-qty-mode win) +win-qty-no-money+))
                                         )
                                       (progn
                                         (sell-item (car (item win)) *player* (get-settlement-by-id (current-settlement-id *player*)) i)
                                         ))
                                     (generate-win-actions win))
                                     
                                     
                                   ))))
                              (t (let ((i (parse-integer (string (code-char unicode)) :junk-allowed t)))
                                   (when (not (eql i nil))
                                     (setf (input win) (get-text-input (input win) key mod unicode)))))))
                          ;; no money mode
                          ((= (cur-qty-mode win) +win-qty-no-money+)
                           (cond
                             ((sdl:key= key :sdl-key-escape)
                              (setf (cur-qty-mode win) +win-qty-none+))
                             ))
                          ;; not in qty entry mode
                          (t
                            (setf (cur-sel win) (run-selection-list key mod unicode (cur-sel win)))
                            (cond
                              ((sdl:key= key :sdl-key-escape)
                               (setf *current-window* (return-to win)) (go exit-func))
                            ((sdl:key= key :sdl-key-tab)
                             (if (= (cur-mode win) +win-mode-buy+)
                               (setf (cur-mode win) +win-mode-sell+)
                               (setf (cur-mode win) +win-mode-buy+))
                             (generate-win-actions win))
                            ((and (sdl:key= key :sdl-key-return) (= mod 0))
                             (when (cdr (nth (cur-sel win) (win-actions win)))
                               (funcall (cdr (nth (cur-sel win) (win-actions win))) (cur-sel win))))
                            ((and (sdl:key= key :sdl-key-return) (/= (logand mod sdl-cffi::sdl-key-mod-ctrl) 0))
                             (if (= (cur-mode win) +win-mode-buy+) 
                               (unless (zerop (get-item-inv (car (nth (cur-sel win) (item-list win))) (market (get-settlement-by-id (current-settlement-id *player*)))))
                                 (setf (item win) (nth (cur-sel win) (item-list win)))
                                 (setf (input win) (make-array (list 1) :element-type 'character :adjustable t :fill-pointer t :initial-contents "0"))
                                 (setf (cur-qty-mode win) +win-qty-input+))
                               (unless (zerop (get-item-inv (car (nth (cur-sel win) (item-list win))) (inv *player*)))
                                 (setf (item win) (nth (cur-sel win) (item-list win)))
                                 (setf (input win) (make-array (list 1) :element-type 'character :adjustable t :fill-pointer t :initial-contents "0"))
                                 (setf (cur-qty-mode win) +win-qty-input+)))
                             ))))
			(make-output *current-window*)
       			(go exit-func)
			)
       (:video-expose-event () (make-output *current-window*)))
     exit-func (make-output *current-window*)))

