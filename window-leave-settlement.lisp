(in-package :trader-rl)

(defclass leave-settlement-window (window)
  ((cur-sel :initform 0 :accessor cur-sel)))

(defmethod generate-win-actions ((win leave-settlement-window))
  (setf (win-actions win) nil)
  ;; generate actions to move to other settlements
  (let ((player-settlement (get-settlement-by-id (current-settlement-id *player*))))
    (loop 
      for link-id in (links player-settlement)
      with link = nil
      do
         (setf link (gethash link-id *links*))
         (setf (win-actions win) (append (win-actions win) (list (cons (format nil "Go to ~A (~A days)" (name (get-settlement-by-id (dst-id link))) (days link)) 
                                                                       #'(lambda (n) 
                                                                           (move-trader-to-settlement *player* 
                                                                                                      (get-settlement-by-id (dst-id (gethash (nth n (links player-settlement)) *links*))) 
                                                                                                      (days (gethash (nth n (links player-settlement)) *links*)))
                                                                           (setf *current-window* (return-to win))
                                                                           (generate-win-actions *current-window*)
                                                                           )))))
      )
    
    
  (setf (win-actions win) (append (win-actions win) (list (cons "Back"  #'(lambda (n) (declare (ignore n)) (setf *current-window* (return-to win)))))))
  ))

(defmethod make-output ((win leave-settlement-window))
  ;; fill with black background
  (sdl:fill-surface sdl:*black*)
  
  (let ((str (create-string)) (cur-line 0) (player-settlement (get-settlement-by-id (current-settlement-id *player*))))
    
    ;; output city info
    (format str "~A" (get-settlement-descr-for-player player-settlement))
    
    
    (sdl:with-default-font ((sdl:initialise-default-font sdl:*font-6x13*))
      (setf cur-line (write-text str (sdl:rectangle :x 1 :y 1 :w 800 :h 600))))
    
    ;; output available commands 
    (setf (cur-sel win) (adjust-selection-list (cur-sel win) (length (win-actions win))))
    
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
      (write-text (format nil "[Enter] Perform action [Esc] Back") (sdl:rectangle :x 0 :y (+ 0 (* cur-line 13) (* 10 13)) :w 800 :h 20)))
    
    )
  (sdl:update-display))

(defmethod run-window ((win leave-settlement-window))
  (tagbody
     (sdl:with-events ()
       (:quit-event () (funcall (quit-func win)) t)
       (:key-down-event (:key key :mod mod :unicode unicode)
			(setf (cur-sel win) (run-selection-list key mod unicode (cur-sel win)))
                        (cond
			  ((sdl:key= key :sdl-key-escape) 
			   (setf *current-window* (return-to win)) (go exit-func))
                          ((sdl:key= key :sdl-key-return)
                           (when (cdr (nth (cur-sel win) (win-actions win)))
                             (funcall (cdr (nth (cur-sel win) (win-actions win))) (cur-sel win))))
                          )
			(make-output *current-window*)
       			(go exit-func)
			)
       (:video-expose-event () (make-output *current-window*)))
     exit-func (make-output *current-window*)))
