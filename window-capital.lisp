(in-package :trader-rl)

(defclass palace-window (window)
  ((cur-sel :initform 0 :accessor cur-sel)))



(defmethod generate-win-actions ((win palace-window))
  (setf (win-actions win) nil)
  
  
  (pushnew (cons "Leave the palace" 
                 #'(lambda (n) (declare (ignore n)) (setf *current-window* (return-to win)))) 
           (win-actions win))

  (when (and (>= (money *player*) +ruler-audience-tax+) 
             (not (audience (get-settlement-realm (get-settlement-by-id (current-settlement-id *player*))))))
    (pushnew (cons (format nil "Bribe scribes to request audience with the ruler (~Ag)" +ruler-audience-tax+) 
                   #'(lambda (n) (declare (ignore n))
                       (decf (money *player*) +ruler-audience-tax+)
                       (setf (audience (get-settlement-realm (get-settlement-by-id (current-settlement-id *player*)))) t)
                       (generate-win-actions win))) 
           (win-actions win))
    )
  
  (when (and (>= (ruler-favor (get-settlement-realm (get-settlement-by-id (current-settlement-id *player*)))) +ruler-favor-win+) 
             (audience (get-settlement-realm (get-settlement-by-id (current-settlement-id *player*)))))
    (pushnew (cons (format nil "Ask the ruler to grant you the position of Master Treasurer") 
                   #'(lambda (n) (declare (ignore n))
                       (setf *current-window* (make-instance 'win-game-window))
                       )) 
           (win-actions win))
    )
  (let ((realm (get-settlement-realm (get-settlement-by-id (current-settlement-id *player*))))
        (available-quest-list nil))
    (loop 
      for quest-id in (quests realm)
      with quest = nil
      do
         (setf quest (get-quest-by-id quest-id))
         (when (= (stage quest) +quest-stage-new+)
           (setf available-quest-list (append available-quest-list (list quest-id)))))
    
    (loop
      for quest-id in available-quest-list
      with quest = nil
      do
         (setf quest (get-quest-by-id quest-id))
         (pushnew (cons (descr quest) 
                        #'(lambda (n) 
                            (format t "~%QUEST ACCEPTED ~A, ~A~%" 
                                    (id (get-quest-by-id (get-n-quest-in-list available-quest-list n))) 
                                    (descr (get-quest-by-id (get-n-quest-in-list available-quest-list n))))
                            
                            (setf (journal *player*) (add-to-journal (journal *player*) :date (wtime *world*) :importance +journal-importance-high+ 
                                                                                        :string (format nil "I have accepted a quest. ~A" (descr (get-quest-by-id (get-n-quest-in-list available-quest-list n))) 
                                                                                                        )))
                            
                            (setf (stage (get-quest-by-id (get-n-quest-in-list available-quest-list n)))
                                  +quest-stage-accepted+)
                            (setf (date (get-quest-by-id (get-n-quest-in-list available-quest-list n)))
                                  (wtime *world*))
                            (setf (quester-id (get-quest-by-id (get-n-quest-in-list available-quest-list n)))
                                  (id *player*))
                            (setf (quests *player*) (add-to-quests (quests *player*) (get-quest-by-id (get-n-quest-in-list available-quest-list n))))
                            (generate-win-actions win))) 
                  (win-actions win))
           )
      )
  
  
  (return-from generate-win-actions nil)
  
  (let ((cur-quest (cur-quest (get-settlement-realm (get-settlement-by-id (current-settlement-id *player*)))))
        (passed nil)
        (str))
    (when cur-quest
      (setf cur-quest (get-quest-type-by-id cur-quest))
      (if (= (quest-item-id cur-quest) +quest-item-money+)
        (when (>= (money *player*) (quest-item-num cur-quest))
          (setf str (format nil "Donate ~A gold to the realm" 
                               (quest-item-num cur-quest)
                               ))
          (setf passed t))
        (when (>= (get-item-inv (quest-item-id cur-quest) (inv *player*))
                  (quest-item-num cur-quest))
          (setf str (format nil "Donate ~A ~(~A~) to the realm" 
                               (quest-item-num cur-quest)
                               (name (get-item-type-by-id (quest-item-id cur-quest)))
                               ))
          (setf passed t)))
      
      (when (and (audience (get-settlement-realm (get-settlement-by-id (current-settlement-id *player*))))
                 passed)
        (pushnew (cons str 
                       #'(lambda (n) (declare (ignore n))
                           (let ((realm (get-settlement-realm (get-settlement-by-id (current-settlement-id *player*))))
                                 )
                             (if (= (quest-item-id (get-quest-type-by-id (cur-quest realm))) +quest-item-money+)
                               (decf (money *player*) (quest-item-num (get-quest-type-by-id (cur-quest realm))))
                               (remove-from-inv (quest-item-id (get-quest-type-by-id (cur-quest realm))) (inv *player*) (quest-item-num (get-quest-type-by-id (cur-quest realm)))))
                                                            
                             (setf (cur-quest realm) nil)
                             (incf (ruler-favor realm) (1+ (random 2))))
                           
                           (generate-win-actions win))) 
                 (win-actions win)))))
  
  (format t "RULER-FAVOR ~A~%" (ruler-favor (get-settlement-realm (get-settlement-by-id (current-settlement-id *player*)))))
  
  
  )
  

(defmethod make-output ((win palace-window))
  ;; fill with black background
  (sdl:fill-surface sdl:*black*)
  
  
  
  (let ((str (create-string)) (cur-line 0) (player-settlement (get-settlement-by-id (current-settlement-id *player*))))
  
    (format t "~%QUESTS ~A~%" (quests (get-settlement-realm player-settlement)))
    
    ;; output palace info
    (format str "You are in the palace of ~A now.~%~%The ruler of the realm ~A Highness ~A the ~A resides here.~%~%" 
            (name player-settlement) 
            (if (ruler-male (get-settlement-realm player-settlement))
              "His"
              "Her")
            (ruler-name (get-settlement-realm player-settlement))
            (ruler-title (get-settlement-realm player-settlement))
            )
    
    (cond
      ((<= (ruler-favor (get-settlement-realm player-settlement)) 2) (format str "The ruler is indifferent to you.~%~%"))
      ((<= (ruler-favor (get-settlement-realm player-settlement)) 4) (format str "The ruler remembers you name.~%~%"))
      ((<= (ruler-favor (get-settlement-realm player-settlement)) 6) (format str "The ruler looks favorably at you.~%~%"))
      ((<= (ruler-favor (get-settlement-realm player-settlement)) 8) (format str "The ruler admires you.~%~%"))
      (t (format str "The ruler is pleased to see you.~%~%")))
    
    (unless (audience (get-settlement-realm player-settlement))
      (format str "The ruler seems too busy to pay any attention to you.~%~%"))

    (when (audience (get-settlement-realm player-settlement))
      (if (cur-quest (get-settlement-realm player-settlement))
        (format str "The ruler says to you: \"~A\"~%~%" (intro-str (get-quest-type-by-id (cur-quest (get-settlement-realm player-settlement)))))
        (format str "The ruler says to you: \"We have no tasks for you for now.\"~%~%")))
    
    
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
      (write-text (format nil "[Enter] Perform action") (sdl:rectangle :x 0 :y (+ 0 (* cur-line 13) (* 10 13)) :w 800 :h 20)))
    
    )
  (sdl:update-display))

(defmethod run-window ((win palace-window))
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
