(in-package :trader-rl)

(defclass settlement-window (window)
  ((cur-sel :initform 0 :accessor cur-sel)))



(defmethod generate-win-actions ((win settlement-window))
  (setf (win-actions win) nil)
  
  
  (pushnew (cons "Leave the place" 
                 #'(lambda (n) (declare (ignore n)) (setf *current-window* (make-instance 'leave-settlement-window))
                           (generate-win-actions win))) 
           (win-actions win))

  (pushnew (cons "Wait 1 day"
                 #'(lambda (n) (declare (ignore n)) (decf (action-done *player*))
                           (generate-win-actions win)))
           (win-actions win))

  (pushnew (cons "View journal"
                 #'(lambda (n) (declare (ignore n)) (setf *current-window* (make-instance 'journal-window :journal (journal *player*) :quests (generate-quest-journal (quests *player*))))))
           (win-actions win))
  
  (let ((player-settlement (get-settlement-by-id (current-settlement-id *player*)))
        (available-commands nil))
    (when (get-settlement-feature player-settlement +feature-type-palace+)
      (pushnew (cons (format nil "Visit the palace") 
                     #'(lambda (n) (declare (ignore n)) (setf *current-window* (make-instance 'palace-window)))) 
               (win-actions win)))
        
    (pushnew (cons (format nil "Visit the marketplace") 
                   #'(lambda (n) (declare (ignore n)) (setf *current-window* (make-instance 'marketplace-window)))) 
             (win-actions win))
    
    ;; iterate through all player's quests to create a list of quests that can be completed
    (loop 
      for quest-id in (quests *player*)
      with quest = nil
      do
         (setf quest (get-quest-by-id quest-id))

         (format t "AVAILABLE COMMANDS, STAGE ~A, ON-CHECK-COMPLETE ~A~%" (stage quest) (funcall (on-check-complete quest) quest))
         
         (when (and (= (stage quest) +quest-stage-accepted+)
                    (on-check-complete quest)
                    (funcall (on-check-complete quest) quest))
           (setf available-commands (append available-commands (list quest-id)))))
    
    
    
    (loop
      for quest-id in available-commands
      with quest = nil
      do
         (setf quest (get-quest-by-id quest-id))
         (pushnew (cons (complete-descr quest) 
                        #'(lambda (n) 
                                                        
                            (setf (journal *player*) (add-to-journal (journal *player*) :date (wtime *world*) :importance +journal-importance-high+ 
                                                                                        :string (format nil "I have completed the quest - ~A" (descr (get-quest-by-id (get-n-quest-in-list available-commands n))) 
                                                                                                        )))
                            (setf (stage (get-quest-by-id (get-n-quest-in-list available-commands n)))
                                  +quest-stage-completed+)
                            (when (on-complete (get-quest-by-id (get-n-quest-in-list available-commands n)))
                              (funcall (on-complete (get-quest-by-id (get-n-quest-in-list available-commands n)))
                                       (get-quest-by-id (get-n-quest-in-list available-commands n))))
                            (generate-win-actions win))) 
                  (win-actions win))
           )
    
    )
  )

(defmethod make-output ((win settlement-window))
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
      (write-text (format nil "[Enter] Perform action  [J] View journal") (sdl:rectangle :x 0 :y (+ 0 (* cur-line 13) (* 10 13)) :w 800 :h 20)))
    
    )
  (sdl:update-display))

(defmethod run-window ((win settlement-window))
  (tagbody
     (sdl:with-events ()
       (:quit-event () (funcall (quit-func win)) t)
       (:key-down-event (:key key :mod mod :unicode unicode)
			(setf (cur-sel win) (run-selection-list key mod unicode (cur-sel win)))
                        (cond
                          ((sdl:key= key :sdl-key-return)
                           (when (cdr (nth (cur-sel win) (win-actions win)))
                             (funcall (cdr (nth (cur-sel win) (win-actions win))) (cur-sel win))))
                          ((sdl:key= key :sdl-key-j)
                           (setf *current-window* (make-instance 'journal-window :journal (journal *player*) :quests (generate-quest-journal (quests *player*)))))
			  )
			(make-output *current-window*)
       			(go exit-func)
			)
       (:video-expose-event () (make-output *current-window*)))
     exit-func (make-output *current-window*)))
