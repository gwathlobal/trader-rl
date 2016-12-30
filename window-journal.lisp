(in-package :trader-rl)

(defclass journal-window (window)
  ((journal :initarg :journal :accessor journal)
   (cur-sel :initform 0 :accessor cur-sel)))



(defmethod generate-win-actions ((win journal-window))
  (setf (win-actions win) nil)
  )
  

(defmethod make-output ((win journal-window))
  ;; fill with black background
  (sdl:fill-surface sdl:*black*)
  
  (sdl:with-default-font ((sdl:initialise-default-font sdl:*font-6x13*))
    (write-text (format nil "Your journal:~%") (sdl:rectangle :x 0 :y 0 :w 800 :h 20)))
  
  (setf (cur-sel win) (adjust-selection-list (cur-sel win) (length (journal win))))
  
  
  (let ((cur-str) (lst (make-list 0)) (color-list (make-list 0)))
    (setf cur-str (cur-sel win))
    (dotimes (i (length (journal win)))
      
      (setf lst (append lst (list (show-journal-entry (get-n-journal-entry (journal win) i)))))
      
      (if (= i cur-str)  
        (setf color-list (append color-list (list sdl:*yellow*)))
        (setf color-list (append color-list (list sdl:*white*)))))
    
    ;;(draw-selection-list lst cur-str 30 0 (+ 13 (* cur-line 13)) color-list)
    (draw-multiline-selection-list lst cur-str 0 13 800 (* 13 30) color-list)
    )
  
  (sdl:with-default-font ((sdl:initialise-default-font sdl:*font-6x13*))
    (write-text (format nil "[Esc] Exit") (sdl:rectangle :x 0 :y (* 13 30) :w 800 :h 20)))
  
  (sdl:update-display))

(defmethod run-window ((win journal-window))
  (tagbody
     (sdl:with-events ()
       (:quit-event () (funcall (quit-func win)) t)
       (:key-down-event (:key key :mod mod :unicode unicode)
                        (setf (cur-sel win) (run-selection-list key mod unicode (cur-sel win)))
                        (cond
			  ((sdl:key= key :sdl-key-escape) 
			   (setf *current-window* (return-to win)) (go exit-func))
                          )
			(make-output *current-window*)
       			(go exit-func)
			)
       (:video-expose-event () (make-output *current-window*)))
     exit-func (make-output *current-window*)))
