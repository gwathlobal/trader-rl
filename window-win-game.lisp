(in-package :trader-rl)

(defclass win-game-window (window)
  ())



(defmethod generate-win-actions ((win win-game-window))
  (setf (win-actions win) nil)
  )
  

(defmethod make-output ((win win-game-window))
  ;; fill with black background
  (sdl:fill-surface sdl:*black*)
  
  (let ((str (create-string)))
    
    ;; output palace info
    (format str "Congratulations! You have won the game!~%~%[Press any key to exit]~%")
    
    (sdl:with-default-font ((sdl:initialise-default-font sdl:*font-6x13*))
      (write-text str (sdl:rectangle :x 1 :y 1 :w 800 :h 600)))
    )
  (sdl:update-display))

(defmethod run-window ((win win-game-window))
  (sdl:with-events ()
    (:quit-event () (setf *save-on-quit* :delete) (funcall (quit-func win)) t)
    (:key-down-event (:key key :mod mod :unicode unicode)
                     (setf *save-on-quit* :delete)
                     (funcall (quit-func win))
                     t)
    (:video-expose-event () (make-output *current-window*)))
  (make-output *current-window*))

