(in-package :trader-rl)

(defclass intro-window (window)
  ())



(defmethod generate-win-actions ((win intro-window))
  (setf (win-actions win) nil)
  )
  

(defmethod make-output ((win intro-window))
  ;; fill with black background
  (sdl:fill-surface sdl:*black*)
  
  (let ((str (create-string)))
    
    ;; output intro info
    (format str "TRADER RL~%~%Welcome to Trader RL. You are a trader in the harsh dark world of Arq. The surface has long become inhospitable and all life has moved underground. People live in lonely settlements linked by long twisted tunnels.~%Traders like you connect these dots of civilization, travelling and trading. Your dream is to become memeber of the ruler's court and ascend as the Master Treasurer of the realm. May the gods be with you!~%~%[Press any key to continue]~%")
    
    (sdl:with-default-font ((sdl:initialise-default-font sdl:*font-6x13*))
      (write-text str (sdl:rectangle :x 1 :y 1 :w 800 :h 600)))
    )
  (sdl:update-display))

(defmethod run-window ((win intro-window))
  (tagbody
     (sdl:with-events ()
       (:quit-event () (funcall (quit-func win)) t)
       (:key-down-event (:key key :mod mod :unicode unicode)
                        (setf *current-window* (make-instance 'settlement-window))
                        (make-output *current-window*)
                        (go exit-func))
       (:video-expose-event () (make-output *current-window*)))
    exit-func (make-output *current-window*)))
