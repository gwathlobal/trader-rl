(in-package :trader-rl)

(defconstant +journal-window-tab-journal+ 0)
(defconstant +journal-window-tab-quests+ 1)

(defclass journal-window (window)
  ((journal :initarg :journal :accessor journal)
   (quests :initform nil :initarg :quests :accessor quests)
   (cur-sel :initform 0 :accessor cur-sel)
   (cur-tab :initform +journal-window-tab-journal+ :accessor cur-tab)))



(defmethod generate-win-actions ((win journal-window))
  (setf (win-actions win) nil)
  )
  

(defmethod make-output ((win journal-window))
  ;; fill with black background
  (sdl:fill-surface sdl:*black*)
  
  (sdl:with-default-font ((sdl:initialise-default-font sdl:*font-6x13*))
    (cond
      ((= (cur-tab win) +journal-window-tab-journal+)
       (write-text (format nil "Your journal:~%") (sdl:rectangle :x 0 :y 0 :w 800 :h 20)))
      ((= (cur-tab win) +journal-window-tab-quests+)
       (write-text (format nil "Your quests:~%") (sdl:rectangle :x 0 :y 0 :w 800 :h 20)))
      )
    )
  
  (setf (cur-sel win) (adjust-selection-list (cur-sel win) (length (journal win))))
  
  
  (let ((cur-str) (lst (make-list 0)) (color-list (make-list 0)))
    (setf cur-str (cur-sel win))
    (cond
      ((= (cur-tab win) +journal-window-tab-journal+)
       (dotimes (i (length (journal win)))
         
         (setf lst (append lst (list (show-journal-entry (get-n-journal-entry (journal win) i)))))
         
         (if (= i cur-str)  
           (setf color-list (append color-list (list sdl:*yellow*)))
           (setf color-list (append color-list (list sdl:*white*)))))
         )
      ((= (cur-tab win) +journal-window-tab-quests+)
       (dotimes (i (length (quests win)))
         
         (setf lst (append lst (list (nth i (quests win)))))
         
         (if (= i cur-str)  
           (setf color-list (append color-list (list sdl:*yellow*)))
           (setf color-list (append color-list (list sdl:*white*)))))
       )
      )
    
    (draw-multiline-selection-list lst cur-str 0 13 800 (* 13 30) color-list)
    )
  
  (sdl:with-default-font ((sdl:initialise-default-font sdl:*font-6x13*))
    (cond
      ((and (= (cur-tab win) +journal-window-tab-journal+) (quests win))
       (write-text (format nil "[Tab] View quests [Esc] Exit") (sdl:rectangle :x 0 :y (* 13 30) :w 800 :h 20)))
      ((= (cur-tab win) +journal-window-tab-journal+)
       (write-text (format nil "[Esc] Exit") (sdl:rectangle :x 0 :y (* 13 30) :w 800 :h 20)))
      ((= (cur-tab win) +journal-window-tab-quests+)
       (write-text (format nil "[Tab] View journal [Esc] Exit") (sdl:rectangle :x 0 :y (* 13 30) :w 800 :h 20)))
      ))
  
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
                          ((sdl:key= key :sdl-key-tab) 
                           (when (quests win)
                             (setf (cur-sel win) 0)
                             (if (= (cur-tab win) +journal-window-tab-journal+)
                               (setf (cur-tab win) +journal-window-tab-quests+)
                               (setf (cur-tab win) +journal-window-tab-journal+))))
                          )
			(make-output *current-window*)
       			(go exit-func)
			)
       (:video-expose-event () (make-output *current-window*)))
     exit-func (make-output *current-window*)))
