(in-package :trader-rl)

(defun game-loop ()
  (loop do
    (make-output *current-window*)
    (format t "CUR-WINDOW ~A~%" *current-window*)
    (loop until (<= (action-done *player*) 0) do
      (get-input-player))
    (make-world-turn)
    (incf (action-done *player*))))

(defun save-game ()
  (let ((save-list (list *world* *settlements* *traders* *links* *realms*)))
    (cl-store:store save-list (merge-pathnames "save" *current-dir*))
    ))

(defun load-game ()
  (let ((load-list))
    (setf load-list (cl-store:restore (merge-pathnames "save" *current-dir*)))
    (setf *world* (nth 0 load-list))
    (setf *settlements* (nth 1 load-list))
    (setf *traders* (nth 2 load-list))
    (setf *links* (nth 3 load-list))
    (setf *realms* (nth 4 load-list))
    (setf *player* (gethash 0 *traders*))))

(defun init-game ()
  (setf *settlements* (make-hash-table))
  (setf *traders* (make-hash-table))
  (setf *links* (make-hash-table))
  (setf *realms* (make-hash-table))
  (setf *world* (make-instance 'world))
  )

(defun start-new-game ()
  (generate-world *settlements* *traders*)
  (dotimes (n 10)
    (make-world-turn)))

(defun trader-rl-main () 
  (setf *random-state* (make-random-state t))

  (sdl:with-init ()
    (setf *window-width* 800 *window-height* 600)
    (sdl:window *window-width* *window-height*
		
		:title-caption "Trader RL"
		:icon-caption "Trader RL")
    (sdl:enable-key-repeat nil nil)
    (sdl:enable-unicode)
    (setf *save-on-quit* t)
    
    (tagbody
       (setf *quit-func* #'(lambda () (go exit-tag)))
       (init-game)

       ;; load game if save file available
       (let ((load-type ':load))
         (if (probe-file (merge-pathnames "save" *current-dir*))
           (handler-case (load-game)
             (t ()
               (format t "~%LOAD GAME FAILED~%NEW GAME CREATED INSTEAD~%~%")
               (start-new-game)
               (setf load-type ':error)))
           (progn
             (start-new-game)
             (setf load-type ':new)))
       
         (setf *current-window* (make-instance 'intro-window :win-type load-type))
         (make-output *current-window*)
         (run-window *current-window*))
       ;; the game loop
       
       (game-loop)
       exit-tag (cond
                  ((eq *save-on-quit* :delete) (when (probe-file (merge-pathnames "save" *current-dir*)) 
                                                 (delete-file (merge-pathnames "save" *current-dir*))))
                  ((eq *save-on-quit* t) (save-game))
                  ((eq *save-on-quit* nil) nil)))))

;    (sdl:update-display)

 ;   (sdl:with-events ()
  ;    (:quit-event () t))
    
    ;(format t "path = ~A~%" (make-pathname :directory (pathname-directory #.(or *compile-file-truename* *load-truename*))))
    ;
    ;(setf *glyph-front* (sdl:load-image (sdl:create-path "data/font.bmp" (make-pathname :host (pathname-host #.(or *compile-file-truename* *load-truename*)) 
;											:directory (pathname-directory #.(or *compile-file-truename* *load-truename*)))) 
;					:color-key sdl:*white*))
 ;   (setf *glyph-temp* (sdl:create-surface +glyph-w+ +glyph-h+ :color-key sdl:*black*))
  ;  (setf *back-tile* (sdl:load-image (sdl:create-path "data/back.bmp" (make-pathname :host (pathname-host #.(or *compile-file-truename* *load-truename*)) 
;										      :directory (pathname-directory #.(or *compile-file-truename* *load-truename*)))) 
;				      :color-key sdl:*white*)) 

    ;(tagbody
     ;  (setf *quit-func* #'(lambda () (go exit-tag)))
      ; (init-game)
       ;(setf *current-window* (make-instance 'cell-window))
       ;(make-output *current-window*)
       ;; the game loop
       ;(game-loop)
       ;exit-tag nil)))

#+clisp
(defun trader-rl-exec ()
  (cffi:define-foreign-library sdl
    (t (:default "libSDL")))
  
  (cffi:use-foreign-library sdl)

  (setf *current-dir* *default-pathname-defaults*)
  
  (sdl:with-init ()  
    (trader-rl-main))
  (ext:quit))

#+sbcl
(defun trader-rl-exec ()
  (cffi:define-foreign-library sdl
    (:darwin (:or (:framework "SDL")
                 (:default "libSDL")))
    (:windows "SDL.dll")
    (:unix (:or "libSDL-1.2.so.0.7.2"
 	      "libSDL-1.2.so.0"
 	      "libSDL-1.2.so"
 	      "libSDL.so"
 	      "libSDL")))

  (cffi:use-foreign-library sdl)

  (setf *current-dir* *default-pathname-defaults*)
  
  (sdl:with-init ()  
    (trader-rl-main))
  (sb-ext:exit))

#+clisp
(defun make-exec ()
  (ext:saveinitmem "trader-rl" 
                   :quiet t :script t :init-function #'trader-rl-exec  
		   :executable t))

#+sbcl
(defun make-exec ()
  (sb-ext:save-lisp-and-die "trader-rl" :toplevel #'trader-rl-exec :executable t :application-type :gui))
