(in-package :trader-rl)

(defparameter *quit-func* nil)
(defparameter *current-window* nil)
(defparameter *window-width* 800)
(defparameter *window-height* 600)

(defclass window ()
  ((return-to :initarg :return-to :initform *current-window* :accessor return-to)
   (quit-func :initform *quit-func* :accessor quit-func)
   (win-actions :initform nil :accessor win-actions) ;; type - ((<action name> <action func>) ...)
   ))

(defmethod initialize-instance :after ((win window) &key)
  (generate-win-actions win))

(defgeneric make-output (win))

(defgeneric get-input (win key mod unicode))

(defgeneric run-window (win))

(defgeneric generate-win-actions (win))

(defun read-word (txt start-pos)
  (let ((txt-length (length txt)) (was-letter nil) (cur-pos) (word-length) (eol nil))
    (loop named outer finally (setf cur-pos read-pos word-length (- read-pos start-pos))
       for read-pos from start-pos to (1- txt-length) do
	 (cond
	   ((eql (char-code (aref txt read-pos)) (char-code #\Space)) (when (eql was-letter t) (setf cur-pos (incf read-pos) word-length (- read-pos start-pos)) (return-from outer nil)))  
	   ((eql (char-code (aref txt read-pos)) 13)
	    (setf eol t cur-pos read-pos word-length (- read-pos start-pos)) 
	    (when (and (< read-pos (1- txt-length)) (eql (char-code (aref txt (1+ read-pos))) 10))
	      (incf cur-pos))
	    (return-from outer nil))
	   ((eql (char-code (aref txt read-pos)) 10) (setf eol t cur-pos (1+ read-pos) word-length (- read-pos start-pos)) (return-from outer nil))
	   (t (setf was-letter t))))
    (values cur-pos word-length eol)))

(defun write-text (txt rect &key (surface sdl:*default-surface*) (justify :left) (color sdl:*white*) (font sdl:*default-font*) (count-only nil) (start-line 0))
  (let ((txt-length (length txt)) (read-pos 0) (x (sdl:x rect)) (y (sdl:y rect)) (eol) (word-length) (row-length-in-pixels 0) (prev-pos) (cur-pos) (read-pos2) (cur-line 0))
    (loop until (or (>= read-pos txt-length) (>= y (+ (sdl:y rect) (sdl:height rect)))) do
	 (setf prev-pos read-pos)
	 (setf cur-pos prev-pos)
	 (multiple-value-setq (read-pos word-length eol) (read-word txt read-pos))
	 (setf row-length-in-pixels (* word-length (sdl:char-width font)))
	 (incf cur-pos word-length)
	 (loop until (or (>= read-pos txt-length) (>= x (+ (sdl:x rect) (sdl:width rect)))) do
	      (when (eql eol t) (loop-finish))
	      (multiple-value-setq (read-pos2 word-length eol) (read-word txt read-pos))
	      (if (< (+ row-length-in-pixels (* word-length (sdl:char-width font))) (sdl:width rect))
		(progn (incf row-length-in-pixels (* word-length (sdl:char-width font))) (setf read-pos read-pos2))
		(progn (loop-finish)))
	      (incf cur-pos word-length))
	 (cond
	   ((eql justify ':left) (setf x (sdl:x rect)))
	   ((eql justify ':center) (setf x (truncate (+ (sdl:x rect) (sdl:width rect)) 2)))
	   ((eql justify ':right) (setf x (+ (sdl:x rect) (sdl:width rect)))))
	 (when (and (eql count-only nil) (>= cur-line start-line))
	   (sdl:draw-string-solid-* (subseq txt prev-pos cur-pos) x y :justify justify :surface surface :font font :color color)
	   (incf y (sdl:char-height font)))
	 (incf cur-line))
    ;; returns the number of lines drawn
    cur-line))

(defun create-string (&optional str)
  (let ((new-str (make-array (list 0) :element-type 'character :adjustable t :fill-pointer t)))
    (when str
      (format new-str str))
    new-str))

(defun get-input-player ()
  
  (run-window *current-window*))

(defun get-text-input (str key mod unicode)
  (when (> unicode 0)
    (vector-push-extend (code-char unicode) str))
  str)

(defvar *sel-y-offset* 0)
(defvar *sel-x-offset* 4)

(defun draw-selection-list (str-list cur-str str-per-page x y &optional (color-list ()))
  (declare (type list color-list str-list))
  (let* ((font (sdl:initialise-default-font sdl:*font-6x13*)) (color) (str)
	 (list-start (* (truncate cur-str str-per-page) str-per-page))
	 (list-end (if (> (+ list-start str-per-page) (length str-list)) (length str-list) (+ list-start str-per-page))))
    ;; from the start of the current page (determined previously) to the end of the page (or end of list whichever is less)
    (dotimes (i (- list-end list-start))
      (setf str (nth (+ i list-start) str-list))
      ;; highlight the current selected item
      (if (eql color-list nil)
	  (setf color sdl:*white*)
	  (setf color (nth (+ i list-start) color-list)))
      (sdl:draw-string-solid-* str (+ x (sdl:char-width font) *sel-x-offset*) (+ y (* i (+ (sdl:char-height font) *sel-y-offset*))) :font font :color color))
    ;; draw a scroll bar when necessary
    (when (> (length str-list) str-per-page)
      (sdl:draw-string-solid-* "*" x (+ y (* (+ (sdl:char-height font) *sel-y-offset*) (truncate (* (/ cur-str (length str-list)) str-per-page)))) :font font :color sdl:*white*))))

(defun draw-multiline-selection-list (item-list cur-item x y w h &optional (color-list ()))
  (sdl:with-rectangle (rect (sdl:rectangle :x x :y y :w (- w 12) :h h))  
    ;;(format t "~%")
    (let ((screen-list ()) (start-item) (is-more-than-one-screen nil))
      ;; assign numbers of screens to the items pertaining to them
      (let ((screen-i 0) (item-h) (is-first t))
	(sdl:initialise-default-font sdl:*font-6x13*)
	(dotimes (i (length item-list))
	  (setf item-h (* 13 (write-text (nth i item-list) rect :count-only t)))
	  
          ;;(format t "BEFORE I ~A, ITEM-H ~A, RECT HEIGHT ~A, SCREEN-I ~A~%" i item-h (sdl:height rect) screen-i)
          
	  (if (or (> (sdl:height rect) item-h) (and is-first (<= (sdl:height rect) item-h)))
	      (progn
		(setf screen-list (append screen-list (list screen-i)))
		(setf is-first nil)
		(if (> (sdl:height rect) item-h)
		    (progn
		      (incf (sdl:y rect) item-h)
		      (decf (sdl:height rect) item-h))
		    (progn 
		      (incf screen-i)
		      (setf is-more-than-one-screen t))))
	      (progn
		(incf screen-i)
		(setf is-more-than-one-screen t)
		(setf screen-list (append screen-list (list screen-i)))
		(setf (sdl:y rect) y)
		(setf (sdl:height rect) h)
                (incf (sdl:y rect) item-h)
                (decf (sdl:height rect) item-h)
		(setf is-first t)))
          
          ;;(format t "AFTER I ~A, ITEM-H ~A, RECT HEIGHT ~A, MAX H ~A, SCREEN-I ~A~%" i item-h (sdl:height rect) h screen-i)
	  ))
      
      ;; find the screen by current item
      (dotimes (i (length item-list))
	(when (= (nth i screen-list) (nth cur-item screen-list))
	  (setf start-item i)
	  (return)))
      
      ;; draw the screen found
      (let ((str) (item-h) (color))
	(sdl:initialise-default-font sdl:*font-6x13*)
	(setf (sdl:y rect) y)
	(setf (sdl:height rect) h)
	;; yes, ugly hack but I was being lazy and did not want to investigate how to create a 'while' statement
	;; iterate through all items until we get to the starting one
	(dotimes (i (- (length item-list) start-item))
	  (if (= (nth (+ i start-item) screen-list) (nth cur-item screen-list))
	      (progn
		(setf str (nth (+ i start-item) item-list))
		;; highlight the current selected item
		(if (eql color-list nil)
		    (setf color sdl:*white*)
		    (setf color (nth (+ i start-item) color-list)))
		(setf item-h (* 13 (write-text str rect :color color :count-only nil)))
		(when (> (sdl:height rect) item-h)
		  (incf (sdl:y rect) item-h)
		  (decf (sdl:height rect) item-h)))
	      (progn
		(return)))))
      ;; draw scroll bar when necessary
      (when is-more-than-one-screen
	(sdl:draw-string-solid-* "*" (- (+ x w) 12) (+ y (truncate (* h (/ cur-item (length item-list))))) :color sdl:*white*)))))

(defun run-selection-list (key mod unicode cur-str)
  (declare (ignore unicode))
  (cond
    ((and (sdl:key= key :sdl-key-up) (= mod 0)) (decf cur-str))
    ((and (sdl:key= key :sdl-key-down) (= mod 0)) (incf cur-str))
    ((and (sdl:key= key :sdl-key-up) (/= (logand mod sdl-cffi::sdl-key-mod-shift) 0)) (decf cur-str 10))
    ((and (sdl:key= key :sdl-key-down) (/= (logand mod sdl-cffi::sdl-key-mod-shift) 0)) (incf cur-str 10))
    ((and (sdl:key= key :sdl-key-pagedown) (= mod 0)) (incf cur-str 10))
    ((and (sdl:key= key :sdl-key-pageup) (= mod 0)) (decf cur-str 10)))
  cur-str)

(defun adjust-selection-list (cur-str max-str)
  (when (< cur-str 0) (setf cur-str 0))
  (when (>= cur-str max-str) (setf cur-str (- max-str 1)))
  cur-str)
