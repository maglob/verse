;;;; verse client

;;; hack: (push #P "/opt/local/lib/" cffi:*foreign-library-directories*)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :verse)
    (defpackage :verse (:use :cl)))
  (let ((base (merge-pathnames *default-pathname-defaults* (user-homedir-pathname))))
    (dolist (s '(:cl-glfw :cl-glfw-opengl-version_2_1 :zpb-ttf))
      (unless (find-package s)
        (ql:quickload s)))
    (dolist (s '("math.lisp" "model.lisp" "gfx.lisp"))
      (load (merge-pathnames s base)))))

(in-package :verse)

(defparameter +FPS+ 60)
(defparameter +thrust-power+ 2400)

(defparameter *mesh-ship* (create-mesh-lines '((30 0) (-10 -15) (-5 -7) (-5 7) (-10 15)) :loop t))
(defparameter *mesh-tile* (create-mesh-rectangle 20))
(defparameter *mesh-ball* (create-mesh-circle 16 12))
(defparameter *mesh-cave* (create-mesh-lines 
			   '((-700 400) (-600 150) (-500 200) (-400 100) (-300 -50) (-200 0) (-100 -100)
			     (0 0) (100 0) (200 -50) (300 100) (400 50) (500 200) (600 250) (700 400))))

(defparameter *meshes-a* (load-ttf "~/Dropbox/prg/verse/FreeMono.ttf"))
;(defparameter *meshes-a* (load-ttf "~/Dropbox/prg/verse/atari_full.ttf"))

(defstruct controls 
  turn-left
  turn-right
  thrust
  fire
  left
  right
  up
  down
  exit)

(defstruct view
  (pos (vec2d) :type vec2d)
  sprite-id)

(defun key-down (key)
  (= (glfw:get-key key) glfw:+press+))

(defun controls-status ()
  "Current status of CONTROLS keys"
  (make-controls :turn-left (key-down (char-int #\A))
                 :turn-right (key-down (char-int #\D))
                 :thrust (key-down glfw:+key-rshift+)
                 :fire (key-down glfw:+key-enter+)
                 :left (key-down glfw:+key-left+)
                 :right (key-down glfw:+key-right+)
                 :up (key-down glfw:+key-up+)
                 :down (key-down glfw:+key-down+)
                 :exit (key-down glfw:+key-esc+)))

(defun update (state dt)
  "Update game world STATE by DT seconds"
  (update-state state dt))

(defun update-player (sprite)
  "Agent function used for controlling player's ship SPRITE"
  (let ((keys (controls-status)))
    (when (controls-fire keys)
      (print "fire")
      (finish-output))
    (when (controls-thrust keys)
      (vec2d-incf (sprite-force sprite) 
		  (vec2d-scale (vec2d-dir (sprite-dir sprite)) 
			       +thrust-power+)))
    (when (controls-turn-left keys)
      (incf (sprite-dir sprite) 0.05))
    (when (controls-turn-right keys)
      (decf (sprite-dir sprite) 0.05))))

(defun game-init ()
  "Init the game model and return STATE"
  (let ((state (make-state :gravity (vec2d 0 -40) 
			   :cave (append (list  *mesh-cave*) *meshes-a*))))
    (model-init)
    (add-sprite (make-sprite :type :ship 
                             :mesh *mesh-ship* 
                             :mass 20d0 
                             :radius 12d0
                             :dir (radian 90) 
                             :agent #'update-player 
                             :label "Ship" 
                             :pos (vec2d 0 50)) 
                state)
    (dotimes (i 20)
      (add-sprite (make-sprite :type :particle :mass 1d0 :pos (vec2d (* 20 (- 10 i)) 200)) state)
      (add-sprite (make-sprite :type :tile :mesh *mesh-tile* :pos (vec2d (* 100 (- 10 i)) 100)) state)
      (add-sprite (make-sprite :type :ball 
                               :mass 10d0 
                               :radius 16d0
                               :mesh *mesh-ball* 
                               :pos (vec2d 100 (* 100 (- 10 i)))) 
                  state))
    state))

(defun game (&key (width 1024) (height 768) (fullscreen nil) (game-init t) (init-state nil))
  "Start and run the game"
  (let ((dt (/ 1.0d0 +FPS+))
        (is-paused)
	(state init-state)
	(playback-frame-delta 0)
        (view (make-view)))
    (when game-init
      (setf state (game-init)))
    (setf (view-sprite-id view) 1)
    (gfx-init)
    (glfw:do-window (:width width :height height 
                     :title "Verse client v0.04"
                     :mode (if fullscreen glfw:+fullscreen+ glfw:+window+))
        ((format t "GL version: ~A~%" (glfw:get-gl-version))
	 (format t "GL video modes: ~A~%" (glfw:get-video-modes 100))
	 (format t "VBO supported: ~A~%" (glfw:extension-supported "GL_ARB_vertex_buffer_object"))
	 (finish-output)
	 (glfw:swap-interval 1)
	 (gl:clear-color 0 0 0 0)
	 (gl:matrix-mode gl:+PROJECTION+)
	 (gl:load-identity)
	 (gl:ortho (- (/ width 2))  (/ width 2)
		   (- (/ height 2)) (/ height 2)
		   -1 1))
      (let ((start-time (glfw:get-time)))
        (if is-paused
	    (draw (elt *old-states* playback-frame-delta) view width height)
	    (progn
	      (update state dt)
	      (draw state view width height)))
        (cond 
	  ((key-down glfw:+key-f1+) 
	   (setf state (game-init)))
	  ((key-down glfw:+key-f2+)
	   (glfw:close-window)
	   (game :width width :height height :fullscreen (not fullscreen) :game-init nil :init-state state))
          ((key-down glfw:+key-esc+)
           (glfw:close-window))
	  ((key-down (char-int #\1))
	   (when (> playback-frame-delta 0)
	     (decf playback-frame-delta)))
	  ((key-down (char-int #\2))
	   (when (< playback-frame-delta (1- (length *old-states*)))
	     (incf playback-frame-delta)))
          ((key-down (char-int #\P))
           (setf is-paused (not is-paused))
	   (setf playback-frame-delta 0)
           (glfw:sleep 0.2d0))
          ((key-down glfw:+key-left+) 
           (decf (aref (view-pos view) 0)))
          ((key-down glfw:+key-right+) 
           (incf (aref (view-pos view) 0)))
          ((key-down glfw:+key-up+) 
           (incf (aref (view-pos view) 1)))
          ((key-down glfw:+key-down+) 
           (decf (aref (view-pos view) 1)))
	  ((> (glfw:get-time) 60)
	   (glfw:close-window)))
        (let ((time-left (- dt (- (glfw:get-time) start-time))))
          (when (> time-left 0.005)
            (glfw:sleep (- time-left 0.005))))))))

(defun draw-status-line (state width height)
  "Draw status line on screen"
  (let ((sx (- (/ width 2)))
        (sy (- (/ height 2) 10))
        (text (format nil "Frame: ~A  Time: ~,2F  DT: ~,4F  Sprites: ~A" 
                      (state-frame-number state)
                      (state-time state)
                      (state-dt state)
                      (length (state-sprites state)))))
    (gl:color-3f 0.6 0.6 0.9)
    (gl:raster-pos-2f sx sy)
    ;; Render sting text
    ;(ftgl:render-font *font* text :front)
    ))

(defun draw (state view width height)
  "Draw world STATE into VIEW"
  (gl:color-3f 1 1 1)
  (gl:clear gl:+COLOR-BUFFER-BIT+)
  (gl:line-width 2)
  (gl:enable gl:+line-smooth+)
  (gl:blend-func gl:+src-alpha+  gl:+one-minus-src-alpha+)
  (gl:enable GL:+BLEND+)
  (let ((pos (if (view-sprite-id view)
		 (sprite-pos (get-sprite (view-sprite-id view) state))
		 (view-pos view))))
    ;; Sprites
    (dolist (s (get-sprites state))
      (let ((rel (vec2d-sub (sprite-pos s) pos)))
	(gl:with-push-matrix
	  (gl:translate-d (aref rel 0) (aref rel 1) 0)
          (case (sprite-type s)
            (:particle 
	     (gl:color-3f .2 .2 1)
             (gl:point-size 4)
             (gl:with-begin gl:+points+
               (gl:vertex-2d 0d0 0d0)))
            (t
	     (gl:color-3f 1 1 1)
             (gl:rotate-d (degree (sprite-dir s)) 0 0 1)
             (draw-mesh (sprite-mesh s)))))))
    (gl:with-push-matrix
      (gl:translate-d (- (aref pos 0)) (- (aref pos 1)) 0)
      (draw-mesh *mesh-cave*))
    ;; Extra stuff
    (gl:with-push-matrix
      (gl:translate-d (- (aref pos 0)) (- (aref pos 1)) 0)
      (draw-mesh (elt *meshes-a* 0))
      (draw-mesh (elt *meshes-a* 1)))
    ;; Hightlight collisions
    (dolist (col (state-collisions state))
      (case (collision-type col)
        (:vertex-edge          
         (gl:with-push-matrix
           (gl:line-width 4)
           (gl:color-3f 1 0 0)
           (gl:translate-d (- (x pos)) (- (y pos)) 0)
           (draw-edge (collision-b col)))))))
  ;; Raster layer
  (draw-status-line state width height)
  )

