;;;; Model stuff

(in-package :verse)

(defvar *sprite-id-sequence* "Sequence for sprite ids")
(defvar *old-states* "List of old states")

(defstruct edge
  (p0 (vec2d) :type vec2d)
  (p1 (vec2d) :type vec2d)
  (unit (vec2d) :type vec2d)
  (length 0d0 :type double-float)
  (normal (vec2d) :type vec2d))

(defstruct mesh
  "Line mesh described as vertices and lines"
  vertices
  lines
  edges)

(deftype collision-type () '(member :none :vertex-edge :vertex-vertex :ball-ball))

(defstruct collision
  "Collision between A and B"
  (type :none :type collision-type)
  a
  b
  distance)

(deftype sprite-type () '(member :none :particle :ship :ball :tile))

(defstruct sprite
  "Moving object"
  (id (incf *sprite-id-sequence*) :type fixnum)
  (pos (vec2d) :type vec2d)
  (v (vec2d) :type vec2d)
  (dir 0.0d0 :type double-float)
  (force (vec2d) :type vec2d)
  (mass 0d0 :type double-float)
  mesh
  agent
  (label "" :type string)
  (type :none :type sprite-type)
  (old-pos (vec2d) :type vec2d)
  (old-v (vec2d) :type vec2d)
  (radius 0d0 :type double-float))

(defstruct state
  time  
  dt
  (frame-number 0)
  sprites
  (gravity (vec2d) :type vec2d)
  collisions
  cave)

(defun check-collision-vertex-vertex (a b &key (epsilon 1.0d0))
  "Return true if A and B are colliding within margin of EPSILON"
  (declare (type vec2d a b)
           (type double-float epsilon))
  (let ((dx (- (x b) (x a)))
        (dy (- (y b) (y a))))
    (<= (+ (* dx dx) (* dy dy)) 
        (* epsilon epsilon))))

(defun check-collision-vertex-edge (vertex v edge &key (epsilon 1.0d0) (penetration-depth -16d0)(radius 0d0))
  "Return true if VERTEX with speed vector V collides with EDGE within margin of epsilon"
  ;; Distance
  (let ((distance (vec2d-dot (vec2d-sub vertex (edge-p0 edge))
                                   (edge-normal edge))))
    (when (< penetration-depth (- distance radius) epsilon)
      ;; Projision on edge?
      (let ((cos (vec2d-dot (vec2d-sub vertex (edge-p0 edge))
                            (edge-unit edge))))
        (when (and (>= cos (- epsilon))
                   (<= cos (+ (edge-length edge) epsilon)))
          ;; Direction
          (when (< (vec2d-dot v (edge-normal edge)) 0)

            (- distance radius)))))))

(defun check-collision-ball-ball (a b)
  "Check collisions between ball sprites A and B"
  (let ((distance (vec2d-length (vec2d-sub (sprite-pos b) (sprite-pos a)))))
    (when (< distance (+ (sprite-radius a) (sprite-radius b)))
      (when t ;(< (vec2d-dot (sprite-v a) (sprite-v b)) 0)
        distance))))

(defun bezier (p0 p1 p2 tt)
  "Point at TT (0..1) on Bezier curve from P0 to P2 with control point P1"
  (declare (type vec2d p0 p1 p2)) 
  (vec2d-add
   (vec2d-scale (vec2d-add (vec2d-scale p0 (- 1 tt))
                           (vec2d-scale p1 tt))
                (- 1 tt))
   (vec2d-scale (vec2d-add (vec2d-scale p1 (- 1 tt))
                           (vec2d-scale p2 tt))
                tt)))

(defun load-ttf (path)
  "Load TTF fontfile PATH"
  (zpb-ttf:with-font-loader (fl path)
    (let ((glyph (zpb-ttf:find-glyph #\a fl))
          res)
      (zpb-ttf:do-contours (contour glyph)
        (flet ((v2 (p)
                 (vec2d (zpb-ttf:x p) (zpb-ttf:y p))))
          (let ((vertices))
            (zpb-ttf:do-contour-segments (p0 p1 p2) contour 
              (if p1
                  (dotimes (i (1+ 4))
                    (push (bezier (v2 p0) (v2 p1) (v2 p2) (/ i 4)) vertices))
                  (progn 
                    (push (v2 p0) vertices)
                    (push (v2 p2) vertices))))
            (push (create-mesh-lines (nreverse vertices) :loop t) res))))
      res)))

(defun model-init ()
  (setf *sprite-id-sequence* 0
	*old-states* nil))

(defun add-sprite (sprite state)
  (push sprite (state-sprites state))
  sprite)

(defun get-sprite (id state)
  "Get SPRITE with given ID from STATE"
  (find id (get-sprites state) :key (lambda (x) (sprite-id x))))

(defun get-sprites (state)
  (state-sprites state))

(defun create-edge (a b)
  "Create EDGE between VEC2D points A and B"
  (declare (type vec2d a b))
  (let ((ab (vec2d-sub b a)))
    (make-edge :p0 (vec2ds a) :p1 (vec2ds b)
	       :unit (vec2d-unit ab)
	       :length (vec2d-length ab)
	       :normal (vec2d-normal a b))))

(defun create-mesh (vertices lines)
  (let ((edges))
    (dolist (l lines)
      (let ((a (vec2ds (elt vertices (elt l 0))))
	    (b (vec2ds (elt vertices (elt l 1)))))
	(push (create-edge a b) edges)))
    (make-mesh :vertices vertices :lines lines :edges (nreverse edges))))

(defun create-mesh-lines (vertices &key loop)
  "Create simple lines mesh straight from VERTICES with optional LOOP"
  (let ((n (length vertices))
	(lines))
    (dotimes (i (1- n))
      (push (list i (1+ i)) lines))
    (when loop
      (push (list (1- n) 0) lines))
    (create-mesh vertices (nreverse lines))))

(defun create-mesh-rectangle (width &optional (height width))
  "Create rectangle MESH with given WIDTH and HEIGHT"
  (create-mesh-lines `((,(/ width 2) ,(/ height 2)) 
		       (,(/ width 2) ,(/ height -2))
		       (,(/ width -2) ,(/ height -2)) 
		       (,(/ width -2) ,(/ height 2)))
		     :loop t))

(defun create-mesh-circle (radius steps)
  "Create circle MESH with given RADIUS and STEPS used for aproximation"
  (let ((rstep (/ (* 2 PI) steps)) 
        vertices)
    (when (> steps 2)
      (dotimes (n steps)
        (push (list (* (cos (* n rstep)) radius)
                    (* (sin (* n rstep)) radius))
              vertices))
      (create-mesh-lines vertices :loop t))))

(defun reflection-vector (v normal &optional (cor 1.0))
  "Reflection vector for V on line defined by NORMAL with optional CoefficentOfRestitution . R=V-(1+cor)*N*(V.N)"
  (vec2d-sub v (vec2d-scale normal
			    (* (+ 1 cor) 
			       (vec2d-dot v normal)))))

(defun deep-copy-state (state)
  "Copy STATE with deep copy of sprites"
  (let ((new (copy-state state)))
    (setf (state-sprites new) (mapcar #'copy-sprite (state-sprites state)))
    new) )

(defun update-state (state dt)
  "Update game STATE by DT seconds"
  ;; Meta info
  (setf (state-dt state) dt)
  (setf (state-time state) (glfw:get-time))
  (incf (state-frame-number state))
  (setf (state-collisions state) nil)
  ;; Reset old positions
  (dolist (s (get-sprites state))
    (setf (sprite-old-pos s) (vec2ds (sprite-pos s))
	  (sprite-old-v s) (vec2ds (sprite-v s))))
  ;; Collect forces
  (dolist (s (get-sprites state))
    (setf (sprite-force s) (vec2d 0 0))
    (when (> (sprite-mass s) 0)
      (vec2d-incf (sprite-force s) (vec2d-scale (state-gravity state) (sprite-mass s))))
    (when (sprite-agent s)
      (funcall (sprite-agent s) s)))
  ;; Apply forces & integrate
  (dolist (s (get-sprites state))
    (when (> (sprite-mass s) 0)
      (let ((impulse (vec2d-scale (sprite-force s) dt)))
	(setf (sprite-v s) (vec2d-add (sprite-v s) 
				      (vec2d-scale impulse (/ (sprite-mass s)))))
	(setf (sprite-pos s) (vec2d-add (sprite-pos s) 
					(vec2d-scale (sprite-v s) dt))))))
  ;; Collisionn detection
  (dolist (s (get-sprites state))
    (dolist (mesh (state-cave state))
      (dolist (edge (mesh-edges mesh))
        (case (sprite-type s)
          ((:particle)
           (let ((d (check-collision-vertex-edge (sprite-pos s) (sprite-v s) edge)))
             (when d
               (push (make-collision :type :vertex-edge :a s :b edge :distance d)
                     (state-collisions state)))))
          ((:ball :ship)
           (let ((d (check-collision-vertex-edge (sprite-pos s) (sprite-v s) edge :radius (sprite-radius s))))
             (when d
               (push (make-collision :type :vertex-edge :a s :b edge :distance d)
                     (state-collisions state))))))))
    (when (eq (sprite-type s) :ball)
      (dolist (s2 (nthcdr (1+ (position s (get-sprites state))) (get-sprites state)))
        (when (eq (sprite-type s2) :ball)
          (let ((d (check-collision-ball-ball s s2)))
            (when d
              (push (make-collision :type :ball-ball :a s :b s2 :distance d)
                    (state-collisions state))))))))
  ;; Collision resolution
  (dolist (col (state-collisions state))
    (let ((sprite (collision-a col))
          (b (collision-b col))
          (distance (collision-distance col)))
      (case (collision-type col)
	(:vertex-edge
	 (setf (sprite-v sprite)
	       (reflection-vector (sprite-v sprite) (edge-normal b) 0.5d0))
         (when (< distance 0)
           (setf (sprite-pos sprite)
                 (vec2d-add (sprite-pos sprite) (vec2d-scale (edge-normal b) (- distance))))))
        (:ball-ball2
         (let* ((normal (Vec2d-unit (vec2d-sub (sprite-pos sprite) (sprite-pos b))))
                (impulse (reflection-vector (vec2d-sub (sprite-v sprite) (sprite-v b))
                                            normal 0d0))
                (d (- (+ (sprite-radius sprite) (sprite-radius b)) distance)))
           (setf (sprite-v sprite)
                 (vec2d-sub (sprite-v sprite) 
                            (vec2d-scale impulse 
                                         (/ (sprite-mass b) 
                                            (+ (sprite-mass sprite) (sprite-mass b))))))
           (setf (sprite-v b)
                 (vec2d-add (sprite-v b) 
                            (vec2d-scale impulse 
                                         (/ (sprite-mass sprite) 
                                            (+ (sprite-mass sprite) (sprite-mass b))))))
           (setf (sprite-pos sprite)
                 (vec2d-add (sprite-pos sprite) 
                            (vec2d-scale normal (/ d 2))))
           (setf (sprite-pos b)
                 (vec2d-add (sprite-pos b) 
                            (vec2d-scale normal (/ d -2))))
           ))
	(:vertex-vertex ))))
  (push (deep-copy-state state) *old-states*))
