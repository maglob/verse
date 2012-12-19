;;;; Math stuff

(in-package :verse)

(deftype vec2d () '(simple-array double-float (2)))

(declaim (inline vec2d vec2ds x y))

(defun x (v)
  "X component ov VEC2D V"
  (declare (type vec2d v))
  (aref v 0))

(defun y (v)
  "Y component ov VEC2D V"
  (declare (type vec2d v))
  (aref v 1))

(defun vec2d (&optional (x 0) (y 0))
  "Create new VEC2D with initial values of X and Y"
  (make-array 2 :element-type 'double-float 
              :initial-contents `(,(coerce x 'double-float) 
                                   ,(coerce y 'double-float))))

(defun vec2d-setf (v x y)
  "Set vector V element values to X and Y"
  (setf (aref v 0) (coerce x 'double-float))
  (setf (aref v 1) (coerce y 'double-float))
  v)

(defun vec2d-length (v)
  "Length of vector V"
  (declare (type vec2d v))
  (sqrt (+  (* (aref v 0) (aref v 0)) 
            (* (aref v 1) (aref v 1)))))

(defun vec2d-unit (v)
  "Unit vector of V"
  (declare (type vec2d v))
  (let ((len (vec2d-length v)))
    (if (zerop len)
	(vec2d 0 0)
	(vec2d-scale v (/ len)))))

(defun vec2d-normal (a b)
  "Normal vector of points A and B"
  (declare (type vec2d a b))
  (let ((dx (- (aref b 0) (aref a 0)))
        (dy (- (aref b 1) (aref a 1))))
    (vec2d-unit (vec2d (- dy) dx))))


(defun vec2d-scale (v x)
  "Scale vector V by X"
  (declare (type vec2d v))
  (vec2d (* (aref v 0) x) (* (aref v 1) x)))

(defun vec2d-translate (v x)
  "Translate vector V by X"
  (declare (type vec2d v))
  (vec2d (+ (aref v 0) x) (+ (aref v 1) x)))

(defun vec2d-incf (a b)
  "Add B into A. Returns modified A."
  (declare (type vec2d a b))
  (incf (aref a 0) (aref b 0))
  (incf (aref a 1) (aref b 1))
  a)

(defun vec2ds (seq)
  "Create new VEC2D with initials values from SEQ"
  (vec2d (elt seq 0) (elt seq 1)))

(defun vec2d-dir (angle &optional (length 1.0d0))
  "Make vector at ANGLE (in radians) with given LENGTH"
  (vec2d  (* (cos angle) length) 
	  (* (sin angle) length)))


(defmacro vec2d-operation (operation a b)
  `(vec2d (,operation (aref ,a 0) (aref ,b 0))
	  (,operation (aref ,a 1) (aref ,b 1))))

(defmacro vec2d-defun (name operation)
  `(defun ,name (a b)
     (declare (type vec2d a b))
     (vec2d-operation ,operation a b)))

(vec2d-defun vec2d-add +)
(vec2d-defun vec2d-sub -)
(vec2d-defun vec2d-mul *)

(defun vec2d-dot (a b)
  "Dot product of VEC2Ds A and B"
  (declare (type vec2d a b))
  (+ (* (x a) (x b))
     (* (y a) (y b))))
         
(defun radian (degree)
  "Convert DEGREE to radians"
  (* (/ degree 180) PI))

(defun degree (radian)
  "Convert RADIAN to degrees"
  (* (/ radian PI) 180))