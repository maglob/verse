;;;; Graphics stuff. Hide OpenGL or other implemententaion level stuff.

(in-package :verse)

(defparameter *use-vob* t)

(defstruct gl-mesh
  "OpenGL spesific data for mesh"
  vertices
  lines)

(defvar +ht-gl-mesh+)

(defun gfx-init ()
  (setf +ht-gl-mesh+ (make-hash-table)))

(defun vertex-array (seq)
  "Crete one dimensional vertex array from SEQ"
  (map 'vector (lambda (x) (coerce x 'double-float)) 
       (apply #'append (map 'list (lambda (x) (coerce x 'list)) seq))))

(defun index-array (seq)
  "Crete one dimensional index array from SEQ"
  (map 'vector (lambda (x) (coerce x '(unsigned-byte 32)))
       (apply #'append (map 'list (lambda (x) (coerce x 'list)) seq))))

(defun create-gl-mesh (mesh) 
  "Create GL-MESH with GL spesific data structures for given MESH"
  (let* ((vbos (make-array 2))
         (vertices (vertex-array (mesh-vertices mesh)))
         (lines (index-array (mesh-lines mesh)))
         (vertices-fi (cffi:foreign-alloc 'gl:double :initial-contents vertices))
         (lines-fi (cffi:foreign-alloc 'gl:uint :initial-contents lines)))
    (gl:gen-buffers 2 vbos)
    (gl:with-bind-buffer (gl:+array-buffer+ (aref vbos 0))
      (gl:buffer-data gl:+array-buffer+ 
                      (* (length vertices) (cffi:foreign-type-size 'gl:double))
                      vertices-fi
                      gl:+static-draw+))
    (gl:with-bind-buffer (gl:+element-array-buffer+ (aref vbos 1))
      (gl:buffer-data gl:+element-array-buffer+ 
                      (* (length lines) (cffi:foreign-type-size 'gl:uint))
                      lines-fi
                      gl:+static-draw+))
    (cffi:foreign-free vertices-fi)
    (cffi:foreign-free lines-fi)
    (make-gl-mesh :vertices (aref vbos 0) :lines (aref vbos 1)))  )

(defun draw-edge (edge)
  "Draw edge"
  (gl:with-begin gl:+lines+
    (gl:vertex-2dv (edge-p0 edge))
    (gl:vertex-2dv (edge-p1 edge))))

(defun draw-mesh (mesh &key debug)
  (let ((gl-mesh (gethash mesh +ht-gl-mesh+)))
    (unless gl-mesh
      (setf gl-mesh
            (setf (gethash mesh +ht-gl-mesh+) (create-gl-mesh mesh))))
    (if (and *use-vob* gl-mesh)
        (progn
          (gl:enable-client-state gl:+vertex-array+)
          (gl:with-bind-buffer (gl:+array-buffer+ (gl-mesh-vertices gl-mesh))
            (gl:vertex-pointer 2 gl:+double+ 0 (cffi:make-pointer 0)))
          (gl:with-bind-buffer (gl:+element-array-buffer+ (gl-mesh-lines gl-mesh))
            (gl:draw-elements gl:+lines+ (* 2  (length (mesh-lines mesh))) gl:+unsigned-int+ (cffi:make-pointer 0)))
          (gl:disable-client-state gl:+vertex-array+))
        (gl:with-begin gl:+lines+
          (dolist (line (mesh-lines mesh))
            (gl:vertex-2d (car (elt (mesh-vertices mesh) (car line))) 
                          (cadr (elt (mesh-vertices mesh) (car line))))
            (gl:vertex-2d (car (elt (mesh-vertices mesh) (cadr line))) 
                          (cadr (elt (mesh-vertices mesh) (cadr line)))))))
    (when debug
      (gl:with-push-attrib (gl:+current-bit+) 
        (gl:color-3f 1 0 0)
        (dolist (e (mesh-edges mesh))
          (gl:with-begin gl:+lines+
            (gl:vertex-2dv (vec2d-add (edge-p0 e) 
                                      (vec2d-scale (edge-unit e) (/ (edge-length e) 2))))
            (gl:vertex-2dv (vec2d-add 
                            (vec2d-add (edge-p0 e) 
                                       (vec2d-scale (edge-unit e) (/ (edge-length e) 2)))
                            (vec2d-scale (edge-normal e) 10)))
            ))))
))