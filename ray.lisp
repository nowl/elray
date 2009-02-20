(in-package :cl-user)

#+pcall
(require 'pcall)

(defpackage :ray-trace
  (:nicknames :rt)
  (:use :common-lisp 
	:vect-utils 
	#+pcall
	:pcall)
  (:shadowing-import-from :vect-utils :+ :-)
  (:shadow :trace)
  (:export :trace
	   :trace-to-file))

(in-package :rt)

;;(declaim (optimize (safety 0) (speed 3)))

(defmacro with-gensyms ((&rest vars) &body body)
  `(let ,(loop for var in vars collect
	      `(,var (gensym)))
     ,@body))

(defstruct camera 
  resx
  resy
  location
  looking-at
  up-vector
  image-dist
  fov-x-min
  fov-x-max 
  fov-y-min
  fov-y-max)

(defclass scene-object ()
  ((color        :initarg :color                      :reader color)
   (ambience     :initarg :ambience                   :reader ambience)
   (reflectivity :initarg :reflectivity :initform 0   :reader reflectivity)
   (transparent  :initarg :transparent  :initform nil :reader transparent-p)
   (ior          :initarg :ior          :initform 0   :reader ior)))

(defclass sphere (scene-object)
  ((center :initarg :center :reader center)
   (radius :initarg :radius :reader radius)))

(defclass plane (scene-object)
  ((position      :initarg :position      :reader pos)
   (normal-facing :initarg :normal-facing :reader normal)))

(defmacro insert-sphere (x y z rad red green blue ambience)
  `(make-instance 'sphere
		  :center (make-vect :x ,x :y ,y :z ,z)
		  :radius ,rad
		  :color (make-instance 'color 
					:red ,red :green ,green :blue ,blue
					:min-color 0 :max-color 255)
		  :ambience ,ambience
		  :reflectivity 0.6))

(defparameter *world* 
  (list 
   (insert-sphere -1.5 0.0 0.0 1.0 0 255 0 0.1)
   (insert-sphere 1.5 1.5 0.0 1.0 0 0 255 0.1)
   (insert-sphere 0.0 0.0 -1.5 1.0 255 0 0 0.1)
   (make-instance 'plane
		  :position (make-vect :x 0.0 :y -1.5 :z 0.0)
		  :normal-facing (make-vect :x 0.0 :y 1.0 :z 0.0)
		  :color (make-instance 'color
					:red 0 :green 200 :blue 200
					:min-color 0 :max-color 255)
		  :ambience 0.1)))

(defparameter *light* (list (make-vect :x 100.0 :y 200.0 :z 50.0)
			    (make-instance 'color :red 255 :green 255 :blue 255 :min-color 0 :max-color 255)))

(defparameter *color-black* (make-instance 'color :red 0 :green 0 :blue 0 :min-color 0 :max-color 255))

(defparameter *maximum-reflection-depth* 10)

(defparameter *camera*
  (make-camera :resx 200
	       :resy 200
	       :location (make-vect :x 0.0 :y 2.0 :z 10.0)
	       :looking-at (make-vect :x 0.0 :y 0.0 :z 0.0)
	       :up-vector (make-vect :x 0.0 :y 1.0 :z 0.0)
	       :image-dist 7.0
	       :fov-x-min -2.0
	       :fov-x-max 2.0
	       :fov-y-min -2.0
	       :fov-y-max 2.0))

(defparameter *ambient-background-color* (make-instance 'color :red 0 :green 0 :blue 0 :min-color 0 :max-color 255))

(defgeneric intersect (p1 p2 object))

(defmethod intersect (p1 p2 (o sphere))
  (let ((line (make-line-from-vects p1 p2)))
    (let ((slope (line-slope line))
	  (x0 (line-offset line))
	  (c (center o))
	  (r (radius o)))
      (declare (single-float r))
      (let* ((t1 (dot slope (- c x0)))
	     (t2 (- 0 (dot (- c x0) (- c x0))))
	     (inner (+ (sqr r) t2 (sqr t1))))
	(declare (single-float t1 t2 inner))
	(if (> inner 0)
	    (let* ((d1 (+ t1 (sqrt inner)))
		   (d2 (- t1 (sqrt inner)))
		   (end1 (get-vect-at-time line d1))
		   (end2 (get-vect-at-time line d2))
		   (dist1 (distance p1 end1))
		   (dist2 (distance p1 end2)))
	      (declare (single-float d1 d2 dist1 dist2))
	      (if (< (- dist1 dist2) 0)
		  (list dist1 end1)
		  (list dist2 end2)))
	    nil)))))

(defmethod intersect (p1 p2 (o plane))
  "Equation of a plane is where N dot x = N dot location-on-plane."
  (let* ((line (make-line-from-vects p1 p2))
	 (slope (line-slope line))
	 (x0 (line-offset line))
	 (t1 (dot (normal o) slope)))
    (when (/= t1 0)
      (let* ((t2 (- (pos o) x0))
	     (t3 (dot (normal o) t2))
	     (time (/ t3 t1)))
	(when (> time 0)
	  (let* ((end (get-vect-at-time line time))
		 (dist (distance p1 end)))
	    (list dist end)))))))

(defun sendray (p1 p2 depth)
  "This should return a list of objects and distances from vect p1 to
the object."
  (when (> depth *maximum-reflection-depth*)
    nil)
  (remove-if #'null
	     (loop for obj in *world* collect
		  (let ((int (intersect p1 p2 obj)))
		    (when int
		      (cons obj (intersect p1 p2 obj)))))))

(defmacro limit (value max)
  `(when (> ,value ,max)
     (setf ,value ,max)))

(defgeneric color-at (obj location))

(defmethod color-at ((obj sphere) location)
  (let ((color 
	 (+
		  
	  ;; diffuse color
	  (let ((light-obstructions (sendray location (first *light*) (1- *maximum-reflection-depth*))))
	    (if (not-obstructed-light light-obstructions obj)
		(let ((v1 (norm-vect (- location (first *light*))))
		      (v2 (norm-vect (- (center obj) location))))
		  (let ((d (dot v1 v2)))
		    (if (> d 0)
			(mult-by-scalar (color obj) d)
			(make-instance 'color :red 0 :green 0 :blue 0 
				       :min-color (min-color (color obj))
				       :max-color (max-color (color obj))))))
		*color-black*))
		  
	  ;; ambient color
	  (mult-by-scalar (color obj) (ambience obj)))))
    (let ((red (red color))
	  (green (green color))
	  (blue (blue color)))
      (limit red 255)
      (limit green 255)
      (limit blue 255)
      (make-instance 'color
		     :red red
		     :green green
		     :blue blue
		     :min-color (min-color (color obj))
		     :max-color (max-color (color obj))))))

(defmacro not-obstructed-light (obs-list obj)
  `(or (null ,obs-list)
       (and (= (length ,obs-list) 1)
	    (equal (first (first ,obs-list)) ,obj))))

(defmethod color-at ((obj plane) location)
  (let ((color 
	 (+

	  ;; diffuse color
	  (let ((light-obstructions (sendray location (first *light*) (1- *maximum-reflection-depth*))))
	    ;;(if (and (consp non-obstructed-light) (first non-obstructed-light))
	    (if (not-obstructed-light light-obstructions obj)
		(let ((v (norm-vect (- (first *light*) location))))
		  (let ((d (dot v (normal obj))))
		    (if (> d 0)
			(mult-by-scalar (color obj) d)
			(make-instance 'color :red 0 :green 0 :blue 0 
				       :min-color (min-color (color obj))
				       :max-color (max-color (color obj))))))
		*color-black*))

	  ;; reflective color
	  (let* ((reflection-point (vect-rotate (camera-location *camera*)
						(make-vect :x 0.0 :y 1.0 :z 0.0) 180.0 :angle-units :degrees))
		 (color-reflective (first (ray->color location
						      reflection-point 
						      0
						      :exclude obj))))
	    (mult-by-scalar color-reflective (reflectivity obj)))

		  
	  ;; ambient color
	  (mult-by-scalar (color obj) (ambience obj)))))
    (let ((red (red color))
	  (green (green color))
	  (blue (blue color)))
      (limit red 255)
      (limit green 255)
      (limit blue 255)
      (make-instance 'color
		     :red red
		     :green green
		     :blue blue
		     :min-color (min-color (color obj))
		     :max-color (max-color (color obj))))))

(defmacro dump-percentage (last-percent num div inc)
  (with-gensyms (percent rounded-percent)
    `(let ((,percent (round (* 100 (/ ,num ,div)))))
       (when (> ,percent (+ ,last-percent ,inc))
	 (let ((,rounded-percent (* (1+ (round ,percent ,inc)) ,inc)))
	   (format t "~a%..~%" ,rounded-percent)
	   (setf ,last-percent ,rounded-percent))))))

(defun make-image-plane ()
  (format t "building image plane..~%")
  (let ((image-plane-points (make-array (list (camera-resy *camera*)
					      (camera-resx *camera*))))
	(plane-center (+ (mult-by-scalar (norm-vect (- (camera-looking-at *camera*)
						       (camera-location *camera*)))
					 (camera-image-dist *camera*))
			 (camera-location *camera*)))
	(fovxmin (camera-fov-x-min *camera*))
	(fovxmax (camera-fov-x-max *camera*))
	(fovymin (camera-fov-y-min *camera*))
	(fovymax (camera-fov-y-max *camera*)))
    (declare (single-float fovxmax fovxmax fovymin fovymax))
    (let*
	((w (norm-vect (- (camera-looking-at *camera*) 
			  (camera-location *camera*))))
	 (u (cross w (camera-up-vector *camera*)))
	 (v (cross w u))
	 (last-percent 0))
      (loop for map-y below (camera-resy *camera*) do
	   (loop for map-x below (camera-resx *camera*) do
		(let ()
		  (declare ((signed-byte 16) map-y map-x))
		  (let* ((y-offset (+ (* (/ (- fovymax fovymin) (camera-resy *camera*)) map-y) fovymin))
			 (x-offset (+ (* (/ (- fovxmax fovxmin) (camera-resx *camera*)) map-x) fovxmin))
			 (total-offset-vect (+ (mult-by-scalar u x-offset)
					       (mult-by-scalar v y-offset))))
		    (setf (aref image-plane-points map-y map-x)
			  (+ plane-center total-offset-vect))))
		(dump-percentage last-percent map-y (camera-resy *camera*) 10))))
    image-plane-points))

(defun get-closest (obj-dist-loc-list &key (exclude nil))
  (let ((closest nil))
    (loop for odl in obj-dist-loc-list do
	 (when (and (or (null closest)
			(and (second odl) (> (second closest) (second odl))))
		    (not (equal (first odl) exclude)))
	   (setf closest odl)))
    closest))

(defun ray->color (p1 p2 depth &rest args)
  (let ((obj-dist-loc (apply #'get-closest (sendray p1 p2 depth) args)))
    (if obj-dist-loc
	(list (color-at (first obj-dist-loc) (third obj-dist-loc))
	      obj-dist-loc)
	(list *ambient-background-color* nil))))

(defun trace-line (image-plane y-pos)
  (loop for x-image-point below (camera-resx *camera*) do
       (let ()
	 (declare ((signed-byte 16) y-pos x-image-point))
	 (let* ((image-point (aref image-plane y-pos x-image-point))
		(color (first (ray->color (camera-location *camera*)
					  image-point
					  (1- *maximum-reflection-depth*)))))
	   (setf (aref image-plane y-pos x-image-point) color)))))

(defmacro trace-n-lines (image-plane y-pos count)
  (with-gensyms (n cur-line)
    `(loop for ,n below ,count do
	  (let ((,cur-line (+ ,n ,y-pos)))
	    (when (< ,cur-line (camera-resy *camera*))
	      (trace-line ,image-plane (+ ,n ,y-pos)))))))

(defun trace ()
  (let ((image-plane (make-image-plane))
	(last-percent 0))

    #+pcall
    (setf (thread-pool-size) 4)

    ;; trace the rays

    (format t "tracing image..~%")

    (let (tasks
	  (lines-at-once 10))
      #-pcall
      (declare (ignore tasks))
      (loop for y-image-point below (camera-resy *camera*) by lines-at-once do
	   #+pcall
	   (let ((y-pos y-image-point))
	     (push (pexec (trace-n-lines image-plane y-pos lines-at-once)) tasks))
	   #-pcall
	   (prog1
	     (trace-n-lines image-plane y-image-point lines-at-once)
	     (dump-percentage last-percent 
			      y-image-point
			      (camera-resy *camera*) 10)))

      #+pcall
      (loop for task in tasks
	 for n below (length tasks) do
	   (join task)
	   (dump-percentage last-percent 
			    n
			    (length tasks) 10)))
					

    image-plane))

(defun write-ppm-header (stream max-color-val)
  (let ((columns (camera-resx *camera*))
	(rows (camera-resy *camera*)))
    (format stream "P3~%~a ~a~%~a~%" columns rows max-color-val)))

(defun write-ppm-color-value (stream color)
  (let ((red (round (red color)))
	(green (round (green color)))
	(blue (round (blue color))))
    (format stream "~a ~a ~a~%" red green blue)))

(defun write-ppm-image (stream color-map)
  (let ((last-percent 0))
    (loop for row below (camera-resy *camera*) do
	 (loop for col below (camera-resx *camera*) do
	      (write-ppm-color-value stream (aref color-map row col)))
	 (dump-percentage last-percent row (camera-resy *camera*) 10))))

(defun trace-to-file (filename)
  (with-open-file (out (pathname filename) :direction :output :if-exists :supersede)
    (let ((color-map (trace)))
      (format t "writing output file..~%")
      (write-ppm-header out 255)
      (write-ppm-image out color-map))))
