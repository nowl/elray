(in-package :elray)

;;(declaim (optimize (safety 0) (speed 3)))

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

(defmacro not-obstructed-light (obs-list obj)
  `(or (null ,obs-list)
       (and (= (length ,obs-list) 1)
	    (equal (first (first ,obs-list)) ,obj))))

(defgeneric color-at (obj location))

(defvar *final-color* *color-black*)
(defmethod color-at :around ((obj scene-object) location)
  (setf *final-color* *color-black*)

  ;; this will call more specific methods to add in the color
  ;; components to *final-color*
  (call-next-method)

  (let ((red (red *final-color*))
	(green (green *final-color*))
	(blue (blue *final-color*)))
    (limit red 255)
    (limit green 255)
    (limit blue 255)
    (make-instance 'color
		   :red red
		   :green green
		   :blue blue
		   :min-color (min-color (color obj))
		   :max-color (max-color (color obj)))))

(defmacro add-final-color-component! (&body body)
  ;; body must be forms returning a color within an implicit progn
  `(setf *final-color* (+ *final-color* (progn ,@body))))
  
(defmethod color-at ((obj scene-object) location)
  ;; ambient color
  (add-final-color-component!
    (mult-by-scalar (color obj) (ambience obj))))

(defmacro diffuse-color-from-norm (diffuse-amount)
  "Pass in a form that returns a number between 0 and 1, 1 being the
directly in line with the light source, 0 being perpendicular. It will
be evaluating in the context of this macro which handles finding any
light obstructions and returns the color component from the diffuse
lighting."
  (with-gensyms (light-obstructions d)
    `(let ((,light-obstructions (sendray location (first *light*)
					 (1- *maximum-reflection-depth*))))
       (if (not-obstructed-light ,light-obstructions obj)
	   (let ((,d ,diffuse-amount))
	     (if (> ,d 0)
		 (mult-by-scalar (color obj) ,d)
		 *color-black*))
	   *color-black*))))

(defmethod color-at ((obj sphere) location)
  ;; diffuse color
  (add-final-color-component!
    (diffuse-color-from-norm 
     (let ((v1 (norm-vect (- location (first *light*))))
	   (v2 (norm-vect (- (center obj) location))))
       (dot v1 v2))))

  (call-next-method))
	 
(defmethod color-at ((obj plane) location)
  ;; diffuse color
  (add-final-color-component!
    (diffuse-color-from-norm
     (let ((v (norm-vect (- (first *light*) location))))
       (dot v (normal obj)))))

  ;; reflective color
  (add-final-color-component!
    (let* ((reflection-point (vect-rotate (camera-location *camera*)
					  (make-vect :x 0.0 :y 1.0 :z 0.0) 180.0 :angle-units :degrees))
	   (color-reflective (first (ray->color location
						reflection-point 
						0
						:exclude obj))))
      (mult-by-scalar color-reflective (reflectivity obj))))

  (call-next-method))

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

(defun trace-to-file (filename)
  (with-open-file (out (pathname filename) :direction :output :if-exists :supersede)
    (let ((color-map (trace)))
      (format t "writing output file..~%")
      (write-ppm-header out 255)
      (write-ppm-image out color-map))))
