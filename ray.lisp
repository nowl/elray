(in-package :elray)

;;(declaim (optimize (safety 0) (speed 3)))

(defun sendray (p1 p2 depth &key (exclude nil))
  "This should return the closest object and the location on that
object the ray hit."
  (when (> depth *maximum-reflection-depth*)
    nil)
  (loop for obj in (remove exclude *world*) do
	   (let ((int (intersect p1 p2 obj)))
		 (when int
		   (return-from sendray (values obj int)))))
  nil)

(defmacro limit (value max)
  `(when (> ,value ,max)
     (setf ,value ,max)))

(defun ambient-color (obj)
  (mult-by-scalar (color obj) (ambience obj)))

(defun diffuse-color-at (obj location)
  (let ((light-obstruction (sendray location (first *light*)
									(1- *maximum-reflection-depth*)
									:exclude obj)))
	(if (null light-obstruction)
		(let* ((v1 (norm-vect (- (first *light*) location)))
			   (d (dot v1 (scene-obj-norm obj location))))
		  (if (> d 0)
			  (mult-by-scalar (color obj) d)
			  *color-black*))
		*color-black*)))

(defun reflective-color-at (obj location p1 reflective-factor current-depth)
  (when (= reflective-factor 0.0)
	*color-black*)
  (let ((reflected-point (vect-rotate p1 
									  (scene-obj-norm obj location) 
									  180
									  :angle-units :degrees)))
	(mult-by-scalar (ray->color location reflected-point obj (1+ current-depth))
					reflective-factor)))

(defun ray->color (p1 p2 exclude-obj depth)
  ;; get closest object along the path excluding the current object
  (multiple-value-bind (obj-hit location-hit)
	  (sendray p1 p2 depth :exclude exclude-obj)
    (if obj-hit
		(let ((ambient-color (ambient-color obj-hit))
			  (diffuse-color (diffuse-color-at obj-hit location-hit))
			  (reflective-color
			   (reflective-color-at obj-hit
									location-hit
									p1
									(reflectivity obj-hit)
									depth)))
		  ;;(format t "~a ~a ~a, ~a ~a ~a, ~a ~a ~a ~%" (red reflective-color) (green reflective-color) (blue reflective-color) (red diffuse-color) (green diffuse-color) (blue diffuse-color) (red ambient-color) (green ambient-color) (blue ambient-color))
		  (let ((final-color (+ reflective-color (+ ambient-color diffuse-color))))
			;; clamps the final color to the actual max values
			(let ((red (red final-color))
				  (green (green final-color))
				  (blue (blue final-color)))
			  (limit red 255)
			  (limit green 255)
			  (limit blue 255)
			  (make-instance 'color
							 :red red
							 :green green
							 :blue blue
							 :min-color 0
							 :max-color 255))))
		*ambient-background-color*)))

(defun trace-line (image-plane y-pos)
  (loop for x-image-point below (camera-resx *camera*) do
       (let ()
		 (declare ((signed-byte 16) y-pos x-image-point))
		 (let* ((image-point (aref image-plane y-pos x-image-point))
				(color (ray->color (camera-location *camera*)
								   image-point
								   nil
								   (1- *maximum-reflection-depth*))))
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
		  ;;(lines-at-once 150))
		  (lines-at-once 25))
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
							  (camera-resy *camera*) 5)))

      #+pcall
      (loop for task in tasks
		 for n below (length tasks) do
		   (join task)
		   (dump-percentage last-percent 
							n
							(length tasks) 5)))
	

    image-plane))

(defun trace-to-file (filename)
  (with-open-file (out (pathname filename) :direction :output :if-exists :supersede)
    (let ((color-map (trace)))
      (format t "writing output file..~%")
      (write-ppm-header out 255)
      (write-ppm-image out color-map))))
