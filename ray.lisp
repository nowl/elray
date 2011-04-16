(in-package :elray)

;;(declaim (optimize (safety 0) (speed 3)))

(defun sendray (p1 p2 depth &key (exclude nil))
  "This should return the closest object and the location on that
object the ray hit."
  (cond ((> depth *maximum-reflection-depth*) (values nil nil))
		(t (let (closest-obj 
                 closest-loc
                 closest-dist
                 (reduced-list (bvh-reduce-to-list p1 p2 *bvh-world*)))
             (loop for obj in (remove exclude reduced-list) do
				  (multiple-value-bind (int dist) (intersect p1 p2 obj)
					(when (and int
							   (or (null closest-dist) (< dist closest-dist)))
					  (setf closest-obj obj
							closest-loc int
							closest-dist dist))))
			 (values closest-obj closest-loc)))))

(defmacro limit (value max)
  `(when (> ,value ,max)
     (setf ,value ,max)))

(defun diffuse-color-at (obj location)
  (let ((obstructed-light (sendray location
                                   (first *light*)
                                   *maximum-reflection-depth*
                                   :exclude obj)))
	(if (null obstructed-light)
		(let* ((v1 (norm-vect (- (first *light*) location)))
			   (d (dot v1 (scene-obj-norm obj location))))
		  (if (> d 0)
			  (mult-by-scalar (diffuse obj) d)
			  *color-black*))
		*color-black*)))

(defun reflective-color-at (obj location p1 current-depth)
  (let ((reflective-factor (reflectivity obj)))
    (if (= reflective-factor 0.0)
        *color-black*
        (let ((reflected-point (vect-rotate p1 
                                            (norm-vect (+ location 
                                                          (scene-obj-norm obj location)))
                                            180
                                            :angle-units :degrees)))
          (mult-by-scalar (ray->color location
                                      reflected-point
                                      obj 
                                      (1+ current-depth))
                          reflective-factor)))))

(defun clamp-color (color)
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
                   :min-color 0
                   :max-color 255)))
  
(defun ray->color (p1 p2 exclude-obj depth)
  ;; get closest object along the path excluding the current object
  (multiple-value-bind (obj-hit location-hit)
	  (sendray p1 p2 depth :exclude exclude-obj)
	(if obj-hit
		(let ((ambient-color (ambient obj-hit))
			  (diffuse-color (diffuse-color-at obj-hit location-hit))
			  (reflective-color (reflective-color-at obj-hit
                                                     location-hit
                                                     p1
                                                     depth)))
		  ;;(format t "~a ~a ~a, ~a ~a ~a, ~a ~a ~a ~%" (red reflective-color) (green reflective-color) (blue reflective-color) (red diffuse-color) (green diffuse-color) (blue diffuse-color) (red ambient-color) (green ambient-color) (blue ambient-color))
		  (let ((final-color (+ reflective-color (+ ambient-color diffuse-color))))
            (clamp-color final-color)))
		*ambient-background-color*)))

(defun trace-line (image-plane image-plane-info y-pos)
  (loop for x-image-point below (camera-resx *camera*) do
       (let ()
		 (declare ((signed-byte 16) y-pos x-image-point))
		 (let* ((image-point (+ (image-plane-info-plane-center image-plane-info)
                                (+ (+ (mult-by-scalar (image-plane-info-offset-x image-plane-info) x-image-point)
                                      (image-plane-info-start-x image-plane-info))
                                   (+ (mult-by-scalar (image-plane-info-offset-y image-plane-info) y-pos)
                                      (image-plane-info-start-y image-plane-info)))))
				(color (ray->color (camera-location *camera*)
								   image-point
								   nil
								   0)))
		   (setf (aref image-plane y-pos x-image-point) color)))))

(defmacro trace-n-lines (image-plane image-plane-info y-pos count)
  (with-gensyms (n cur-line)
    `(loop for ,n below ,count do
		  (let ((,cur-line (+ ,n ,y-pos)))
			(when (< ,cur-line (camera-resy *camera*))
			  (trace-line ,image-plane ,image-plane-info (+ ,n ,y-pos)))))))

(defun trace ()
  (let ((image-plane (make-array (list (camera-resy *camera*)
                                       (camera-resx *camera*))))
        (image-plane-info (make-image-plane *camera*))
		(last-percent 0))

    ;;#+pcall
    (setf (pcall:thread-pool-size) 4)

    ;; build bounding volume heirarchy

    (format t "building bounding volume hierarchy..~%")
    (setf *bvh-world* (bvh *world*))

    ;; trace the rays

    (format t "tracing image..~%")

    (let (tasks
		  (lines-at-once 150))
      #-pcall
      (declare (ignore tasks))
      (loop for y-image-point below (camera-resy *camera*) by lines-at-once do
		   #+pcall
		   (let ((y-pos y-image-point))
			 (push (pcall:pexec (trace-n-lines image-plane image-plane-info y-pos lines-at-once)) tasks))
		   #-pcall
		   (prog1
			   (trace-n-lines image-plane image-plane-info y-image-point lines-at-once)
			 (dump-percentage last-percent 
							  y-image-point
							  (camera-resy *camera*) 5)))

      #+pcall
      (loop for task in tasks
		 for n below (length tasks) do
		   (pcall:join task)
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
