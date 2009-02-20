(in-package :elray)

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