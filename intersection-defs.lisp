(in-package :elray)

(defgeneric intersect (p1 p2 object))

(defmethod intersect (p1 p2 (o sphere))
  (let ((line (make-line-from-vects p1 p2)))
    (let ((slope (line-slope line))
		  (x0 (line-offset line))
		  (c (position o))
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
			  (cond ((or (< d1 0) (< d2 0)) (values nil nil))
					((< d1 d2) (values end1 dist1))
					(t (values end2 dist2)))))))))

(defmethod intersect (p1 p2 (o plane))
  "Equation of a plane is where N dot x = N dot location-on-plane."
  (let* ((line (make-line-from-vects p1 p2))
		 (slope (line-slope line))
		 (x0 (line-offset line))
		 (t1 (dot (normal o) slope)))
    (when (/= t1 0)
      (let* ((t2 (- (position o) x0))
			 (t3 (dot (normal o) t2))
			 (time (/ t3 t1)))
		(when (> time 0)
		  (let ((end (get-vect-at-time line time)))
			(values end (distance p1 end))))))))


;; intersection with axis aligned bounding box (AABB)
(defconstant +dir-threshold+ 0.00001)

(defun line-min-max-axis-intersect (pos dir min max tmin tmax)
  (declare (single-float pos dir min max tmin tmax))

  ;; first check if perpendicular to axis (dir approx. 0)
  (when (< (abs dir) +dir-threshold+)
    (if (and (> pos min) (< pos max))
        ;; inside
        (return-from line-min-max-axis-intersect
          (values t tmin tmax))
        ;; outside
        (return-from line-min-max-axis-intersect
          (values nil nil nil))))

  ;; find times to intersect min and max
  (let ((t1 (/ (- min pos) dir))
        (t2 (/ (- max pos) dir)))

    ;; sort times
    (when (> t1 t2)
      (psetf t1 t2 t2 t1))
    
    ;; check for no intersection
    (when (or (> t1 tmax) (< t2 tmin))
      (return-from line-min-max-axis-intersect
        (values nil nil nil)))
   
    ;; return time subset
    (return-from line-min-max-axis-intersect
      (values t
              (if (> t1 tmin) t1 tmin)
              (if (< t2 tmax) t2 tmax)))))



(defun line-aabb-intersect (line-pos line-dir line-length 
                            box-min-x box-max-x 
                            box-min-y box-max-y
                            box-min-z box-max-z)
  (macrolet ((test (axis-accessor min max)
               `(multiple-value-bind (intersect min max)
                    (line-min-max-axis-intersect (,axis-accessor line-pos)
                                                 (,axis-accessor line-dir)
                                                 ,min ,max tmin tmax)
                  (if (null intersect)
                      (return-from line-aabb-intersect nil)
                      (setf tmin min tmax max)))))
    
    (let ((tmin 0.0) (tmax line-length))
      
      ;; test x axis
      (test vect-x box-min-x box-max-x)
      ;; test y axis with new tmin and tmax
      (test vect-y box-min-y box-max-y)
      ;; test z axis with new tmin and tmax
      (test vect-z box-min-z box-max-z)
   
      ;; if we've made it here that means the line does intersect the
      ;; aabb if tmax is greater than tmin
      (if (< tmax tmin)
          nil
          t))))

;; assume a given start point and an infinite end point - this is the
;; case with a cast ray
(defconstant +max-ray-length+ 1e6)
(defmethod intersect (p1 p2 (o bounded-volume))
  (let ((line (make-line-from-vects p1 p2)))
    (let* ((slope (line-slope line))
           (x0 (line-offset line))
           ;;(length (dot slope (- p2 p1)))
           (length +max-ray-length+))
      (line-aabb-intersect x0 slope length
                           (bounded-volume-min-x o)
                           (bounded-volume-max-x o)
                           (bounded-volume-min-y o)
                           (bounded-volume-max-y o)
                           (bounded-volume-min-z o)
                           (bounded-volume-max-z o)))))
