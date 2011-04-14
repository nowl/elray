(in-package :elray)

(defstruct bounded-volume
  (min-x 0.0 :type single-float)
  (max-x 0.0 :type single-float)
  (min-y 0.0 :type single-float)
  (max-y 0.0 :type single-float)
  (min-z 0.0 :type single-float)
  (max-z 0.0 :type single-float))


(defgeneric bounded-volume (object))

(defmethod bounded-volume ((o sphere))
  (with-slots (radius position) o
    (make-bounded-volume
     :min-x (- (vect-x position) radius)
     :max-x (+ (vect-x position) radius)
     :min-y (- (vect-y position) radius)
     :max-y (+ (vect-y position) radius)
     :min-z (- (vect-z position) radius)
     :max-z (+ (vect-z position) radius))))

(defmethod bounded-volume ((o plane))
  (make-bounded-volume
   :min-x most-negative-single-float
   :max-x most-positive-single-float
   :min-y most-negative-single-float
   :max-y most-positive-single-float
   :min-z most-negative-single-float
   :max-z most-positive-single-float))
