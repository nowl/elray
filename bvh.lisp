;; bounded volume heirarchy

(in-package :elray)

#|
(defun bb-center (bb)
  (declare (bounded-volume bb))
  (make-vect :x (/ (+ (bounded-volume-min-x bb)
                      (bounded-volume-max-x bb))
                   2)
             :y (/ (+ (bounded-volume-min-y bb)
                      (bounded-volume-max-y bb))
                   2)
             :z (/ (+ (bounded-volume-min-z bb)
                      (bounded-volume-max-z bb))
                   2)))
|#

(defun bvh-cluster (objects)
  (let ((bounding-volumes 
         (loop for obj in objects collect
              (let ((volume (bounded-volume obj))
                    (res (make-sequence '(vector single-float) 6)))
                (setf (aref res 0) (bounded-volume-min-x volume)
                      (aref res 1) (bounded-volume-min-y volume)
                      (aref res 2) (bounded-volume-min-z volume)
                      (aref res 3) (bounded-volume-max-x volume)
                      (aref res 4) (bounded-volume-max-y volume)
                      (aref res 5) (bounded-volume-max-z volume))
                (list res obj)))))
                      ;;obj)))))
    (flet ((sum (a b)
             (let ((v1 (car a)) (v2 (car b)))
               (make-array 6 :element-type 'single-float
                           :initial-contents (list (+ (aref v1 0) (aref v2 0))
                                                   (+ (aref v1 1) (aref v2 1))
                                                   (+ (aref v1 2) (aref v2 2))
                                                   (+ (aref v1 3) (aref v2 3))
                                                   (+ (aref v1 4) (aref v2 4))
                                                   (+ (aref v1 5) (aref v2 5))))))
           (div (vect b)
             (let ((v1 (car vect)))
               (make-array 6 :element-type 'single-float
                           :initial-contents (list (/ (aref v1 0) b)
                                                   (/ (aref v1 1) b)
                                                   (/ (aref v1 2) b)
                                                   (/ (aref v1 3) b)
                                                   (/ (aref v1 4) b)
                                                   (/ (aref v1 5) b)))))
           (dist (a b)
             (let ((v1 (car a)) (v2 (car b)))
               (sqrt
                (+ (sqr (- (aref v1 0) (aref v2 0)))
                   (sqr (- (aref v1 1) (aref v2 1)))
                   (sqr (- (aref v1 2) (aref v2 2)))
                   (sqr (- (aref v1 3) (aref v2 3)))
                   (sqr (- (aref v1 4) (aref v2 4)))
                   (sqr (- (aref v1 5) (aref v2 5))))))))
      (k-means 2 bounding-volumes #'sum #'div #'dist))))
