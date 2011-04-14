;; bounded volume heirarchy

(in-package :elray)

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
                      