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
    (flet ((sum (sets)
             (let ((res (make-sequence '(vector single-float) 6 :initial-element 0.0)))
               (loop for set in sets do
                    (let ((v1 (car set)))
                      (incf (aref res 0) (aref v1 0))
                      (incf (aref res 1) (aref v1 1))
                      (incf (aref res 2) (aref v1 2))
                      (incf (aref res 3) (aref v1 3))
                      (incf (aref res 4) (aref v1 4))
                      (incf (aref res 5) (aref v1 5))))
               (list res nil)))

           (div (vect b)
             (let ((res (make-sequence '(vector single-float) 6))
                   (v1 (car vect)))
               (setf (aref res 0) (/ (aref v1 0) b)
                     (aref res 1) (/ (aref v1 1) b)
                     (aref res 2) (/ (aref v1 2) b)
                     (aref res 3) (/ (aref v1 3) b)
                     (aref res 4) (/ (aref v1 4) b)
                     (aref res 5) (/ (aref v1 5) b))
               (list res nil)))


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

(defstruct bvh

  ;; children will either be a list of two bvh structures or an actual
  ;; scene-object
  children

  ;; volume will be the total bounding volume of this node plus all
  ;; children
  volume)

(defun bvh-build (objects)
  (if (= (length objects) 1)
      
      ;; assign this node to the actual object
      (car objects)
      
      (let ((clusters (bvh-cluster objects)))
        ;; recurse
        (make-bvh :children (list (bvh-build (mapcar #'cadr (first clusters)))
                                  (bvh-build (mapcar #'cadr (second clusters))))))))
  
(defun expand-volume (bvh volume)
  (setf (bvh-volume bvh)
        (if (null (bvh-volume bvh))
            volume
            (make-bounded-volume
             :min-x (min (bounded-volume-min-x volume)
                         (bounded-volume-min-x (bvh-volume bvh)))
             :min-y (min (bounded-volume-min-y volume)
                         (bounded-volume-min-y (bvh-volume bvh)))
             :min-z (min (bounded-volume-min-z volume)
                         (bounded-volume-min-z (bvh-volume bvh)))
             :max-x (max (bounded-volume-max-x volume)
                         (bounded-volume-max-x (bvh-volume bvh)))
             :max-y (max (bounded-volume-max-y volume)
                         (bounded-volume-max-y (bvh-volume bvh)))
             :max-z (max (bounded-volume-max-z volume)
                         (bounded-volume-max-z (bvh-volume bvh)))))))

(defun fill-in-volumes (bvh)
  (etypecase bvh
    (bvh (expand-volume bvh (fill-in-volumes (first (bvh-children bvh))))
         (expand-volume bvh (fill-in-volumes (second (bvh-children bvh))))
         (bvh-volume bvh))
    (scene-object (bounded-volume bvh))))

(defun bvh-print (bvh depth)
  (dotimes (i depth)
    (format t " "))
  (etypecase bvh
    (bvh (format t "depth ~a volume ~a~%" depth (bvh-volume bvh))
         (bvh-print (first (bvh-children bvh)) (1+ depth))
         (bvh-print (second (bvh-children bvh)) (1+ depth)))
    (scene-object (format t "~a volume ~a~%" bvh (bounded-volume bvh)))))

;; This is the high level function to actually build the bvh-tree
(defun bvh (objects)
  (let ((bvh-tree (bvh-build objects)))
    (fill-in-volumes bvh-tree)
    bvh-tree))

;; This is used to intersect a ray with the given bvh and should
;; return a list of objects that are possibly intersected by this
;; ray. This function should only be checking bounding boxes - once an
;; actual scene-object is reached that object should be added to the
;; returned list even if it does not intersect at a fine-grained
;; object level.
(defun bvh-reduce-to-list (p1 p2 bvh)
  (let (results)
    (etypecase bvh
      (bvh (when (intersect p1 p2 (bvh-volume bvh))
             (setf results (append results (bvh-reduce-to-list p1 p2 (first (bvh-children bvh)))))           
             (setf results (append results (bvh-reduce-to-list p1 p2 (second (bvh-children bvh)))))))
      (scene-object (setf results (list bvh))))
    results))
       
