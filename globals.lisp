(in-package :elray)

(defparameter *camera*
  (make-camera :resx 800
               :resy 800
               :location (make-vect :x 0.0 :y 2.0 :z 10.0)
               :looking-at (make-vect :x 0.0 :y 0.0 :z 0.0)
               :up-vector (make-vect :x 0.0 :y 1.0 :z 0.0)
               :image-dist 7.0
               :fov-x-min -2.0
               :fov-x-max 2.0
               :fov-y-min -2.0
               :fov-y-max 2.0))

(defparameter *basic-world* 
  (list 
   (insert-sphere 2.5 0.0 -0.2 1.2 (0 0 0) (0 0 255) (0 0 0) 0.3)
   (insert-sphere -2.5 0.0 -0.2 1.2 (0 0 0) (0 255 0) (0 0 0) 0.3)
   (insert-sphere 0.0 0.0 -1.5 1.5 (0 0 0) (255 0 0) (0 0 0) 0.9)
   (insert-sphere 0.0 -0.5 3.5 0.75 (0 0 0) (255 0 255) (0 0 0) 0.3)
   (make-instance 'plane
		  :position (make-vect :x 0.0 :y -1.5 :z 0.0)
		  :normal-facing (make-vect :x 0.0 :y 1.0 :z 0.0)
		  :ambient (std-color 0 0 0)
          :diffuse (std-color 0 200 200)
          :specular (std-color 0 0 0)
		  :reflectivity 0.0)))

(defparameter *bvh-world* nil
  "Bounding volume hierarchy representation of the world. This is
  created on trace initialization.")

(defparameter *spiral-world*
  (append
   (loop for count below 50 collect
        (let* ((r (random 256))
               (g (random 256))
               (b (random 256))
               (angle (/ (* 6 3.14159 count) 50))
               (rad (- 2 (* 2 (/ count 50.0))))
               (x (* rad (sin angle)))
               (z (* rad (cos angle)))
               (y (- (/ count 12.0) 1.5)))
          (insert-sphere x y z 0.2 (0 0 0) (r g b) (0 0 0) 0.05)))
   (list
    (make-instance 'plane
                   :position (make-vect :x 0.0 :y -1.5 :z 0.0)
                   :normal-facing (make-vect :x 0.0 :y 1.0 :z 0.0)
                   :ambient (std-color 0 0 0)
                   :diffuse (std-color 0 200 200)
                   :specular (std-color 0 0 0)
                   :reflectivity 0.0))))

(defparameter *random-circles-world*
  (append
   (loop for count below 1000 collect
        (let* ((r (random 256))
               (g (random 256))
               (b (random 256))
               (x (- (* (random 1.0) 5) 2.5))
               (z (- (* (random 1.0) 5) 2.5))
               (y (- (* (random 1.0) 5) 2.5))
               (rad (+ (random 0.15) 0.03)))
          (insert-sphere x y z rad (0 0 0) (r g b) (0 0 0) 0.5)))
   (list
    (make-instance 'plane
                   :position (make-vect :x 0.0 :y -1.5 :z 0.0)
                   :normal-facing (make-vect :x 0.0 :y 1.0 :z 0.0)
                   :ambient (std-color 0 0 0)
                   :diffuse (std-color 0 200 200)
                   :specular (std-color 0 0 0)
                   :reflectivity 0.0))))


(defparameter *world* *random-circles-world*)

(defparameter *light* 
  (list (make-vect :x 100.0 :y 100.0 :z 100.0)
	(make-instance 'color :red 255 :green 255 :blue 255
		       :min-color 0 :max-color 255)))

(defparameter *color-black* (make-instance 'color :red 0 :green 0 :blue 0
					   :min-color 0 :max-color 255))

(defparameter *maximum-reflection-depth* 20)

(defparameter *ambient-background-color* 
  (make-instance 'color :red 0 :green 0 :blue 0 
		 :min-color 0 :max-color 255))
