(in-package :elray)

(defparameter *camera*
  (make-camera :resx 300
	       :resy 300
	       :location (make-vect :x 0.0 :y 2.0 :z 10.0)
	       :looking-at (make-vect :x 0.0 :y 0.0 :z 0.0)
	       :up-vector (make-vect :x 0.0 :y 1.0 :z 0.0)
	       :image-dist 7.0
	       :fov-x-min -2.0
	       :fov-x-max 2.0
	       :fov-y-min -2.0
	       :fov-y-max 2.0))

(defparameter *world* 
  (list 
   (insert-sphere -1.5 0.0 0.0 1.0 0 255 0 0.1)
   (insert-sphere 1.5 1.5 0.0 1.0 0 0 255 0.1)
   (insert-sphere 0.0 0.0 -1.5 1.0 255 0 0 0.1)
   (make-instance 'plane
		  :position (make-vect :x 0.0 :y -1.5 :z 0.0)
		  :normal-facing (make-vect :x 0.0 :y 1.0 :z 0.0)
		  :color (make-instance 'color
					:red 0 :green 200 :blue 200
					:min-color 0 :max-color 255)
		  :ambience 0.1)))

(defparameter *light* 
  (list (make-vect :x 100.0 :y 200.0 :z 50.0)
	(make-instance 'color :red 255 :green 255 :blue 255
		       :min-color 0 :max-color 255)))

(defparameter *color-black* (make-instance 'color :red 0 :green 0 :blue 0
					   :min-color 0 :max-color 255))

(defparameter *maximum-reflection-depth* 10)

(defparameter *ambient-background-color* 
  (make-instance 'color :red 0 :green 0 :blue 0 
		 :min-color 0 :max-color 255))
