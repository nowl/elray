(in-package :elray)

(defstruct camera 
  resx
  resy
  location
  looking-at
  up-vector
  image-dist
  fov-x-min
  fov-x-max 
  fov-y-min
  fov-y-max)

(defclass scene-object ()
  ((color        :initarg :color                      :reader color)
   (ambience     :initarg :ambience                   :reader ambience)
   (reflectivity :initarg :reflectivity :initform 0.0 :reader reflectivity)
   (transparent  :initarg :transparent  :initform nil :reader transparent-p)
   (ior          :initarg :ior          :initform 0.0 :reader ior)
   (position     :initarg :position                   :reader position)))

(defclass sphere (scene-object)
  ((radius :initarg :radius :reader radius)))

(defclass plane (scene-object)
  ((normal-facing :initarg :normal-facing :reader normal)))

(defmacro insert-sphere (x y z rad red green blue ambience)
  `(make-instance 'sphere
		  :position (make-vect :x ,x :y ,y :z ,z)
		  :radius ,rad
		  :color (make-instance 'color 
					:red ,red :green ,green :blue ,blue
					:min-color 0 :max-color 255)
		  :ambience ,ambience
		  :reflectivity 0.5))


(defgeneric scene-obj-norm (obj location)
  (:documentation "Returns the norm vector of an object based on a passed in location
vector."))

(defmethod scene-obj-norm ((obj plane) location)
  (declare (ignore location))
  (normal obj))

(defmethod scene-obj-norm ((obj sphere) location)
  (norm-vect (- location (position obj))))
