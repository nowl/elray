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
  ((ambient      :initarg :ambient                    :reader ambient)
   (diffuse      :initarg :diffuse                    :reader diffuse)
   (specular     :initarg :specular                   :reader specular)
   (reflectivity :initarg :reflectivity :initform 0.0 :reader reflectivity)
   (transparent  :initarg :transparent  :initform nil :reader transparent-p)
   (ior          :initarg :ior          :initform 0.0 :reader ior)
   (position     :initarg :position                   :reader position)))

(defclass sphere (scene-object)
  ((radius :initarg :radius :reader radius)))

(defclass plane (scene-object)
  ((normal-facing :initarg :normal-facing :reader normal)))

(defmacro std-color (r g b)
  `(make-instance 'color 
                  :red ,r
                  :green ,g
                  :blue ,b
                  :min-color 0 :max-color 255))

(defmacro insert-sphere (x y z rad ambient diffuse specular reflectivity)
  `(make-instance 'sphere
                  :position (make-vect :x ,x :y ,y :z ,z)
                  :radius ,rad
                  :ambient (make-instance 'color 
                                          :red ,(first ambient)
                                          :green ,(second ambient)
                                          :blue ,(third ambient)
                                          :min-color 0 :max-color 255)
                  :diffuse (make-instance 'color 
                                          :red ,(first diffuse)
                                          :green ,(second diffuse)
                                          :blue ,(third diffuse)
                                          :min-color 0 :max-color 255)
                  :specular (make-instance 'color 
                                           :red ,(first specular)
                                           :green ,(second specular)
                                           :blue ,(third specular)
                                           :min-color 0 :max-color 255)
                  :reflectivity ,reflectivity))


(defgeneric scene-obj-norm (obj location)
  (:documentation "Returns the norm vector of an object based on a passed in location
vector."))

(defmethod scene-obj-norm ((obj plane) location)
  (declare (ignore location))
  (normal obj))

(defmethod scene-obj-norm ((obj sphere) location)
  (norm-vect (- location (position obj))))
