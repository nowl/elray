(in-package :elray)

(defstruct image-plane-info
  start-x
  start-y
  offset-x
  offset-y
  plane-center)
  
(defun make-image-plane (camera)
  (let ((ipi (make-image-plane-info))
        (fovxmin (camera-fov-x-min camera))
        (fovxmax (camera-fov-x-max camera))
        (fovymin (camera-fov-y-min camera))
        (fovymax (camera-fov-y-max camera)))
    (declare (single-float fovxmax fovxmax fovymin fovymax))
    (let* ((w (norm-vect (- (camera-looking-at camera) 
                            (camera-location camera))))
           (u (cross w (camera-up-vector camera)))
           (v (cross w u)))
      (setf (image-plane-info-start-x ipi) (mult-by-scalar u fovxmin)
            (image-plane-info-start-y ipi) (mult-by-scalar v fovymin)
            (image-plane-info-offset-x ipi) (mult-by-scalar u (/ (- fovxmax fovxmin) (camera-resx camera)))
            (image-plane-info-offset-y ipi) (mult-by-scalar v (/ (- fovymax fovymin) (camera-resy camera)))
            (image-plane-info-plane-center ipi)
            (+ (mult-by-scalar (norm-vect (- (camera-looking-at camera)
                                             (camera-location camera)))
                               (camera-image-dist camera))
               (camera-location camera))))
    ipi))
