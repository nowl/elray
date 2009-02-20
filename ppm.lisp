(in-package :elray)

(defun write-ppm-header (stream max-color-val)
  (let ((columns (camera-resx *camera*))
	(rows (camera-resy *camera*)))
    (format stream "P3~%~a ~a~%~a~%" columns rows max-color-val)))

(defun write-ppm-color-value (stream color)
  (let ((red (round (red color)))
	(green (round (green color)))
	(blue (round (blue color))))
    (format stream "~a ~a ~a~%" red green blue)))

(defun write-ppm-image (stream color-map)
  (let ((last-percent 0))
    (loop for row below (camera-resy *camera*) do
	 (loop for col below (camera-resx *camera*) do
	      (write-ppm-color-value stream (aref color-map row col)))
	 (dump-percentage last-percent row (camera-resy *camera*) 10))))