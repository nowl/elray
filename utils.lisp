(in-package :elray)

(defmacro with-gensyms ((&rest vars) &body body)
  `(let ,(loop for var in vars collect
	      `(,var (gensym)))
     ,@body))

(defmacro dump-percentage (last-percent num div inc)
  (with-gensyms (percent rounded-percent)
    `(let ((,percent (round (* 100 (/ ,num ,div)))))
       (when (> ,percent (+ ,last-percent ,inc))
	 (let ((,rounded-percent (* (1+ (round ,percent ,inc)) ,inc)))
	   (format t "~a%..~%" ,rounded-percent)
	   (setf ,last-percent ,rounded-percent))))))