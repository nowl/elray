(defpackage :elray
  (:use :common-lisp 
	#+pcall
	:pcall)
  (:shadow :trace :+ :-)
  (:export :trace
	   :trace-to-file))