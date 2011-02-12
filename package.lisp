(defpackage :elray
  (:use :common-lisp 
        #+pcall
        :pcall)
  (:shadow :trace :+ :- :position)
  (:export :trace
           :trace-to-file))