(asdf:defsystem :elray
    :description "A Common Lisp Ray-Tracer."
    :version "0.0.1"
    :author "Nowl <firestaff@gmail.com>"
    :license "GNU Public License"
    #+pcall
    :depends-on #+pcall (:pcall)
    :components ((:file "package")
		 (:file "vect-utils" :depends-on ("package"))
		 (:file "ray" :depends-on ("vect-utils"))))
