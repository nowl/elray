(asdf:defsystem :elray
    :description "A Common Lisp Ray-Tracer."
    :version "0.0.1"
    :author "Nowl <firestaff@gmail.com>"
    :license "GNU Public License"
    #+pcall
    :depends-on #+pcall (:pcall)
    :components ((:file "package")
		 (:file "utils")
		 (:file "vect-utils")
		 (:file "scene-objects")
		 (:file "globals")
		 (:file "intersection-defs")		 
		 (:file "ppm")
		 (:file "image-plane")
		 (:file "ray"))
    :serial t)
