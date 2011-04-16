(require 'asdf)

(pushnew :pcall *features*)
(load "~/quicklisp/setup.lisp")
(ql:quickload "pcall")

(asdf:oos 'asdf:load-op 'elray)

;;(elray:trace-to-file "test.pgm")
