(require 'asdf)

;(pushnew :pcall *features*)

(asdf:oos 'asdf:load-op 'elray)

(elray:trace-to-file "test.pgm")
