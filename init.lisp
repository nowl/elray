(require 'asdf)

(asdf:oos 'asdf:load-op 'elray)

(elray:trace-to-file "elray.pgm")

