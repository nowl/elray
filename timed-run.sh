#!/bin/sh

sbcl --load "init.lisp" --eval "(time (elray:trace-to-file \"test.pgm\"))" --eval "(quit)"
