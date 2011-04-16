#!/bin/sh

sbcl --load "init.lisp" --eval "(elray:trace-to-file \"test.pgm\")" --eval "(quit)"
