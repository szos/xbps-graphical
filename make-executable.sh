#!/bin/sh

LISP=$(which sbcl)
exit="$?"
## echo "$LISP"
## echo "$exit"

if [ ! "$exit" -eq 0 ]; then
    echo "NO SBCL"
    exit 1
fi

sbcl --eval "(ql:quickload :xbps-graphical)" --eval "(xbps-graphical::make-xbps-graphical-executable)"
