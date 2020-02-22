;;;; xbps-graphical.asd

(asdf:defsystem #:xbps-graphical
  :description "Describe xbps-graphical here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (;; #:ltk
	       #:clawk
	       #:str
	       #:mcclim
	       #:slim)
  :components ((:file "package")
	       (:file "xbps-query")
	       (:file "xbps-install")
               (:file "xbps-graphical")))
