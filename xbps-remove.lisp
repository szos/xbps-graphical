
(in-package :xbps-graphical)

(defun xbps-remove (flags &rest args)
  (xbps 'remove flags args))

(defun remove-package (name)
  (check-type name string)
  (xbps-remove nil name))

(defun remove-package-and-dependencies (name)
  (check-type name string)
  (xbps-remove "R" name))

(defun clean-cache ()
  (xbps-remove "O"))

(defun clean-orphaned-packages ()
  (xbps-remove "o"))
