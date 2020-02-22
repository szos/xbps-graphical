
(in-package :xbps-graphical)

(defun synchronize-repositories ()
  (xbps 'install "S"))

(defun update-package (name)
  (check-type name string)
  (xbps 'install "S" name))

(defun reinstall-package (name)
  (check-type name string)
  (xbps 'install "Sfy" name))

(defun update-system ()
  (xbps 'install "Suy"))

(defun install-package (name)
  (check-type name string)
  (xbps 'install "y" name))
   
