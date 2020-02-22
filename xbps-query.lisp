
(in-package #:xbps-graphical)

;; (defmacro xbps-query (flags &rest args)
;;   `(uiop:run-program (format nil "xbps-query -~a ~{~a ~}" ,flags ,args)
;; 		    :output t))

(defun xbps-query (flags &rest args)
  (xbps 'query flags args))

(defun list-repositories ()
  (xbps-query "L"))

(defun list-installed-packages ()
  (xbps-query "l"))

(defun search-package-name (name)
  (check-type name string)
  (xbps-query "Rs" name))

(defun search-package-filename ()) ;; this doesnt work... or at least it hangs

(defun package-information (name)
  (check-type name string)
  (xbps-query "R" name))

(defun package-files-list (name)
  (check-type name string)
  (xbps-query "Rf" name))

(defun package-dependencies (name)
  (check-type name string)
  (xbps-query "Rx" name))

(defun package-reverse-dependencies (name)
  (check-type name string)
  (xbps-query "RX" name))
