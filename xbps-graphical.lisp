;;;; xbps-graphical.lisp

(in-package #:xbps-graphical)

(defclass xbps-package ()
  ((name :initarg :name
	 :accessor name)
   (operation :initarg :operation
	      :accessor operation)
   (flags :initarg :flags
	  :accessor flags
	  :initfor "")))

(defun make-xbps-graphical-executable ()
  (sb-ext:save-lisp-and-die "xbps-graphical"
			    :toplevel (lambda ()
					(app-main))
			    :executable t
			    :purify t))

(defun xbps (program flags &rest args)
  (handler-case
      (with-output-to-string (stdout)
	(uiop:run-program (format nil (cond ((and (listp (car args))
						  (listp (caar args)))
					     "xbps-~a ~a~{~{~{~a ~}~}~}")
					    ((listp (car args))
					     "xbps-~a ~a~{~{~a ~}~}")
					    (t "xbps-~a ~a~{~a ~}"))
				  ;; (if (listp (car args))
				  ;; 	    "xbps-~a ~a~{~{~a ~}~}"
				  ;; 	    "xbps-~a ~a~{~a ~}")
				  (or (and (symbolp program)
					   (string-downcase
					    (symbol-name program)))
				      (and (stringp program) program))
				  (or (and flags
					   (< 0 (length flags))
					   (concatenate 'string
							"-" flags " "))
				      "")
				  args)
			  :output stdout)
	stdout)
    (uiop:subprocess-error (err)
      (let ((code (uiop:subprocess-error-code err))
	    (command (uiop:subprocess-error-command err)))
	(cond ((= code 13)
	       ;; (with-end-of)
	       (with-drawing-options (*standard-output* :ink +red+)
		 (format t "Insufficient permissions to run “~a”, try running xbps-graphical as root.~%"
			 command)))
	      (t
	       (error 'uiop:subprocess-error
		      :process (uiop:subprocess-error-process err)
		      :command (uiop:subprocess-error-command err)
		      :code (uiop:subprocess-error-code err))))
	;; (format nil "An error was encountered, errorcode ~a~%" code)
	))))

(defmacro bold ((stream) &body body)
  `(with-text-face (,stream :bold)
     ,@body))

(defmacro italic ((stream) &body body)
  `(with-text-face (,stream :italic)
     ,@body))

(defmacro xbps-table-output ((string &rest presentation-args)
			     (&optional (output *standard-output*)))
  (alexandria:with-gensyms (stream)
    `(let ((,stream (make-string-input-stream
		     (format nil "~a" ,string)))
	   i n d)
       (for-stream-lines (,stream)
	 (with-fields ((x y &rest z))
	   (setf i (append i (list x))
		 n (append n (list y))
		 d (append d (list z)))))
       (slim:with-table (,output)
	 (loop for installed in i
	       for name in n
	       for description in d
	       do ,(if (not presentation-args)
		       `(slim:row
			  (slim:cell (format t "~a" installed))
			  (slim:cell (format t "~a" name))
			  (slim:cell (format t "~{~a ~}" description)))
		       `(with-output-as-presentation ,presentation-args
			  (slim:row
			    (slim:cell (format t "~a" installed))
			    (slim:cell (format t "~a" name))
			    (slim:cell (format t "~{~a ~}" description)))))) ))))

(define-application-frame xbps ()
  ()
  (:menu-bar xbps-menubar)
  (:panes
   (interactor :interactor)
   (info :application
	 ;; :display-function #'display-info
	 :display-time :command-loop
	 :incremental-redisplay t))
  (:top-level (clim:default-frame-top-level :prompt 'prompt))
  (:layouts
   (default
    (vertically ()
      (:fill info)
      (1/4 interactor)))))

(make-command-table 'xbps-menubar
		    :errorp nil
		    :menu '(("Quit" :command com-quit)))

(defun prompt (pane frame)
  (declare (ignore frame))
  (terpri pane)
  (surrounding-output-with-border (pane :move-cursor nil :shape :drop-shadow)
    (format pane "Enter Command: "))
  (stream-increment-cursor-position pane 15 3))

(defun app-main ()
  (run-frame-top-level (make-application-frame 'xbps)))

(define-presentation-type only-install-package-presentation ())

(define-presentation-to-command-translator install-this-package
    (only-install-package-presentation com-install-package xbps :gesture :select)
    (obj)
  (list obj))

(define-presentation-type operate-on-package-presentation ())

(define-presentation-to-command-translator inspect-package
    (operate-on-package-presentation com-inspect-package xbps :gesture :select)
    (obj)
  (list obj))

(define-presentation-to-command-translator install-package
    (operate-on-package-presentation com-install-package xbps :gesture :select)
    (obj)
  (list obj))

(define-presentation-to-command-translator remove-package
    (operate-on-package-presentation com-remove-package xbps :gesture :select)
    (obj)
  (list obj))

(define-xbps-command (com-quit :name "Exit") ()
  (frame-exit *application-frame*))

(define-xbps-command (com-run-command :name "Run Shell Command")
    ((command string :prompt "Shell Command"))
  (bold (*standard-output*)
    (format t "Running Command ~a:~%" command))
  (format t "~a" (handler-case (with-output-to-string (s)
				 (uiop:run-program command :output s)
				 s)
		   (t () "an error occured"))))

(define-xbps-command (com-smart-operation)
    ((package package))
  (if (not (listp (operation package)))
      (xbps (operation package) (flags package) (name package))
      (loop for op (operation package)
	    for fl (flags package)
	    do (xbps op fl (name package)))))

(define-xbps-command (com-inspect-package :name "Inspect Package")
    ((package string :prompt "Package"))
  (let* ((res (xbps 'query "R"package))
	 (stream (make-string-input-stream res)))
    (bold (*standard-output*)
      (format t "Inspecting Package: ")
      (italic (*standard-output*)
	(format t "~a~%" package)))
    (with-output-as-presentation (*standard-output*
				  package 'only-install-package-presentation
				  :single-box t)
      (slim:with-table (*standard-output*)
	(for-stream-lines (stream)
	  (with-fields ((part &rest end) nil ":")
	    (if (char= #\tab (char part 0))
		(slim:row
		  (slim:cell (format t "↳"))
		  (slim:cell (format t "~a" (subseq part 1))))
		(slim:row
		  (slim:cell (format t "~a:" part))
		  (slim:cell
		    ;; (format t "~{~a:~}" end)
		    (if (< 1 (length end))
			(progn
			  (format t "~a" (car end))
			  (loop for str in (cdr end)
				do (format t ":~a" str)))
			(format t "~{~a~}" end)))))))))
    (format t " ")))

(define-xbps-command (com-remove-package :name "Remove Package")
    ((package string :prompt "Package"))
  (with-end-of-line-action (*standard-output* :wrap*)
    (let ((res (remove-package package)))
      (when res (format t "~a" res)))
    (format t " ")))

(define-xbps-command (com-install-package :name "Install Package")
    ((package string :prompt "Package"))
  (with-end-of-line-action (*standard-output* :wrap*)
    (let ((res (install-package package)))
      (when res
	(format t "~a" res)))
    (format t " ")))

(define-xbps-command (com-update :name "Update System") ()
  (bold (*standard-output*)
    (format t "Updating System~%"))
  (let ((resp (xbps 'install "Suy")))
    (when resp
      (format t "~a" resp))
    (format t " ")))

(define-xbps-command (com-update-test :name "test update system") ()
  (format t "~a" (xbps 'install "Su")))

(define-xbps-command (com-list-repositories :name "List Repositories") ()
  (bold (*standard-output*)
    (format t "Current Repositories:~%"))
  (format t "~a" (list-repositories)))

(define-xbps-command (com-package-dependencies :name "Package Dependencies")
    ((name string :prompt "Package: "))
  (let ((str (package-dependencies name))
	pkgs versions)
    (if str 
	(progn
	  (bold (*standard-output*)
	    (format t "Package Dependencies for ")
	    (italic (*standard-output*)
	      (format t "~a:~%" name)))
	  (for-stream-lines ((make-string-input-stream
			      (let ((str (package-dependencies name)))
				(if str str "No Package"))))
	    (with-fields ((pkg &optional version) nil ">=")
	      (setf pkgs (append pkgs (list pkg)))
	      (setf versions (append versions (list version)))
	      ;; (format t "~%~a version ~a or higher" pkg version)
	      ;; ($print pkg version)
	      ))
	  (slim:with-table (*standard-output*)
	    (loop for pkg in pkgs
		  for version in versions
		  do (with-output-as-presentation (*standard-output*
						   name
						   'operate-on-package-presentation
						   :single-box t)
			 (slim:row
			   (slim:cell (format t "~a" pkg))
			   (slim:cell (format t " version "))
			   (slim:cell (format t "~a" version))
			   (slim:cell (format t " or higher"))))))
	  (terpri))
	(format t "No Package Named~%"))))

(define-xbps-command (com-search-for-package :name "Search")
    ((name string :prompt "Package: "))
  (bold (*standard-output*)
    (format t "Searching for package ")
    (italic (*standard-output*)
      (format t "~a:~%" name)))
  (with-end-of-line-action (*standard-output* :scroll)
    (let ((stream (make-string-input-stream
		    (format nil "~a" (search-package-name name))))
	  i n d)
      (for-stream-lines (stream)
	(with-fields ((x y &rest z))
	  (setf i (append i (list x))
		n (append n (list y))
		d (append d (list z)))))
      (slim:with-table (*standard-output*)
	(loop for installed in i
	      for name in n
	      for description in d
	      do (let ((formatted-name
			 (subseq (reverse
				  (format nil "~{~a-~}"
					  (cdr (str:split "-"
							  (reverse name)))))
				 1)))
		   (with-output-as-presentation (*standard-output*
						 formatted-name
						 'operate-on-package-presentation
						 :single-box t)
		     (slim:row
		       (slim:cell (format t "~a" installed))
		       (slim:cell (format t "~a" name))
		       (slim:cell (format t "~{~a ~}" description)))))) ))
    (format t " "))
  (format t " "))
