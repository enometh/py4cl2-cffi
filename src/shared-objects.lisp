(in-package :py4cl2-cffi)

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defvar *utils-source-file-path*
    (merge-pathnames
     #+asdf
     (asdf:component-pathname (asdf:find-system "py4cl2-cffi"))
     #-asdf
     (merge-pathnames "src/" cl-user::*py4cl2-cffi-source-dir*)
     #+(and nil (not asdf) mk-defsystem)
     (mk::component-root-dir (mk:find-system "py4cl2-cffi" :load-or-nil) :source)
     #p"py4cl-utils.c"))

  (defvar *utils-shared-object-path*
    (merge-pathnames
     #+asdf
     (asdf:component-pathname (asdf:find-system "py4cl2-cffi"))
     #-asdf
     (merge-pathnames "src/" cl-user::*py4cl2-cffi-source-dir*)
     #p"libpy4cl-utils.so"))

  (defvar *numpy-installed-p*)

  (defun compile-utils-shared-object ()
    (uiop:with-current-directory (#+asdf
				  (asdf:component-pathname (asdf:find-system "py4cl2-cffi"))
				  #-asdf
				  (merge-pathnames "src/" cl-user::*py4cl2-cffi-source-dir*)
				  )
      (multiple-value-bind (numpy-path error-output error-status)
          (uiop:run-program
           "cd ~/; python3 -c 'import numpy; print(numpy.__path__[0])'"
           :output :string :ignore-error-status t)
        (declare (ignore error-output))
        (let* ((numpy-installed-p
                 (zerop error-status))
               (program-string
                 (format nil
                         *python-compile-command*
                         (namestring *python-include-path*)
                         (if numpy-installed-p
                             (format nil "~A/core/include/"
                                     (string-trim (list #\newline) numpy-path))
                             (namestring
			      #+asdf
			      (asdf:component-pathname
			       (asdf:find-system "py4cl2-cffi"))
			      #-asdf
			      (merge-pathnames "src/" cl-user::*py4cl2-cffi-source-dir*))))))
          (setq *numpy-installed-p* numpy-installed-p)
          (format t "~&~A~%" program-string)
          (uiop:run-program program-string
                            :error-output *error-output*
                            :output *standard-output*))))))

(eval-when (:compile-toplevel)
  (compile-utils-shared-object))

(eval-when (:compile-toplevel :load-toplevel)
  (let* ((numpy-installed-p-file
	  #+asdf
           (asdf:component-pathname
            (asdf:find-component
             "py4cl2-cffi" "numpy-installed-p"))
	   #-asdf
	   (merge-pathnames "src/numpy-installed-p.lisp"
			    cl-user::*py4cl2-cffi-source-dir*))
         (numpy-installed-p-old
           (read-file-into-string numpy-installed-p-file))
         (numpy-installed-p (zerop (nth-value 2
                                              (uiop:run-program
                                               "python3 -c 'import numpy'"
                                               :ignore-error-status t))))
         (numpy-installed-p-new
           (format nil "CL:~A" numpy-installed-p)))
    (when (string/= numpy-installed-p-old
                    numpy-installed-p-new)
      (write-string-into-file numpy-installed-p-new numpy-installed-p-file
                              :if-exists :supersede
                              :if-does-not-exist :create))
    (setq *numpy-installed-p* numpy-installed-p)))

