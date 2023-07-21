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

  (defun compile-utils-shared-object (&key force)
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
                         ;; *python-compile-command*
                         "gcc -I'~A' -I'~A' -c -Wall -Werror -fpic py4cl-utils.c"
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
        (flet ((compile-if-newer (source target command)
                 (when (or force (not (probe-file target))
                           (< (file-write-date target)
                              (file-write-date source)))
		   (format t "~&~A~%" command)
                   (uiop:run-program command
                                     :error-output *error-output*
                                     :output *standard-output*))))
          (compile-if-newer "py4cl-utils.c" "py4cl-utils.o" program-string)
          (compile-if-newer "py4cl-utils.o" "libpy4cl-utils.so"
                            "gcc -shared -o libpy4cl-utils.so py4cl-utils.o")))))))

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
                                               "python3.9 -c 'import numpy'"
                                               :ignore-error-status t))))
         (numpy-installed-p-new
           (format nil "CL:~A" numpy-installed-p)))
    (when (string/= numpy-installed-p-old
                    numpy-installed-p-new)
      (write-string-into-file numpy-installed-p-new numpy-installed-p-file
                              :if-exists :supersede
                              :if-does-not-exist :create))
    (setq *numpy-installed-p* numpy-installed-p)))

