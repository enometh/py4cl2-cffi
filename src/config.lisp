(defpackage :py4cl2-cffi/config
  (:use :cl)
  (:export #:+python-version-string+
           #:*python-ldflags*
           #:*python-ignore-ldflags*
           #:*python-includes*
           #:*python-compile-command*
           #:*python-numpy-compile-command*
           #:*python-executable-path*
           #:*python-site-packages-path*
           #:*disable-pystop*
           #:*python-call-mode*
           #:*pystart-verbosity*
           #:print-configuration
           #:shared-library-from-ldflag))

(in-package :py4cl2-cffi/config)

;; Basic principle: We aim to use python3-config or equivalent to discover these values
;; However, whenever there is a problem with getting the python-executable path, then we
;; set it *explicitly* externally using an environment variable, e.g., via including
;; something like
;;
;; (setf (uiop:getenv "PY4CL2_CONFIG_PYTHON_EXECUTABLE_PATH")	
;;      "/usr/local/Caskroom/miniconda/base/bin/python3")
;;
;; into .sbclrc
;;
;; Then, we use that path (if set) for inferring the other values

#+nil
(mapcar (lambda (x)
	  (let ((sym (find-symbol (symbol-name x) :py4cl2-cffi/config)))
	    (when sym
	      (unintern sym :py4cl2-cffi/config))))
	'(*python-ldflags*
	  *python-includes*
	  *python-compile-command*))

(declaim (type list
               *python-ldflags*
               *python-includes*))

(declaim (type string *python-compile-command*))

(defvar *python-compile-command*
  (concatenate
   'string
   "gcc ~A -c -Wall -Werror -fpic py4cl-utils.c && "
   "gcc -shared -o libpy4cl-utils.so py4cl-utils.o")
  "~A corresponds to the *python-includes*
")

(defvar *python-numpy-compile-command*
  (concatenate
   'string
   "gcc ~A -I'~A' -c -Wall -Werror -fpic py4cl-numpy-utils.c && "
   "gcc -shared -o libpy4cl-numpy-utils.so py4cl-numpy-utils.o")
  "The first~A corresponds to the *python-includes*
The second ~A corresponds to the numpy include files discovered
  in shared-objects.lisp
")


(eval-when (:compile-toplevel :load-toplevel :execute)

  (defvar +default-python+ nil
    "If NON-NIL should be a string like \"python3.9\" which
is used instead of python3 or python3.9-config.")

  (defun return-value-as-list (cmd &optional override-+default-python+)
    "If OVERRIDE-DEFAULT-PYTHON is non-NIL, skip the codepath that kludges
the commandline to use +DEFAULT-PYTHON+."
    (when (and +default-python+ (not override-+default-python+))
      (let* ((prefix "python3")
	     (len (length prefix)))
	(assert (string= prefix cmd :end2 len))
	(setq cmd (with-output-to-string (stream)
		    (write-string +default-python+ stream)
		    (write-string cmd stream :start len)))))
    (remove ""
            (mapcar (lambda (value) ;madhu 240520 this is redundant
                      (string-trim '(#\newline) value))
                    (uiop:split-string
                     (uiop:run-program cmd
                                       :output :string
                                       :error-output *error-output*)
                     :separator '(#\newline #\tab #\space)))
            :test #'string=))
  
  (defparameter *python-executable-path*
    (if (uiop:getenv "PY4CL2_CONFIG_PYTHON_EXECUTABLE_PATH")
	(uiop:getenv "PY4CL2_CONFIG_PYTHON_EXECUTABLE_PATH")
	(first (return-value-as-list
		(format nil "which ~A" (or +default-python+ "python3"))
		'override-+default-python+)))
    "The path to python executable. This will be used to set sys.path.
This is useful in cases such as venv when python3-config does not lead
to expected paths.")

  #+nil ;; redefine +default-python+
  (alexandria:define-constant +python-version-string+
      (second (return-value-as-list (format nil "~A --version"
					    *python-executable-path*)))
    :test #'string=)
  (defparameter +python-version-string+
    (second (return-value-as-list "python3 --version")))

  (if (uiop:version< +python-version-string+ "3.8.0")
      (defparameter *python-ldflags*
        (return-value-as-list "python3-config --ldflags"))
      (defparameter *python-ldflags*
        (return-value-as-list "python3-config --embed --ldflags"))))


(defparameter *python-includes*
  (return-value-as-list "python3-config --includes"))


#||
HARDCODE
(defvar *python-ldflags* (return-value-as-list "python3.9-config --embed --ldflags" t))
(defvar *python-includes* (return-value-as-list "python3.9-config --includes" t))
||#

(defparameter *python-site-packages-path*
  (return-value-as-list
   (format nil "~A -c 'import sys; print(\"\\n\".join(sys.path))'"
	   *python-executable-path*)
   'override-+default-python+))

(defun print-configuration ()
  "Prints the ldflags and includes that will be used for the compilation
of the utility shared object/library that bridges the python C-API with lisp."
  (format t "Python ldflags: ~{~A~^ ~}~%Python includes: ~{~A~^ ~}~%"
          *python-ldflags*
          *python-includes*)
  (format t "Python site path (additional):~%  ~S~%" *python-site-packages-path*)
  (format t "  These are appended to sys.path after the embedded python starts.~%")
  (format t "Python executable used for site path:~%  ~A"
          *python-executable-path*))

(defvar *disable-pystop* nil
  "If non-NIL (PY4CL2-CFFI:PYSTOP) becomes a no-op.")

(defvar *python-ignore-ldflags*
  ;; python3-config of older versions of python (such as python3.6) includes
  ;; librt in --ldflags, but apparantly, older SBCL (SBCL 1.5.4) versions fail
  ;; to load it, and librt itself seems unnecessary.
  '("-lpthread" "-ldl" "-lutil" "-lanl" "-lm" "-lrt")
  "A list of ldflags that will be ignored during the compilation of
the utility shared object/library.")

(declaim (type (member :standard :dedicated-thread) *python-call-mode*))
(defvar *python-call-mode* :standard
  "PY4CL2-CFFI:+PYTHON-CALL-MODE+ is a constant assigned to the value of
PY4CL2-CFFI/CONFIG:*PYTHON-CALL-MODE* at compile time. Thus, if
PY4CL2-CFFI/CONFIG:*PYTHON-CALL-MODE* is changed, PY4CL2-CFFi must be recompiled.

Possible values are :STANDARD and :DEDICATED-THREAD

- When the value is :DEDICATED-THREAD, a dedicated thread is used for starting
  the embedded python interpreter. All calls to python from lisp are made
  through this thread. This may be required for using libraries like matplotlib
  that expect all calls from the single thread that loads the library. On the
  other hand, in emacs, each lisp buffer may communicate to the lisp process
  (and thus python) through separate threads. It is however possible to
  configure emacs and slime/swank to communicate using a single thread. See
  SWANK:*COMMUNICATION-STYLE*. Python call mode provides a way of sidestepping
  the slime/swank configuration.

- When the value is :STANDARD, no such thread is created. Calls to python can
  take place from any thread.")

(declaim (type boolean *pystart-verbosity*))
(defvar *pystart-verbosity* t
  "Default value of :VERBOSE argument to PY4CL2-CFFI:PYSTART")

(defun %shared-library-from-ldflag (ldflag)
  "Given a ldflag, for example, \"-lpython3.10\", return the shared library name
corresponding to it. In this case, on linux, it will return libpython3.10.so"
  (shared-library-from-ldflag ldflag (uiop:operating-system)))

(defgeneric shared-library-from-ldflag (ldflag operating-system)
  (:documentation "This is a generic function which takes in two arguments. The first argument is an ldflag (like `-lpython3.10`) and the second argument is the `(uiop:operating-system)` as a keyword to be used for specialization on the users systems. Each method should return the shared library name associated with that ldflag and software type. For example, when `(uiop:operating-system)` is `:linux`, the relevant method should return `python3.10.so`"))

(defmethod shared-library-from-ldflag (ldflag (operating-system (eql :linux)))
  (format nil "lib~A.so" (subseq ldflag 2)))

(defmethod shared-library-from-ldflag (ldflag (operating-system (eql :macosx)))
  (format nil "lib~A.dylib" (subseq ldflag 2)))

(defmethod shared-library-from-ldflag (ldflag (operating-system (eql :win)))
  (format nil "lib~A.dll" (subseq ldflag 2)))
