(in-package :py4cl2-cffi)

;; Basic Reference: https://www.linuxjournal.com/article/8497
;; Multithreading reference: https://www.linuxjournal.com/article/3641

(defvar *python-state* :uninitialized)
(declaim (type (member :uninitialized :initialized :initializing) *python-state*))

#-no-optim
(declaim (inline python-alive-p python-start-if-not-alive))
(defun python-alive-p () (eq :initialized *python-state*))

(defun python-start-if-not-alive ()
  (case *python-state*
    (:uninitialized (pystart))
    (t nil)))

(defvar *py-module-pointer-table* (make-hash-table :test #'equal)
  "Key: A string indicating module name
Value: The pointer to the module in embedded python")
(declaim (type hash-table *py-module-pointer-table*))

(defun py-module-pointer (name)
  (declare (type string name))
  (or (nth-value 0 (gethash name *py-module-pointer-table*))
      (progn
        (python-start-if-not-alive)
        (setf (py-module-pointer name)
              (pyforeign-funcall "PyImport_AddModule" :string name :pointer)))))

(defun (setf py-module-pointer) (module-pointer name)
  (declare (type string name))
  (setf (gethash name *py-module-pointer-table*) module-pointer))

(defvar *py-module-dict-pointer-table* (make-hash-table :test #'equal)
  "Key: A string indicating module name
Value: The pointer to the module dictionary in embedded python")
(declaim (type hash-table *py-module-dict-pointer-table*))
(defun py-module-dict (name)
  (declare (type string name)
           #-no-optim(optimize speed))
  (or (nth-value 0 (gethash name *py-module-dict-pointer-table*))
      (progn
        (python-start-if-not-alive)
        (setf (py-module-dict name)
              (pyforeign-funcall "PyModule_GetDict"
                                 :pointer (py-module-pointer name)
                                 :pointer)))))

(defun (setf py-module-dict) (module-dict-pointer name)
  (declare (type string name)
           #-no-optim(optimize speed))
  (setf (gethash name *py-module-dict-pointer-table*)
        module-dict-pointer))

(defvar *py-global-dict* nil
  "Pointer to the dictionary mapping names to PyObjects in the global namespace of embedded python.")
(defvar *py-builtins-dict* nil
  "Pointer to the dictionary mapping names to PyObjects in the builtins namespace of embedded python.")

(defstruct output-sync-struct
  py-stream
  in-with-python-count
  with-python-count-lock
  with-python-stream
  with-python-start-semaphore
  with-python-end-semaphore)

(defvar *py-output-sync-struct*
  (make-output-sync-struct
   ;; py-stream will be set inside the thread
   :in-with-python-count 0
   :with-python-count-lock (bt:make-recursive-lock "output-count-lock")
   :with-python-stream nil
   :with-python-start-semaphore (bt:make-semaphore :name "output-start-semaphore")
   :with-python-end-semaphore (bt:make-semaphore :name "output-end-semaphore")))

(defvar *py-error-sync-struct*
  (make-output-sync-struct
   ;; py-stream will be set inside the thread
   :in-with-python-count 0
   :with-python-count-lock (bt:make-recursive-lock "error-count-lock")
   :with-python-stream nil
   :with-python-start-semaphore (bt:make-semaphore :name "error-start-semaphore")
   :with-python-end-semaphore (bt:make-semaphore :name "error-end-semaphore")))

;; This is called from a separate thread
(defun read-output-with-sync (output-sync-struct default-output-stream)
  "Reads output from the stream in the struct while
checking whether to output to the standard streams, or
to the stream specified in WITH-PYTHON-*-OUTPUT "
  (with-slots (py-stream with-python-stream
               in-with-python-count with-python-count-lock
               with-python-start-semaphore with-python-end-semaphore)
      output-sync-struct
    (loop :if (< 0 in-with-python-count)
            :do ;; Wait until the thunk finishes executing
                ;; This is "START" in the sense that we start reading now.
                ;; If the thunk has not finished executing and not flushed,
                ;; then LISTEN will return NIL, and this will end
                (without-python-gil
                  (bt:wait-on-semaphore with-python-start-semaphore))
                (loop :while (listen py-stream)
                      :do (let ((char (read-char-no-hang py-stream nil)))
                            (when char
                              (write-char char with-python-stream))))
                ;; Signal the WITH-PYTHON-*-OUTPUT to continue
                (bt:signal-semaphore with-python-end-semaphore)
                (without-python-gil
                  (bt:with-recursive-lock-held (with-python-count-lock)
                    (decf in-with-python-count)))
          :else
            :do ;; PEEK-CHAR waits for input
                (peek-char nil py-stream nil)
                (when (zerop in-with-python-count)
                  (let ((char (read-char-no-hang py-stream nil)))
                    (when char
                      (write-char char default-output-stream)))))))

(defvar *flush-both* t
  "If Non-NIL CALL-THUNK-PYTHON-ERROR-OR-OUTPUT flushes both stdout and stderr.")

;; Alternate idea: Instead of having the thread write to the string-output-stream
;; open the named pipe and read from it directly.
;; Opening the pipe multiple times does not work, at least for small read/writes,
;; the data in the pipes accumulates. Thus every time the pipe is opened, *all*
;; the writes until that point of time are read.
(defun call-thunk-python-error-or-output (thunk output-sync-struct stderr-or-stdout)
  "Capture and return the output produced by python during the
execution of THUNK as a string."
  (assert (or (equal stderr-or-stdout "stderr")
	      (equal stderr-or-stdout "stdout")))
  (with-slots (py-stream with-python-stream
               in-with-python-count with-python-count-lock
               with-python-start-semaphore with-python-end-semaphore)
      output-sync-struct
    (let ((all-signalled-p)
          (old-with-python-stream-value with-python-stream))
      (unwind-protect
           (progn
             (setf with-python-stream (make-string-output-stream))
             (without-python-gil
               (bt:with-recursive-lock-held (with-python-count-lock)
                 (incf in-with-python-count)))
             (pycall (format nil "sys.~A.write" stderr-or-stdout) ".")
             (unwind-protect
                  (funcall thunk)
	       (if (not *flush-both*)
		   (pycall (format nil "sys.~A.flush" stderr-or-stdout))
		   (progn (py4cl2-cffi:pycall (py4cl2-cffi:pyvalue "sys.stderr.flush"))
			  (py4cl2-cffi:pycall (py4cl2-cffi:pyvalue "sys.stdout.flush"))))
               (bt:signal-semaphore with-python-start-semaphore)
               (without-python-gil
                 (bt:wait-on-semaphore with-python-end-semaphore))
               (setq all-signalled-p t))
             (let* ((output (get-output-stream-string with-python-stream))
                    (len    (length output)))
               (if (zerop len)
                   output
                   (subseq output 1))))
        (progn
          (unless all-signalled-p
            (bt:signal-semaphore with-python-start-semaphore)
            (without-python-gil
              (bt:wait-on-semaphore with-python-end-semaphore)))
          (setf with-python-stream old-with-python-stream-value)
          (pycall (format nil "sys.~A.flush" stderr-or-stdout)))))))

;;; This is more of a global variable than a dynamic variable.

(defvar *py-output-stream-pipe*
  (format nil "/tmp/py4cl2-cffi-output-~D"
          (cffi:foreign-funcall "getpid" :int)))
(defvar *py-error-output-stream-pipe*
  (format nil "/tmp/py4cl2-cffi-error-output-~D"
          (cffi:foreign-funcall "getpid" :int)))
(defvar *py-output-reader-thread* nil)
(defvar *py-error-output-reader-thread* nil)

(defun python-output-thread ()
  (when (and *py-output-reader-thread*
             (bt:thread-alive-p *py-output-reader-thread*))
    (bt:destroy-thread *py-output-reader-thread*))
  (setq *py-output-reader-thread*
        (bt:make-thread
         (lambda ()
           (setf (slot-value *py-output-sync-struct* 'py-stream)
                 (open *py-output-stream-pipe*
                       :direction :input
                       #+ccl :sharing #+ccl :lock))
           (read-output-with-sync *py-output-sync-struct* *standard-output*))
         :name "py4cl2-cffi-output-thread"))
  (when (and *py-error-output-reader-thread*
             (bt:thread-alive-p *py-error-output-reader-thread*))
    (bt:destroy-thread *py-error-output-reader-thread*))
  (setq *py-error-output-reader-thread*
        (bt:make-thread
         (lambda ()
           (setf (slot-value *py-error-sync-struct* 'py-stream)
                 (open *py-error-output-stream-pipe*
                       :direction :input
                       #+ccl :sharing #+ccl :lock))
           (read-output-with-sync *py-error-sync-struct* *error-output*))
         :name "py4cl2-cffi-error-output-thread")))

(defmacro with-python-output (&body forms-decl)
  "Gets the output of the python program executed in FORMS-DECL in the form a string."
  `(call-thunk-python-error-or-output (lambda () ,@forms-decl)
                                      *py-output-sync-struct*
                                      "stdout"))

(defmacro with-python-error-output (&body forms-decl)
  "Gets the output of the python program executed in FORMS-DECL in the form a string."
  `(call-thunk-python-error-or-output (lambda () ,@forms-decl)
                                      *py-error-sync-struct*
                                      "stderr"))

(defvar *additional-init-codes* nil
  "A list of strings each of which should be python code. All the code
will be executed by PYSTART.")


(defun %pystart ()

  (when (probe-file *py-output-stream-pipe*)
    (delete-file *py-output-stream-pipe*))
  (mkfifo *py-output-stream-pipe*)
  (when (probe-file *py-error-output-stream-pipe*)
    (delete-file *py-error-output-stream-pipe*))
  (mkfifo *py-error-output-stream-pipe*)

  (load-python-and-libraries)
  (foreign-funcall "Py_Initialize")
  (when (pygil-held-p)
    (setq *py-thread-state* (pyeval-save-thread))
    (when (pygil-held-p)
      (warn "Python GIL was not released from the main thread. This means on implementations (like SBCL) that call lisp object finalizers from a separate thread may never get a chance to run, and thus python foreign objects associated with PYOBJECT-WRAPPER
can lead to memory leak.")))
  (import-module "sys")
  (etypecase *python-site-packages-path*
    (list
     (raw-pyexec (format nil "sys.path += [~{\"~A\"~^, ~}]"
                         *python-site-packages-path*)))
    (string
     (raw-pyexec (format nil "sys.path.append('~A')"
                         *python-site-packages-path*))))
  (import-module "traceback")
  (when *numpy-installed-p*
    (handler-case
        (import-module "numpy")
      (error (e)
        (warn (format nil "Could not import numpy: ~S~%" e))))
    (pushnew :typed-arrays *internal-features*)
    (when (member :typed-arrays *internal-features*)
      (setq *numpy-c-api-pointer*
            (with-python-gil (foreign-funcall "import_numpy" :pointer)))))
  (import-module "fractions")
  (raw-pyexec "from fractions import Fraction")

  (python-output-thread)
  (raw-pyexec (format nil "sys.stdout = open('~A', 'w')"
                      *py-output-stream-pipe*))
  (raw-pyexec (format nil "sys.stderr = open('~A', 'w')"
                      *py-error-output-stream-pipe*))

  (dolist (mod '("__main__" "builtins" "sys"))
    (setf (py-module-pointer mod)
          (pyforeign-funcall "PyImport_AddModule" :string mod :pointer)))
  (setq *py-global-dict*
        (pyforeign-funcall "PyModule_GetDict" :pointer (py-module-pointer "__main__") :pointer))
  (setq *py-builtins-dict*
        (pyforeign-funcall "PyModule_GetDict" :pointer (py-module-pointer "builtins") :pointer))

  (setf *lisp-callback-fn-ptr* (callback lisp-callback-fn)
        *getattr-ptr* (callback getattr-fn)
        *setattr-ptr* (callback setattr-fn))
  (raw-pyexec #.(format nil "import ctypes
py4cl_utils = ctypes.pydll.LoadLibrary(\"~A\")
" (namestring *utils-shared-object-path*)))
  (raw-pyexec (read-file-into-string
	       #+(and asdf (not mk-defsystem))
               (asdf:component-pathname
                (asdf:find-component (asdf:find-system "py4cl2-cffi") "py4cl.py"))
	       #+mk-defsystem
	       (mk::system-relative-pathname :py4cl2-cffi "src/py4cl.py")))
  (raw-pyexec "import decimal; Decimal = decimal.Decimal")
  (setq +py-empty-tuple-pointer+ (pycall* "tuple"))
  (setq +py-empty-tuple+ (pycall "tuple"))
  (setq +py-none-pointer+ (pyvalue* "None"))
  (setq +py-none+ (pyvalue "None"))
  (cond ((and *numpy-installed-p*
              (not (member :arrays *internal-features*)))
         (push :arrays *internal-features*))
        ((and (not *numpy-installed-p*)
              (member :arrays *internal-features*))
         (removef *internal-features* :arrays)))
  (mapc #'raw-pyexec *additional-init-codes*))

(defun pystart ()

  (when (eq *python-state* :initialized)
    (return-from pystart))

  (let ((*python-state* :initializing))
    (%pystart))
  (setq *python-state* :initialized)
  t)

(define-condition pyerror (error)
  ((format-control   :initarg :format-control
                     :initform "A python error occured")
   (format-arguments :initarg :format-arguments
                     :initform ()))
  (:report (lambda (condition stream)
             (with-slots (format-control format-arguments)
                 condition
               (apply #'format stream format-control format-arguments))))
  (:documentation "A lisp error to indicate all kinds of python error."))

(define-condition simple-pyerror (pyerror)
  ((type :initarg :type))
  (:documentation "A specialization of PYERROR to hold the python error type."))

(defun python-error-fetch ()
  "Fetches the python error, already assuming that it has occurred."
  (with-pygc
    (with-foreign-objects ((ptype  :pointer)
                           (pvalue :pointer)
                           (ptraceback :pointer))
      (pyforeign-funcall "PyErr_Fetch"
                         :pointer ptype
                         :pointer pvalue
                         :pointer ptraceback)
      (pyforeign-funcall "PyErr_NormalizeException"
                         :pointer ptype
                         :pointer pvalue
                         :pointer ptraceback)
      (let* ((type      (mem-aref ptype :pointer))
             (value     (mem-aref pvalue :pointer))
             (traceback (mem-aref ptraceback :pointer))
             (type-str  (foreign-string-to-lisp
                         (pytypeobject-name type)))
             (value-str
               (foreign-string-to-lisp
                (pyforeign-funcall "PyUnicode_AsUTF8"
                                   :pointer (pyforeign-funcall "PyObject_Str"
                                                               :pointer value
                                                               :pointer)
                                   :pointer)))
             (traceback-str
               (let ((*pyobject-translation-mode* :lisp))
                 (cond ((null-pointer-p value)
                        (pycall "traceback.format_exception_only" type))
                       ((null-pointer-p traceback)
                        (pycall "traceback.format_exception_only" type value))
                       (t
                        (pycall "traceback.format_exception" type value traceback))))))
        (with-simple-restart (continue-ignoring-errors "")
          (cond ((string= "" value-str)
                 (error 'simple-pyerror
                        :type type-str
                        :format-control "A python error occurred:~%  ~A"
                        :format-arguments
                        (list (etypecase traceback-str
                                (string traceback-str)
                                (vector (apply #'uiop:strcat
                                               (coerce traceback-str 'list)))
                                (list (apply #'uiop:strcat traceback-str))))))
                (t
                 (error 'simple-pyerror
                        :type type-str
                        :format-control "A python error occurred:~%  ~A~%~%~A"
                        :format-arguments
                        (list value-str
                              (etypecase traceback-str
                                (string traceback-str)
                                (vector (apply #'uiop:strcat
                                               (coerce traceback-str 'list)))
                                (list (apply #'uiop:strcat traceback-str))))))))))))

(defvar *already-retrieving-exceptions* nil
  "Set to non-NIL inside PYTHON-MAY-BE-ERROR so that calling PYFOREIGN-FUNCALL
from inside PYTHON-MAY-BE-ERROR does not lead to an infinite recursion.")
;;; FIXME: Perhaps this leaves open the case of an exception occuring during
;;; the handling of an exception.

(defun python-may-be-error ()
  (declare (optimize speed))
  (python-start-if-not-alive)
  (unless *already-retrieving-exceptions*
    (let ((*already-retrieving-exceptions* t))
      (with-python-gil
        (let ((may-be-error-type (pyerr-occurred/simple)))
          (unless (null-pointer-p may-be-error-type)
            (python-error-fetch)))))))

;; this was the earlier implementation of raw-py see commit 88c9812c25c2
#+nil
(defun raw-py (cmd-char &rest code-strings)
  (python-start-if-not-alive)
  (unless (zerop (pyforeign-funcall "PyRun_SimpleString"
                                    :string (apply #'concatenate
                                                   'string
                                                   (ecase cmd-char
                                                     (#\e "_ = ")
                                                     (#\x ""))
                                                   code-strings)
                                    :int))
    (error 'pyerror
           :format-control "An unknown python error occurred.
Unfortunately, no more information about the error can be provided
while using RAW-PYEVAL or RAW-PYEXEC on ~A"
           :format-arguments (lisp-implementation-version)))
  (ecase cmd-char
    (#\e (let ((ptr (pyvalue* "_")))
           (pyforeign-funcall "Py_IncRef" :pointer ptr)
           (pytrack ptr)))
    (#\x (values))))


(defun raw-py (cmd-char &rest code-strings)
  "CMD-CHAR should be #\e for eval and #\x for exec.

Unlike PY4CL or PY4CL2, the use of RAW-PY, RAW-PYEVAL and RAW-PYEXEC,
PYEVAL, PYEXEC should be avoided unless necessary.
Instead, use PYCALL, PYVALUE, (SETF PYVALUE), PYSLOT-VALUE, (SETF PYSLOT-VALUE), and PYMETHOD.

RAW-PY, RAW-PYEVAL, RAW-PYEXEC are only provided for backward compatibility."
  (python-start-if-not-alive)
  (with-python-gil
    (let* ((return-code nil)
           (command (ecase cmd-char
                      (#\e (apply #'concatenate 'string "_ = " code-strings))
                      (#\x (apply #'concatenate 'string code-strings))))
           (error-output
             (ecase *python-state*
               (:initialized
                (with-python-error-output
                  (setq return-code
                        (pyforeign-funcall "PyRun_SimpleString" :string command :int))))
               (:initializing
                (setq return-code
                      (pyforeign-funcall "PyRun_SimpleString" :string command :int))
                ""))))
      (unless (zerop return-code)
        (error 'pyerror
               :format-control error-output))
      (ecase cmd-char
        (#\e (let ((ptr (pyvalue* "_")))
               (pyforeign-funcall "Py_IncRef" :pointer ptr)
               (pytrack ptr)))
        (#\x (values))))))

(defun raw-pyeval (&rest code-strings)
  "
Unlike PY4CL or PY4CL2, the use of RAW-PY, RAW-PYEVAL and RAW-PYEXEC,
PYEVAL, PYEXEC should be avoided unless necessary.
Instead, use PYCALL, PYVALUE, (SETF PYVALUE), PYSLOT-VALUE, (SETF PYSLOT-VALUE), and PYMETHOD.

RAW-PY, RAW-PYEVAL, RAW-PYEXEC are only provided for backward compatibility."
  (pyobject-pointer-translate (apply #'raw-py #\e code-strings)))

(defun raw-pyexec (&rest code-strings)
  "
Unlike PY4CL or PY4CL2, the use of RAW-PY, RAW-PYEVAL and RAW-PYEXEC,
PYEVAL, PYEXEC should be avoided unless necessary.
Instead, use PYCALL, PYVALUE, (SETF PYVALUE), PYSLOT-VALUE, (SETF PYSLOT-VALUE), and PYMETHOD.

RAW-PY, RAW-PYEVAL, RAW-PYEXEC are only provided for backward compatibility."
  (apply #'raw-py #\x code-strings))

(defun pystop ()
  (when +disable-pystop+
    (return-from pystop))
  (when (python-alive-p)
    (trivial-garbage:gc :full t)
    (pygc)
    (pycall (pyvalue* "sys.stdout.close"))
    (pycall (pyvalue* "sys.stderr.close"))
    (pymethod *py-global-dict* "clear")
    (setq *py-module-pointer-table* (make-hash-table :test #'equal))
    (setq *py-module-dict-pointer-table* (make-hash-table :test #'equal))
    (setq *py-global-dict* nil)
    (setq *py-builtins-dict* nil)
    (setq *python-state* :uninitialized)
    (makunbound '+empty-tuple+)
    (makunbound '+empty-tuple-pointer+)
    (makunbound '+py-none+)
    (makunbound '+py-none-pointer+)
    (bt:destroy-thread *py-output-reader-thread*)
    (bt:destroy-thread *py-error-output-reader-thread*)
    (sleep 0.01)
    nil))

(defun pytype (name)
  (python-start-if-not-alive)
  (with-foreign-string (name name)
    (pyforeign-funcall "PyDict_GetItemString"
                       :pointer *py-builtins-dict*
                       :pointer name
                       :pointer)))

(defun %pyvalue (python-value-or-variable)
  "Get the foreign pointer associated with PYTHON-VALUE-OR-VARIABLE.
The PYTHON-VALUE-OR-VARIABLE may not contain '.' (full-stops)
Use PYVALUE* if you want to refer to names containing full-stops."
  (declare (type (or foreign-pointer string) python-value-or-variable)
           #-no-optim(optimize speed))
  (python-start-if-not-alive)
  (if (typep python-value-or-variable 'foreign-pointer)
      python-value-or-variable
      (let* ((name python-value-or-variable)
             ;; GetItemString returns a borrowed reference - neither stolen nor new
             (value (pyforeign-funcall "PyDict_GetItemString"
                                       :pointer (py-module-dict "__main__")
                                       :string name
                                       :pointer))
             (value (if (null-pointer-p value)
                        (pyforeign-funcall "PyDict_GetItemString"
                                           :pointer (py-module-dict "builtins")
                                           :string name
                                           :pointer)
                        value)))
        (ensure-non-null-pointer value
                                 :format-control "~A is undefined in python"
                                 :format-arguments (list name))
        value)))

(defun (setf %pyvalue) (new-value python-variable)
  "Sets the value of PYTHON-VARIABLE in the global namespace to NEW-VALUE"
  (declare (type string python-variable)
           (type foreign-pointer new-value))
  (python-start-if-not-alive)
  (pyforeign-funcall "PyDict_SetItemString"
                     :pointer (py-module-dict "__main__")
                     :string python-variable
                     :pointer new-value
                     :int)
  (python-may-be-error)
  new-value)

(defun %pyslot-value (object-pointer slot-name)
  (declare (type (or symbol string) slot-name)
           (type foreign-pointer object-pointer)
           #-no-optim(optimize speed))
  (python-start-if-not-alive)
  (ensure-non-null-pointer object-pointer
                           :format-control
                           "Trying to access ~A slot of object with null pointer"
                           :format-arguments (list slot-name))
  (let* ((slot-name (etypecase slot-name
                      (string slot-name)
                      (symbol (pythonize-symbol slot-name))))
         (return-value (pyforeign-funcall "PyObject_GetAttrString"
                                          :pointer object-pointer
                                          :string slot-name
                                          :pointer)))
    (ensure-non-null-pointer return-value
                             :format-control
                             "~A~%in python does not have the attribute ~A"
                             :format-arguments (list (pycall "str" object-pointer)
                                                     slot-name))))

(defun (setf %pyslot-value) (new-value object-pointer slot-name)
  (declare (type (or string symbol) slot-name)
           (type foreign-pointer object-pointer new-value)
           #-no-optim(optimize speed))
  (python-start-if-not-alive)
  (let* ((slot-name (etypecase slot-name
                      (string slot-name)
                      (symbol (pythonize-symbol slot-name))))
         (return-value (pyforeign-funcall "PyObject_SetAttrString"
                                          :pointer object-pointer
                                          :string slot-name
                                          :pointer new-value
                                          :int)))
    (if (zerop return-value)
        nil
        (python-may-be-error))))

(defun pyvalue* (python-value-or-variable)
  "Get the non-lispified value associated with PYTHON-VALUE-OR-VARIABLE"
  (declare (type (or foreign-pointer pyobject-wrapper string)
                 python-value-or-variable)
           #-no-optim(optimize speed))
  (typecase python-value-or-variable
    (foreign-pointer python-value-or-variable)
    (pyobject-wrapper python-value-or-variable)
    (t
     (let (value)
       (do-subseq-until (name python-value-or-variable #\. :test #'char=)
         (setq value (if value
                         (%pyslot-value value name)
                         (%pyvalue name))))
       value))))

(defun (setf pyvalue*) (new-value python-value-or-variable)
  (declare (type (or pyobject-wrapper string) python-value-or-variable)
           (type foreign-pointer new-value))
  (python-start-if-not-alive)
  (if (pyobject-wrapper-p python-value-or-variable)
      ;; FIXME
      python-value-or-variable
      (let (value previous-value previous-name)
        (do-subseq-until (name python-value-or-variable #\. :test #'char=)
          (setq previous-value value)
          (setq value
                (cond ((string= name python-value-or-variable)
                       ;; No need to traverse or reassign.
                       (setf (%pyvalue name) new-value))
                      (value
                       (%pyslot-value value name))
                      (t
                       (%pyvalue name))))
          (setq previous-name name))
        (when previous-value
          (setf (%pyslot-value previous-value previous-name) new-value)))))

(defun pyvalue (python-name-or-variable)
  "Get the value of a python-name-or-variable.

Example:

    (pyvalue \"sys\") ;=> <module 'sys' (built-in)>
    (pyvalue \"sys.path\")
    ;=>
      #(\"/home/user/miniconda3/lib/python310.zip\"
        \"/home/user/miniconda3/lib/python3.10\"
        \"/home/user/miniconda3/lib/python3.10/lib-dynload\"
        \"/home/user/miniconda3/lib/python3.10/site-packages\")
"
  (declare (type (or foreign-pointer pyobject-wrapper string)
                 python-name-or-variable))
  (python-start-if-not-alive)
  (pyobject-pointer-translate (pyvalue* python-name-or-variable)))

(defun (setf pyvalue) (new-value python-name-or-variable)
  "Set the value of a python-name-or-variable.
Example:

(setf (pyvalue \"foo\") 5) ;=> 5
(pyvalue \"foo\") ;=> 5

(with-lispifiers (((and vector (not string))
                   (lambda (x) (coerce x 'list))))
  (with-pythonizers ((list \"list\"))
    (push \"./\" (pyvalue \"sys.path\"))))
;=>
   (\"./\" \"/home/user/miniconda3/lib/python310.zip\"
    \"/home/user/miniconda3/lib/python3.10\"
    \"/home/user/miniconda3/lib/python3.10/lib-dynload\"
    \"/home/user/miniconda3/lib/python3.10/site-packages\")
"
  (declare (type string python-name-or-variable))
  (python-start-if-not-alive)
  (with-pygc
    (setf (pyvalue* python-name-or-variable)
          ;; UNTRACK because we do not want to lose reference to this object!
          (pyuntrack (%pythonize new-value)))
    new-value))
