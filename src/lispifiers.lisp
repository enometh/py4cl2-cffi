(in-package :py4cl2-cffi)

(declaim (type hash-table *py-type-lispifier-table*))
(defvar *py-type-lispifier-table* (make-hash-table :test #'equal))

(defmacro define-lispifier (name (pyobject-var) &body body)
  (declare (type string name))
  ;; FIXME: If djb2 collides, we are in trouble. Let's hope that never happens!
  (with-gensyms (cstr)
    `(setf (gethash (with-foreign-string (,cstr ,name)
                      (djb2-foreign-string-hash ,cstr))
                    *py-type-lispifier-table*)
           (lambda (,pyobject-var) ,@body))))

(define-lispifier "NoneType" (o)
  (if (boundp '+py-none+)
      +py-none+
      (progn
        (pyuntrack o)
        (make-tracked-pyobject-wrapper o))))

(define-lispifier "UnknownLispObject" (o)
  (lisp-object (pyslot-value o "handle")))

(define-lispifier "int" (o)
  (pyforeign-funcall "PyLong_AsLong" :pointer o :long))

(define-lispifier "float" (o)
  (pyforeign-funcall "PyFloat_AsDouble" :pointer o :double))

(define-lispifier "str" (o)
  (nth-value 0
             (foreign-string-to-lisp
              (pyforeign-funcall "PyUnicode_AsUTF8" :pointer o :pointer))))

(define-lispifier "bool" (o)
  (cond ((pointer-eq o (pyvalue* "True"))
         t)
        ((pointer-eq o (pyvalue* "False"))
         nil)
        (t
         (error "Object at ~S is a bool but is neither 'True' or 'False'" o))))

(define-lispifier "tuple" (o)
  (let ((py-size (pyforeign-funcall "PyTuple_Size" :pointer o :int)))
    (if (zerop py-size)
        (if (boundp '+py-empty-tuple+)
            +py-empty-tuple+
            (progn
              (pyuntrack o)
              (make-tracked-pyobject-wrapper o)))
        (loop :for i :below py-size
              :collect (lispify (pyforeign-funcall "PyTuple_GetItem" :pointer o
                                                                     :int i
                                                                     :pointer))))))

(define-lispifier "list" (o)
  (let* ((py-size (pyforeign-funcall "PyList_Size" :pointer o :int))
         (vec     (make-array py-size :element-type t)))
    (loop :for i :below py-size
          :do (setf (svref vec i)
                    (lispify (pyforeign-funcall "PyList_GetItem" :pointer o
                                                                 :int i
                                                                 :pointer))))
    vec))

(define-lispifier "dict" (o)
  (let ((py-size (pyforeign-funcall "PyDict_Size" :pointer o :long))
        (hash-table (make-hash-table :test #'equal))
        (py-keys (pyforeign-funcall "PyDict_Keys" :pointer o :pointer)))
    (loop :for i :below py-size
          :for py-key := (pyforeign-funcall "PyList_GetItem"
                                            :pointer py-keys
                                            :int i
                                            :pointer)
          :for key := (lispify py-key)
          :for value := (lispify (pyforeign-funcall "PyDict_GetItem"
                                                    :pointer o
                                                    :pointer py-key
                                                    :pointer))
          :do (setf (gethash key hash-table) value))
    hash-table))

(define-lispifier "Fraction" (o)
  (cl:/ (pyslot-value o "numerator") (pyslot-value o "denominator")))

(define-lispifier "complex" (o)
  (complex (foreign-funcall "PyComplex_RealAsDouble" :pointer o :double)
           (foreign-funcall "PyComplex_ImagAsDouble" :pointer o :double)))

(define-lispifier "numpy.ndarray" (o)
  (let* ((dims     (pyslot-value o "shape"))
         (element-type (let* ((*read-eval* nil)
                              (*package* (find-package :cl)))
                         (read-from-string
                          (foreign-string-to-lisp
                           (pyforeign-funcall "PyArray_element_type_from_array"
                                              :pointer o :pointer)))))
         (from-vec  (pyforeign-funcall "PyArray_Data" :pointer o :pointer))
         (array     (make-array dims :element-type element-type))
         (num-bytes (* (array-element-type-num-bytes array)
                       (reduce #'* dims :initial-value 1))))
    (when (zerop (foreign-funcall "PyArray_Is_C_Contiguous"
                                  :pointer o :int))
      (let ((new-pyarray
              (pyforeign-funcall
               "PY4CL_PyArray_FromArray"
               :pointer o
               :pointer (pyforeign-funcall
                         "PyArray_Descr_from_element_type_code"
                         :string (array-element-typecode array)
                         :pointer)
               :int (mem-ref (foreign-symbol-pointer
                              "PyArray_C_Contiguous")
                             :int)
               :pointer)))
        (setq from-vec (pyforeign-funcall "PyArray_Data"
                                          :pointer new-pyarray
                                          :pointer))))
    (if (type= element-type t)
        (loop :for idx :below (array-total-size array)
              :do (setf (row-major-aref array idx)
                        (lispify
                         (pyforeign-funcall "PyArray_GetItem"
                                            :pointer o
                                            :pointer (inc-pointer from-vec
                                                                  (cl:* idx 8))
                                            :pointer))))
        (with-pointer-to-vector-data (to-vec (array-storage array))
          (pyforeign-funcall "memcpy" :pointer to-vec :pointer from-vec
                                      :int num-bytes)))
    array))

  ;; TODO: Test these aka find reference in documentation for why this works
(macrolet ((def (numpy-type ctype)
             `(define-lispifier ,numpy-type (o) (mem-ref o ,ctype 16))))
  (def "numpy.float64" :double)
  (def "numpy.float32" :float)
  (def "numpy.uint64" :uint64)
  (def "numpy.uint32" :uint32)
  (def "numpy.uint16" :uint16)
  (def "numpy.uint8"  :uint8)
  (def "numpy.int64"  :int64)
  (def "numpy.int32"  :int32)
  (def "numpy.int16"  :int16)
  (def "numpy.int8"   :int8))

#+ecl
(define-lispifier "numpy.longdouble" (o)
  (let ((o (ffi:make-pointer (pointer-address (inc-pointer o 16))
                             :long-double)))
    (ffi:deref-pointer o :long-double)))

(defun array-element-type-num-bytes (array)
  (eswitch ((array-element-type array) :test #'type=)
    ('single-float 4)
    ('double-float 8)
    ('(signed-byte 64) 8)
    ('(signed-byte 32) 4)
    ('(signed-byte 16) 2)
    ('(signed-byte 08) 1)
    ('(unsigned-byte 64) 8)
    ('(unsigned-byte 32) 4)
    ('(unsigned-byte 16) 2)
    ('(unsigned-byte 08) 1)
    ('t 8)))

(defun lispify (pyobject)
  (declare (type foreign-pointer pyobject)
	   #+no-optim
           (optimize speed)
	   #+no-optim
           (inline gethash pyobject-typename/simple djb2-foreign-string-hash))
  (assert (eq :lisp *pyobject-translation-mode*))
  ;; The performance impact of this check is less than 2%
  ;; -- so it's worth it for a slightly more robust system.
  (if (null-pointer-p pyobject)
      (return-from lispify nil))
  (let* ((pytype-name-foreign (pyobject-typename/simple pyobject))
         (pytype-name-hash (djb2-foreign-string-hash pytype-name-foreign))
         ;; FIXME: What about names in modules?
         (lispifier (gethash pytype-name-hash *py-type-lispifier-table*)))
    (customize
     (if (functionp lispifier)
         (funcall lispifier pyobject)
         (progn
           (pyuntrack pyobject)
           (make-tracked-pyobject-wrapper pyobject))))))

(defvar *lispifiers*
  ()
  ;; FIXME: Making new objects can be expensive
  "Each entry in the alist *LISPIFIERS* maps from a lisp-type to
a single-argument lisp function. This function takes as input the \"default\" lisp
objects and is expected to appropriately parse it to the corresponding lisp object.

NOTE: This is a new feature and hence unstable; recommended to avoid in production code.")

(defmacro with-lispifiers ((&rest overriding-lispifiers) &body body)
  "Each entry of OVERRIDING-LISPIFIERS is a two-element list of the form

    (TYPE LISPIFIER)

Here, TYPE is unevaluated, while LISPIFIER will be evaluated; the LISPIFIER is expected
to take a default-lispified object (see lisp-python types translation table in docs)
and return the appropriate object user expects.

For example,

    (raw-pyeval \"[1, 2, 3]\") ;=> #(1 2 3) ; the default lispified object
    (with-lispifiers ((vector (lambda (x) (coerce x 'list))))
      (print (raw-pyeval \"[1,2,3]\"))
      (print (raw-pyeval \"5\")))
    ; #(1 2 3) ; default lispified object
    ; (1 2 3)  ; coerced to LIST by the lispifier
    ; 5        ; lispifier uncalled for non-VECTOR
    5

NOTE: This is a new feature and hence unstable; recommended to avoid in production code."
  `(let ((*lispifiers* (list* ,@(loop :for (type lispifier) :in overriding-lispifiers
                                      :collect `(cons ',type ,lispifier))
                              *lispifiers*)))
     ,@body))

(defun customize (object)
  (loop :for (type . lispifier) :in *lispifiers*
        :if (typep object type)
          :do (return-from customize (funcall lispifier object)))
  object)
