;;;; config.lisp

(defpackage :py4cl2-cffi/config-darwin
  (:documentation "Configures macOS operating systems.")
  (:use #:cl
        #:cl-ppcre
        #:py4cl2-cffi/config)
  (:import-from #:py4cl2-cffi/config
                #:return-value-as-list))
(in-package #:py4cl2-cffi/config-darwin)

(defun python-system ()
  "The path to the Python install or where the virtual environment originates."
  (values-list (return-value-as-list (format nil "~A -c \"
import sys
print(sys.base_exec_prefix)
print(sys.exec_prefix)\"" *python-executable-path*))))

(defun configure ()
  (multiple-value-bind (base-exec-prefix exec-prefix)
      (python-system)
    (let* ((python-version (ppcre:register-groups-bind (version)
			       ("^(\\d+\\.\\d+)" +python-version-string+ :sharedp t)
			     version)))
      (setq py4cl2-cffi/config:*python-ldflags*
            (list (format nil "-L'~A' -L'lib/~A' -l'~A'"
                          exec-prefix
                          base-exec-prefix
                          (format nil "python~A" python-version)))

            py4cl2-cffi/config:*python-compile-command*
            (concatenate
             'string
             "gcc ~A -c -Wall -Werror -fpic py4cl-utils.c && "
             (format
              nil
              "gcc -L'~A/lib' -Wl,-rpath,'~A/lib' -framework CoreFoundation -dynamiclib -o libpy4cl-utils.dylib py4cl-utils.o -lpython~A"
              base-exec-prefix base-exec-prefix python-version))

            py4cl2-cffi/config:*python-numpy-compile-command*
            (concatenate
             'string
             "gcc ~A -I'~A' -c -Wall -Werror -fpic py4cl-numpy-utils.c && "
             (format
              nil
              "gcc -L'~A/lib' -framework CoreFoundation -dynamiclib -o libpy4cl-numpy-utils.dylib py4cl-numpy-utils.o -lpython~A"
              base-exec-prefix python-version))))))



;; One possibility is we avoid checking (software-type) or (uiop:operating-system)
;;   On non-M1, (software-type) is "Darwin".
;;   On M1, is it "64-bit Apple macOS 12.0 (Apple Silicon)”
;; However, quicklisp will try to load all the systems.
;; So, not being able to load this system on non-mac
;; will remove py4cl2-cffi from quicklisp.

;; (software-type) is not portable.
;; (uiop:operating-system) seems reasonably portable.
(when (eq :macosx (uiop:operating-system))
  (configure))
