;;;
;;;   Time-stamp: <>
;;;   Touched: Sun Jan 1 11:35:32 2023 +0530 <enometh@net.meer>
;;;   Bugs-To: enometh@net.meer
;;;   Status: Experimental.  Do not redistribute
;;;   Copyright (C) 2024 Madhu.  All Rights Reserved.
;;;
;;;
;; py4cl2-cffi-send.el - advice python-shell-send-region (from
;; python-mode.el) to send python expressions from a python buffer via
;; sly

(when nil
(sly-interactive-eval "(+ 2 3)")
(ad-unadvise 'python-shell-send-region)
(ad-deactivate 'python-shell-send-region)
(sly-interactive-eval (format "(py4cl2-cffi::raw-pyeval %S)" "10+20"))
(ad-activate 'python-shell-send-region)
(sly-interactive-eval (format "(list 1 #\\a)"))
(sly-eval-with-transcript `(cl:+ 2 3)))

(defadvice python-shell-send-region (around
				     py4cl2-send-advice
				     (start end &optional e send-main
					    no-cookie)
				     activate)
  "Send the region delimited by START and END to python via py4cl2-cffi
via sly-interactive-eval and py4cl2-cffi::raw-py.

When called with a prefix arg evaluate the argument call raw-py as e for
eval (assigns to _ and returns the value) otherwise call raw-py as x for exec.

When optional argument SEND-MAIN is non-nil, allow execution of
code inside blocks delimited by \"if __name__== \\='__main__\\=':\"."
  (interactive
   (list (region-beginning) (region-end) current-prefix-arg nil t))
  (let* ((string (python-shell-buffer-substring start end (not send-main)
                                                no-cookie))
         (original-string (buffer-substring-no-properties start end))
         (_ (string-match "\\`\n*\\(.*\\)" original-string)))
    (message "Sent: %s..." (match-string 1 original-string))
    ;; we can't call sly-interactive-eval because it'll get the prefix
    ;; arg. we can't call eval-with-transcript directly on an elisp
    ;; sexp because we can't send common lisp characters.
    (sly-eval-with-transcript `(slynk:interactive-eval
				,(format "(py4cl2-cffi::raw-py #\\%c %S)"
					 (if e ?e ?x)
					 original-string)))
    (deactivate-mark)))

;; side note: fix .inputrc for emacs M-x run-python
;;
;; # https://debbugs.gnu.org/cgi/bugreport.cgi?bug=24401
;; $if Bash
;; set colored-completion-prefix on
;; set colored-stats on
;; $endif


(defun python-shell-send-line ()
  "Send LINE to inferior Python PROCESS."
  (interactive)
  (python-shell-send-region (line-beginning-position) (line-end-position)))

;; (lookup-key python-mode-map (kbd "C-c C-l")) ;python-shell-send-file
;; (define-key python-mode-map (kbd "C-c C-l") 'python-shell-send-line)