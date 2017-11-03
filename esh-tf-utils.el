(require 'cl-lib)
(require 'eshell)

(defvar esh-tf--all-executables nil)

(defun esh-tf--get-all-executables ()
  (or esh-tf--all-executables
      (setq esh-tf--all-executables
            (delete-dups
             (append
              (mapcar #'file-name-base
                      (cl-loop
                       for path in (split-string (getenv "PATH") ":")
                       when (file-exists-p path)
                       append
                       (cl-remove-if-not
                        #'file-executable-p
                        (directory-files
                         path
                         'full
                         directory-files-no-dot-files-regexp
                         'nosort))))
              (let (cands)
                (mapatoms
                 (lambda (x)
                   (let ((name (symbol-name x)))
                     (when (and (fboundp x)
                                (or esh-tf-include-lisp-commands
                                    (string-prefix-p "eshell/" name)))
                       (push (s-chop-prefix "eshell/" name) cands)))))
                cands))))))

(defun esh-tf--which (name)
  (let (program alias direct)
    (if (eq (aref name 0) eshell-explicit-command-char)
	    (setq name (substring name 1)
		      direct t))
    (if (and (not direct)
	         (eshell-using-module 'eshell-alias)
	         (setq alias
		           (funcall (symbol-function 'eshell-lookup-alias)
			                name)))
	    (setq program t))
    (unless program
      (setq program
            (let* ((esym (eshell-find-alias-function name))
                   (sym (or esym (intern-soft name))))
              (if (and (or esym (and sym (fboundp sym)))
                       (or eshell-prefer-lisp-functions (not direct)))
                  name
                (eshell-search-path name)))))
    program))

(defun esh-tf--format-raw-script (raw-script)
  (string-trim (string-join ra_script " ")))

;; TODO: implement alias expansion
;; (defun esh-tf--expand-aliases (script)
;;   (let ((aliases eshell-command-aliases-list))
;;     (mapcar (lambda (cell)
;;               (let ((def cadr cell))
;;                 (when string-match-p "\\$\\(?:[[:digit:]]\\|\\*\\)"
;;                       ))))))

(defun esh-tf--get-output (script)
  (when (get-buffer " *eshell-tf-process*"))
  (let ((proc (start-process-shell-command "eshell-tf-process" (get-buffer-create) script)))))

(provide 'esh-tf-utils)
