(require 'cl-lib)
(require 'eshell)
(require 'difflib)

(defun esh-tf--get-all-executables ()
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
            cands)))))

(cl-defun esh-tf--get-closest (word possibilities &key (n 3) (cutoff 0.6) (fallback-to-first t))
  (or (car (difflib-get-close-matches word possibilities :n n :cutoff cutoff))
      (when fallback-to-first
        (car possibilities))))

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

(defun esh-tf--get-rules ()
  (let (cands)
    (mapatoms
     (lambda (var)
       (let ((name (symbol-name var)))
         (when (and (or (get var 'variable-documentation)
                        (and (boundp var) (not (keywordp var))))
                    (string-prefix-p "esh-tf--rule-" name)
                    (esh-tf-rule-p (symbol-value var))
                    (oref (symbol-value var) :enabled))
           (push (symbol-value var) cands)))))
    cands))

(defun esh-tf--organize-commands (corrected-commands)
  (let ((no-dups (cl-remove-duplicates
                  corrected-commands
                  :key (lambda (x)
                         (oref x :script)))))
    (cl-sort no-dups #'< :key (lambda (x) (oref x :priority)))))

(defun esh-tf--get-corrected-commands (command)
  (let* ((rules (esh-tf--get-rules))
         (corrected (cl-loop for rule in rules
                             if (esh-tf-is-match rule command)
                             append (esh-tf-get-corrected-commands rule command))))
    (esh-tf--organize-commands corrected)))

(cl-defun esh-tf--get-all-matched-commands (stderr &key (separator "Did you mean"))
  (when (not (listp separator))
    (setq separator (list separator)))
  (let (should-yield)
    (cl-loop for line in (split-string stderr "\n" 'omit-nulls " ")
             as clean-for = t
             do (cl-loop named inner
                         for sep in separator
                         if (string-match-p sep line)
                         do (progn (setq should-yield t
                                         clean-for nil)
                                   (cl-return-from inner)))
             if (and clean-for should-yield)
             collect (string-trim line))))

(defun esh-tf--replace-regexp-in-string (regexp rep string &optional count)
  (with-temp-buffer
    (insert string)
    (if count
        (while (/= 0 count)
          (beginning-of-buffer)
          (and (search-forward-regexp regexp nil 'noerror)
               (replace-match rep))
          (setq count (1- count)))
      (beginning-of-buffer)
      (while (search-forward-regexp regexp nil 'noerror)
        (replace-match rep)))
    (buffer-string)))

(defun esh-tf--replace-argument (script from to)
  (let ((replaced-in-the-end (esh-tf--replace-regexp-in-string
                              (format " %s$" (regexp-quote from))
                              (format " %s" to)
                              script
                              1)))
    (if (not (string= replaced-in-the-end script))
        replaced-in-the-end
      (esh-tf--replace-regexp-in-string (format " %s " (regexp-quote from))
                                        (format " %s " to)
                                        script
                                        1))))

(defun esh-tf--replace-command (command broken matched)
  (let ((new-cmds (difflib-get-close-matches broken matched :cutoff 0.1)))
    (mapcar
     (lambda (cmd)
       (esh-tf--replace-argument (oref command :script)
                                 broken
                                 (string-trim cmd)))
     new-cmds)))

;; TODO: implement alias expansion
;; (defun esh-tf--expand-aliases (script)
;;   (let ((aliases eshell-command-aliases-list))
;;     (mapcar (lambda (cell)
;;               (let ((def cadr cell))
;;                 (when string-match-p "\\$\\(?:[[:digit:]]\\|\\*\\)"
;;                       ))))))

(provide 'esh-tf-utils)
