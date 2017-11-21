;;; esh-tf.el --- Correct the previous eshell command. -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Diego A. Mundo
;; Author: Diego A. Mundo <diegoamundo@gmail.com>
;; URL: http://github.com/dieggsy/esh-tf
;; Git-Repository: git://github.com/dieggsy/esh-tf
;; Created: 2017-11-01
;; Version: 0.1.0
;; Keywords:
;; Package-Requires: ((emacs "25"))

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This is a loose port of https://github.com/nvbn/thefuck for eshell.

;;; Code:
(require 'cl-lib)
(require 'difflib)
(require 'eshell)
(require 'em-prompt)
(require 'eieio)

;;* Customization
(defgroup esh-tf nil
  "Correct the previous eshell command."
  :group 'eshell)

(defgroup esh-tf-faces nil
  "Faces for esh-tf"
  :group 'esh-tf)

(defcustom esh-tf-include-lisp-commands nil
  "If t, include all known emacs-lisp functions in known commands."
  :group 'esh-tf
  :type 'boolean)

(defcustom esh-tf-alter-history nil
  "Replace incorrect command with corrected one in `eshell-history-ring'."
  :group 'esh-tf
  :type 'boolean)

(defcustom esh-tf-alter-buffer nil
  "Directly replace incorrect command with correct one in eshell buffer.

Also erases call to `eshell/fuck'."
  :group 'esh-tf
  :type 'boolean)

(defface esh-tf-enter-face '((t (:foreground "#B8BB26")))
  "Face used for enter."
  :group 'esh-tf-faces)

(defface esh-tf-up-down-face '((t (:foreground "#83A598")))
  "Face used for up/down."
  :group 'esh-tf-faces)

(defface esh-tf-c-c-face '((t (:foreground "#FB4933")))
  "Face used for C-c"
  :group 'esh-tf-faces)

;; TODO: implement repetition
;; (defcustom esh-tf-repeat nil
;;   "Whether to attempt running command a second time if it fails after
;; invocation of `eshell-fuck'"
;;   :group 'esh-tf
;;   :type 'boolean)

;;* Types
;;** Command
(defclass esh-tf-command ()
  ((script :initarg :script
           :initform ""
           :type string
           :documentation "Command run.")
   (output :initarg :output
           :initform ""
           :type string
           :documentation "Output of command.")
   (script-parts :initarg :script-parts
                 :initform nil
                 :type list
                 :documentation "Parts of command."))
  "Command that should be fixed")

(cl-defmethod initialize-instance :after ((command esh-tf-command) &rest _args)
  (oset command :script (string-trim (oref command :script)))
  (oset command :script-parts (split-string (oref command :script)
                                            nil
                                            'omit-nulls)))

(cl-defmethod esh-tf-update ((command esh-tf-command)
                             &key (script (oref command :script))
                             (output (oref command :output)))
  (esh-tf-command :script script :output output))

;;** Corrected command
(defclass esh-tf-corrected-command ()
  ((script :initarg :script
           :initform ""
           :type string
           :documentation "Command to run.")
   (side-effect :initarg :side-effect
                :initform nil
                :type (or null function)
                :documentation "Side effect of command.")
   (priority :initarg :priority
             :initform 0
             :type integer
             :documentation "New command priority."))
  "Corrected by rule command.")

;; TODO: implement repetition
;; (cl-defmethod esh-tf--get-script ((corrected esh-tf-corrected-command))
;;   (if esh-tf-repeat
;;       (eshell)))

;;** Rule

(defclass esh-tf-rule ()
  ((match :initarg :match
          :initform ignore
          :type function
          :docuemntation "Function that determines whether command matches.")
   (get-new-command :initarg :get-new-command
                    :initform ignore
                    :type function
                    :documentation "Function that gets the new command for current command.")
   (enabled :initarg :enabled
            :initform nil
            :type t
            :documentation "Whether rule is enabled.")
   (side-effect :initarg :side-effect
                :initform ignore
                :type function
                :documentation "Side effect function.")
   (priority :initarg :priority
             :initform 0
             :type integer
             :documentation "Rule priority."))
  "Initializes rule with given fields.")

(cl-defmethod esh-tf-is-match ((rule esh-tf-rule) command)
  (if (and (string-empty-p (oref command :output)))
      nil
    (funcall (oref rule :match) command)))

(cl-defmethod esh-tf-get-corrected-commands ((rule esh-tf-rule) command)
  (let ((new-commands (funcall (oref rule :get-new-command) command)))
    (when (not (listp new-commands))
      (setq new-commands (list new-commands)))
    (cl-loop for new-command in new-commands
             as n = 0 then (1+ n)
             collect (esh-tf-corrected-command
                      :script new-command
                      :side-effect (oref rule :side-effect)
                      :priority (* (oref rule :priority) (1+ n))))))

;;* Utils
(defun esh-tf--get-all-executables ()
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
      cands))))

(cl-defun esh-tf--get-closest (word possibilities &key (n 3) (cutoff 0.6) (fallback-to-first t))
  (or (car (difflib-get-close-matches word possibilities :n n :cutoff cutoff))
      (when fallback-to-first
        (car possibilities))))

(defun esh-tf--which (name)
  (let (program
        alias
        direct)
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
                    (string-prefix-p "esh-tf-rule-" name)
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
          (goto-char (point-min))
          (and (search-forward-regexp regexp nil 'noerror)
               (replace-match rep))
          (setq count (1- count)))
      (goto-char (point-min))
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

(defun esh-tf--escape-quotes (str)
  (replace-regexp-in-string "\"" "\\\\\"" str))

(cl-defmacro esh-tf--for-app (app-names &rest body &key (at-least 0) &allow-other-keys)
  (declare (indent defun))
  (cl-remf body :at-least)
  (let ((app-names (if (not (listp app-names)) (list app-names) app-names)))
    `(if (and (> (length (oref command :script-parts)) ,at-least)
              (member (car (oref command :script-parts)) ',app-names))
         ,@body
       nil)))

(cl-defmacro esh-tf--sudo-support (func)
  (declare (indent defun))
  `(lambda (command)
     (let ((fn ,func))
       (if (not (string-prefix-p "sudo " (oref command :script)))
           (funcall fn command)
         (let ((result
                (funcall
                 fn
                 (esh-tf-update command
                                :script
                                (substring (oref command :script) 5)))))
           (cond ((stringp result)
                  (format "sudo %s" result))
                 ((listp result)
                  (mapcar (lambda (x) (format "sudo %s" x)) result))
                 (t result)))))))

;; TODO: implement alias expansion
;; (defun esh-tf--expand-aliases (script)
;;   (let ((aliases eshell-command-aliases-list))
;;     (mapcar (lambda (cell)
;;               (let ((def cadr cell))
;;                 (when string-match-p "\\$\\(?:[[:digit:]]\\|\\*\\)"
;;                       ))))))

;;* Rules
;;** no-command
(defun esh-tf--get-used-executables (command)
  (let ((not-corrected (;; We just want to exclude "fuck" and the last
                        ;; command, is cddr may be too much?
                        cddr
                        (ring-elements eshell-history-ring)))
        (executables (esh-tf--get-all-executables)))
    (mapcar
     (lambda (x) (car (split-string x " ")))
     (cl-remove-if
      (lambda (line)
        (or
         (string-prefix-p "fuck" (string-trim line))
         (string-prefix-p "eshell/fuck" (string-trim line))
         (string= (string-trim line) (oref command :script))
         (not (member (car (split-string line " ")) executables))))
      not-corrected))))

(defvar esh-tf-rule-no-command
  (esh-tf-rule
   :match
   (esh-tf--sudo-support
     (lambda (command)
       (let ((cmd (car (oref command :script-parts))))
         (and (not (esh-tf--which cmd))
              (string-match-p "command not found" (oref command :output))
              (difflib-get-close-matches cmd (esh-tf--get-all-executables))
              t))))
   :get-new-command
   (esh-tf--sudo-support
     (lambda (command)
       (let* ((old-command (car (oref command :script-parts)))
              (already-used (esh-tf--get-closest
                             old-command
                             (esh-tf--get-used-executables command)
                             :fallback-to-first nil))
              (new-cmds (when already-used (list already-used)))
              (new-commands
               (append new-cmds
                       (cl-remove-if (lambda (cmd)
                                       (member cmd new-cmds))
                                     (difflib-get-close-matches
                                      old-command
                                      (esh-tf--get-all-executables))))))
         (mapcar
          (lambda (new-command)
            (string-join
             (append (list new-command) (cdr (oref command :script-parts)))
             " "))
          new-commands))))
   :priority 3000
   :enabled t))

;;** git-not-command
(defvar esh-tf-rule-git-not-command
  (esh-tf-rule
   :match
   (lambda (command)
     (esh-tf--for-app ("git" "hub")
       (let ((output (oref command :output)))
         (and (string-match-p (regexp-quote " is not a git command. See 'git --help'.") output)
              (or (string-match-p "The most similar command" output)
                  (string-match-p "Did you mean" output))))))
   :get-new-command
   (lambda (command)
     (let* ((output (oref command :output))
            (broken-cmd (and (string-match "git: '\\([^']*\\)' is not a git command"
                                           output)
                             (match-string 1 output)))
            (matched (esh-tf--get-all-matched-commands
                      output
                      :separator
                      '("The most similar command"
                        "Did you mean"))))
       (message "matched: %S" matched)
       (esh-tf--replace-command command broken-cmd matched)))
   :enabled t))

;; (defvar esh-tf-rule-git-)

;;** cd-correction
(defvar esh-tf-rule-cd-correction
  ;; TODO: this should be able to replace cd anywhere in the command.
  ;; TODO: looks like there's too many string-match-p's, pretty sure this
  ;; should be fixed to one for eshell
  ;; TODO: check out the behavior of "cd cd foo" in eshell
  (esh-tf-rule
   :match
   (lambda (command)
     (esh-tf--for-app "cd"
       (let ((output (downcase (oref command :output))))
         (or (string-match-p "no such file or directory" output)
             (string-match-p "cd: can't cd to" output)
             (string-match-p "no such directory found" output)))))
   :get-new-command
   (lambda (command)
     (let* ((sep (substring (concat (file-name-as-directory "a")
                                    "b")
                            1
                            2))
            (dest (split-string (cadr (oref command :script-parts))
                                sep
                                'omit-nulls))
            (cwd default-directory))
       (when (string= (car (last dest)) "")
         (setq dest (butlast dest)))
       (cl-loop for directory in dest
                as best-matches = nil
                do (cl-block body
                     (cond ((string= directory ".")
                            (cl-return-from body))
                           ((string= directory "..")
                            (setq cwd (car (split-string cwd sep 'omit-nulls)))
                            (cl-return-from body)))
                     (message "%S" directory)
                     (setq best-matches
                           (difflib-get-close-matches
                            directory
                            (mapcar
                             #'file-name-nondirectory
                             (cl-remove-if-not
                              #'file-directory-p
                              (directory-files
                               cwd
                               'full
                               nil
                               'nosort)))))
                     (message "%S" (car best-matches))
                     (if best-matches
                         (setq cwd (expand-file-name (car best-matches) cwd)))))
       (format "cd \"%s\"" cwd)))
   :enabled t))

;;** cd-mkdir
(defvar esh-tf-rule-cd-mkdir
  ;; TODO: this should be able to replace cd anywhere in the command.
  ;; TODO: looks like there's too many string-match-p's, pretty sure this
  ;; should be fixed to one for eshell
  (esh-tf-rule
   :match
   (lambda (command)
     (esh-tf--for-app "cd"
       (let ((output (downcase (oref command :output))))
         (or (string-match-p "no such file or directory" output)
             (string-match-p "cd: can't cd to" output)
             (string-match-p "no such directory found" output)
             (string-match-p "the system cannot find the path specified." output)))))
   :get-new-command
   (lambda (command)
     (replace-regexp-in-string "^cd \\(.*\\)"
                               "mkdir -p \\1 && cd \\1"
                               (oref command :script)))
   :enabled t))
;;** cd-parent
(defvar esh-tf-rule-cd-parent
  (esh-tf-rule
   :match
   (lambda (command)
     (string= (oref command :script) "cd.."))
   :get-new-command
   (lambda (_command)
     "cd ..")
   :enabled t))

;;** chmod-x
(defvar esh-tf-rule-chmod-x
  (esh-tf-rule
   :match
   (lambda (command)
     (let ((script-parts (oref command :script-parts)))
       (and (string-prefix-p "./" (oref command :script))
            (string-match-p "permission denied" (downcase (oref command :output)))
            (file-exists-p (car script-parts))
            (not (file-executable-p (car script-parts))))))
   :get-new-command
   (lambda (command)
     (format "chmod +x %s && %s"
             (substring (car (oref command :script-parts)) 2)
             (oref command :script)))
   :enabled t))

;;** cp-omitting-directory
(defvar esh-tf-rule-cp-omitting-directory
  ;; TODO: Should be able to replace cp anywhere in command
  (esh-tf-rule
   :match
   (esh-tf--sudo-support
     (lambda (command)
       (esh-tf--for-app "cp"
         (let ((output (downcase (oref command :output))))
           (and
            (or (string-match-p "omitting directory" output)
                (string-match-p "is a directory" output))
            t)))))
   :get-new-command
   (esh-tf--sudo-support
     (lambda (command)
       (replace-regexp-in-string "^cp" "cp -a" (oref command :script))))
   :enabled t))

;;** dirty-untar
(defun esh-tf--is-tar-extract (cmd)
  (or (string-match-p "--extract" cmd)
      (let ((split-cmd (split-string cmd " " 'omit-nulls)))
        (and (> (length split-cmd) 1) (string-match-p "x" (cadr split-cmd))))))

(defvar esh-tf--tar-extensions '(".tar" ".tar.Z" ".tar.bz2" ".tar.gz" ".tar.lz"
                                 ".tar.lzma" ".tar.xz" ".taz" ".tb2" ".tbz" ".tbz2"
                                 ".tgz" ".tlz" ".txz" ".tz"))

(defun esh-tf--tar-file (cmd)
  (let ((c (cl-find-if
            (lambda (c)
              (cl-find-if
               (lambda (suf)
                 (string-suffix-p suf c))
               esh-tf--tar-extensions))
            cmd)))
    (when c
      (list c (car (split-string c "\\." 'omit-nulls))))))

(defvar esh-tf-rule-dirty-untar
  (esh-tf-rule
   :match
   (lambda (command)
     (esh-tf--for-app "tar"
       (let ((script (oref command :script)))
         (and (not (string-match-p "-C" script))
              (esh-tf--is-tar-extract script)
              (esh-tf--tar-file (oref command :script-parts))))))
   :get-new-command
   (lambda (command)
     (let ((dir (eshell-quote-argument (cadr (esh-tf--tar-file
                                              (oref command :script-parts))))))
       (format "mkdir -p %s && %s -C %s" dir (oref command :script) dir)))
   :side-effect
   (lambda (old-cmd _command)
     (let ((tar-files
            (split-string
             (shell-command-to-string
              (concat "tar -tf " (car (esh-tf--tar-file
                                       (oref old-cmd :script-parts)))))
             "\n"
             'omit-nulls)))
       (message "FILES: %S" tar-files)
       (cl-loop for file in tar-files
                do (ignore-errors
                     (delete-file file)))))
   :enabled t))

;;** sudo
(defvar esh-tf-rule-sudo
  (esh-tf-rule
   :match
   (lambda (command)
     (let ((output (downcase (oref command :output)))
           (script-parts (oref command :script-parts))
           (patterns '("permission denied"
                       "eacces"
                       "pkg: insufficient privileges"
                       "you cannot perform this operation unless you are root"
                       "non-root users cannot"
                       "operation not permitted"
                       "root privilege"
                       "this command has to be run under the root user."
                       "this operation requires root."
                       "requested operation requires superuser privilege"
                       "must be run as root"
                       "must run as root"
                       "must be superuser"
                       "must be root"
                       "need to be root"
                       "need root"
                       "needs to be run as root"
                       "only root can "
                       "you don\"t have access to the history db."
                       "authentication is required"
                       "edspermissionerror"
                       "you don\"t have write permissions"
                       "use `sudo`"
                       "SudoRequiredError"
                       "error: insufficient privileges")))
       (cl-block match
         (when (and (not (member "&&" script-parts))
                    (string= (car script-parts) "sudo"))
           (cl-return-from match nil))
         (cl-loop
          for pattern in patterns
          if (string-match-p (regexp-quote pattern) output)
          do (cl-return-from match t)))))
   :get-new-command
   (lambda (command)
     (let ((script (oref command :script)))
       (cond ((string-match-p "&&" script)
              (format "sudo sh -c \"%s\""
                      (esh-tf--escape-quotes
                       (string-join (cl-remove-if
                                     (lambda (part)
                                       (string= part "sudo"))
                                     (oref command :script-parts))
                                    " "))))
             ((string-match-p ">" script)
              (format "sudo sh -c \"%s\""
                      (esh-tf--escape-quotes (oref command :script-parts))))
             (t (format "sudo %s" script)))))
   :enabled t))

;;** ls-all
(defvar esh-tf-rule-ls-all
  (esh-tf-rule
   :match
   (lambda (command)
     (esh-tf--for-app "ls"
       (string= (string-trim (oref command :output)) "")))
   :get-new-command
   (lambda (command)
     (string-join (append '("ls" "-a") (cdr (oref command :script-parts))) " "))
   :enabled t))

;;** mkdir-p
(defvar esh-tf-rule-mkdir-p
  (esh-tf-rule
   :match
   (esh-tf--sudo-support
     (lambda (command)
       (and (string-match-p "mkdir" (oref command :script))
            (string-match-p "No such file or directory" (oref command :output))
            t)))
   :get-new-command
   (esh-tf--sudo-support
     (lambda (command)
       (replace-regexp-in-string (rx bow "mkdir " (group (0+ nonl)))
                                 "mkdir -p \\1"
                                 (oref command :script))))
   :enabled t))

;;** touch
(defvar esh-tf-rule-touch
  (esh-tf-rule
   :match
   (lambda (command)
     (esh-tf--for-app "touch"
       (and (string-match-p "No such file or directory" (oref command :output)))))
   :get-new-command
   (lambda (command)
     (let* ((output (oref command :output))
            (path (and (string-match "touch: cannot touch '\\(.+\\)/.+?':"
                                     output)
                       (match-string 1 output))))
       (format "mkdir -p %s && %s" path (oref command :script))))
   :enabled t))

;;** dry
(defvar esh-tf-rule-dry
  (esh-tf-rule
   :match
   (lambda (command)
     (let ((split-command (oref command :script-parts)))
       (and (>= (length split-command) 2)
            (string= (car split-command) (cadr split-command)))))
   :get-new-command
   (lambda (command)
     (string-join (cdr (oref command :script-parts)) " "))
   :priority 900
   :enabled t))

;;** pacman
(defun esh-tf--get-pkgfile (parts)
  (let* ((cmd (if (string= (car parts) "sudo") (cadr parts) (car parts)))
         (packages (split-string
                    (shell-command-to-string (concat "pkgfile -b " cmd))
                    "\n"
                    'omit-nulls)))
    packages))

(defvar esh-tf-rule-pacman
  ;; TODO: this conflicts with pacman-not-found, example: sudo pacman -S llc
  (esh-tf-rule
   :match
   (lambda (command)
     (and (string-match-p "command not found" (oref command :output))
          (esh-tf--get-pkgfile (oref command :script-parts))))
   :get-new-command
   (lambda (command)
     (let ((script (oref command :script))
           (packages (esh-tf--get-pkgfile (oref command :script-parts)))
           (pacman (if (executable-find "yaourt") "yaourt" "sudo pacman")))
       (mapcar (lambda (package)
                 (format "%s -S %s && %s" pacman package script))
               packages)))
   :enabled (and (executable-find "pkgfile")
                 (executable-find "pacman"))))

;;** pacman-not-found
(defvar esh-tf-rule-pacman-not-found
  (esh-tf-rule
   :match
   (lambda (command)
     (let ((script-parts (oref command :script-parts)))
       (and (or (member (car script-parts) '("pacman" "yaourt"))
                (and (>= (length script-parts) 2)
                     (equal (cl-subseq script-parts 0 2) '("sudo" "pacman"))))
            (string-match-p "error: target not found:" (oref command :output)))))
   :get-new-command
   (lambda (command)
     (let ((pgr (car (last (oref command :script-parts)))))
       (esh-tf--replace-command command pgr (esh-tf--get-pkgfile `(,pgr)))))
   :enabled (and (executable-find "pkgfile")
                 (executable-find "pacman"))))

;;** rm-dir
(defvar esh-tf-rule-rm-dir
  (esh-tf-rule
   :match
   (lambda (command)
     (and (member "rm" (oref command :script-parts))
          (string-match-p "is a directory" (downcase (oref command :output)))))
   :get-new-command
   (lambda (command)
     (let* ((script (oref command :script))
            (arguments (if (string-match-p "hdfs" script) "-r" "-rf")))
       (replace-regexp-in-string (rx bow "rm " (group (0+ nonl)))
                                 (concat "rm " arguments " \\1")
                                 script)))
   :enabled t))

;;** sl-ls
(defvar esh-tf-rule-sl-ls
  (esh-tf-rule
   :match
   (lambda (command)
     (string= (oref command :script) "sl"))
   :get-new-command
   (lambda (_command)
     "ls")
   :enabled t))
(defvar-local esh-tf--old-command nil)

(defvar-local esh-tf--buffer-commands nil)

(defvar-local esh-tf--command-ind nil)

(defvar esh-tf-active-map (let ((map (make-sparse-keymap)))
                            (define-key map [up] 'esh-tf--selector-prev)
                            (define-key map [down] 'esh-tf--selector-next)
                            (define-key map [return] 'esh-tf--selector-select)
                            (define-key map (kbd "C-c") 'eshell-interrupt-process)
                            map))

(defun esh-tf--insert-prompt ()
  "Insert prompt for current corrected command.

Command is taken from index `esh-tf--comand-ind' in
buffer-local-variable `esh-tf--buffer-commands'."

  (insert
   (oref (nth esh-tf--command-ind esh-tf--buffer-commands) :script)
   " "
   "["
   (propertize "enter" 'face 'esh-tf-enter-face)
   (if (< 1 (length esh-tf--buffer-commands))
       (concat "/"
               (propertize "↑" 'face 'esh-tf-up-down-face)
               "/"
               (propertize "↓" 'face 'esh-tf-up-down-face))
     "")
   "/"
   (propertize "C-c" 'face 'esh-tf-c-c-face)
   "]"))

(defun esh-tf--selector ()
  "Initialize corected command selector and transient map."
  (esh-tf--insert-prompt)
  (ignore
   (set-transient-map esh-tf-active-map
                      t
                      (lambda ()
                        (eshell-bol)
                        (kill-line)))))

(defun esh-tf--selector-prev ()
  "Display previous corrected command."
  (interactive)
  (setq esh-tf--command-ind
        (if (= esh-tf--command-ind 0)
            (1- (length esh-tf--buffer-commands))
          (1- esh-tf--command-ind)))
  (eshell-bol)
  (kill-line)
  (insert
   (oref (nth esh-tf--command-ind esh-tf--buffer-commands) :script)
   " "
   "["
   (propertize "enter" 'face 'esh-tf-enter-face)
   (if (< 1 (length esh-tf--buffer-commands))
       (concat "/"
               (propertize "↑" 'face 'esh-tf-up-down-face)
               "/"
               (propertize "↓" 'face 'esh-tf-up-down-face))
     "")
   "/"
   (propertize "C-c" 'face 'esh-tf-c-c-face)
   "]"))

(defun esh-tf--selector-next ()
  "Display next corrected command."
  (interactive)
  (setq esh-tf--command-ind
        (if (= esh-tf--command-ind (1- (length esh-tf--buffer-commands)))
            0
          (1+ esh-tf--command-ind)))
  (eshell-bol)
  (kill-line)
  (insert
   (oref (nth esh-tf--command-ind esh-tf--buffer-commands) :script)
   " "
   "["
   (propertize "enter" 'face 'esh-tf-enter-face)
   (if (< 1 (length esh-tf--buffer-commands))
       (concat "/"
               (propertize "↑" 'face 'esh-tf-up-down-face)
               "/"
               (propertize "↓" 'face 'esh-tf-up-down-face))
     "")
   "/"
   (propertize "C-c" 'face 'esh-tf-c-c-face)
   "]"))

(defun esh-tf--selector-select ()
  "Run currently displayed corrected command."
  (interactive)
  (let ((corrected (nth esh-tf--command-ind esh-tf--buffer-commands)))
    (when (oref corrected :side-effect)
      (funcall (oref corrected :side-effect))))
  (search-backward " ")
  (kill-line)
  (eshell-send-input))

;;* User
;;;###autoload
(defun eshell/fuck ()
  "Correct previous eshell command."
  (let* ((debug-on-error t)
         (out-start (save-excursion (eshell-next-prompt -2)
                                    (forward-line)
                                    (line-beginning-position)))
         (out-end (save-excursion (eshell-next-prompt -1)
                                  (line-beginning-position)))
         (out (buffer-substring-no-properties
               out-start
               (- out-end 1)))
         (input (save-excursion (eshell-next-prompt -2)
                                (buffer-substring-no-properties
                                 (point)
                                 (line-end-position))))
         (command (esh-tf-command :script input :output out))
         (corrected-commands (esh-tf--get-corrected-commands command)))
    (if corrected-commands
        (progn
          (setq esh-tf--old-command command
                esh-tf--buffer-commands corrected-commands
                esh-tf--command-ind 0)
          (when esh-tf-alter-buffer
            (eshell-next-prompt -2)
            (let ((inhibit-read-only t))
              (delete-region (line-beginning-position) (point-max))))
          (esh-tf--selector))
      (message "No fucks given!"))))

(provide 'esh-tf)
;;; esh-tf.el ends here
