;;; eshell-thefuck.el --- Correct the previous eshell command. -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Diego A. Mundo
;; Author: Diego A. Mundo <dieggsy@pm.me>
;; URL: http://github.com/dieggsy/eshell-thefuck
;; Git-Repository: git://github.com/dieggsy/eshell-thefuck
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
(eval-when-compile
  (require 'rx)
  (require 'em-hist)
  (require 'subr-x))

;;* Customization
(defgroup eshell-thefuck nil
  "Correct the previous eshell command."
  :group 'eshell)

(defcustom eshell-thefuck-include-lisp-commands nil
  "If t, include all known emacs-lisp functions in known commands."
  :group 'eshell-thefuck
  :type 'boolean)

(defcustom eshell-thefuck-alter-history nil
  "Replace incorrect command with corrected one in `eshell-history-ring'."
  :group 'eshell-thefuck
  :type 'boolean)

(defcustom eshell-thefuck-alter-buffer nil
  "Directly replace incorrect command with correct one in eshell buffer.

Also erases call to `eshell/fuck'."
  :group 'eshell-thefuck
  :type 'boolean)

(defgroup eshell-thefuck-faces nil
  "Faces for eshell-thefuck"
  :group 'eshell-thefuck)

(defface eshell-thefuck-enter-face '((t (:foreground "#B8BB26")))
  "Face used for enter."
  :group 'eshell-thefuck-faces)

(defface eshell-thefuck-up-down-face '((t (:foreground "#83A598")))
  "Face used for up/down."
  :group 'eshell-thefuck-faces)

(defface eshell-thefuck-c-c-face '((t (:foreground "#FB4933")))
  "Face used for C-c"
  :group 'eshell-thefuck-faces)

;; TODO: implement repetition
;; (defcustom eshell-thefuck-repeat nil
;;   "Whether to attempt running command a second time if it fails after
;; invocation of `eshell-fuck'"
;;   :group 'eshell-thefuck
;;   :type 'boolean)

;;* Types
;;** Command
(defclass eshell-thefuck-command ()
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

(cl-defmethod initialize-instance :after ((command eshell-thefuck-command) &rest _args)
  (with-slots (script) command
    (oset command :script (string-trim script))
    (oset command :script-parts (split-string script nil 'omit-nulls))))

(cl-defmethod eshell-thefuck-update ((command eshell-thefuck-command)
                                     &key
                                     (script (eieio-oref command 'script))
                                     (output (eieio-oref command 'output)))
  (eshell-thefuck-command :script script :output output))

;;** Corrected command
(defclass eshell-thefuck-corrected-command ()
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
;; (cl-defmethod eshell-thefuck--get-script ((corrected eshell-thefuck-corrected-command))
;;   (if eshell-thefuck-repeat
;;       (eshell)))

;;** Rule

(defclass eshell-thefuck-rule ()
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
                :initform nil
                :type (or function null)
                :documentation "Side effect function.")
   (priority :initarg :priority
             :initform 0
             :type integer
             :documentation "Rule priority."))
  "Initializes rule with given fields.")

(cl-defmethod eshell-thefuck-is-match ((rule eshell-thefuck-rule) command)
  (if (and (string-empty-p (eieio-oref command 'output)))
      nil
    (funcall (eieio-oref rule 'match) command)))

(cl-defmethod eshell-thefuck-get-corrected-commands ((rule eshell-thefuck-rule) command)
  (with-slots (get-new-command side-effect priority) rule
    (let ((new-commands (funcall get-new-command command)))
      (when (not (listp new-commands))
        (setq new-commands (list new-commands)))
      (cl-loop for new-command in new-commands
               as n = 0 then (1+ n)
               collect (eshell-thefuck-corrected-command
                        :script new-command
                        :side-effect side-effect
                        :priority (* priority (1+ n)))))))

;;* Utils
(cl-defun eshell-thefuck--parse-plist (plist)
  (let ((known-kwords '(:for-app
                        :sudo-support
                        :match
                        :get-new-command
                        :enabled
                        :side-effect
                        :priority)))
    (append
     (when (stringp (car plist))
       (prog1
           `(:doc ,(car plist))
         (setq plist (cdr plist))))
     (cl-loop
      while plist
      as kword = (car plist)
      as rest = (cdr plist)
      append
      (if (and (keywordp kword)
               (member kword known-kwords))
          (if (member kword '(:match :get-new-command :side-effect))
              (let ((loc (or (cl-position-if #'keywordp rest)
                             (length rest))))
                (setq plist (cl-subseq rest loc))
                (cons kword (list (cl-subseq rest 0 loc))))
            (setq plist (cdr rest))
            (list kword (car rest)))
        (error "Keyword argument %s is not one of %s" kword known-kwords))))))

(cl-defmacro eshell-thefuck-new-rule (name
                                      &rest args)
  (declare (indent defun))
  (let* ((arg-plist (eshell-thefuck--parse-plist args))
         (doc (plist-get arg-plist :doc))
         (for-app (plist-get arg-plist :for-app))
         (sudo-support (plist-get arg-plist :sudo-support))
         (match (plist-get arg-plist :match))
         (get-new-command (plist-get arg-plist :get-new-command))
         (enabled (plist-get arg-plist :enabled))
         (side-effect (plist-get arg-plist :side-effect))
         (priority (plist-get arg-plist :priority))
         (rule-decl
          `(eshell-thefuck-rule
            :match
            ,(if match
                 (progn
                   (when for-app
                     (setq match `((eshell-thefuck--for-app ,for-app :special t ,@match))))
                   (setq match `(with-slots ((<script> script)
                                             (<output> output)
                                             (<parts> script-parts))
                                    <cmd>
                                  ,@match))
                   (setq match `(lambda (<cmd>) ,match))
                   (if sudo-support
                       `(eshell-thefuck--sudo-support ,match)
                     match))
               '#'ignore)
            :get-new-command
            ,(if get-new-command
                 (progn
                   (setq get-new-command `(lambda (<cmd>)
                                            (with-slots ((<script> script)
                                                         (<output> output)
                                                         (<parts> script-parts))
                                                <cmd>
                                              ,@get-new-command)))
                   (if sudo-support
                       `(eshell-thefuck--sudo-support ,get-new-command)
                     get-new-command))
               '#'ignore)
            :enabled ,enabled
            :side-effect
            ,(when side-effect
               `(lambda (<old-cmd> <new-script>)
                  (ignore <new-script> <old-cmd>)
                  (with-slots ((<old-script> script)
                               (<old-output> output)
                               (<old-parts> script-parts))
                      <old-cmd>
                    ,@side-effect)))
            :priority ,(or priority 0)))
         (rule-name (intern (concat "eshell-thefuck-rule-" (symbol-name name)))))
    (if  (boundp rule-name)
        `(progn
           (when ,doc
             (put ',rule-name 'variable-documentation ,doc))
           (setq ,rule-name ,rule-decl))
      `(defvar ,rule-name ,rule-decl
         ,doc))))

(defun eshell-thefuck--get-all-executables ()
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
                      (or eshell-thefuck-include-lisp-commands
                          (string-prefix-p "eshell/" name)))
             (push (s-chop-prefix "eshell/" name) cands)))))
      cands))))

(cl-defun eshell-thefuck--get-closest (word possibilities &key (n 3) (cutoff 0.6) (fallback-to-first t))
  (or (car (difflib-get-close-matches word possibilities :n n :cutoff cutoff))
      (when fallback-to-first
        (car possibilities))))

(defun eshell-thefuck--which (name)
  (let (program
        direct)
    (when (eq (aref name 0) eshell-explicit-command-char)
	  (setq name (substring name 1)
		    direct t))
    (when (and (not direct)
	           (eshell-using-module 'eshell-alias)
		       (funcall (symbol-function 'eshell-lookup-alias)
			            name))
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

(defun eshell-thefuck--get-rules ()
  (let (cands)
    (mapatoms
     (lambda (var)
       (let ((name (symbol-name var)))
         (when (and (or (get var 'variable-documentation)
                        (and (boundp var) (not (keywordp var))))
                    (string-prefix-p "eshell-thefuck-rule-" name)
                    (eshell-thefuck-rule-p (symbol-value var))
                    (eieio-oref (symbol-value var) 'enabled))
           (push (symbol-value var) cands)))))
    cands))

(defun eshell-thefuck--organize-commands (corrected-commands)
  (let ((no-dups (cl-remove-duplicates
                  corrected-commands
                  :key (lambda (x)
                         (eieio-oref x 'script)))))
    (cl-sort no-dups #'< :key (lambda (x) (eieio-oref x 'priority)))))

(defun eshell-thefuck--get-corrected-commands (command)
  (let* ((rules (eshell-thefuck--get-rules))
         (corrected (cl-loop for rule in rules
                             if (eshell-thefuck-is-match rule command)
                             append (eshell-thefuck-get-corrected-commands rule command))))
    (eshell-thefuck--organize-commands corrected)))

(cl-defun eshell-thefuck--get-all-matched-commands (stderr &key (separator "Did you mean"))
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

(defun eshell-thefuck--replace-regexp-in-string (regexp rep string &optional count)
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

(defun eshell-thefuck--replace-argument (script from to)
  (let ((replaced-in-the-end (eshell-thefuck--replace-regexp-in-string
                              (format " %s$" (regexp-quote from))
                              (format " %s" to)
                              script
                              1)))
    (if (not (string= replaced-in-the-end script))
        replaced-in-the-end
      (eshell-thefuck--replace-regexp-in-string (format " %s " (regexp-quote from))
                                                (format " %s " to)
                                                script
                                                1))))

(defun eshell-thefuck--replace-command (command broken matched)
  (with-slots (script) command
    (let ((new-cmds (difflib-get-close-matches broken matched :cutoff 0.1)))
      (mapcar
       (lambda (cmd)
         (eshell-thefuck--replace-argument script broken (string-trim cmd)))
       new-cmds))))

(defun eshell-thefuck--escape-quotes (str)
  (replace-regexp-in-string "\"" "\\\\\"" str))

(cl-defmacro eshell-thefuck--for-app (app-names
                                      &rest
                                      body
                                      &key
                                      (at-least 0)
                                      special
                                      &allow-other-keys)
  (declare (indent defun))
  (cl-remf body :at-least)
  (cl-remf body :special)
  (let ((app-names (if (not (listp app-names)) (list app-names) app-names)))
    (if special
        `(if (and (> (length <parts>) ,at-least)
                  (member (car <parts>)',app-names))
             ,@body
           nil)
      `(with-slots (script-parts) command
         (if (and (> (length script-parts) ,at-least)
                  (member (car script-parts) ',app-names))
             ,@body
           nil)))))

(cl-defmacro eshell-thefuck--sudo-support (func)
  (declare (indent defun))
  `(lambda (command)
     (with-slots (script) command
       (let ((fn ,func))
         (if (not (string-prefix-p "sudo " script))
             (funcall fn command)
           (let ((result
                  (funcall
                   fn
                   (eshell-thefuck-update command
                                          :script
                                          (substring script 5)))))
             (cond ((stringp result)
                    (format "sudo %s" result))
                   ((listp result)
                    (mapcar (lambda (x) (format "sudo %s" x)) result))
                   (t result))))))))

(defun eshell-thefuck--re-find-all (regex string)
  "Find all matches of REGEX in STRING."
  (let ((start-pos 0))
    (cl-loop
     for match-pos = (string-match regex string start-pos)
     while match-pos
     collect (match-string 1 string)
     do (setq start-pos (1+ match-pos)))))

(defun eshell-thefuck--expand-aliases (script)
  "Expand all eshell aliases in SCRIPT."
  ;; Catch duplicate &&
  (setq script
        (replace-regexp-in-string "\\(&& \\)+" "&& " script))
  ;; Find all sub-commands (beginning, parts between && and |)
  (let* ((regex (rx (group (or bol "&&" "|")
                           (opt " ")
                           (minimal-match
                            (1+ nonl)))
                    (or eol "&&" "|")))
         (sub-str (eshell-thefuck--re-find-all regex script))
         (sub-parts (mapcar (lambda (str)
                              (split-string str nil 'omit-nulls "\s"))
                            sub-str)))

    (string-join
     ;; for every sub command, split and expand if possible
     (cl-loop for thing in sub-parts
              as second = (string-match-p "\\(&&\\||\\)" (car thing))
              as cmd = (if second (cadr thing) (car thing))
              as args = (nthcdr (if second 2 1) thing)
              as alias = (eshell-lookup-alias cmd)
              if alias
              collect (concat
                       (when second
                         (format "%s "
                                 (car thing)))
                       (eshell-thefuck--replace-pos-args (cadr alias) args))
              else
              collect (string-join thing " "))
     " ")))

(defun eshell-thefuck--replace-pos-args (string args)
  "Replace positional args ARGS in STRING.

Assumes string is an eshell alias definition."
  (setq string
        (replace-regexp-in-string "\\$\\*" (string-join args " ") string))
  (cl-loop while (string-match-p "\\$\\|\\([[:digit:]]\\|\\*\\)" string)
           as digit = 1 then (1+ digit)
           do (setq string (replace-regexp-in-string (format "$%s" digit)
                                                     (nth (1- digit) args)
                                                     string))
           finally return string))
;;* Rules
;;** no-command
(defun eshell-thefuck--get-used-executables (command)
  (let ((not-corrected (;; We just want to exclude "fuck" and the last
                        ;; command, is cddr may be too much?
                        cddr
                        (ring-elements eshell-history-ring)))
        (executables (eshell-thefuck--get-all-executables)))
    (mapcar
     (lambda (x) (car (split-string x " ")))
     (cl-remove-if
      (lambda (line)
        (or
         (string-prefix-p "fuck" (string-trim line))
         (string-prefix-p "eshell/fuck" (string-trim line))
         (string= (string-trim line) (eieio-oref command 'script))
         (not (member (car (split-string line " ")) executables))))
      not-corrected))))

(eshell-thefuck-new-rule no-command
  "Fixes wrong console commands.

For example, vom -> vim."
  :sudo-support t
  :match
  (let ((cmd (car <parts>)))
    (and (not (eshell-thefuck--which cmd))
         (string-match-p "command not found" <output>)
         (difflib-get-close-matches cmd (eshell-thefuck--get-all-executables))
         t))
  :get-new-command
  (let* ((old-command (car <parts>))
         (already-used (eshell-thefuck--get-closest
                        old-command
                        (eshell-thefuck--get-used-executables <cmd>)
                        :fallback-to-first nil))
         (new-cmds (when already-used (list already-used)))
         (new-commands
          (append new-cmds
                  (cl-remove-if (lambda (cmd)
                                  (member cmd new-cmds))
                                (difflib-get-close-matches
                                 old-command
                                 (eshell-thefuck--get-all-executables))))))
    (mapcar
     (lambda (new-command)
       (string-join
        (append (list new-command) (cdr <parts>))
        " "))
     new-commands))
  :priority 3000
  :enabled t)
;;** git-branch-delete
(eshell-thefuck-new-rule git-branch-delete
  :for-app ("git" "hub")
  :match
  (and (string-match-p (regexp-quote "branch -d") <script>)
       (string-match-p "If you are sure you want to delete it" <output>)
       t)
  :get-new-command
  (eshell-thefuck--replace-argument <script> "-d" "-D")
  :enabled t)
;;** git-branch-exists
(eshell-thefuck-new-rule git-branch-exists
  :for-app ("git" "hub")
  :match
  (and (string-match-p
        "fatal: a branch named '\\([^']*\\)' already exists."
        <output>)

       t)
  :get-new-command
  (let ((branch-name
         (and (string-match
               "fatal: a branch named '\\([^']*\\)' already exists."
               <output>)
              (match-string 1 <output>)))
        (new-command-templates '("git branch -d %s && git branch %s"
                                 "git branch -d %s && git checkout -b %s"
                                 "git branch -D %s && git branch %s"
                                 "git branch -D %s && git checkout -b %s")))
    (cons
     (format "git checkout %s" branch-name)
     (mapcar (lambda (x)
               (format x branch-name branch-name))
             new-command-templates)))
  :enabled t)

;;** git-branch-list
(eshell-thefuck-new-rule git-branch-list
  :for-app ("git" "hub")
  :match
  (and <parts>
       (equal (cl-subseq <parts> 1) '("branch" "list")))
  :get-new-command
  "git branch --delete list && git branch"
  :enabled t)

;;** git-checkout
(defun eshell-thefuck--get-git-branches ()
  (let ((branches (split-string
                   (shell-command-to-string
                    "git branch -a --no-color --no-column")
                   "\n"
                   'omit-nulls)))
    (delete-dups
     (mapcar (lambda (line)
               (when (string-prefix-p "*" line)
                 (setq line (cadr (split-string line " "))))
               (when (string-match-p "/" line)
                 (setq line (car (last (split-string line "/")))))
               (string-trim line))
             branches))))

(eshell-thefuck-new-rule git-checkout
  :for-app ("git" "hub")
  :match
  (and (string-match-p (regexp-quote "did not match any file(s) known to git")
                       <output>)
       (not
        (string-match-p (regexp-quote "Did you forget to 'git add'?")
                        <output>))
       t)
  :get-new-command
  (let* ((missing-file
          (and (string-match
                (concat "error: pathspec '\\([^']*\\)' "
                        "did not match any "
                        (regexp-quote "file(s) known to git."))
                <output>)
               (match-string 1 <output>)))
         (closest-branch
          (eshell-thefuck--get-closest missing-file
                                       (eshell-thefuck--get-git-branches)
                                       :fallback-to-first nil)))
    (list
     (when closest-branch
       (eshell-thefuck--replace-argument <script> missing-file closest-branch))
     (format "git checkout -b %s" missing-file)))
  :enabled t)

;;** git-fix-stash
(eshell-thefuck-new-rule git-fix-stash
  :for-app ("git" "hub")
  :match
  (and <parts>
       (> (length <parts>) 1)
       (string= "stash" (cadr <parts>))
       (string-match-p "usage:" <output>)
       t)
  :get-new-command
  (let* ((stash-commands '("apply"
                           "branch"
                           "clear"
                           "drop"
                           "list"
                           "pop"
                           "save"
                           "show"))
         (stash-cmd (caddr <parts>))
         (fixed (eshell-thefuck--get-closest stash-cmd
                                             stash-commands
                                             :fallback-to-first nil)))
    (list
     (when fixed
       (eshell-thefuck--replace-argument <script> stash-cmd fixed))
     (string-join
      (append
       (cl-subseq <parts> 0 2)
       (cons "save" (nthcdr 2 <parts>)))
      " ")))
  :enabled t)
;;** git-not-command
(eshell-thefuck-new-rule git-not-command
  :for-app ("git" "hub")
  :match
  (and (string-match-p (regexp-quote
                        " is not a git command. See 'git --help'.")
                       <output>)
       (or (string-match-p "The most similar command" <output>)
           (string-match-p "Did you mean" <output>))
       t)
  :get-new-command
  (let* ((broken-cmd
          (and (string-match "git: '\\([^']*\\)' is not a git command"
                             <output>)
               (match-string 1 <output>)))
         (matched (eshell-thefuck--get-all-matched-commands
                   <output>
                   :separator
                   '("The most similar command"
                     "Did you mean"))))
    (eshell-thefuck--replace-command <cmd> broken-cmd matched))
  :enabled t)
;;** git-pull
(eshell-thefuck-new-rule git-pull
  :for-app ("git" "hub")
  :match
  (and (string-match-p "pull" <script>)
       (string-match-p "set-upstream" <output>)
       t)
  :get-new-command
  (let* ((line (car (last (split-string <output> "\n" 'omit-nulls "\s*"))))
         (branch (car (last (split-string line " "))))
         (set-upstream (replace-regexp-in-string
                        "<branch>"
                        branch
                        (replace-regexp-in-string "<remote>" "origin" line))))
    (format "%s && %s" set-upstream <script>))
  :enabled t)
;;** git-push
(defun eshell-thefuck--get-upstream-option-index (command-parts)
  (cond ((member "--set-upstream" command-parts)
         (cl-position "--set-upstream" command-parts :test #'string=))
        ((member "-u" command-parts)
         (cl-position "-u" command-parts :test #'string=))))

(eshell-thefuck-new-rule git-push
  :for-app ("git" "hub")
  :match
  (and (string-match-p "push" <script>)
       (string-match-p "set-upstream" <output>)
       t)
  :get-new-command
  (let* ((command-parts <parts>)
         (upstream-option-index
          (eshell-thefuck--get-upstream-option-index command-parts))
         (arguments (and (string-match "git push \\(.*\\)" <output>)
                         (string-trim
                          (match-string 1 <output>)))))
    (when upstream-option-index
      (pop (nthcdr command-parts upstream-option-index))
      (when (> (length command-parts) (upstream-option-index))
        (pop (nthcdr command-parts upstream-option-index))))
    (eshell-thefuck--replace-argument (string-join command-parts " ")
                                      "push"
                                      (format "push %s" arguments)))
  :enabled t)
;;** git-push-different-branch-names
(eshell-thefuck-new-rule git-push-different-branch-names
  :for-app ("git" "hub")
  :match
  (and (string-match-p "push" <script>)
       (string-match-p "The upstream branch of your current branch does not match"
                       <output>)
       t)
  :get-new-command
  (let* ((all-matches
          (eshell-thefuck--re-find-all "\\(git push [^\s\n]+ [^\s\n]+$\\)"
                                       <output>))
         (branch-name (car (last (split-string (car all-matches) ":")))))
    (cons
     (format "git branch -m %s && git push" branch-name)
     all-matches))
  :enabled t)
;;** git-push-pull
(eshell-thefuck-new-rule git-push-pull
  :for-app ("git" "hub")
  :match
  (and (string-match-p "push" <script>)
       (string-match-p (regexp-quote "! [rejected]") <output>)
       (string-match-p "failed to push some refs to" <output>)
       (or
        (string-match-p "Updates were rejected because the tip of your current branch is behind"
                        <output>)
        (string-match-p "Updates were rejected because the remote constains work that you do"
                        <output>))
       t)
  :get-new-command
  (format "%s && %s"
          (eshell-thefuck--replace-argument <script> "push" "pull")
          <script>)
  :enabled t)
;;** git-push-without-commits
(eshell-thefuck-new-rule git-push-without-commits
  :for-app ("git" "hub")
  :match
  (and (string-match-p "src refspec [^\n\s]+ does not match any\\." <output>)
       t)
  :get-new-command
  (format "git commit -m \"Initial commit\" && %s" <script>)
  :enabled t)
;;** cd-correction
;; TODO: this should be able to replace cd anywhere in the command.
;; TODO: looks like there's too many string-match-p's, pretty sure this
;; should be fixed to one for eshell
;; TODO: check out the behavior of "cd cd foo" in eshell
(eshell-thefuck-new-rule cd-correction
  :sudo-support t
  :for-app "cd"
  :match
  (let ((output (downcase <output>)))
    (or (string-match-p "no such file or directory" output)
        (string-match-p "cd: can't cd to" output)
        (string-match-p "no such directory found" output)
        t))
  :get-new-command
  (let* ((sep (substring (concat (file-name-as-directory "a")
                                 "b")
                         1
                         2))
         (dest (split-string (cadr <parts>)
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
                  (if best-matches
                      (setq cwd (expand-file-name (car best-matches) cwd)))))
    (format "cd \"%s\"" cwd))
  :enabled t)

;;** cd-mkdir
;; TODO: this should be able to replace cd anywhere in the command.
;; TODO: looks like there's too many string-match-p's, pretty sure this
;; should be fixed to one for eshell
(eshell-thefuck-new-rule cd-mkdir
  :sudo-support t
  :for-app "cd"
  :match
  (let ((output (downcase <output>)))
    (or (string-match-p "no such file or directory" output)
        (string-match-p "cd: can't cd to" output)
        (string-match-p "no such directory found" output)
        (string-match-p "the system cannot find the path specified." output)
        t))
  :get-new-command
  (replace-regexp-in-string "^cd \\(.*\\)"
                            "mkdir -p \\1 && cd \\1"
                            <script>)
  :enabled t)
;;** cd-parent
(eshell-thefuck-new-rule cd-parent
  :match
  (string= <script> "cd..")
  :get-new-command
  "cd .."
  :enabled t)

;;** chmod-x
(eshell-thefuck-new-rule chmod-x
  :match
  (and (string-prefix-p "./" <script>)
       (string-match-p "permission denied" (downcase <output>))
       (file-exists-p (car <parts>))
       (not (file-executable-p (car <parts>))))
  :get-new-command
  (format "chmod +x %s && %s" (substring (car <parts>) 2) <script>)
  :enabled t)

;;** cp-omitting-directory
;; TODO: Should be able to replace cp anywhere in command
(eshell-thefuck-new-rule cp-omitting-directory
  :for-app "cp"
  :sudo-support t
  :match
  (let ((output (downcase <output>)))
    (and
     (or (string-match-p "omitting directory" output)
         (string-match-p "is a directory" output))
     t))
  :get-new-command
  (replace-regexp-in-string "^cp" "cp -a" <script>)
  :enabled t)

;;** dirty-untar
(defun eshell-thefuck--is-tar-extract (cmd)
  (or (string-match-p "--extract" cmd)
      (let ((split-cmd (split-string cmd " " 'omit-nulls)))
        (and (> (length split-cmd) 1) (string-match-p "x" (cadr split-cmd))))))

(defvar eshell-thefuck--tar-extensions '(".tar" ".tar.Z" ".tar.bz2" ".tar.gz" ".tar.lz"
                                         ".tar.lzma" ".tar.xz" ".taz" ".tb2" ".tbz" ".tbz2"
                                         ".tgz" ".tlz" ".txz" ".tz"))

(defun eshell-thefuck--tar-file (cmd)
  (let ((c (cl-find-if
            (lambda (c)
              (cl-find-if
               (lambda (suf)
                 (string-suffix-p suf c))
               eshell-thefuck--tar-extensions))
            cmd)))
    (when c
      (list c (car (split-string c "\\." 'omit-nulls))))))

(eshell-thefuck-new-rule dirty-untar
  :for-app "tar"
  :match
  (and (not (string-match-p "-C" <script>))
       (eshell-thefuck--is-tar-extract <script>)
       (eshell-thefuck--tar-file <parts>)
       t)
  :get-new-command
  (let ((dir (eshell-quote-argument
              (cadr (eshell-thefuck--tar-file <parts>)))))
    (format "mkdir -p %s && %s -C %s" dir <script> dir))
  :side-effect
  (when (y-or-n-p "SIDE EFFECT: Remove untarred files?")
    (let ((tar-files
           (split-string
            (shell-command-to-string
             (concat "tar -tf " (car (eshell-thefuck--tar-file
                                      <old-parts>))))
            "\n"
            'omit-nulls)))
      (cl-loop for file in tar-files
               do (ignore-errors
                    (delete-file file)))))
  :enabled t)

;;** sudo
(eshell-thefuck-new-rule sudo
  :match
  (let ((patterns '("permission denied"
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
      (when (and (not (member "&&" <parts>))
                 (string= (car <parts>) "sudo"))
        (cl-return-from match nil))
      (cl-loop
       for pattern in patterns
       if (string-match-p (regexp-quote pattern) <output>)
       do (cl-return-from match t))))
  :get-new-command
  (cond ((string-match-p "&&" <script>)
         (format "sudo sh -c \"%s\""
                 (eshell-thefuck--escape-quotes
                  (string-join (cl-remove-if
                                (lambda (part)
                                  (string= part "sudo"))
                                <parts>)
                               " "))))
        ((string-match-p ">" <script>)
         (format "sudo sh -c \"%s\""
                 (eshell-thefuck--escape-quotes <parts>)))
        (t (format "sudo %s" <script>)))
  :enabled t)

;;** ls-all
(eshell-thefuck-new-rule ls-all
  :for-app "ls"
  :match
  (string= (string-trim <output>) "")
  :get-new-command
  (string-join (append '("ls" "-a") (cdr <parts>)) " ")
  :enabled t)

;;** mkdir-p
(eshell-thefuck-new-rule mkdir-p
  :sudo-support t
  :match
  (and (string-match-p "mkdir" <script>)
       (string-match-p "No such file or directory" <output>)
       t)
  :get-new-command
  (replace-regexp-in-string
   (rx bow "mkdir " (group (0+ nonl))) "mkdir -p \\1" <script>)
  :enabled t)

;;** touch
(eshell-thefuck-new-rule touch
  :for-app "touch"
  :match
  (and (string-match-p "No such file or directory" <output>)
       t)
  :get-new-command
  (let ((path (and (string-match "touch: cannot touch '\\(.+\\)/.+?':"
                                 <output>)
                   (match-string 1 <output>))))
    (format "mkdir -p %s && %s" path <script>))
  :enabled t)

;;** dry
(eshell-thefuck-new-rule dry
  :match
  (and (>= (length <parts>) 2)
       (string= (car <parts>) (cadr <parts>)))
  :get-new-command
  (string-join (cdr <parts>) " ")
  :priority 900
  :enabled t)

;;** pacman
(defun eshell-thefuck--get-pkgfile (parts)
  (let* ((cmd (if (string= (car parts) "sudo") (cadr parts) (car parts)))
         (packages (split-string
                    (shell-command-to-string (concat "pkgfile -b " cmd))
                    "\n"
                    'omit-nulls)))
    packages))

;; TODO: this conflicts with pacman-not-found, example: sudo pacman -S llc
(eshell-thefuck-new-rule pacman
  :match
  (and (string-match-p "command not found" <output>)
       (eshell-thefuck--get-pkgfile <parts>)
       t)
  :get-new-command
  (let ((packages (eshell-thefuck--get-pkgfile <parts>))
        (pacman (if (executable-find "yaourt") "yaourt" "sudo pacman")))
    (mapcar (lambda (package)
              (format "%s -S %s && %s" pacman package <script>))
            packages))
  :enabled (and (executable-find "pkgfile")
                (executable-find "pacman"))
  :priority 3000)

;;** pacman-not-found
(eshell-thefuck-new-rule pacman-not-found
  :sudo-support t
  :match
  (and (or (member (car <parts>)'("pacman" "yaourt"))
           (and (>= (length <parts>) 2)
                (equal (cl-subseq <parts> 0 2) '("sudo" "pacman"))))
       (string-match-p "error: target not found:" <output>)
       t)
  :get-new-command
  (let ((pgr (car (last <parts>))))
    (eshell-thefuck--replace-command command
                                     pgr
                                     (eshell-thefuck--get-pkgfile `(,pgr))))
  :enabled (and (executable-find "pkgfile")
                (executable-find "pacman")))

;;** rm-dir
(eshell-thefuck-new-rule rm-dir
  :sudo-support t
  :match
  (and (member "rm" <parts>)
       (string-match-p "is a directory" (downcase <output>))
       t)
  :get-new-command
  (let ((arguments (if (string-match-p "hdfs" <script>) "-r" "-rf")))
    (replace-regexp-in-string (rx bow "rm " (group (0+ nonl)))
                              (concat "rm " arguments " \\1")
                              <script>))
  :enabled t)

;;** sl-ls
(eshell-thefuck-new-rule sl-ls
  :match
  (string=  <script> "sl")
  :get-new-command
  "ls"
  :enabled t)

;;* UI/backend
(defvar-local eshell-thefuck--old-command nil)

(defvar-local eshell-thefuck--buffer-commands nil)

(defvar-local eshell-thefuck--command-ind nil)

(defvar eshell-thefuck-active-map (let ((map (make-sparse-keymap)))
                                    (define-key map [up] 'eshell-thefuck--selector-prev)
                                    (define-key map [down] 'eshell-thefuck--selector-next)
                                    (define-key map [return] 'eshell-thefuck--selector-select)
                                    (define-key map (kbd "C-c") 'eshell-interrupt-process)
                                    map))

(defun eshell-thefuck--insert-prompt ()
  "Insert prompt for current corrected command.

Command is taken from index `eshell-thefuck--comand-ind' in
buffer-local-variable `eshell-thefuck--buffer-commands'."

  (insert
   (eieio-oref (nth eshell-thefuck--command-ind
                    eshell-thefuck--buffer-commands)
               'script)
   " "
   "["
   (propertize "enter" 'face 'eshell-thefuck-enter-face)
   (if (< 1 (length eshell-thefuck--buffer-commands))
       (concat "/"
               (propertize "↑" 'face 'eshell-thefuck-up-down-face)
               "/"
               (propertize "↓" 'face 'eshell-thefuck-up-down-face))
     "")
   "/"
   (propertize "C-c" 'face 'eshell-thefuck-c-c-face)
   "]"))

(defun eshell-thefuck--selector ()
  "Initialize corected command selector and transient map."
  (eshell-thefuck--insert-prompt)
  (ignore
   (set-transient-map eshell-thefuck-active-map
                      t
                      (lambda ()
                        (eshell-bol)
                        (kill-line)))))

(defun eshell-thefuck--selector-prev ()
  "Display previous corrected command."
  (interactive)
  (setq eshell-thefuck--command-ind
        (if (= eshell-thefuck--command-ind 0)
            (1- (length eshell-thefuck--buffer-commands))
          (1- eshell-thefuck--command-ind)))
  (eshell-bol)
  (kill-line)
  (insert
   (eieio-oref (nth eshell-thefuck--command-ind
                    eshell-thefuck--buffer-commands)
               'script)
   " "
   "["
   (propertize "enter" 'face 'eshell-thefuck-enter-face)
   (if (< 1 (length eshell-thefuck--buffer-commands))
       (concat "/"
               (propertize "↑" 'face 'eshell-thefuck-up-down-face)
               "/"
               (propertize "↓" 'face 'eshell-thefuck-up-down-face))
     "")
   "/"
   (propertize "C-c" 'face 'eshell-thefuck-c-c-face)
   "]"))

(defun eshell-thefuck--selector-next ()
  "Display next corrected command."
  (interactive)
  (setq eshell-thefuck--command-ind
        (if (= eshell-thefuck--command-ind (1- (length eshell-thefuck--buffer-commands)))
            0
          (1+ eshell-thefuck--command-ind)))
  (eshell-bol)
  (kill-line)
  (insert
   (eieio-oref (nth eshell-thefuck--command-ind
                    eshell-thefuck--buffer-commands)
               'script)
   " "
   "["
   (propertize "enter" 'face 'eshell-thefuck-enter-face)
   (if (< 1 (length eshell-thefuck--buffer-commands))
       (concat "/"
               (propertize "↑" 'face 'eshell-thefuck-up-down-face)
               "/"
               (propertize "↓" 'face 'eshell-thefuck-up-down-face))
     "")
   "/"
   (propertize "C-c" 'face 'eshell-thefuck-c-c-face)
   "]"))

(defun eshell-thefuck--selector-select ()
  "Run currently displayed corrected command."
  (interactive)
  (let ((corrected (nth eshell-thefuck--command-ind
                        eshell-thefuck--buffer-commands)))
    (with-slots (side-effect script) corrected
      (when side-effect
        (funcall side-effect eshell-thefuck--old-command script))))
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
         (command (eshell-thefuck-command :script input :output out))
         (corrected-commands (eshell-thefuck--get-corrected-commands command)))
    (if corrected-commands
        (progn
          (setq eshell-thefuck--old-command command
                eshell-thefuck--buffer-commands corrected-commands
                eshell-thefuck--command-ind 0)
          (when eshell-thefuck-alter-buffer
            (eshell-next-prompt -2)
            (let ((inhibit-read-only t))
              (delete-region (line-beginning-position) (point-max))))
          (eshell-thefuck--selector))
      (message "No fucks given!"))))

(provide 'eshell-thefuck)
;;; eshell-thefuck.el ends here
