(require 'cl-lib)

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

(defvar esh-tf--rule-no-command
  (esh-tf-rule
   :match
   (esh-tf--sudo-support
     (lambda (command)
       (let ((cmd (car (oref command :script-parts))))
         (and (not (esh-tf--which cmd))
              (string-match-p "not found" (oref command :output))
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
              (new-cmds (when already-used (list already-used))))
         (setq new-commands
               (append new-cmds
                       (cl-remove-if (lambda (cmd)
                                       (member cmd new-cmds))
                                     (difflib-get-close-matches
                                      old-command
                                      (esh-tf--get-all-executables)))))
         (mapcar
          (lambda (new-command)
            (string-join
             (append (list new-command) (cdr (oref command :script-parts)))
             " "))
          new-commands))))
   :priority 3000
   :enabled t))

(defvar esh-tf--rule-git-not-command
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

(defvar esh-tf--rule-cd-correction
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

(defvar esh-tf--rule-cd-mkdir
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

(defvar esh-tf--rule-cd-parent
  (esh-tf-rule
   :match
   (lambda (command)
     (string= (oref command :script) "cd.."))
   :get-new-command
   (lambda (command)
     "cd ..")
   :enabled t))

(defvar esh-tf--rule-chmod-x
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

(defvar esh-tf--rule-cp-omitting-directory
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

(defvar esh-tf--rule-dirty-untar
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
   (lambda (old-cmd command)
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

(defvar esh-tf--rule-sudo
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

(defvar esh-tf--rule-ls-all
  (esh-tf-rule
   :match
   (lambda (command)
     (esh-tf--for-app "ls"
       (string= (string-trim (oref command :output)) "")))
   :get-new-command
   (lambda (command)
     (string-join (append '("ls" "-a") (cdr (oref command :script-parts))) " "))
   :enabled t))

(defvar esh-tf--rule-mkdir-p
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

(defvar esh-tf--rule-touch
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

(defvar esh-tf--rule-dry
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

(defun esh-tf--get-pkgfile (parts)
  (let* ((cmd (if (string= (car parts) "sudo") (cadr parts) (car parts)))
         (packages (split-string
                    (shell-command-to-string (concat "pkgfile -b " cmd))
                    "\n"
                    'omit-nulls)))
    packages))

(defvar esh-tf--rule-pacman
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

(defvar esh-tf--rule-pacman-not-found
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

(defvar esh-tf--rule-rm-dir
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

(defvar esh-tf--rule-sl-ls
  (esh-tf-rule
   :match
   (lambda (command)
     (string= (oref command :script) "sl"))
   :get-new-command
   (lambda (command)
     "ls")
   :enabled t))

