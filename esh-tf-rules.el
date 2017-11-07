(defvar esh-tf--rule-no-command
  (esh-tf-rule
   :match
   (lambda (command)
     (let ((cmd (car (oref command :script-parts))))
       (and (not (esh-tf--which cmd))
            (string-match-p "not found" (oref command :output))
            (difflib-get-close-matches cmd (esh-tf--get-all-executables)))))
   :get-new-command
   (lambda (command)
     (cl-flet
         ((get-used-executables
           (command)
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
               not-corrected)))))
       (let* ((old-command (car (oref command :script-parts)))
              (already-used (esh-tf--get-closest
                             old-command
                             (get-used-executables command)
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
   :enabled t))

(defvar esh-tf--rule-git-not-command
  (esh-tf-rule
   :match
   (lambda (command)
     (let ((output (oref command :output)))
       (and (string-match-p (regexp-quote " is not a git command. See 'git --help'.") output)
            (or (string-match-p "The most similar command" output)
                (string-match-p "Did you mean" output)))))
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
