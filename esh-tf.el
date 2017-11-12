(require 'cl-lib)
(require 'esh-tf-utils)
(require 'difflib)

(defcustom esh-tf-include-lisp-commands nil
  "If t, include all known lisp funcions in known commands."
  :group 'esh-tf
  :type 'boolean)

(defcustom esh-tf-alter-history nil
  "If t, replace incorrect command with corrected one in
`eshell-history-ring'."
  :group 'esh-tf
  :type 'boolean)

(defcustom esh-tf-alter-buffer nil
  "If t, directly replace incorrect command with correct one, and
erase call to `eshell/fuck'."
  :group 'esh-tf
  :type 'boolean)

(defface esh-tf-enter-face '((t (:foreground "#B8BB26")))
  "Face used for enter."
  :group 'esh-tf)

(defface esh-tf-up-down-face '((t (:foreground "#83A598")))
  "Face used for up/down."
  :group 'esh-tf)

(defface esh-tf-c-c-face '((t (:foreground "#FB4933")))
  "Face used for C-c"
  :group 'esh-tf)

;; TODO: implement repetition
;; (defcustom esh-tf-repeat nil
;;   "Whether to attempt running command a second time if it fails after
;; invocation of `eshell-fuck'"
;;   :group 'esh-tf
;;   :type 'boolean)

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
  :documentation "Command that should be fixed")

(cl-defmethod initialize-instance :after ((command esh-tf-command) &rest args)
  (oset command :script (string-trim (oref command :script)))
  (oset command :script-parts (split-string (oref command :script)
                                            nil
                                            'omit-nulls)))

(cl-defmethod esh-tf-update ((command esh-tf-command)
                             &key (script (oref command :script))
                             (output (oref command :output)))
  (esh-tf-command :script script :output output))

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
             :documentation "Rule priority.")
   (requires-output :initarg :requires-output
                    :initform nil
                    :type boolean
                    :documentation "Whether rule requires output."))
  :documentation "Initializes rule with given fields.")

(cl-defmethod esh-tf-is-match ((rule esh-tf-rule) command)
  (if (and (string-empty-p (oref command :output)) (oref rule :requires-output))
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
  :documentation "Corrected by rule command.")

;; TODO: implement repetition
;; (cl-defmethod esh-tf--get-script ((corrected esh-tf-corrected-command))
;;   (if esh-tf-repeat
;;       (eshell)))

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
  (esh-tf--insert-prompt)
  (ignore
   (set-transient-map esh-tf-active-map
                      t
                      (lambda ()
                        (eshell-bol)
                        (kill-line)))))

(defun esh-tf--selector-prev ()
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
  (interactive)
  (let ((corrected (nth esh-tf--command-ind esh-tf--buffer-commands)))
    (when (oref corrected :side-effect)
      (funcall (oref corrected :side-effect))))
  (search-backward " ")
  (kill-line)
  (eshell-send-input))

;;;###autoload
(defun eshell/fuck ()
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
         ;; (rules (esh-tf--get-rules))
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
