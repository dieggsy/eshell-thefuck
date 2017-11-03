(require 'cl-lib)
(require 'esh-tf-utils)
;; (require 'difflib)

(defcustom esh-tf-include-lisp-commands nil
  "If t, include all known lisp funcions in known commands."
  :group 'esh-tf
  :type 'boolean)

;;;###autoload
(defun eshell/fuck ()
  (let* ((out-start (save-excursion (eshell-next-prompt -2)
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
         (rules (esh-tf--get-rules))
         (command `(:command ,input :parts ,(split-string input) :output ,out)))
    (cl-loop for rule in rules
             when (funcall rule command)
             append (funcall rule command 'corrections))))


(defun esh-tf--get-rules ()
  (cl-remove-if-not
   #'fboundp
   (mapcar
    (lambda (cell)
      (intern (concat "esh-tf--rule-" (symbol-name (car cell)))))
    esh-tf-rules)))


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
                 :documentation "Parts of command.")))

(defmethod esh-tf-script-parts ((command esh-tf-command))
  (when (not (oref command :script-parts))
    (condition-case err
        (oset command :script-parts (split-string (oref command :script)))
      (warn "Can't split command because: %s"
            (error-message-string err)))
    (oref command :script-parts)))

(defun esh-tf-from-raw-script (raw-script)
  (let ((script (esh-tf--format-raw-script raw-script)))
    (when (not script)
      (throw 'esh-tf-empty-command))
    (let (;; TODO: implement alias expansion
          ;; (expanded (eshell-tf--expand-aliases raw-script))
          (output (eshell-tf--get-output script)))
      (esh-tf-command :script expanded :output output))))

