;;; test-helper.el --- Helpers for eshell-thefuck-test.el

(let ((eshell-thefuck-dir (f-parent (f-dirname (f-this-file)))))
  (add-to-list 'load-path eshell-thefuck-dir))

;;; test-helper.el ends here
