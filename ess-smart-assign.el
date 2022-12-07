;;; ess-smart-assign.el --- replace = by -> in R  -*- lexical-binding: t; -*-

(defmacro esa--with-temporary-insert (text where &rest body)
  "Inserting TEXT after point, execute BODY, delete TEXT.
Returns the value of BODY and does not change point."
  (declare (indent 2) (debug (sexp def-body)))
  (let ((txt (make-symbol "text"))
        (len (make-symbol "text-len"))
        (after (eq where :after)))
    `(let ((,txt ,text)
           (,len ,(if (stringp text) (length text) `(length ,txt))))
       (save-excursion
         ,(if after `(save-excursion (insert ,txt)) `(insert ,txt))
         (prog1 (save-excursion ,@body)
           (delete-char ,(if after len `(- ,len))))))))

(defun ess-smart-inside-call-p ()
  (or (ess-inside-call-p)
      (esa--with-temporary-insert ")" :after (ess-inside-call-p))))

(defun ess-smart-inside-brackets-p ()
  (or (ess-inside-brackets-p)
      (esa--with-temporary-insert "]" :after (ess-inside-brackets-p))))

(defun ess-smart-assign ()
  (interactive)
  (if (or (ess-smart-inside-call-p)
          (ess-smart-inside-brackets-p)
          (ess-inside-string-or-comment-p))
      (insert "=")
    (insert "<-")))


(define-key ess-r-mode-map "=" #'ess-smart-assign)
(define-key inferior-ess-r-mode-map "=" #'ess-smart-assign)

(provide 'ess-smart-assign)
;;; ess-smart-assign.el ends here
