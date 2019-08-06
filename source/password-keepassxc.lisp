(in-package :next)

(defmacro with-password (password-interface &body body)
  `(if (eq 'keepassxc-interface (type-of ,password-interface))
       (if (null (password ,password-interface))
           (progn
             (setf (symbol-function 'generate-input-html) (symbol-function 'generate-input-html-new))
             (with-result (password-set (read-from-minibuffer
                                         (minibuffer *interface*)
                                         :input-prompt "Password:"))
               (setf (password ,password-interface) password-set)
               (setf (symbol-function 'generate-input-html) (symbol-function 'generate-input-html-original))
               ,@body))
           ,@body)
       ,@body))

(defmethod list-passwords ((password-interface keepassxc-interface))
  (let* ((st (make-string-input-stream (password password-interface)))
         (output (uiop:run-program `("keepassxc-cli" "ls" ,(password-file password-interface))
                                   :input st :output '(:string :stripped t))))
    (remove "Recycle Bin/" (rest (cl-ppcre:split "\\n" output)) :test #'equal)))

(defmethod clip-password ((password-interface keepassxc-interface) password-name)
  (let* ((st (make-string-input-stream (password password-interface)))
         (output (uiop:run-program `("keepassxc-cli" "show" ,(password-file password-interface) ,password-name)
                                   :input st :output '(:string :stripped t))))
    (clip-password-string
     (cl-ppcre:regex-replace "[\\S\\s]*Password: \(.*\)[\\S\\s]*" output "\\1"))))
