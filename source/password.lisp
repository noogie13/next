(in-package :next)

(defclass password-interface ()
  ())

(defclass password-store-interface (password-interface)
  ((password-directory :reader password-directory
                       :initarg :directory
                       :initform (or (uiop:getenv "PASSWORD_STORE_DIR")
                                     (namestring (format nil "~a/.password-store"
                                                         (uiop:getenv "HOME")))))))

(defclass keepassxc-interface (password-interface)
  ())

(defgeneric list-passwords (password-interface)
  (:documentation "Retrieve all available passwords."))

(defgeneric clip-password (password-interface password-name)
  (:documentation "Retrieve specific password by name."))

(defgeneric save-password (password-interface password-name password)
  (:documentation "Save password to database."))

;;; Password-Store implementation.
(defmethod list-passwords ((password-interface password-store-interface))
  (let ((raw-list (directory (format nil "~a/**/*.gpg"
                                     (password-directory password-interface)))))
    (mapcar #'(lambda (x) (cl-ppcre:regex-replace
                           (format nil "(~a)/(.*).gpg"
                                   (password-directory password-interface))
                           (namestring x) "\\2"))
            raw-list)))

(defmethod clip-password ((password-interface password-store-interface) password-name)
  (uiop:run-program `("pass" "--clip" ,password-name)))

(defun copy-password-completion-fn (password-instance)
  (let ((password-list (list-passwords password-instance)))
    (lambda (input)
      (fuzzy-match input password-list))))

(define-command copy-password ()
  "Copy's chosen password from minibuffer."
  (with-result (password-name (read-from-minibuffer
                               (minibuffer *interface*)
                               :completion-function (copy-password-completion-fn (password *interface*))))
    (clip-password (password *interface*) password-name)))

;; to test this, load this file from init, and (setf (password *interface*) (make-instance 'password-store-interface))
