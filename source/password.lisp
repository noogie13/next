(in-package :next)

(defvar *password-instance* nil)

(defclass password-interface ()
  ())

(defclass password-store-interface (password-interface)
  ((password-directory
    :initarg :directory
    :initform (if (uiop:getenv "PASSWORD_STORE_DIR")
                  (uiop:getenv "PASSWORD_STORE_DIR")
                  (namestring (format nil "~a/.password-store"
                                      (uiop:getenv "HOME"))))
    :reader password-directory)))

(defclass keepassxc-interface (password-interface)
  ())

(defgeneric list-passwords (password-interface)
  (:documentation "Retrieve all available passwords."))

(defgeneric clip-password (password-interface password-name)
  (:documentation "Retrieve specific password by name."))

(defmethod clip-password ((password-interface password-store-interface) password-name)
  (uiop:run-program `("pass" "-c" ,password-name)))

(defmethod list-passwords ((password-interface password-store-interface))
  (let ((raw-list (directory (concatenate 'string (password-directory password-store-interface)
                                          "/**/*.gpg"))))
    (mapcar #'(lambda (x) (cl-ppcre:regex-replace
                           (concatenate 'string "(" (password-directory *password-instance*) ")/(.*).gpg")
                           (namestring x) "\\2"))
            raw-list)))

(defun copy-password-completing-fn (password-instance)
  (let ((password-list (list-passwords password-instance)))
    (lambda (input)
      (fuzzy-match input password-list))))

(define-command list-and-copy-password ()
  "Lists passwords, then copies chosen password."
  (with-result (password-name (read-from-minibuffer
                        (minibuffer *interface*)
                        :completion-function (copy-password-completing-fn *password-instance*)))
    (clip-password *password-interface* password-name)))
