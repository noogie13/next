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
                                   (password-directory (password *interface*)))
                           (namestring x) "\\2"))
            raw-list)))

(defmethod clip-password ((password-interface password-store-interface) password-name)
  (uiop:run-program `("pass" "--clip" ,password-name)))

(defmethod save-password ((password-interface password-store-interface) password-name password)
  (uiop:run-program (str:concat "echo \"" password "\" | pass insert --echo " password-name)))

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

(defun generate-input-html-original (input-buffer cursor-index)
  (cond ((equal "" input-buffer) (cl-markup:markup (:span :id "cursor" (cl-markup:raw "&nbsp;"))))
        ((eql cursor-index (length input-buffer)) (cl-markup:markup (:span input-buffer)
                                                                    (:span :id "cursor" (cl-markup:raw "&nbsp;"))))
        (t (cl-markup:markup (:span (subseq input-buffer 0 cursor-index))
                             (:span :id "cursor" (subseq input-buffer cursor-index (+ 1 cursor-index)))
                             (:span (subseq input-buffer (+ 1  cursor-index)))))))

(defun generate-input-html-new (input-buffer cursor-index)
  (let ((input-buffer-password (make-string (length input-buffer) :initial-element #\*)))
    (cond ((equal "" input-buffer-password) (cl-markup:markup (:span :id "cursor" (cl-markup:raw "&nbsp;"))))
          ((eql cursor-index (length input-buffer-password)) (cl-markup:markup (:span input-buffer-password)
                                                                               (:span :id "cursor" (cl-markup:raw "&nbsp;"))))
          (t (cl-markup:markup (:span (subseq input-buffer-password 0 cursor-index))
                               (:span :id "cursor" (subseq input-buffer-password cursor-index (+ 1 cursor-index)))
                               (:span (subseq input-buffer-password (+ 1  cursor-index))))))))

(define-command save-new-password ()
  "Saves password to password interface."
  (with-result (password-name (read-from-minibuffer
                               (minibuffer *interface*)
                               :input-prompt "Name for new password:"))
    (setf (symbol-function 'generate-input-html) (symbol-function 'generate-input-html-new))
    (with-result (password (read-from-minibuffer
                            (minibuffer *interface*)
                            :input-prompt "New password:"))
      (save-password (password *interface*) password-name password)))
  (setf (symbol-function 'generate-input-html) (symbol-function 'generate-input-html-original)))


;;; to test this, load this file from init, and (setf (password *interface*) (make-instance 'password-store-interface))
