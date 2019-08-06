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
  ((password-file :reader password-file
                  :initarg :file)
   (master-password :accessor master-password
                    :initarg :master-password
                    :initform nil)))

(defgeneric list-passwords (password-interface)
  (:documentation "Retrieve all available passwords."))

(defgeneric clip-password (password-interface password-name)
  (:documentation "Retrieve specific password by name."))

(defgeneric save-password (password-interface password-name password)
  (:documentation "Save password to database."))

;;; Prerequisite Functions

(defun clip-password-string (pass)
  (let ((original-clipboard (trivial-clipboard:text)))
    (trivial-clipboard:text pass)
    (bt:make-thread
     (lambda ()
       (sleep 5)
       (when (string= (trivial-clipboard:text) pass)
         (trivial-clipboard:text original-clipboard))))))


;;; Commands to wrap together.
(defun copy-password-completion-fn (password-instance)
  (let ((password-list (list-passwords password-instance)))
    (lambda (input)
      (fuzzy-match input password-list))))

(define-command copy-password ()
  "Copy chosen password from minibuffer."
  (with-password (password-interface *interface*)
    (with-result (password-name (read-from-minibuffer
                                 (minibuffer *interface*)
                                 :completion-function (copy-password-completion-fn
                                                       (password-interface *interface*))))
      (clip-password (password-interface *interface*) password-name))))

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
          ((eql cursor-index (length input-buffer-password))
           (cl-markup:markup (:span input-buffer-password)
                             (:span :id "cursor" (cl-markup:raw "&nbsp;"))))
          (t (cl-markup:markup (:span (subseq input-buffer-password 0 cursor-index))
                               (:span :id "cursor" (subseq input-buffer-password cursor-index (+ 1 cursor-index)))
                               (:span (subseq input-buffer-password (+ 1  cursor-index))))))))

(define-command save-new-password ()
  "Save password to password interface."
  (setf (symbol-function 'generate-input-html) (symbol-function 'generate-input-html-new))
  (with-result* ((password-name (read-from-minibuffer
                                 (minibuffer *interface*)
                                 :input-prompt "Name for new password:"))
                 (master-password (read-from-minibuffer
                                   (minibuffer *interface*)
                                   :input-prompt "New password:")))
    (save-password (password-interface *interface*) password-name master-password)
    (setf (symbol-function 'generate-input-html) (symbol-function 'generate-input-html-original))))

(defun executable-find (command)
  "Search for COMMAND in the PATH and return the absolute file name.
Return nil if COMMAND is not found anywhere."
  (multiple-value-bind (path)
      (ignore-errors
       (uiop:run-program (format nil "command -v ~A" command)
                         :output '(:string :stripped t)))
    path))


