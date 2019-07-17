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

;;; Password-Store implementation.
(defmethod list-passwords ((password-interface password-store-interface))
  (let ((raw-list (directory (format nil "~a/**/*.gpg"
                                     (password-directory password-interface))))
        (dir-length (length (namestring (truename (password-directory password-interface))))))
    (mapcar #'(lambda (x)
                (subseq (namestring x) dir-length (- (length (namestring x)) 4)))
            raw-list)))


(defmethod clip-password ((password-interface password-store-interface) password-name)
  (clip-password-string (with-open-stream (st (make-string-output-stream))
                          (uiop:run-program `("pass" "show" ,password-name) :output st)
                          (get-output-stream-string st))))

(defmethod save-password ((password-interface password-store-interface) password-name password)
  (with-open-stream (st (make-string-input-stream password))
    (uiop:run-program `("pass" "insert" "--echo" ,password-name) :input st)))

;;; KeepassXC implementation.
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
  (let ((st (make-string-input-stream (password password-interface)))
        (output-st (make-string-output-stream)))
    (uiop:run-program `("keepassxc-cli" "ls" ,(password-file password-interface))
                      :input st :output output-st)
    (remove "Recycle Bin/" (rest (cl-ppcre:split "\\n" (get-output-stream-string output-st))) :test #'equal)))

(defmethod clip-password ((password-interface keepassxc-interface) password-name)
  (let ((st (make-string-input-stream (password password-interface)))
        (output-st (make-string-output-stream)))
    (uiop:run-program `("keepassxc-cli" "show" ,(password-file password-interface) ,password-name)
                      :input st :output output-st)
    (clip-password-string
     (cl-ppcre:regex-replace "[\\S\\s]*Password: \(.*\)[\\S\\s]*" (get-output-stream-string output-st) "\\1"))))

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
