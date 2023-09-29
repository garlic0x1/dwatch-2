(ql:quickload :file-notify)
(ql:quickload :str)

(defpackage :dwatch
  (:use :cl :alexandria-2)
  (:local-nicknames (:notify :org.shirakumo.file-notify)))
(in-package :dwatch)

(defun make-worker (cmd)
  "Return a lambda that starts or restarts cmd when called"
  (let ((p (uiop:launch-program "")))
    (lambda ()
      (uiop:close-streams p)
      (setf p (uiop:launch-program cmd :output *standard-output*)))))

(defun right-type? (file types)
  "Test if file is of a set of types"
  (if types (find-if (rcurry #'str:ends-with? (namestring file)) types) t))

(defun dwatch (&key dir cmd types)
  "Watch files of specified types, and run a command when one is modified"
  (let ((worker (make-worker cmd)))
    (notify:watch dir)
    (notify:with-events (file ev :timeout t)
      (when (and (eq ev :close-write) (right-type? file types))
        (funcall worker)))))
