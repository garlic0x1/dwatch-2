(ql:quickload :sb-posix)
(ql:quickload :uiop)

(ql:quickload :alexandria)
(ql:quickload :str)
(use-package :alexandria-2)

(defun make-worker (cmd)
  "Return a lambda that starts or restarts cmd when called"
  (let ((p (uiop:launch-program "")))
    (lambda ()
      (uiop:close-streams p)
      (setf p (uiop:launch-program cmd :output *standard-output*)))))

(defun last-modified (file)
  (sb-posix:stat-mtime (sb-posix:stat (probe-file file))))

(defun modified-file? (table file)
  "Update table and see if anything changed"
  (not (equal (gethash file table)
              (setf (gethash file table)
                    (last-modified file)))))

(defun modified-some? (table files)
  (remove-if-not (curry #'modified-file? table) files))

(defun fs-tree (dir)
  "Hacky directory crawl, im using GNU find"
  (line-up-last
   (uiop:run-program `("/bin/find" ,(namestring dir)) :output :string)
   (str:lines)
   (mapcar #'uiop:file-exists-p)
   (remove-if-not #'identity)))

(defun right-type? (file types)
  "Test if file is of a set of types"
  (if types (find-if (rcurry #'str:ends-with? file) types) t))

(defun watched-files (dir types)
  "List of files to check for updates"
  (remove-if-not (rcurry #'right-type? types) (fs-tree dir)))

(defun dwatch (&key dir cmd types (interval 1))
  "Watch files of specified types, and run a command when one is modified"
  (let ((worker (make-worker cmd))
        (table (make-hash-table))
        (dir (uiop:directory-exists-p dir)))
    (loop :for files := (watched-files dir types)
          :do (when (modified-some? table files) (funcall worker))
          :do (sleep interval))))
