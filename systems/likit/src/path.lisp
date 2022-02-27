(in-package :likit)

(defun filep (path)
  (let ((p (and path (probe-file path))))
    (when (and p (pathname-name p)) p)))

(defun folderp (path)
  (let ((p (and path (probe-file path))))
    (when (and p (not (pathname-name p))) p)))

(defun path (string &optional (home *home*))
  (if (char= #\@ (char string 0))
      (if (string= string "@")
          home
          (merge-pathnames (subseq string 2) home))
      (parse-namestring string)))

(defun get-children (&optional folder)
  (directory
   (make-pathname
    :name :wild
    :type #-clisp :wild #+clisp :nil
    :defaults (probe-file folder))))

(defun map-child (folder operation &key (test (lambda (path) t)) (recursive nil) (greedy nil))
  (dolist (path (get-children folder))
    (when (and recursive (folderp path))
      (map-child path operation :test #'test :recursive recursive :greedy greedy))
    (when (funcall test path)
      (funcall operation path))))
