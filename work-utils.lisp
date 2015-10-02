;;;; work-utils.lisp

(in-package #:work-utils)

;;; "work-utils" goes here. Hacks and glory await!

(defconstant +buffer-size+ 8192)

;; drakma's %read-body
(defun %read-body (stream)
  "Helper function to read from stream into a buffer of character, which is returned."
  (let ((buffer (make-array +buffer-size+ :element-type 'character))
        (result (make-array 0 :element-type 'character :adjustable t)))
    (loop for index = 0 then (+ index pos)
       for pos = (read-sequence buffer stream)
       do (adjust-array result (+ index pos))
         (replace result buffer :start1 index :end2 pos)
       while (= pos +buffer-size+))
    result))

;; 对 com.gigamonkeys.pathnames 中的 walk-directory 添加了一个 list-directory-test 参数
(defun walk-directory (dirname fn &key directories (test (constantly t))
				    (list-directory-test (constantly t)))
  "Walk a directory invoking `fn' on each pathname found. If `test' is
supplied fn is invoked only on pathnames for which `test' returns
true. If `directories' is t invokes `test' and `fn' on directory
pathnames as well."
  (labels
      ((walk (name)
         (cond
           ((directory-pathname-p name)
            (when (and directories (funcall test name))
              (funcall fn name))
	    (when (funcall list-directory-test name)
	      (dolist (x (list-directory name)) (walk x))))
           ((funcall test name) (funcall fn name)))))
    (walk (pathname-as-directory dirname))))

(defmacro aif (test then &optional else)
  "if 的指代宏，在 then 和 else 的 form 中，可以用 it 来指代 test 的结果"
  `(let ((it ,test))
     (if it ,then ,else)))
