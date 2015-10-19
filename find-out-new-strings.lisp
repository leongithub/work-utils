;;;; 要求找出现在的工程与原生工程之间，新增加的字符串

;;; 把原生文件中的字符串加载到 hash－table 中，然后循环目前工程中的文件，
;;; 找出新增的(目前只比较 name 是否相同)

(in-package #:work-utils)

(defun find-out-new-strings (old-res new-res values-lst out-dir)
  "old-res，旧的工程 res 目录。new-res，新的工程 res 目录。values-lst，values-* 目录列表。out-dir，结果输出目录"
  (dolist (dir-values values-lst 'done)
    (with-open-file (out (make-pathname
			  :name dir-values
			  :type "txt"
			  :defaults (pathname-as-directory out-dir))
			 :direction :output
			 :if-exists :supersede
			 :external-format :utf8)
      (let ((old-file (generate-strings-file-pathname old-res dir-values))
	    (new-file (generate-strings-file-pathname new-res dir-values)))
	(unless (file-exists-p old-file)
	  (format out "~a is not exists~%" (namestring old-file)))
	(unless (file-exists-p new-file)
	  (format out "~a is not exists~%" (namestring new-file)))
	(when (and (file-exists-p old-file) (file-exists-p new-file))
	  (loop for (nil . str) in (find-out-new-string-for-file
				     old-file
				     new-file)
	       do (format out "~a~%" str)))))))

(defun generate-strings-file-pathname (dir-res dir-values)
  (make-pathname
   :directory (append (pathname-directory
		       (pathname-as-directory dir-res))
		      (list dir-values))
   :name "strings"
   :type "xml"))

(defun find-out-new-string-for-file (old-file new-file)
  "传递旧文件和新文件，返回新增字符串的列表"
  (labels ((find-out-new-string-for-file1 (hash-table)
	     (with-open-file (in new-file
				 :if-does-not-exist nil
				 :external-format :utf8)
	       (let ((target-string (%read-body in))
		     (lst))
		 (do-string
		     target-string
		   #'(lambda (name str)
		       (unless (gethash name hash-table)
			 (push (cons name str) lst))))
		 (nreverse lst)))))
    (let ((hash-table (old-file-to-hash-table old-file)))
      (and hash-table
	   (find-out-new-string-for-file1 hash-table)))))

(defun old-file-to-hash-table (old-file)
  "旧文件转换成 hash-table"
  (with-open-file (in old-file
		      :if-does-not-exist nil
		      :external-format :utf8)
    (when in
      (let ((target-string (%read-body in))
	    (hash-table (make-hash-table :test 'equal)))
	(do-string
	    target-string
	  #'(lambda (name str)
	      (setf (gethash name hash-table)
		    str)))
	hash-table))))

(defun do-string (target-string fn)
  "Helper function to loop target-string apply fn"
  (cl-ppcre:do-matches-as-strings
      (str "<string .*</string>" target-string)
    (funcall fn
	     (svref (nth-value 1
			       (cl-ppcre:scan-to-strings
				"name=\"([^\"]+)\"" str))
		    0)
	     str)))
