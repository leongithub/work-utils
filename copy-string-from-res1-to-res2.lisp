;; 由于其他应用中有字段：<string name="test">test</string>
;; 我想移到我的应用，但是res目录下的values*目录好多，所以此程式诞生

;; (copy-string-from-res1-to-res2 "path/res/" "path/res/" "name")
;; 目前查找只能是单行的，因为此次需求copy的都是单行的。

(in-package #:work-utils)

(defparameter *search-string* nil)

(defun copy-string-from-res1-to-res2 (dir-res1 dir-res2 name)
  (setf *search-string* (concatenate 'string "name=\"" name "\""))
  (dolist (filepath (find-strxml-files dir-res1))
    (aif (find-string-line filepath)
	 (let ((out-path (merge-pathnames
			  (enough-namestring filepath dir-res1)
			  dir-res2)))
	   (add-content-to-file out-path it)))))

(defun add-content-to-file (filepath content)
  (with-open-file (in filepath
		      :direction :input
		      :if-does-not-exist nil
		      :external-format :utf8)
    (when in
      (with-open-file (out filepath
			   :direction :output
			   :if-exists :overwrite
			   :external-format :utf8)
	(do ((line (read-line in nil 'eof)
		   (read-line in nil 'eof)))
	    ((eql line 'eof))
	  (if (search "</resources>" line)
	      (format out "~A~%" content))
	  (format out "~A~%" line))))))

(defun find-string-line (filepath)
  (with-open-file (in filepath
		      :direction :input
		      :if-does-not-exist nil
		      :external-format :utf8)
    (if in
	(do ((line (read-line in nil 'eof)
		   (read-line in nil 'eof)))
	    ((or
	      (eql line 'eof)
	      (search *search-string* line))
	     (if (not (eql line 'eof)) line))))))

;; 从所给的res目录下找出所有strings.xml文件
(defun find-strxml-files (dir-parent)
  (let ((dir-object (make-pathname :name :wild
				   :type :wild
				   :defaults dir-parent)))
    (mapcar #'(lambda (dir)
		(merge-pathnames dir
				 (make-pathname :name "strings"
						:type "xml")))
	    (directory dir-object))))
