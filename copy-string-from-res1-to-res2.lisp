;; 由于其他应用中有字段：<string name="test">test</string>
;; 我想移到我的应用，但是res目录下的values*目录好多，所以此程式诞生

;; (copy-string-from-res1-to-res2 "path/res/" "path/res/" "name")
;; 目前查找只能是单行的，因为此次需求copy的都是单行的。

;; 1 月 3 日更新，使用 com.gigamonkeys.pathnames 和 cl-ppcre 包重写
;; 加入了匹配多行功能（由于是多语言，所以 lo-rLA 里找字符 \\w 匹配不了)
;; 加入移除功能，相当于可以选择是从 res1 复制/剪切到 res2

(in-package #:work-utils)

(defun copy-string-from-res1-to-res2 (dir-res1 dir-res2 names &optional deletep)
  (let ((name-regex (create-scanner names)))
    (walk-directory
     dir-res1
     #'(lambda (p)
	 (aif (find-out-strings p name-regex deletep)
	      (let ((out-path (merge-pathnames
			       (enough-namestring p dir-res1)
			       dir-res2)))
		(add-content-to-file out-path it))))
     :test #'(lambda (p)
	       (and (string= (pathname-type p) "xml")
		    (string= (pathname-name p) "strings"))))))

(defun find-out-strings (filespec name-regex &optional deletep)
  (let ((file-string (read-file filespec))
	(lst))
    (cl-ppcre:do-matches-as-strings (str name-regex file-string)
      (push str lst))
    (if deletep
	(with-open-file (out filespec
			     :direction :output
			     :if-exists :supersede
			     :external-format :utf8)
	  (write-sequence
	   (cl-ppcre:regex-replace-all name-regex file-string "")
	   out)))
    lst))

(defun create-scanner (names)
  (let ((names (mklist names)))
    (cl-ppcre:create-scanner
     (concatenate
      'string
      "[^\\n]*<string.*name=\""
      (if (= (length names) 1)
	  (car names)
	  (concatenate 'string
		       "("
		       (let ((lst))
			 (dolist (name names)
			   (push name lst)
			   (push "|" lst))
			 (apply #'concatenate 'string (cdr lst)))
		       ")"))
      "\"( [a-zA-Z]+=\"\\w*\")*>[^<]*</string>"))))

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
	  (when (search "</resources>" line)
	    (dolist (str content)
	      (write-line str out)))
	  (format out "~A~%" line))))))
