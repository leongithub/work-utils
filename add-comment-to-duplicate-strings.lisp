;;;; 用来批量替换文件中的字符串

;; 应用场景：在看Android源码的时候，发现values-*/strings.xml有很多
;; <string name="account_phone" product="tablet">Tablet-only, unsynced</string>
;; <string name="account_phone" product="default">Phone-only, unsynced</string>
;;
;; <string name="fail_reason_too_many_vcard" product="nosdcard">Too many vCard files are in the storage.</string>
;; <string name="fail_reason_too_many_vcard" product="default">Too many vCard files are on the SD card.</string>
;; 会使我用Android studio引入源码自己build报错，所以要留下default ，注释掉其他的
;;
;; 程序设计：由于这些strings.xml文件分布在res/values*/strings.xml，res目录大部分是vaules*目录，所以就把
;; res目录下的所有目录加载进来，然后再与strings.xml合并为一个完整的文件路径。进入文件，查看每一行，是否包
;; 含"product="字符串，如果有再查看product的值是否不为“default”，如果符合替换条件，在输出原本行的同时加上
;; 注释前后缀。

;; (add-comment-for-strxml "/home/.../res/")

(in-package #:work-utils)

(defparameter *multi-line* nil)
(defparameter *pre-str* "<!--")
(defparameter *post-str* "-->")

(defun add-comment-for-strxml (dir-res)
  (loop-strxml dir-res #'add-comment-for-line))

(defun remove-comment-for-strxml (dir-res)
  (loop-strxml dir-res #'remove-comment-for-line))

;; 去除strings.xml注释
(defun remove-comment-for-line (filepath)
  (labels ((need-modify-p (line)
	     (or
	      ; 符合修改条件
	      (let ((pos 0))
		(and (setq pos (search "product=" line))
		     (not (search "default" line
				  :start2 (+ pos 9)
				  :end2 (+ pos 16)))
		     (search *pre-str* line
			     :end2 10)))
	      ; 多行的<string></string>，并且为</string>
	      (and *multi-line*
		   (search "</string>" line :from-end t)
		   (search *post-str* line :from-end t))))
	   (modify-line (line)
	     (if (search "</string>" line :from-end t)
		 (subseq line
			 (if *multi-line*
			     (progn
			       (setf *multi-line* nil)
			       0)
			     (length *pre-str*))
			 (search *post-str* line))
		 (progn
		   (setf *multi-line* t)
		   (subseq line (length *pre-str*)))))
	   (handle-line (line)
	     (if (need-modify-p line)
		 (modify-line line)
		 line)))
    (replace-line filepath #'handle-line)))

;; 给strings.xml加入注释
(defun add-comment-for-line (filepath)
  (labels ((need-modify-p (line)
	     (or
	      ; 符合修改条件
	      (let ((pos 0))
		(and (setq pos (search "product=" line))
		     (not (search "default" line
				  :start2 (+ pos 9)
				  :end2 (+ pos 16)))
		     (not (search *pre-str* line
				  :end2 10))))
	      ; 多行的<string></string>，并且为</string>
	      (and *multi-line*
		   (search "</string>" line :from-end t)
		   (not (search *post-str* line :from-end t)))))
	   (modify-line (line)
	     (if (search "</string>" line :from-end t)
		 (concatenate 'string
			      (if *multi-line*
				  (setf *multi-line* nil)
				  *pre-str*)
			      line
			      *post-str*)
		 (progn
		   (setf *multi-line* t)
		   (concatenate 'string *pre-str* line))))
	   (handle-line (line)
	     (if (need-modify-p line)
		 (modify-line line)
		 line)))
    (replace-line filepath #'handle-line)))

;; 之前输出流用:if-exists :overwrite，
;; 如果是去掉注释就会出现输出比输入少，导致文件尾部会有多余字符
;; 现在采用输出新文件，再删除原有文件，最后重命名新文件为旧文件
(defun replace-line (filepath handle-line)
    (with-open-file (in filepath
			:direction :input
			:if-does-not-exist nil
			:external-format :utf8)
      (when in
	(let ((temp-filename
	       (make-pathname :name "temp-strings"
			      :defaults filepath)))
	  (with-open-file (out temp-filename
			       :direction :output
			       :if-exists :supersede
			       :external-format :utf8)
	    (do ((line (read-line in nil 'eof)
		       (read-line in nil 'eof)))
		((eql line 'eof))
	      (write-line (funcall handle-line line) out)))
	  (delete-file in)
	  (rename-file temp-filename filepath)))))

;; 找出在res目录下的所有strings.xml文件，并循环应用到fn函数上
(defun loop-strxml (dir-res fn)
  (dolist (filepath (find-strxml-files dir-res))
    (funcall fn filepath)))

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
