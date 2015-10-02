;;;; 由于说高通原生对小语种支持缺失，如：values/strings.xml 有 <string name="a">a</string>，而values-fi/strings.xml 没有此字段。
;;;; 此代码寻找出缺失的字符串

(in-package #:work-utils)

(defparameter *base-strings* "values")
(defparameter *compare-strings-list*
  (list "values-ca" "values-ca-rES" "values-cs" "values-cs-rCZ" "values-da" "values-da-rDK" "values-de" "values-de-rAT" "values-de-rch" "values-de-rDE" "values-el" "values-el-rGR" "values-en-rGB" "values-es" "values-es-rES" "values-fi" "values-fi-rFI" "values-fr" "values-fr-rBE" "values-fr-rCH" "values-fr-rFR" "values-hu" "values-hu-rHU" "values-it" "values-it-rIT" "values-nb" "values-nb-rNO" "values-nl-rBE" "values-nl-rNL" "values-pl" "values-pl-rPL" "values-pt" "values-pt-rBR" "values-pt-rPT" "values-ro" "values-ro-rRO" "values-ru" "values-ru-rRU" "values-sv" "values-sv-rSE" "values-tr" "values-tr-rTR" "values-zh-rCN" "values-zh-rHK" "values-zh-rTW"))

(defparameter *source-table* (make-hash-table))
(defparameter *compare-table* (make-hash-table :test 'equal))

(defstruct tag content line-number)

(defun find-out-lack-strings (dir-res)
  (base-xml-to-hash-table dir-res)
  (with-open-file (out "~/compare-strings-result.txt"
		       :direction :output
		       :if-exists :supersede
		       :external-format :utf8)
    (dolist (dir-value *compare-strings-list*)
      (format out "~%~%~A~%" dir-value)
      (if (compare-xml-to-hash-table dir-res dir-value)
	  (compare-strings out)
	  (format out "file ~A is not exist~%" dir-value))))
  (clrhash *source-table*)
  (clrhash *compare-table*))

(defun base-xml-to-hash-table (dir-res)
  (with-open-file (in (rtn-strings-xml dir-res *base-strings*)
		      :direction :input
		      :if-does-not-exist nil
		      :external-format :utf8)
    (let ((multi-line-p)
	  (name)
	  (multi-line-buffer))
      (do ((line (read-line in nil 'eof)
		 (read-line in nil 'eof))
	   (line-number 1 (1+ line-number)))
	  ((eql line 'eof))
	(let ((name-position (search "name=" line)))
	  (cond (name-position
		 (setf name
		       (subseq line
			       (+ name-position 6)
			       (search "\"" line :start2 (+ name-position 6))))
		 (setf multi-line-p (not (search "</string>" line :from-end t)))
		 (if multi-line-p
		     (setf multi-line-buffer line)
		     (set-source-hash name line line-number)))
		(multi-line-p
		 (setf multi-line-buffer (concatenate 'string multi-line-buffer line))
		 (setf multi-line-p (not (search "</string>" line :from-end t)))
		 (unless multi-line-p
		   (set-source-hash name multi-line-buffer line-number)))))))))


(defun set-source-hash (name content line-number)
  (setf (gethash name *source-table*)
	(make-tag :content content
		  :line-number line-number)))

(defun compare-xml-to-hash-table (dir-res dir-value)
  (clrhash *compare-table*)
  (with-open-file (in (rtn-strings-xml dir-res dir-value)
		      :direction :input
		      :if-does-not-exist nil
		      :external-format :utf-8)
    (when in
      (do ((line (read-line in nil 'eof)
		 (read-line in nil 'eof)))
	  ((eql line 'eof) t)
	(let ((name-position (search "name=" line)))
	  (if name-position
	      (let ((name (subseq line
				  (+ name-position 6)
				  (search "\"" line :start2 (+ name-position 6)))))
		(setf (gethash name *compare-table*) t))))))))


(defun compare-strings (out)
  (maphash #'(lambda (k v)
	       (unless (gethash k *compare-table*)
		 (format out "~A:~A~%" (tag-line-number v) (tag-content v))))
	   *source-table*))


(defun rtn-strings-xml (dir-res dir-values)
  (merge-pathnames (make-pathname :directory `(:relative ,dir-values)
				  :name "strings"
				  :type "xml")
		   dir-res))
