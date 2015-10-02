;;;; 需求：
;;;; 由于公司网页表格里有很多这样的数据：1+2+3+2+4
;;;; 最后要求去掉最大最小，然后算平均值，最后两个平均值按照a*8+b*2计算得除结果。
;;;; 应该有的计算器会支持，但还是练习下cl吧:)

;;;; 解决大概步骤：
;;;; 1.先把1+2+3去掉+("123")，然后转换成list(#\1 #\2 #\3)，再转换成数字列表(1 2 3)
;;;; 2.套用a*8+b*2计算

(in-package #:work-utils)

;; 现在出现打分数据较少，就不减去最高最低的值了
(defun compute-all ()
  (read-data-for-compute t))

(defun compute-sub-max-min ()
  (read-data-for-compute))

;; 循环读取用户输入的数据
(defun read-data-for-compute (&OPTIONAL allp)
  (labels ((prompt (x)
	     (format t "~&please input ~A : " x)
	     (string-trim '(#\Space #\Tab) (read-line))))
    (let ((x (prompt "需求评分"))
	  (y (prompt "风险评分")))
      (print (compute x y allp))
      (read-data-for-compute))))

(defun compute (x y &OPTIONAL allp)
  (let ((ppx (preproccess x))
	(ppy (preproccess y)))
    (+ (* 8
	  (number-seq-average ppx allp))
       (* 2
	  (number-seq-average ppy allp)))))

(defun number-seq-average (seq allp)
  (print
   (if allp
       (all-number-seq-average seq)
       (n-sub-max-min-to-average seq))))

(defun n-sub-max-min-to-average (seq)
  (/ (- (reduce #'+ seq)
	(reduce #'max seq)
	(reduce #'min seq))
     (- (length seq) 2.0)))

(defun all-number-seq-average (seq)
  (float 
   (/ (reduce #'+ seq)
      (length seq))))

(defun preproccess (x)
  (char-list->number-list
   (concatenate 'list 
		(remove #\+
			(string x)))))
 
(defun char-list->number-list (char-lst)
  (mapcar #'(lambda (c)
	      (parse-integer (string c)))
	  char-lst))

