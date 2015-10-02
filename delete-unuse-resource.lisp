;;;; 最近对项目中的无用资源（主要图片和xml文件）进行清理
;;;; 用法:
;;;; (delete-unuse-resource "path-to-project")
;;;; (delete-unuse-resource "path-to-project" '(other-path-to-search))

(in-package #:work-utils)

(defun delete-unuse-resource (project-pathname &optional search-project-lst)
  "删除 Android 项目中的无用资源"
  (pushnew project-pathname search-project-lst :test 'equal)
  (dolist (pathname search-project-lst)
    (unless (file-exists-p pathname)
      (error "~a is not exists" pathname)))
  (let ((result (find-out-unuse-resource project-pathname search-project-lst)))
    (print (mapcar #'(lambda (p) (namestring p)) result))
    (when (y-or-n-p "是否删除以上资源")
      (dolist (p result 'done)
        (delete-file p)))))

(defun find-out-unuse-resource (project-pathname search-project-lst)
  (labels ((find-out-unuse-resource-1 (name)
             (let ((regex (concatenate 'string name "[^a-z_]")))
               (dolist (project search-project-lst)
                 (walk-directory
                  project
                  #'(lambda (p)
                      (with-open-file (stream p :external-format :utf8)
                        (when (cl-ppcre:scan regex (%read-body stream))
			  ;; 当找到后就退出此函数
                          (return-from find-out-unuse-resource-1))))
                  :test
		  #'(lambda (p)
		      (let ((type (pathname-type p)))
			(or (string= type "java")
			    (string= type "xml"))))
                  :list-directory-test
		  #'(lambda (p)
		      (let ((last-dir (car (last (pathname-directory p)))))
			;; 这里排除 build 目录，查找 build 目录会花很多时间
			(not (string= "build" last-dir))))))
               name)))
    (let (lst
          (res-dir (make-pathname :directory
                                  (append (pathname-directory
                                           (pathname-as-directory project-pathname))
                                          (list "res")))))
      (dolist (x (list-directory res-dir))
        (let ((last-dir (car (last (pathname-directory x)))))
	  ;; 只找 drawable** 和 layout** 目录下的文件
          (when (or (search "drawable" last-dir)
                    (search "layout" last-dir))
            (dolist (resource (list-directory x))
	      ;; 因为有 xxx.9.png 这样的，所以用两次 pathname-name 函数
              (when (find-out-unuse-resource-1 (pathname-name (pathname-name resource)))
                (push resource lst))))))
      (nreverse lst))))
