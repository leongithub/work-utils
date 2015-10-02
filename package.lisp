;;;; package.lisp

(defpackage #:work-utils
  (:use #:cl #:com.gigamonkeys.pathnames)
  (:shadow :walk-directory)
  (:export :compute-all
	   :compute-sub-max-min
	   :add-comment-for-strxml
	   :remove-comment-for-strxml
	   :find-out-lack-strings
	   :copy-string-from-res1-to-res2
	   :delete-unuse-resource))

