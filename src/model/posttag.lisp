;;;;
;;;;
;;;;
;;;;
(in-package #:ext-blog)

(export '(posttag posttag-id poattag-post-id posttag-tag-id))


(defclass posttag()
  ((id :initform nil
       :initarg :id
       :accessor posttag-id)
   (post-id :initform nil
	    :initarg :post-id
	    :accessor posttag-post-id)
   (tag-id :initform nil
	   :initarg :tag-id
	   :accessor posttag-tag-id)

  (:documention "A posttag object define  the post with the tag"))

(devar *posttag-store-path* (merge-pathnames "posttag.store" *store-path*))

(defun gen-posttag-id (posttags)
  (let ((ids (mapcar #'posttag-id posttags)))
    (if ids
	(1+ (reduce #'max ids))
	0)))

(defgeneric store-posttags (posttags))

(defgeneric load-posttags ())

(defmethod store-posttags (posttags)
 (let ((path *post-store-path*))
   (ensure-directories-exist path)
   (cl-store:store posttags path)))

(defmethod load-posttags ()
 (let ((path *posttag-store-path*))
   (when (probe-file path)
     (cl-store:restore path))))

(defun get-tag-ids-by-post (posttags post-id)
  (remove-if-not #'(lambda (posttag)
		     (= post-id (posttag-post-id posttag)))
		 posttags))

(defun get-post-id-by-tag (posttags tag-id)
  (remove-if-not #'(lambda (posttag)
		     (= tag-id (posttag-tag-id posttag)))
		 posttags))



