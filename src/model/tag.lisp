;;;;
;;;;
;;;;
;;;;
(in-package #:ext-blog)




(defclass tag()
  ((id :initform nil
       :initarg :id
       :accessor tag-id)
   (name :initform nil
	 :initarg :name
	 :accessor tag-name)
   (time :initform nil
	 :initarg :time
	 :accessor tag-create-time))

  (:documentation "A tag object is a attr of post"))

(defvar *tag-store-path* (merge-pathnames "tag.store" *store-path*))

(defun gen-tag-id (tags)
  (let ((ids (mapcar #'tag-id tags)))
    (if ids
	(1+ (reduce #'max ids))
	0)))

(defgeneric store-tags (tags))

(defgeneric load-tags ())

(defmethod store-tags (tags)
 (let ((path *tag-store-path*))
   (ensure-directories-exist path)
   (cl-store:store tags path)))

(defmethod load-tags ()
 (let ((path *tag-store-path*))
   (when (probe-file path)
     (cl-store:restore path))))



