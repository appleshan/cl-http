;;; (defpackage www-utils (:use :cl))

(in-package :www-utils)

(defparameter *test-socket* nil)
(defparameter *socket-stream*  nil)
(defparameter *text* nil)

(setq *test-socket* (%make-server-socket :port 8888))

(class-of *test-socket*)


;;;This will block
(setq *socket-stream* ( %wait-for-connection *test-socket*))

(class-of  *socket-stream*)

;;;This will block
(setq *text* (read-line  *socket-stream*))

(write-line "Hello Client" *socket-stream* )
(force-output *socket-stream*)

(close *socket-stream*)
(close  *test-socket*)
