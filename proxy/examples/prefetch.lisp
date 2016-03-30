;;;-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: w4 -*-

;;; Copyright John C. Mallery,  2000.
;;; All rights reserved.
;;;
;;; Rewritten prefetching example originally by Christopher R. Vincent, 1997.
;;;
;;;------------------------------------------------------------------- 
;;;
;;; USING THE WEB WALKER TO PREFILL THE PROXY CACHE
;;;

(in-package :w4)

(defun proxy-cache-url-p (url)
  (not (get-value url :proxy-has-cached-url-p)))

(define-action-type
  proxy-cache-url
  (:standard
    :documentation "Caches or revalidates a web resource in the HTTP proxy cache.")
  (action activity url &key (cache http:*proxy-cache*))
  (declare (ignore action))
  (let ((report-stream (activity-report-stream activity))
	(url-string (name-string url)))
    (format report-stream "~&;Caching ~A..." url-string)
    (http::proxy-cache-ensure-valid-cache-entry cache url)
    (clear-cache activity url t)
    (setf (get-value url :proxy-has-cached-url-p) t)
    (format report-stream "~&;Cached ~A" url-string)
    #+ignore(sleep 1)))

(defgeneric prefetch-cache (url cache  &key search-method threads depth operator domain constraints
				proxy minimize-dns-p respect-no-robots-p report-stream)
  (:documentation "Walks URL caching resources visited in CACHE according to DEPTH and CONSTRAINTS.
When provided, domain retricts the walk to the specified domain.")) 

(defmethod prefetch-cache ((url string) (cache http::proxy-cache) &key (search-method :multi-thread-breadth-first) (threads 1)
			   (depth 0) operator domain constraints
			   (proxy (local-proxy)) minimize-dns-p (respect-no-robots-p t) (report-stream *standard-output*))
  (prefetch-cache (intern-url url :if-does-not-exist :uninterned) cache
		  :search-method search-method :threads threads
		  :depth depth :operator operator :domain domain :constraints constraints
		  :proxy proxy :minimize-dns-p minimize-dns-p
		  :respect-no-robots-p respect-no-robots-p :report-stream report-stream))

(defmethod prefetch-cache ((url url) (cache http::proxy-cache) &key (search-method :multi-thread-breadth-first) (threads 1)
			   (depth 0) operator domain constraints
			   (proxy (local-proxy)) minimize-dns-p (respect-no-robots-p t) (report-stream *standard-output*))
  (let* ((constraint-spec `((depth ,depth)
			    (no-cycles)
			    (url-satisfies proxy-cache-url-p)
			    ,@(when respect-no-robots-p '((header-robots-allowed)))
			    ,.(when domain `((url-host-domain ,domain)))
			    ,@(when constraints constraints)))
	 (action-spec `((proxy-cache-url :cache ,cache)
			(generate-inferiors))))
    (with-activity
      ("Proxy-Cache-Prefetch"
       (:url-host-name-resolution (if minimize-dns-p :never :preferred) 
	:if-does-not-exist :uninterned 
	:operator operator
	:search-method search-method
	:threads threads
	:proxy proxy
	:uri-universe "Proxy-Cache-Prefetch"
	:report-stream report-stream)
       :constraints constraint-spec
       :actions action-spec)
      (let ((queue (activity-queue activity)))
	(typecase (activity-queue activity)
	  (depth-first-queue
	    (cerror "Proceed Using Depth First Queue"
		    "You are prefetching cache data using ~S, which is a depth-first queue.
Depth-first queues may require extremely large amounts of stack space and are
not recommended for pretching. Try using a breadth-first queue." queue))))
      (walk url activity))))

#|

(load "http:examples;configuration.lisp")

(define-client-proxy-mapping
   :standard-proxies  ((:http :default "1cust60.tnt1.mammoth-lakes.ca.da.uu.net" 80)))

(define-client-proxy-mapping)

(prefetch-cache "http://www.cnn.com/" http:*proxy-cache*
		:depth 3 :domain '("cnn.com" "akamai.net")
		:search-method :multi-thread-breadth-first :threads 8
		:proxy (http::intern-proxy "fuji-vlm.ai.mit.edu" 80 :if-does-not-exist :create)
		:respect-no-robots-p nil :report-stream tv:initial-lisp-listener )

(prefetch-cache "http://www.cnn.com/" http:*proxy-cache*
		:depth 3 :domain '("cnn.com" "akamai.net")
		:search-method #+ignore :breadth-first :multi-thread-breadth-first :threads 1
		#|:proxy (http::intern-proxy "fuji-vlm.ai.mit.edu" 80 :if-does-not-exist :create) |#
		:respect-no-robots-p nil :report-stream tv:initial-lisp-listener)

|#

