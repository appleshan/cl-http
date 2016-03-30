;;;-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: http -*-

;;; (C) Copyright 1996-1997, Christopher R. Vincent
;;;     All Rights Reserved.
;;;
;;; (C) Enhancements Copyright 1999-2001, John C. Mallery.
;;;     All Rights Reserved.
;;;


;;;------------------------------------------------------------------- 
;;;
;;; CL-HTTP PROXY SERVER
;;;


(in-package :http)


;;;------------------------------------------------------------------- 
;;;
;;; DATABASE DOCUMENTATION
;;;


(w3p:define-presentation-type database-handle ()
  :inherit-from t)

(w3p:define-presentation-method w3p:present (handle (type database-handle) stream (view w3p:html-view) &key)
  "Call generic function on identifier, varys with database type."
  (handle-write handle stream))


;;;------------------------------------------------------------------- 
;;;
;;; CACHE DOCUMENTATION
;;;

(w3p:define-presentation-type resource ()
  :inherit-from t 
  :options ((verbose-p t)))

(w3p:define-presentation-type representation ()
  :inherit-from t 
  :options ((verbose-p t)))

(w3p:define-presentation-method w3p:accept ((type resource) stream (view w3p:textual-view) &key)
  (let* ((string (w3p:accept 'string :view w3p:+textual-view+ :stream stream))
         (res (intern-resource *server* string :if-does-not-exist :soft)))
    (unless res
      (w3p:handle-input-error string type))
    res))

(w3p:define-presentation-method w3p:present (resource (type resource) stream (view w3p:html-view) &key)
  (cond (verbose-p
         (with-slots (uri-string creation-date vary representations) resource
           (html:with-paragraph (:stream stream)
             (html:with-rendition (:bold :stream stream)
               (write-string "Resource: " stream))
             (write-string uri-string stream)
             (html:break-line :stream stream)
             (html:with-rendition (:bold :stream stream)
               (write-string "Creation Date: " stream))
             (write-time creation-date stream)
             (html:break-line :stream stream)
             (html:with-rendition (:bold :stream stream)
               (write-string "Vary: " stream))
             (w3p:present vary '(sequence symbol) :view w3p:+textual-view+ :stream stream)
             (html:break-line :stream stream)
             (html:with-rendition (:bold :stream stream)
               (write-string "Total Size: " stream))
             (write (cache-object-size resource) :stream stream)
             (write-string " bytes" stream)
             (html:break-line :stream stream)
             (html:with-rendition (:bold :stream stream)
               (write-string "Representations: " stream))
             (cond (representations
                    (html:break-line :stream stream)
                    (loop for item in representations
                          do (w3p:present item '((representation) :verbose-p nil) 
                                       :view w3p:+html-view+ :stream stream)
                          (html:break-line :stream stream)))
                   (t (write-string "None" stream))))))
        (t (with-slots (uri-string) resource
             (html:note-anchor uri-string 
                               :reference (concatenate 'string "resource?" uri-string)
                               :stream stream)))))

(w3p:define-presentation-method w3p:present (representation (type representation) stream (view w3p:html-view) &key)
  (macrolet ((writing-entry ((name stream) &body body)
	       `(progn
		  (html:with-rendition (:bold :stream ,stream)
		    (html:fast-format ,stream "~A: " ,name))
		  ,@body
		  (html:break-line :stream ,stream)))
	     (writing-headers ((name stream) &body body)
	       `(writing-entry (,name ,stream)
			       (html:with-paragraph-style (:quotation :fresh-line nil :stream ,stream)
				 (html:with-verbatim-text (:fresh-line nil :width 120 :stream ,stream)
				   ,@body)))))
    (cond (verbose-p
	   (with-slots (resource uid etag entity-handle creation-date verified-date last-reference must-revalidate-p
				 last-modification) representation
	     (html:with-paragraph (:stream stream)
	       (writing-entry ("Resource" stream)
			      (w3p:present resource '((resource) :verbose-p nil) :view w3p:+html-view+ :stream stream))
	       (writing-entry ("Creation Date" stream) (write-time creation-date stream))
	       (when verified-date
		 (writing-entry ("Last Verified" stream) (write-time verified-date stream)))
	       (when last-reference
		 (writing-entry ("Last Reference" stream)(write-time last-reference stream)))
	       (when last-modification
		 (writing-entry ("Last Modified" stream) (write-time last-modification stream)))
	       (writing-entry ("Expiration Date" stream)
			      (write-time (representation-expiration-time representation) stream))
	       (when must-revalidate-p 
		 (writing-entry ("Must Revalidate" stream) (fast-format stream "yes")))
	       (writing-entry ("HTTP Status" stream) (fast-format stream "~D" (representation-http-status representation)))
	       (writing-entry ("Server Version" stream) (fast-format stream "~A" (representation-http-version representation)))
	       (writing-entry ("UID" stream) (write uid :stream stream))
	       (when etag
		 (writing-entry ("Etag" stream) (print-entity-tag-header etag stream)))
	       (writing-entry ("Entity Size" stream) (fast-format stream "~D Bytes" (representation-entity-size representation)))
	       (writing-entry ("Cached Entity" stream)
			      (flet ((write-ref (stream)
				       (fast-format stream "/cl-http/proxy/representation?~A+~A+entity"
						    (uri-string resource) uid)))
				(declare (dynamic-extent #'write-ref))
				(html:with-anchor-noted (:reference #'write-ref :stream stream)
				  (w3p:present entity-handle '((database-handle) :representation representation) 
					       :view w3p:+html-view+ :stream stream)))))
	     (html:with-paragraph (:stream stream)
	       (writing-headers ("Response Headers" stream)
				(write-response-headers representation stream))
	       (writing-headers ("Request Headers" stream)
				(write-request-headers representation stream)))))
	  (t (with-slots (resource uid) representation
	       (fast-format stream "Representation: ")
	       (flet ((write-ref (stream)
			(fast-format stream "/cl-http/proxy/representation?~A+~A" (uri-string resource) uid)))
		 (declare (dynamic-extent #'write-ref))
		 (html:with-anchor-noted (:reference #'write-ref :stream stream)
		   (fast-format stream "ID#~D" uid))))))))
             
(defmacro write-back-to-resources (stream)
  `(html:with-paragraph (:stream ,stream)
     (write-string "Back to " stream)
     (note-anchor "cached resources" :reference "resource?" :stream stream)
     (write-char #\. stream)))

(defmethod respond-to-show-resource ((url url:http-search) stream)
  (with-slots (url:search-keys) url
    (let ((title (format nil "Resources Caches by Proxy Server (~A)" (local-port-context *standard-proxy-port*))))
      (with-successful-response (stream :html :expires (server-request-time *server*) :content-location url)
	(html:with-html-document (:stream stream)
	  (html:with-document-preamble (:stream stream)
	    (html:declare-title title :stream stream)
	    (html:declare-base-reference url :stream stream)
            (declare-link :reference "/cl-http/css/base.css" 
                          :relation "stylesheet" :media-type "text/css" :stream stream))
	  (html:with-document-body (:stream stream :background :white :foreground :black)
	    (html:with-section-heading (title :level 2 :stream stream)
	      (html:horizontal-line :size 3 :stream stream)
              (cond (url:search-keys
                     (handler-case 
                         (let ((resource (w3p:accept-from-string 'resource (car url:search-keys))))
                           (if resource
                               (w3p:present resource '((resource) :verbose t) :view w3p:+html-view+ :stream stream)
			     (fast-format stream "Cached resource ~A not found." (car url:search-keys))))
                       (w3p:input-not-of-required-type () (fast-format stream "Cached resource not found.")))
                     (write-back-to-resources stream))
                    (t (flet ((write-resource (resource)
				(html:enumerating-item (stream)
				  (fast-format stream "[~I] ~I"
					       (write-standard-time (cache-object-last-reference-date resource) stream)
					       (w3p:present resource '((resource) :verbose-p nil) :view w3p:+html-view+ :stream stream)))))
			 (declare (dynamic-extent #'write-resource))
			 (let ((proxy-cache *proxy-cache*))
			   (html:with-paragraph (:stream stream)
			     (fast-format stream "~I ~D bytes" 
					  (html:with-rendition (:bold :stream stream)
					    (fast-format stream "Total Cache Size:"))
					  (cache-size proxy-cache)))
			   (html:with-paragraph (:stream stream)
			     (fast-format stream "~D resources currently cached are displayed in increasing recency of reference."
					  (resources-count proxy-cache)))
			   (html:horizontal-line :stream stream)
			   (html:with-paragraph (:stream stream)
			     (html:with-font (:size -1 :relative-size-p t :stream stream)
			       (html:with-enumeration (stream :enumerate)
				 (cache-recency-map-resources proxy-cache #'write-resource)))))))))
	    (html:horizontal-line :size 3 :stream stream)
	    (cl-http-signature stream)))))))

(defmethod respond-to-show-representation ((url url:http-search) stream)
  (macrolet ((with-html-response (&body body)
               `(with-successful-response (stream :html :expires (get-universal-time)
                                                  :content-location url)
                  (let ((title "Proxy Server Cached Representation"))
                    (html:with-html-document (:stream stream)
                      (html:with-document-preamble (:stream stream)
                        (html:declare-title title :stream stream)
                        (html:declare-base-reference url :stream stream)
                        (declare-link :reference "/cl-http/css/base.css" 
                                      :relation "stylesheet" :media-type "text/css" :stream stream))
                      (html:with-document-body (:stream stream :background :white :foreground :black)
                        (html:with-section-heading (title :level 2 :stream stream)
                          ,@body)))))))
    (destructuring-bind (&optional uri cache-id type) (url:search-keys url)
      (cond ((and uri cache-id) ;required arguments
             (handler-case 
                 (let* ((resource (w3p:accept-from-string 'resource uri))
                        (id (w3p:accept-from-string 'integer cache-id))
                        (representation (when resource (get-representation resource id nil))))
                   (cond ((and type (string-equal type "entity"))
                          (proxy-respond-with-representation *server* representation (server-http-version *server*)))
                         (t (with-html-response
                             (cond (representation
                                    (w3p:present representation '((representation) :verbose-p t) 
                                                 :view w3p:+html-view+
                                                 :stream stream))  
                                   (t (fast-format stream "Cached representation ~A for resource ~A not found." cache-id uri)))
                             (write-back-to-resources stream)
                             (html:horizontal-line :stream stream)
                             (cl-http-signature stream)))))
               (w3p:input-not-of-required-type 
                () (with-html-response
                    (fast-format stream "Cached representation not found.")
                    (write-back-to-resources stream)
                    (html:horizontal-line :stream stream)
                    (cl-http-signature stream)))))
            (t (error 'bad-syntax-provided :format-string "URI and CACHE-ID were not provided." :url url))))))

(define export-proxy-interfaces (&rest export-args &key (context (local-context)) &allow-other-keys)
  "Exports the proxy interfaces.
CONTEXT is the host and port on which to export the interfaces.
EXPORT-ARGS are any valid arguments to export URL, such as security parameters."
  (declare (dynamic-extent export-args))
  (with-local-context (context)
    (apply #'export-url #u"/cl-http/proxy/resource?"
	   :search
	   :response-function #'respond-to-show-resource
	   :expiration '(:no-expiration-header)
	   :keywords '(:cl-http :proxy :documentation)
	   :documentation "Describes proxy constraints."
	   export-args)
    (apply #'export-url #u"/cl-http/proxy/representation?"
	   :search
	   :response-function #'respond-to-show-representation
	   :search-parser #'parse-search-info
	   :search-writer #'write-search-info
	   :expiration '(:no-expiration-header)
	   :keywords '(:cl-http :proxy :documentation)
	   :documentation "Describes proxy predicates."
	   export-args)))