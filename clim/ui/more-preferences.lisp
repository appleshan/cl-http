;;;   -*- Mode: LISP; Package: HTTP-UI; BASE: 10; Syntax: ANSI-Common-Lisp; -*-

;;;
;;; (c) Copyright  2000, Rainer Joswig, joswig@corporate-world.lisp.de
;;;     All Rights Reserved.
;;;

(in-package "HTTP-UI")


;;; -------------------------------------------------------------------
;;; More Preferences

(http::define-preference :debug-server
			 :name "Debug Server"
			 :presentation-type 'w3p:boolean
			 :prompt "Should the server run in debugging mode?"
			 :default-value-form nil
			 :get-value-form http::*debug-server* 
			 :set-value-form (setq http::*debug-server* http::value)
			 :description "Controls whether the server runs in debug mode.")

(http::define-preference :debug-client
			 :name "Debug Client"
			 :presentation-type 'w3p:boolean
			 :prompt "Should the client run in debugging mode?"
			 :default-value-form nil
			 :get-value-form http::*debug-client* 
			 :set-value-form (setq http::*debug-client* http::value)
			 :description "Controls whether the client runs in debug mode.")

(http::define-preference :debug-proxy
			 :name "Debug Proxy"
			 :presentation-type 'w3p:boolean
			 :prompt "Should the proxy run in debugging mode?"
			 :default-value-form nil
			 :get-value-form http::*debug-proxy* 
			 :set-value-form (setq http::*debug-proxy* http::value)
			 :description "Controls whether the proxy runs in debug mode.")

;;; -------------------------------------------------------------------
;;; More Preference Types

(http::define-preference-type :proxy-preferences
			      :name "Proxy"
			      :display-string "Proxy"
			      :inferiors (:debug-proxy)
			      :description "Parameters related to the HTTP proxy.")


;;; -------------------------------------------------------------------
;;; Register new Preferences

(loop for (preference-type preference)
      in '((:http-preferences :debug-server)
	   (:http-preferences :debug-client))
      do (http::add-inferior (http::find-preference-type preference-type)
			     (http::find-preference preference)))


;;; -------------------------------------------------------------------
;;; End Of File more-preferences.lisp
