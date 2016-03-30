(in-package :www-utils)

;;; This is the condition hierarchy, based on the LispM one.
;;; network-error
;;;   domain-resolver-error
;;;   local-network-error
;;;     network-resources-exhausted
;;;     unknown-address
;;;     unknown-host-name
;;;   remote-network-error
;;;     bad-connection-state
;;;       connection-closed
;;;       connection-lost
;;;       host-stopped-responding
;;;     connection-error
;;;       connection-refused
;;;     host-not-responding
;;;     protocol-timeout
;;; network-parse-error

(define-condition network-error (simple-error)
  ()
  (:report (lambda (condition stream)
             (let ((control (simple-condition-format-control condition)))
               (if control
                   (apply #'format stream control (simple-condition-format-arguments condition))
                 (format stream "A network error of type ~S occurred." (type-of condition))))))
  (:default-initargs
   :format-control nil
    :format-arguments nil))

(define-condition domain-resolver-error (network-error)
  ((address :initarg :address))
  (:report (lambda (condition stream)
             (if (slot-boundp condition 'address)
	         (format stream "Cannot resolve IP address ~A" (slot-value condition 'address))
               (format stream "Cannot find current domainname")))))

(define-condition local-network-error (network-error)
  ())

(define-condition unknown-host-name (local-network-error)
  ((hostname :initarg :hostname))
  (:report (lambda (condition stream)
	     (format stream "Unknown host name ~A" (slot-value condition 'hostname)))))

(define-condition unknown-address (local-network-error)
  ((address :initarg :address))
  (:report (lambda (condition stream)
	     (let ((address (slot-value condition 'address)))
	       (format stream "Unknown address ~A" address)))))

(define-condition remote-network-error (network-error) ())

(define-condition bad-connection-state (remote-network-error) ())

(define-condition connection-closed (bad-connection-state) ())

(define-condition connection-lost (bad-connection-state) ())

(define-condition host-stopped-responding (bad-connection-state) ())

(define-condition connection-error (remote-network-error) ())

(define-condition connection-refused (connection-error) ())

(define-condition host-not-responding (remote-network-error) ())

(define-condition protocol-timeout (remote-network-error) ())

(define-condition network-error-mixin (network-error)
  ()
  (:documentation "Mixin to allow ports to inherit instance variables and methods to network conditions
defined at the portable code level."))