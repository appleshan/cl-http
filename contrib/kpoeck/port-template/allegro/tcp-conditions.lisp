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

;;; Just define them to keep the compiler happy,
;;; but how should I throw them?
;;; In usocket there is a macro to rethrow native conditions to portable conditions, 
;;; Probably a good idea

;;; network error already defined in implementation-socket-low
(define-condition DOMAIN-RESOLVER-ERROR (network-error)())
(define-condition local-network-error (network-error)())
(define-condition unknown-host-name (network-error)())
(define-condition remote-network-error (network-error)())
(define-condition bad-connection-state (network-error)())
(define-condition CONNECTION-CLOSED (network-error)())
(define-condition CONNECTION-LOST (network-error)())
(define-condition HOST-NOT-RESPONDING (network-error)())
(define-condition CONNECTION-REFUSED (network-error)())
(define-condition PROTOCOL-TIMEOUT (network-error)())



