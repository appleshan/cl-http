
(defpackage #:www-utils
  (:use future-common-lisp)
  (:export
   #:abort-http-stream
   #:bad-connection-state
   #:bytes-received
   #:bytes-transmitted
   #:chunk-transfer-content-length
   #:chunk-transfer-content-length-header
   #:chunk-transfer-decoding-mode
   #:chunk-transfer-decoding-mode-end
   #:chunk-transfer-encoding-mode
   #:common-logfile-notify
   #:connection-closed
   #:connection-error
   #:connection-lost
   #:connection-refused
   #:directory-not-found
   #:domain-error
   #:domain-resolver-error
   #:end-of-chunk-transfer-decoding
   #:expose-log-window
   #:file-not-found
   #:http-stream-error
   #:http-user-email-address
   #:host-not-responding
   #:host-stopped-responding
   #:local-network-error
   #:log-window
   #:network-error
   #:network-parse-error
   #:network-resources-exhausted
   #:note-first-chunk
   #:note-last-chunk
   #:notify-log-window
   #:open-http-stream-to-host
   #:open-ssl-stream-to-host
   #:protocol-timeout
   #:remote-network-error
   #:ssl-connection-error
   #:tcp-service-port-number
   #:unix-socket-error
   #:unknown-address
   #:unknown-host-name
   #:with-timeout
   #:with-stream-timeout
   #:with-crlf-stream)
  #+mp
  (:import-from #:mp
   #:process-state
   #:process-whostate
   #:process-name
   #:process-preset
   #:process-active-p)
  (:export
   #:process-run-time
   #:process-idle-time
   #:process-run-function
   #:make-process
   #:process-wait
   #:process-wait-with-timeout
   #:process-disable
   #:process-enable
   #:process-kill
   #:process-reset
   #:current-process
   #:process-state
   #:process-whostate
   #:process-name
   #:process-preset
   #:process-active-p
   #:process-priority
   )
  (:import-from #:clos
   #:class-direct-superclasses
   #:class-slots
   #:generic-function-methods
   #:method-specializers
   #:slot-definition-name)
  (:export
   #:class-direct-superclasses
   #:class-slots
   #:generic-function-methods
   #:method-specializers
   #:slot-definition-name)
  (:import-from #:resources
   #:defresource
   #:clear-resource
   #:allocate-resource
   #:deallocate-resource
   #:using-resource)
  (:export
   #:defresource
   #:clear-resource
   #:allocate-resource
   #:deallocate-resource
   #:using-resource))
