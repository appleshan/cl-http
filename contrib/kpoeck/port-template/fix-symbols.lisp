(in-package :cl-user)

(defun fix-symbols-resources ()
  (dolist (symbol
           '(
             #+no 
             "*DEFAULT-RESOURCE-SIZE*"
             "ALLOCATE-RESOURCE"
             "CLEAR-RESOURCE"
             "DEALLOCATE-RESOURCE"
             "DEFRESOURCE"
             #+no 
             "DESCRIBE-RESOURCE"
             "MAP-RESOURCE"
             #+no "MATCHING-RESOURCE"
             #+no "RESOURCE"
             "USING-RESOURCE"
             ))
    (import 
     (find-symbol symbol (find-package :resources))
     (find-package :www-utils)
     )
    (export
     (find-symbol symbol (find-package :resources))
     (find-package :www-utils))
    )
  )

(defun fix-symbols-processes ()
  (dolist (symbol '(
                    "MAKE-PROCESS"
                    "PROCESS-ACTIVE-P"
                    "PROCESS-DISABLE"
                    "PROCESS-ENABLE"
                    "PROCESS-KILL"
                    "PROCESS-PRESET"
                    "PROCESS-PRIORITY"
                    "PROCESS-RUN-FUNCTION"
                    "PROCESS-RUN-TIME"
                    "PROCESS-IDLE-TIME"
                    "PROCESS-WAIT"
                    "PROCESS-WHOSTATE"
                    "PROCESS-WAIT-WITH-TIMEOUT"
                    "PROCESS-NAME"
                    "ALL-PROCESSES"
                    ))
    (intern 
     symbol
     (find-package :www-utils)
     )
    (export
     (find-symbol symbol (find-package :www-utils))
     (find-package :www-utils))))

(defun fix-symbols-tcp ()
  (dolist (symbol '(
                    "WITH-STREAM-TIMEOUT"
                    "CONNECTION-CLOSED" 
                    "CONNECTION-LOST" 
                    "CONNECTION-REFUSED" 
                    "PROTOCOL-TIMEOUT"
                    "HOST-STOPPED-RESPONDING"
                    "HOST-NOT-RESPONDING"
                    "NETWORK-ERROR"
                    "NETWORK-ERROR-MIXIN"
                    "UNKNOWN-HOST-NAME"
                    "DOMAIN-RESOLVER-ERROR"
                    "LOCAL-NETWORK-ERROR"
                    "REMOTE-NETWORK-ERROR"
                    "FILE-NOT-FOUND"
                    "CLOSE-THE-SOCKET"))
    (intern 
     symbol
     (find-package :www-utils)
     )
    (export
     (find-symbol symbol (find-package :www-utils))
     (find-package :www-utils))))

(defun fix-symbols-chunking ()
  (dolist (symbol '("CHUNK-TRANSFER-ENCODING-MODE"
                    "NOTE-FIRST-CHUNK"
                    "NOTE-LAST-CHUNK"
                    "CHUNK-TRANSFER-DECODING-MODE-END"
                    "CHUNK-TRANSFER-DECODING-MODE"
                    "END-OF-CHUNK-TRANSFER-DECODING"
                    "CHUNK-TRANSFER-CONTENT-LENGTH"
                    ))
    (intern 
     symbol
     (find-package :www-utils)
     )
    (export
     (find-symbol symbol (find-package :www-utils))
     (find-package :www-utils))))

(defun fix-symbols-other ()
  (dolist (symbol '("BYTES-RECEIVED"
                    "BYTES-TRANSMITTED"
		    "%BUFFERED-STREAM-READ-DELIMITED-LINE"
		    "EXPOSE-LOG-WINDOW"))
    (intern
     symbol
     (find-package :www-utils)
     )
    (export
     (find-symbol symbol (find-package :www-utils))
     (find-package :www-utils))))

(eval-when (:load-toplevel :execute)
  (fix-symbols-resources)
  (fix-symbols-processes)
  (fix-symbols-tcp)
  (fix-symbols-chunking)
  (fix-symbols-other)
  )
