(in-package :http-user)

(defmethod server-push-test (url stream)
  (ns4.0:with-server-push-response 
     (stream)
     (dotimes (x 5)
       (ns1.1:with-block
        (stream :content-type :html :content-location url :sleep-interval 1 :force-output t :last-block-p (= x 4))
        (with-font (:size 2 :stream stream)
            (with-verbatim-text (:fresh-line nil :width 256 :stream stream)
              (format stream (concatenate 'string "Hugo" (princ-to-string (get-internal-run-time))))
              (format stream (concatenate 'string "Iteration " (princ-to-string x)))
              )))
       (force-output stream)
       (let ((now (get-internal-real-time)))
         (flet ((return-p ()
                          (> (get-internal-real-time) (+ now internal-time-units-per-second))))
           (mp:process-wait "blah" #'return-p))
         :ready)
       (unless (live-connection-p stream)
         (return))                                      
       )
     )
  )

(export-url #u"/karsten.html"
            :computed
            :response-function #'server-push-test
            :no-cache t
            :documentation "Displays refreshing thing.")

