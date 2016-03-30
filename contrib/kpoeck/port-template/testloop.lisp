(in-package :mit-loop)

(defmacro with-null-stream ((&rest streams) &body body)
  "Binds STREAMS to the null-stream within BODY."
  (mit-loop::loop for stream in streams
        collect `(,stream (make-broadcast-stream)) into bindings
        finally (return `(let ,bindings ,@body))))
        
(pprint 
 (macroexpand-1
  '(with-null-stream (a b)
     (print a))))