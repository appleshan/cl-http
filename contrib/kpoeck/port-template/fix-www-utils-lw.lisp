(in-package :www-utils)

(defun fix-symbols-www-utils ()
  (dolist (symbol '(
                    "WITHOUT-PREEMPTION"
                    "MODIFY-HASH" 
                    ))
    (intern 
     symbol
     (find-package :www-utils)
     )
    (export
     (find-symbol symbol (find-package :www-utils))
     (find-package :www-utils))))

(defun modify-hash (table key function &aux value)
  "Combines the action of setf of gethash into one call to modify- hash. It lets
you both examine the value of key and change it. It is more efficient because
it does the lookup once instead of twice.

Finds the value associated with key in table, then calls function with key,
this value, a flag indicating whether or not the value was found.  Puts
whatever is returned by this call to function into table, associating it
with key. Returns the new value and the key of the entry. Note:  The actual
key stored in table is the one that is used on function, not the one you
supply with key."
  (declare (values new-value key))
  (multiple-value-bind (entry foundp)
      (gethash key table)
    (cond (foundp
	   (setq value (funcall function key entry foundp))
	   (unless (eq value entry)
	     (setf (gethash key table) value))
	   (values value key))
	  (t (setq value (funcall function key nil nil))
	     (setf (gethash key table) value)
	     (values  value key)))))

(defun port-address (port)
  "Returns the IP address for the network interface associated with PORT.
When PORT is accessible over all interfaces, this returns the local
host domain name.  When no service is defined for port, it returns
nil. The second value indicates whether service is restricted to this
network interface."
  (declare (values address restricted-to-this-interface-p))
  (when (http:http-service-enabled-p port)
    (values (local-host-domain-name) nil)))

(defun port-protocol (port)
  "Returns a keyword denoting the protocol specified for PORT,
or NIL if no protocol has been defined for PORT."
  (declare (ignore port))
  :http)

(eval-when (:load-toplevel :execute)
  (fix-symbols-www-utils)
  )


