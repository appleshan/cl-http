(in-package :http)

(defmethod read-headers-into-buffer ((header-set header-set) (stream comm:socket-stream))
  (macrolet ((grow-vector (vector size requested-size element-type)
	       `(let ((n-size (floor (* (the fixnum ,requested-size) *header-set-growth-factor*))))
		  (setq ,vector (adjust-array ,vector n-size :element-type ,element-type)
			,size n-size))))
    (with-fast-array-references ((line-ends (%header-set-line-ends header-set) vector))
      (let* ((line-ends-size (array-total-size line-ends))
	     (buffer (%header-set-buffer header-set))
	     (buffer-size (array-total-size buffer))
	     (end (fill-pointer buffer))
	     line error-p delimiter length)
	  delimiter					;ignore
	(using-resource (line-buffer line-buffer *line-buffer-size*)
	  (loop for line-idx fixnum upfrom (fill-pointer line-ends)
		for idx fixnum = end
		do (multiple-value-setq (line error-p delimiter length)
		     (read-delimited-line stream '(#\Linefeed #\return) t line-buffer))
		until (or error-p (blank-line-p line 0 length))
		do (setq end (+ idx (the fixnum length)))
		   (when (< buffer-size end)
		     (grow-vector buffer buffer-size end *standard-character-type*)
		     (setf (%header-set-buffer header-set) buffer))
		   (copy-vector-portion line 0 length buffer idx end)
		   (unless (< line-idx line-ends-size)
		     (grow-vector line-ends line-ends-size line-idx 'fixnum)
		     (setf (%header-set-line-ends header-set) line-ends))
		   ;; Track line end
		   (setf (aref line-ends line-idx) end)
		finally (setf (fill-pointer buffer) end
			      (fill-pointer line-ends) line-idx)))))))