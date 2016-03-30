;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: ccl; Base: 10 -*-

;;; (C) Copyright 1997-2001, John C. Mallery.
;;;     All Rights Reserved.
;;;

(in-package :ccl)

;; 03/20/2000 jcma Fixed http::stream-copy-until-eof to work in chunking case.
;; 11/13/1998 akh -- defined stream-write-io-buffer correctly

; This seems to make things work...  not too cool performance-wise.
#-ccl-4.3
(defmethod stream-write-byte ((stream broadcast-stream) byte)
  (loop for s in (broadcast-stream-streams stream)
        doing (write-byte byte s))) 

(defmethod ascii-output-mode ((stream broadcast-stream))
  (loop for s in (broadcast-stream-streams stream)
        do (ascii-output-mode s)))

(defmethod ascii-input-mode ((stream broadcast-stream))
  (loop for s in (broadcast-stream-streams stream)
        do (ascii-input-mode s)))

(defmethod binary-output-mode ((stream broadcast-stream))
  (loop for s in (broadcast-stream-streams stream)
        do (binary-output-mode s)))

(defmethod binary-input-mode ((stream broadcast-stream))
  (loop for s in (broadcast-stream-streams stream)
        do (binary-input-mode s)))

(defmethod output-mode ((stream broadcast-stream))
   (loop for s in (broadcast-stream-streams stream)
            when (typep s 'modal-ascii-or-binary-tcp-stream-mixin)
            return (output-mode s)
            finally (return :ascii)))

(defmethod set-output-mode ((stream broadcast-stream) mode)
   (ecase mode
      (:ascii (ascii-output-mode stream))
      (:binary (binary-output-mode stream)))) 

(defmethod input-mode ((stream broadcast-stream))
   (loop for s in (broadcast-stream-streams stream)
            when (typep s 'modal-ascii-or-binary-tcp-stream-mixin)
            return (input-mode s)
            finally (return :ascii)))

(defmethod set-input-mode ((stream broadcast-stream) mode)
  (ecase mode
    (:ascii (ascii-input-mode stream))
    (:binary (binary-input-mode stream))))

;; methods on the binary file stream into the disk cache

;; new method for output-binary-file-stream -- JCMa 10/23/1998.
(defmethod stream-write-vector ((stream output-binary-file-stream) vector start end)
  (%fwrite-from-vector (slot-value stream 'fblock) vector start end)) 

;; -- akh 11/13/1998.
(defmethod stream-write-io-buffer ((to-stream output-binary-file-stream) inptr buffer-size)
   (let* ((fblock (slot-value to-stream 'fblock))
             (pb (when fblock (%fblock.pb fblock))))
      (if pb
         (fswrite pb buffer-size inptr)
         (error "~s is brain dead" to-stream)))) 

;; -- akh 11/13/1998
#+Open-Transport
(defmethod stream-write-io-buffer ((to-stream buffered-stream-mixin) inptr buffer-size)
  (stream-write-ptr to-stream inptr 0 buffer-size))

#+Open-Transport
(defmethod http::stream-copy-until-eof ((from-stream modal-ascii-or-binary-tcp-stream)  (to-stream  broadcast-stream) &optional (copy-mode :text))
   (declare (ignore copy-mode))
   (let ((from-io-buffer (stream-io-buffer from-stream))
           (streams (broadcast-stream-streams to-stream)))
      (with-io-buffer-locked (from-io-buffer)
          (loop while (if (eql 0 (io-buffer-incount from-io-buffer))
                              (%io-buffer-advance from-io-buffer t nil)
                              t)
                   for read-ptr =  (io-buffer-inptr from-io-buffer)
                   for buffer-size = (the fixnum (io-buffer-incount from-io-buffer))
                   do (dolist (stream streams)
                           (stream-write-io-buffer stream read-ptr buffer-size))
                   (decf (io-buffer-incount from-io-buffer) buffer-size)
                   (%incf-ptr (io-buffer-inptr from-io-buffer) buffer-size)	; required for chunk decoding -- JCMa 3/17/2000.
                   (incf (io-buffer-bytes-read from-io-buffer) buffer-size)))))

