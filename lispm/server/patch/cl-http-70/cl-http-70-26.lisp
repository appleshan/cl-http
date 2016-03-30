;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: USER; Base: 10; Patch-File: T -*-
;;; Patch file for CL-HTTP version 70.26
;;; Reason: Function CL-USER::CLOSE-TCB:  proper handling of timeout case in CLOSE-TCB
;;; Written by Reti, 3/09/00 11:04:35
;;; while running on FUJI-2 from FUJI:/usr/lib/symbolics/Relatus-C-MIT-8-5.vlod
;;; with Open Genera 2.0, Genera 8.5, Documentation Database 440.12,
;;; Logical Pathnames Translation Files NEWEST, Color 427.1, Graphics Support 431.0,
;;; Genera Extensions 16.0, Essential Image Substrate 433.0,
;;; Color System Documentation 10.0, SGD Book Design 10.0, Images 431.2,
;;; Image Substrate 440.4, CLIM 72.0, Genera CLIM 72.0, CLX CLIM 72.0,
;;; PostScript CLIM 72.0, CLIM Documentation 72.0, Metering 444.0,
;;; Metering Substrate 444.1, Conversion Tools 436.0, Hacks 440.0, 8-5-Patches 2.16,
;;; MAC 414.0, Relatus Natural Language Environment 183, RELATUS Utilities 29.3,
;;; Experimental Gnoscere Representation System 13.9,
;;; Dynamic Window Hardcopy Extensions 4.1, Background Bug Reporting 12.0,
;;; Experimental Relatus Parser Semantic Perception 27.1, Showable Procedures 36.3,
;;; Binary Tree 34.0, Experimental Reference System 32.2,
;;; Experimental Semantic Inversion 19.3, Experimental Lexical Classifier 3.0,
;;; Experimental Gnoscere Activity System 6.2, Flavor Resource 1.0,
;;; Relatus Parser 5.7, Experimental Relatus Generator 6.2,
;;; Lisp System Extensions 72.2, Object Resource 36.0, Agent Utilities 45.0,
;;; HTTP Server 70.25, W3 Presentation System 8.1, Ivory Revision 5,
;;; VLM Debugger 329, Genera program 8.8, DEC OSF/1 V4.0 (Rev. 110),
;;; 1280x976 8-bit PSEUDO-COLOR X Screen FUJI:0.0 with 224 Genera fonts (DECWINDOWS Digital Equipment Corporation Digital UNIX V4.0 R1),
;;; Machine serial number -2141189584,
;;; Patch TCP hang on close when client drops connection. (from HTTP:LISPM;SERVER;TCP-PATCH-HANG-ON-CLOSE.LISP.10).



(SCT:FILES-PATCHED-IN-THIS-PATCH-FILE 
  "SYS:IP-TCP;TCP.LISP.3078")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "SYS:IP-TCP;TCP.LISP.3078")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: Lisp; Syntax: Common-Lisp; Package: TCP; Base: 10; Lowercase: Yes -*-")

(defun close-tcb (tcb &optional abort-p)
  (process:with-no-other-processes
    (when (or (tcb-read-segs tcb)
              (tcb-received-segs tcb))
      ;; the theory here is that if the LispM is closing a connection, it
      ;; is really finished with it.  There shouldn't be any input
      ;; packets, and if there are, they will never be read.  Therefore,
      ;; it isn't really a syncronous close.
;     (setq abort-p t)                          ;If abort is set, we send a reset
                                                ;in abort-tcb below -- Kalman
      (free-all-read-segs tcb)
      (free-all-received-segs tcb))
    (if abort-p
        (abort-tcb tcb)
	(let ((completed-normally nil))
	  (unwind-protect
	      (case (tcb-state tcb)
		((:listen :syn-sent)
		 (setq completed-normally t)
		 (remove-tcb tcb))
		((:syn-received :established :close-wait)
		 (setf (tcb-substate tcb) :closing)
		 (send-fin-for-tcb tcb (get-tcp-segment tcb))
		 ;; the lispm hangs forever here if there is no timeout  12/5/95 -- JCMa.
		 #|(tcb-travel-through-states tcb "TCP Closing" nil
                                        :syn-received :established :close-wait :last-ack
                                        :fin-wait-1 :fin-wait-2 :closing)|#
		 (tcb-travel-through-states tcb "TCP Closing" *tcp-close-timeout*
					    :syn-received :established :close-wait :last-ack
					    :fin-wait-1 :fin-wait-2 :closing)
		 (unless (member (tcp::tcb-state tcb) '(:time-wait :closed))
		   #+ignore
		   (tv:notify nil "Old state was ~s" (tcp::tcb-state tcb))
		   (setf (tcp::tcb-state tcb) :closed))
		 (setq completed-normally t)
		 (when (eq (tcb-state tcb) :closed)
		   (remove-tcb tcb)
		   ;; otherwise it is in :time-wait and will be removed by the background
		   ))
		((:fin-wait-1 :fin-wait-2) (setq completed-normally t))
		(:closed
		  (setq completed-normally t)
		  (unless (eq (tcb-substate tcb) :inactive)
		    (remove-tcb tcb)))
		(otherwise (bad-tcp-connection-state tcb "close a connection")))
	    (tcb-wakeup-reader tcb)
	    (tcb-wakeup-writer tcb)
	    (unless completed-normally
	      (abort-tcb tcb)))))))

