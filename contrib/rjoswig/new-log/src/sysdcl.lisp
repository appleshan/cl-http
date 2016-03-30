;;;   -*- Mode: LISP; Package: cl-user; BASE: 10; SYNTAX: ansi-common-lisp; -*-
;;;
;;;

;;;------------------------------------------------------------------- 
;;;
;;; SYSTEM DEFINITION for NEW-LOG
;;;
;;; Load this File!


#+Genera
(sct:defsystem new-log
    (:pretty-name "New Log"
     :default-pathname "camille:>software>new-log>"
     :journal-directory "camille:>software>new-log>patch>"
     :initial-status :experimental
     :patchable t
     :source-category :basic)
  (:module pointers
   ("sysdcl"
    "example")
   (:type :lisp-example))
  (:serial
    "PACKAGE"
    "KDTREE"
    "TIME"
    "NEW-LOG"))

#+(and clim (not genera))
(clim-defsys:defsystem new-log
  (:default-pathname "ccl:lisp-software;lavielle;new-log;")
  ("package")
  ("kdtree"          :load-before-compile ("package"))
  ("time"            :load-before-compile ("package"))
  ("new-log"         :load-before-compile ("time" "package"))
  ("clim-ui"         :load-before-compile ("new-log")
                     :features CLIM))

(setf (logical-pathname-translations "NEW-LOG")
      `(("NEW-LOG:**;*.*" ,(make-pathname :defaults *load-pathname*
                                         :type :wild
                                         :name :wild
                                         :directory (append (butlast (pathname-directory *load-pathname*))
                                                            (list :wild-inferiors))))))

#+(and (or LispWorks4 LispWorks5 LispWorks6) CL-HTTP)
(sct-defsystem
 new-log
 (:pretty-name "Rainer Joswig's New-Log CL-HTTP Log Analysis Package"
  :default-pathname "NEW-LOG:SRC;"
  :journal-directory "NEW-LOG;SRC;PATCH;"
  :initial-status :experimental
  :patchable t
  :source-category :basic)
 (:serial
  "PACKAGE"
  "KDTREE"
  "TIME"
  "NEW-LOG"
  "REPORT"))


