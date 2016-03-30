;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: xml-query-data-model; -*-

(in-package "XML-QUERY-DATA-MODEL")


;; nb. timing does ot work for parse state-machine functions if they are
;; continuation based.
(defParameter *collected-time* 0)
(defParameter *collection-points* nil)
(defParameter *time-points* nil)
(defMacro collect-runtimes (&rest names)
  `(progn
     (setf *collected-time* 0)
     (setf *collection-points* ',names)
     ,@(mapcar #'(lambda (name) `(collect-runtime ',name)) names)))

(defun collect-runtime (name)
  (setf (get name :time) 0)
  (unless (get name :terms) ;; parse functions use multiple values
    (eval `(advise ,name (let ((time0 (get-internal-run-time))
                               (time1 0)
                               (delta 0)
                               (result nil)
                               (result-time nil))
                           (let ((*collected-time* 0))
                             (setf result (:do-it)
                                   result-time *collected-time*
                                   time1 (get-internal-run-time)
                                   delta (- time1 (+ time0 result-time))))
                           ;(print (list ',name time0 time1 delta result-time))
                           (incf *collected-time* (+ delta result-time))
                           (incf (get ',name :time) delta)
                           result)
                   :when :around))))

(defun runtimes ()
  (list* (prog1 *collected-time* (setf *collected-time* 0))
         (mapcar #'(lambda (name &aux time)
                     `(,name
                       ,@(unless (get name :terms)
                           (list (setf time (or (get name :time) 0)
                                       (get name :time) 0
                                       time (/  time (/ internal-time-units-per-second 1000.0)))))
                       ,@(when (setf time (get name :internal-time))
                           (setf (get name :internal-time) 0)
                           (list :internal (/ time (/ internal-time-units-per-second 1000.0))))
                       ,@(when (setf time (get name :elapsed-time))
                           (setf (get name :elapsed-time) 0)
                           (list :elapsed (/ time (/ internal-time-units-per-second 1000.0))))))
                 *collection-points*)))

(defun clear-runtimes ()
  (map nil #'(lambda (name) (eval `(unadvise ,name :when :around)))
        *collection-points*))

;;;
;;;
;;;

(defMethod make-function-printer-dialog ((symbol symbol))
  (make-function-printer-dialog (get symbol :atn-system)))

(defMethod make-function-printer-dialog ((atn bnfp::atn-system))
  (make-function-printer-dialog (mapcar #'bnfp::atn-name (bnfp::system-nets atn))))

(defMethod make-function-printer-dialog ((target null))
  (ed-beep))

(defMethod make-function-printer-dialog ((names cons) &aux selected-name (expand-macros t)
                                         (title (format nil "parser functions (~a)" (first names))))
  (setf names (sort names #'string-lessp))
  (make-instance 'dialog
    :window-title title
    :window-type :tool
    :view-size #@(332 84)
    :view-position (make-point (- *screen-width* 334) 44)
    :view-subviews
    (list (let ((menu nil))
            (setf menu (make-instance 'pop-up-menu
                         :view-nick-name :function-names
                         :view-position #@(4 4) :view-size #@(256 18)
                         :menu-items (mapcar #'(lambda (name)
                                                 (make-instance 'menu-item
                                                   :menu-item-title (string name)
                                                   :menu-item-action
                                                   #'(lambda (&aux (*package* (or (symbol-package name)
                                                                                  *package*)))
                                                       (setf selected-name name)
                                                       (set-dialog-item-text
                                                        (find-named-sibling menu :terms)
                                                        (write-to-string (get name :terms))))))
                                             names)))
            (setf selected-name (first names))
            menu)
          (make-instance 'button-dialog-item
            :view-position #@(268 6)
            :default-button t
            :dialog-item-text "pprint"
            :dialog-item-action
            #'(lambda (view &aux expression)
                (when selected-name
                  (setf expression
                        (function-lambda-expression (symbol-function selected-name))))
                (cond (expression
                       (setf view (make-instance 'fred-window
                                    :window-title (string selected-name)
                                    :view-position #@(4 60)
                                    :view-size (make-point (- *screen-width* 8) 512)
                                    :scratch-p t))
                       (if expand-macros
                         (bnfp::atn-pprint expression view)
                         (pprint expression view))
                       (finish-output view))
                      (t
                       (ed-beep)))))
          (make-instance 'static-text-dialog-item
            :dialog-item-text ""
            :view-font '("Monaco" :plain 9)
            :part-color-list (list :body (make-color 56000 56000 56000)
                                   :frame *black-color*)
            :view-position #@(4 30) :view-size #@(256 42)
            :view-nick-name :terms)
          (make-instance 'check-box-dialog-item
            :dialog-item-text "expand"
            :view-position #@(264 30)
            :check-box-checked-p expand-macros
            :dialog-item-action #'(lambda (item)
                                    (setf expand-macros (check-box-checked-p item)))))))
  

#|

(collect-runtimes bnfp:atn-reduce-structure
         token-reader input-reference
         prefix-value)
(runtimes)
(clear-runtimes)






(time (dotimes (x 10000) (test1 1)))
(unadvise test1)
(get 'test1 :time)

(let ((BNF-PARSER::*ATN-REDUCE t)
      (bnfp::*atn-reduce-arglist nil))
  (time (dotimes (x 1000)
          (bnfp:atn-reduce-structure #'|AttChildSequence-Constructor|
                                     '|AttChildSequence-Constructor|
                                     '((:first  "first") (:second  "second"))
                                     0))))



(make-function-printer-dialog 'xmlp::|Document-Parser|)
(make-function-printer-dialog 'xq::|Query-Parser|)
(make-function-printer-dialog 'xp::|LocationPath-Parser|)


(defun test1 (x) (+ x x))
(setf (get 'test1 :time) 0)
(advise test1 (let ((time0 (get-internal-run-time)) (time1 0))
               (:do-it)
               (setf time1 (get-internal-run-time))
               (incf (get 'test1 :time) (- time1 time0)))
        :when :around)

|#
