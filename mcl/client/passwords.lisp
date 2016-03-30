;;;-*- Mode: Lisp; Package: CCL -*-
;;;------------------------------------------------------------------- 
;;;
;;;  The definition of a class of editable-text-dialog-item that doesn't
;;;  echo the characters entered. Denis R Howlett <drh@world.std.com> 

;; 06/07/01 JCMa Updated initialize-instance to be an after method and handle the init arg :dialog-item-text correctly
;;                            Moved character interception onto view-key-event-handler around method rather than the after method on keystroke-function

(in-package :CCL)

(export '(password-text-dialog-item) :CCL) 

(defclass password-text-dialog-item 
   (editable-text-dialog-item)
   ((alter-ego :initform nil :initarg :alter-ego :accessor password-text-alter-ego)     ;regular editable-text-dialog-item which holds the true text
     (echo-char :initform #\245 :initarg :echo-char :accessor password-text-echo-char)))        ;holds the character to be used for echoing

;; this method creates a regular editable-text-dialog-item and stores it in the alter-ego slot.
;; Handles initialization args correctly -- JCMa 6/7/2001. 
(defmethod initialize-instance :after ((item password-text-dialog-item) &key (dialog-item-text nil dialog-item-text-supplied-p))
   (declare (ignore args))
   (let* ((text (when dialog-item-text-supplied-p dialog-item-text))
             (size (length text)))
      (setf (password-text-alter-ego item) (make-instance 'editable-text-dialog-item :dialog-item-text text))
      (when (< 0 size)
          (ccl::set-dialog-item-text item (make-array (max 10 size) :element-type 'character :adjustable t :fill-pointer size 
                                                                               :initial-element (password-text-echo-char item))))))

;; Replaces method on keystroke-function for correct operation -- JCMa 6/7/2001.
(defmethod view-key-event-handler :around ((item password-text-dialog-item) char)
   (call-next-method item (if (graphic-char-p char) (password-text-echo-char item) char))
   (view-key-event-handler (password-text-alter-ego item) char)) 

;; To handle the mouse, we have to see if the user has marked a region
;; or moved the insertion point. Fortunately, the functions 
;; selection-range and set-selection-range do both for us, so, whenever
;; the user uses the mouse, update the selection range and cursor 
;; position. This ensures that the user can delete a whole range etc.
(defmethod view-click-event-handler :after ((item password-text-dialog-item) where) 
   (declare (ignore where))
   (let ((alter-ego (password-text-alter-ego item)))
      (multiple-value-bind (position cursorpos)
                                      (selection-range item)
          (set-selection-range alter-ego position cursorpos)))) 

;; this allows transparent access to the clear text - call this just like for any dialog item
(defmethod dialog-item-text ((item password-text-dialog-item))
  (dialog-item-text (password-text-alter-ego item))) 

#|
   ;; This is a simple example of the use of the password-text-dialog-item
(defun get-password ()
   (let ((win (make-instance 'dialog :window-type :double-edge-box :view-position :centered :view-size #@(200 100) :close-box-p nil
                                           :view-font '("Chicago" 12 :SRCOR :PLAIN)))
           (password (make-dialog-item 'password-text-dialog-item #@(20 44) #@(133 16) "" nil :allow-returns nil)))
      (add-subviews win 
         (make-dialog-item 'static-text-dialog-item #@(16 14) #@(141 16) "Enter the password:" nil)
         password
         (make-dialog-item 'button-dialog-item #@(91 81) #@(62 16) "OK"
                                      #'(lambda
                                            (item)
                                            item
                                            (return-from-modal-dialog
                                              (dialog-item-text password)))
                                      :default-button t))
      
      (modal-dialog win)))
|#

