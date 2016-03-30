;;;   -*- Mode: lisp; Package: (netscape4.0 :use (future-common-lisp ns3.0 www-utils url)); BASE: 10; Syntax: ansi-common-lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-

;;;
;;; (c) Copyright 1997, 2005, John C. Mallery
;;;     All Rights Reserved.
;;;
;;;------------------------------------------------------------------- 
;;;
;;; INTERFACE FOR AUTHORING HTML USING NETSCAPE 4.0 EXTENSIONS
;;;
;;; Issues:
;;;
;;; 1. Conditional comments (based on Javascript predicate evaluation) not implemented.
;;;

(in-package :netscape4.0)

;; Make sure we're exporting a compete set of symbols.
(eval-when (:execute :load-toplevel)   
(let ((html-pkg (find-package :ns3.0))
      (netscape-pkg (find-package :netscape4.0)))
  (do-external-symbols (sym html-pkg)
    (export (intern (symbol-name sym) netscape-pkg) netscape-pkg))))                                             ;close eval-when

;;;------------------------------------------------------------------- 
;;;
;;; LAYERS
;;;
;;; http://developer.netscape.com/library/documentation/communicator/layers/index.htm

(defun %get-layer-class (arg)
  (ecase arg
    (:fixed "LAYER")
    (:in-flow "ILAYER")))

(defun %write-pixel-or-percent-argument (stream option value)
  (if (<= 0 value 1)
      (let ((w (concatenate 'string
                            (write-to-string (floor (* value 100)) :base 10. :escape nil)
                            "%")))
        (declare (dynamic-extent w))
        (%write-command-key-arg stream option w))
      (%write-command-key-arg stream option value t)))

(defun %make-layer-clip-args (clip-x1 clip-y1 clip-x2 clip-y2)
  (flet ((prepare-arg (value)
           (cond ((<= 0 value 1)
                  (let ((val (write-to-string (floor (* value 100)) :base 10. :escape nil)))
                    (declare (dynamic-extent val))
                    (concatenate 'string val "%")))
                 (t (check-type value integer)
                    value))))
    (values (prepare-arg clip-x1)
            (prepare-arg clip-y1)
            (prepare-arg clip-x2)
            (prepare-arg clip-y2))))

(defun %write-layer-arguments (stream name reference visibility background background-url
                                      x-page-origin y-page-origin x-origin y-origin width height z-index parent
                                      clip-x1 clip-y1 clip-x2 clip-y2 events)
  (cond-every
    (name (%write-command-key-arg stream "NAME" name))
    (x-page-origin (%write-pixel-or-percent-argument stream "PAGEX" x-page-origin))
    (y-page-origin (%write-pixel-or-percent-argument stream "PAGEY" y-page-origin))
    (x-origin (%write-pixel-or-percent-argument stream "LEFT" x-origin))
    (y-origin (%write-pixel-or-percent-argument stream "TOP" y-origin))
    (background
      (%write-command-key-arg stream "BGCOLOR" (color-mapping background t)))
    (background-url
      (%write-command-key-arg stream "BACKGROUND" (url:coerce-url-string background-url)))
    (reference
      (%write-command-key-arg stream "SRC" (url:coerce-url-string reference t)))
    (width (%write-command-key-arg stream "WIDTH" width t))
    (height (%write-command-key-arg stream "height" height t))
    (z-index
      (typecase z-index
        (integer (%write-command-key-arg stream "Z-INDEX" z-index t))
        (t (check-type parent string)
           (unless parent
             (error "When z-index is ~S, PARENT must be specified." z-index))
           (%write-command-key-arg stream
                                   (ecase z-index
                                     (:above "ABOVE")
                                     (:below "BELOW"))
                                   parent))))
    (visibility
      (unless (member visibility '(:show :hide :inherit))
        (error "visibility is ~S, which is not one of :SHOW, :HIDE or :INHERIT." visibility))
      (%write-command-key-arg stream "VISIBILITY" visibility))
    ((or clip-x2 clip-y2)
     (cond ((and clip-x2 clip-y2 clip-x1 clip-y1)
            (multiple-value-bind (x1 y1 x2 y2)
                (%make-layer-clip-args clip-x1 clip-y1 clip-x2 clip-y2)
              (declare (dynamic-extent x1 y1 x2 y2))
              (fast-format stream " CLIP=~D,~D,~D,~D" x1 y1 x2 y2)))
           (t (error "WITH-LAYER: Incomplete arguments for clipping box:~
                      CLIP-X1: ~S~&CLIP-Y1: ~S~&CLIP-X2: ~S~&CLIP-Y2:~S"
                     clip-x1 clip-y1 clip-x2 clip-y2))))
    (events
      (dolist (event events)
        (html2::%write-input-type-event-arg stream event)))))

(define-macro with-layer ((&key name (class :fixed) reference visibility background background-url
                                x-page-origin y-page-origin x-origin y-origin z-index parent width height
                                (clip-x1 0) (clip-y1 0) clip-x2 clip-y2 events (stream '*output-stream*)) &body body)
  "Establishes a layer in the HTML output on STREAM.
Layers provide idependent control of different regions of an HTML page and allow
overlay operations. Javascript operations make it possible to manipulate layers
dynamically on the client side. 
See: http://developer.netscape.com/library/documentation/communicator/layers/index.htm

NAME (optional) is a string identifying the layer.
CLASS (optional) is either :FIXED or :IN-FLOW.
REFERENCE (optional) is a URL containing HTML for display in the layer.

VISIBILITY (optional) is one of :SHOW, :HIDE or :INHERIT. The default is to
inherit the visibility of the parent.

BACKGROUND  (optional) is a color for the background.
BACKGROUND-URL (optional) is  an image URL to use as the background. 
Layers are transparent so that lower layers show through transparent areas.
These attributes will make a layer opaque.

X-PAGE-ORIGIN (optional) is the x-origin in pixels relative to the enclosing document. 
Y-PAGE-ORIGIN (optional) is the y-origin in pixels relative to the enclosing document.

X-ORIGIN (optional) is the X-origin in pixels relative to the enclosing layer. 
Y-ORIGIN (optional) is the Y-origin in pixels relative to the enclosing layer.

For positioned layers, the origin is the the upper-left hand corner of layer
containing the layer. Coordinates increase downwards and rightwards.  For
in-flow layers, the origin is the current position in the broswer rendering
when the layer is encountered. X and Y provides offset from the default
positioning.

WIDTH (optional) is the number of pixels on the horizontal axis before
wrapping begins.  If elements like images extend beyond width, the layer
will be extended horizontally.

HEIGHT (optional) is the number of pixels on the vertical axis.  If the
contents do not fit within HEIGHT, the layer will be extended vertically.
HEIGHT is intended to provide a reference height for percentage attributes of
inferior layers. HEIGHT defaults to the minimum height that contains all the
contents.

Z-INDEX (optional) is the position of the layer relative to other layers. The
default behavior is to stack new layers above previous layers. Z-INDEX can be
a number indicating the stacking of the layers.  Positive values place the
layer above its parent whereas negative values place it below. If Z-INDEX is
either :ABOVE or :BELOW, the PARENT argument must supply the name of an
earlier layer.

The clipping box is the visible area within the layer. CLIP-X1, CLIP-Y1 is the
origin and CLIP-X2, CLIP-Y2 is the extent of the box. All values are in
pixels.

Coordinate parameters may be specified as proportions relative to the
corresponding component of the enclosing layer or document. A proportion is a
float between zero and 1. Parameters accepting floats are: X-PAGE-ORIGIN,
Y-PAGE-ORIGIN, X-ORIGIN, Y-ORIGIN, WIDTH, HEIGHT, CLIP-X1, CLIP-Y1, CLIP-X2,
CLIP-Y2.

EVENTS (optional) is a list of client-side events processed when the form is
submitted."
  `(%with-environment (,(typecase class
                          (keyword
                            (%get-layer-class class))
                          (t `(%get-layer-class ,class)))
                       :stream ,stream)
                      (%write-layer-arguments
                        ,stream ,name ,reference ,visibility ,background ,background-url
                        ,x-page-origin ,y-page-origin ,x-origin ,y-origin ,width ,height ,z-index
                        ,parent ,clip-x1 ,clip-y1 ,clip-x2 ,clip-y2 ,events)
     ,@body))

(define-macro without-layers-capability ((&key (stream '*output-stream*)) &body body)
  "Any HTML emitted within BODY is ignored by layers capable clients but can be used
to provide alternate displays for clients without layers capability."
  `(with-environment ("NOLAYER" :stream ,stream)
     ,@body))

(defun %write-make-font-args (stream color face point-size size weight)
  (cond-every
    (color
      (%write-command-key-arg stream "COLOR" (color-mapping color t)))
    (face
      (typecase face
        (atom (%write-command-key-arg stream "FACE" face))
        (cons
          (%write-command-key-arg stream "FACE" (car face))
          (loop for item in (cdr face)
                do (write-char #\, stream)
                   (write (etypecase item
                            (keyword (symbol-name item))
                            (string item))
                          :escape t :stream stream)))))    
    (point-size
      (%write-command-key-arg stream "POINT-SIZE" point-size t))
    (size
      (unless (< 0 size 8)
        (error "SIZE, ~S, is not an integer from 1 through 7." size))
      (%write-command-key-arg stream "SIZE" size t))
    (weight
      (unless (< 99 weight 901)
        (error "WEIGHT, ~S, is not an integer from 100 through 900." weight))
      (%write-command-key-arg stream "WEIGHT" weight t))))

(define-macro with-font ((&key color face point-size size weight (stream '*output-stream*)) &body body)
  "Declares the current font size to be SIZE or POINT-SIZE, the font face to be
FACE, the weight or boldness to be WEIGHT, and the color to be COLOR within BODY."
  `(%with-environment ("FONT" :fresh-line nil :stream ,stream)
                      (%write-make-font-args ,stream ,color ,face ,point-size ,size ,weight)
     ,@body))
