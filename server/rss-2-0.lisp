;;;-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: rss2.0; -*- 
;;;
;;; Copyright John C. Mallery, 2004.
;;; All rights reserved. 
;;;------------------------------------------------------------------- 
;;;
;;; REALLY SIMPLE SYNDICATION (RSS) 2.0 GENERATION
;;;
;;; Based on spec found at: http://blogs.law.harvard.edu/tech/rss
;;;
;;; The toplevel interface consists in:
;;;
;;;      WITH-RSS-CHANNEL creates an XML environment describing an RSS channel.
;;;
;;;      DECLARE-RSS-ITEM declares an entry in the RSS channel.
;;;------------------------------------------------------------------- 
;;;
;;; PACKAGE DEFINITION
;;;

#|
(eval-when (:load-toplevel :execute :compile-toplevel)

(defpackage rss2.0
  (:use future-common-lisp www-utils url)
  (:nicknames rss)
  (:import-from html
   "FAST-FORMAT")
  (:import-from html2
   "*OUTPUT-STREAM*"
   "%WRITE-COMMAND-KEY-ARG"
   "%ISSUE-COMMAND"
   "%WITH-ENVIRONMENT"
   "ISSUE-COMMAND"
   "ISSUE-COMMAND*"
   "WITH-ENVIRONMENT")
  (:import-from http
   "WRITE-MIME-CONTENT-TYPE")
  (:export
   "DECLARE-RSS-CHANNEL-IMAGE"
   "DECLARE-RSS-ENCLOSURE"
   "DECLARE-RSS-ITEM"
   "WITH-RSS-CHANNEL"
   "WITH-RSS-DOCUMENT"
   "ENCODING-CHARACTER-DATA")))
|#

(in-package :rss2.0)

;;;------------------------------------------------------------------- 
;;;
;;; VERSION DEFINITIONS
;;;

(defconstant *xml-version* "1.0")

(defconstant *rss-version* "2.0")

(defconstant *rss-specification-uri* "http://blogs.law.harvard.edu/tech/rss"
  "A URL that points to the documentation for the format used in the RSS file.")

(define declare-xml-version (&optional (stream *output-stream*))
  "Declares the XML version of the current document."
  (fast-format stream "<?xml version=~S encoding=\"iso-8859-1\" ?>" *xml-version*))

(define-macro with-rss-document ((&key (stream '*output-stream*)) &body body)
  "Asserts the contents of BODY as an RSS document."
  `(progn (declare-xml-version ,stream)
     (%with-environment ("rss" :stream ,stream)
         (%write-command-key-arg ,stream "version" *rss-version*)
       (prog1 (progn . ,body)
         (fresh-line ,stream)))))

#|
(with-rss-document (:stream *standard-output*)
  (print 'foo))

|#

;;;------------------------------------------------------------------- 
;;;
;;; UTILITIES
;;;

(defun write-string-item (item stream)
  (etypecase item
    (string (write-string item stream))
    (function (funcall item stream))))

;; RSS places restrictions on the first non-whitespace characters of the
;; data in <link> and <url> elements. The data in these elements must
;; begin with an IANA-registered URI scheme, such as http://, https://
;; mailto:, news:, and javascript: are prohibited. Prior to RSS 2.0, 
;; the specification only allowed http:// and ftp://, however, in practice 
;; other URI schemes were in use by content developers and supported by aggregators.
;; Aggregators may have limits on the URI schemes they support. Content
;; developers should not assume that all aggregators support all schemes.

;; does not check for uri where no content can be accessed
(defun write-uri-string (uri stream &optional escape-search-url-p)
  (etypecase uri
    (function (funcall uri stream))
    (string
     (write-string (if (scheme-prefixed-url-p uri)
                       (coerce-url-string uri escape-search-url-p)
                     uri) ;let unprefixed strings through
                   stream))
    (uri (write-string (coerce-url-string uri escape-search-url-p) stream))))

(defun write-email-address (address stream)
  (write-string-item address stream))

(defmacro with-xml-comment ((&key (fresh-line t) (stream *output-stream*)) &body body)
  `(multiple-value-prog1
       (progn
         ,@(when fresh-line `((fresh-line ,stream))) 
         (fast-format ,stream "<!")
         ,@body)
     (fast-format ,stream ">")))

;; for encoding HTML data within descriptions, see: http://blogs.law.harvard.edu/tech/encodingDescriptions
(defmacro encoding-character-data ((&key (fresh-line t) string-for-null-stream (stream '*output-stream*))
                                   &body body)
  "Encodes character data emitted from BODY for proper handling by XML."
  (let ((code `(with-xml-comment (:fresh-line ,fresh-line :stream ,stream)
                 (multiple-value-prog1
                     (progn
                       (fast-format stream "[CDATA[")
                       ,@body)
                   (fast-format stream "]]")))))
    (if string-for-null-stream
        `(with-string-for-null-stream (,stream :inline ,(eq string-for-null-stream :inline))
                                      ,code)
      code))) 

;;;------------------------------------------------------------------- 
;;;
;;; CHANNEL DESCRIPTION
;;;

(define declare-title (title &optional (stream *output-stream*))
  "Declares a title element.
TITLE can be either a string or a function which is called with (stream)."
  (with-environment ("title" :stream stream)
    (rss:encoding-character-data (:fresh-line nil :stream stream)
      (write-string-item title stream))))

(define declare-link (reference &optional (stream *output-stream*))
  "Declares a link element.
REFERENCE can be either a string or a function which is called with (stream)."
  (with-environment ("link" :stream stream)
    (rss:encoding-character-data (:fresh-line nil :stream stream)
      (write-uri-string reference stream))))

(define declare-description (description &optional (stream *output-stream*))
  "Declares a description element.
DESCRIPTION can be either a string or a function which is called with (stream)."
  (with-environment ("description" :stream stream)
    (rss:encoding-character-data (:fresh-line nil :stream stream)
      (write-string-item description stream))))

(define %write-required-channel-elements (stream title link description)
  (declare-title title stream)
  (declare-link link stream)
  (declare-description description stream))

(define declare-publication-date (universal-time &optional (stream *output-stream*))
  "Declares the publication date, UNIVERSAL-TIME, on STREAM."
  (with-environment ("pubDate" :stream stream)
    (print-gmt-time stream universal-time)))

(define declare-category (category taxonomy &optional (stream *output-stream*))
  "Declares content to be an instance of CATEGORY in TAXONOMY on STREAM.

CATEGORY is a string that may be forward-slash-separated to identify a
hierarchic location in the TAXONOMY. Processors may establish
conventions for the interpretation of categories.

TAXONOMY is either a string or a uri that identifies locates the
categorization taxonomy."
  (%with-environment ("category" :stream stream)
      (when taxonomy
        (flet ((write-taxonomy (stream)
                 (write-char #\" stream)
                 (write-uri-string taxonomy stream)
                 (write-char #\" stream)))
          (declare (dynamic-extent #'write-taxonomy))
          (%write-command-key-arg stream "domain" #'write-taxonomy)))
    (fast-format stream "~S" category)))

(defun declare-categories (categories &optional (stream *output-stream*)) 
  (dolist (item categories)
    do (etypecase item
         (string (declare-category item nil stream))
         (cons 
          (destructuring-bind (category . taxonomy) item
            (declare-category category taxonomy stream))))))

(defun declare-rss-channel-image (stream uri title link &key width height description)
  "Declares an image for display with the RSS channel.

URI is the URL to an image (GIF, JPEG or PNG) that represents the
channel.

TITLE describes the image, it's used in the ALT attribute of the HTML
<img> tag when the channel is rendered in HTML.

LINK is the URL of the site, when the channel is rendered, the image
is a link to the site. (Note, in practice the image <title> and <link>
should have the same value as the channel's <title> and <link>.

DESCRIPTION contains text that is included in the TITLE attribute of
the link formed around the image in the HTML rendering.

WIDTH and HEIGHT are integers indicating the dimension of the image in
pixels.  Maximum value for WIDTH is 144, default value is 88. Maximum
value for HEIGHT is 400, default value is 31."
  ;; Required elements
  (with-environment ("image" :stream stream)
    (with-environment ("url" :stream stream)
      (rss:encoding-character-data (:fresh-line nil :stream stream)
        (write-uri-string uri stream)))
    (declare-title title stream)
    (declare-link link stream)
    ;; optional elements
    (cond-every
     (width
      (check-type width (integer 0 144))
      (with-environment ("width" :stream stream)
        (fast-format stream "~D" width)))
     (height
      (check-type height (integer 0 400))
      (with-environment ("height" :stream stream)
        (fast-format stream "~D" width)))
     (description
      (declare-description description stream)))))

(defun write-channel-image (spec stream)
  (etypecase spec
    (function (funcall spec stream))
    (cons
     (destructuring-bind (uri title link &key width height description) spec
       (declare-rss-channel-image stream uri title link :width width :height height :description description)))))

(defun declare-rating (rating &optional (stream *output-stream*))
  (with-environment ("rating" :stream stream)
    (typecase rating
      (string (write-string rating stream))
      (function (funcall rating stream)))))

(defun declare-skipped-hours (hours &optional (stream *output-stream*))
  (with-environment ("skipHours" :stream stream)
    (dolist (hour hours)
      (check-type hour (integer 0 23))
      (with-environment ("hour" :stream stream)
        (fast-format stream "~D" hour)))))

(defconstant +weekdays+ '("Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday" "Sunday"))

(defun declare-skipped-weekdays (weekdays &optional (stream *output-stream*))
  (flet ((day-string (day)
           (check-type day (integer 0 6))
           (nth day +weekdays+)))
    (declare (inline day-string))
    (with-environment ("skipDays" :stream stream)
      (dolist (day weekdays)
        (with-environment ("day" :stream stream)
          (fast-format stream "~A" (day-string day)))))))

;; Omitted optional channel elements: cloud, textInput
(defun %write-optional-channel-elements (stream publication-date categories build-date max-age copyright language image
                                                rating webmaster-email editor-email generator
                                                skipped-hours skipped-weekdays)
  (cond-every
   (publication-date
    (declare-publication-date publication-date stream))
   (categories
    (declare-categories categories stream))
   (build-date
    (with-environment ("lastBuildDate" :stream stream)
      (print-gmt-time stream build-date)))
   (max-age
    (with-environment ("ttl" :stream stream) ;time to live is in minutes, max-age is in seconds
      (fast-format stream "~D" (floor (/ (the integer max-age) 60)))))
   (copyright
    (with-environment ("copyright" :stream stream)
      (write-string-item copyright stream)))
   (language
    (with-environment ("language" :stream stream)
      (fast-format stream "~A" language)))
   (image
    (write-channel-image image stream))
   (rating
    (declare-rating rating stream))
   (editor-email
    (with-environment ("managingEditor" :stream stream)
      (write-email-address editor-email stream)))
   (webmaster-email
    (with-environment ("webMaster" :stream stream)
      (write-email-address webmaster-email stream)))
   (generator
    (with-environment ("generator" :stream stream)
      (fast-format stream "~A" generator)))
   (skipped-hours
    (declare-skipped-hours skipped-hours stream)) 
   (skipped-weekdays
    (declare-skipped-weekdays skipped-weekdays stream)))
  ;; Put the spec for this version
  (with-environment ("docs" :stream stream)
    (fast-format stream "~A" *rss-specification-uri*)))

(define-macro with-rss-channel ((title link description &key publication-date categories build-date max-age 
                                       (language "en") copyright image rating 
                                       webmaster-email editor-email generator 
                                       skipped-hours skipped-weekdays (stream '*output-stream*))
                                &body body)
  "Asserts the contents of BODY as an RSS document with specified channel descriptors.

Required Descriptors

     TITLE -- A string containing the name of the channel. This may also be
     a function that writes the string to a stream.  It's how people
     refer to your service. If you have an HTML website that contains the
     same information as your RSS file, the title of your channel should be
     the same as the title of your website.

     LINK -- A URI to the HTML website corresponding to the channel. This
     may  be a URI, a string,  or a function that writes the string to a stream.

     DESCRIPTION -- A string containing a phrase or sentence describing the
     channel. This may also be a function that writes the string to a a stream.

Optional Channel Descriptors

     BUILD-DATE -- The last time in universal time the content of the channel changed.

     CATEGORIES -- A list of category specs that declare the RSS channel to
     contain instances of the categories. A category spec is a category
     string or a dotted pair (CATEGORY . TAXONOMY).

          CATEGORY is a string that may be forward-slash-separated to
          identify a hierarchic location in the TAXONOMY. Processors
          may establish conventions for the interpretation of
          categories.

          TAXONOMY is either a string or a uri that identifies locates
          the categorization taxonomy.

     COPYRIGHT -- A string containing the copyright notice for content in the 
     channel. This may also be a function that writes the string to a stream.


     EDITOR-EMAIL -- A string containing an RFC 822 Email address for
     person responsible for editorial content.

     GENERATOR -- A string indicating the program used to generate the channel.

     IMAGE provides an image for display with the RSS channel. The
     format of image is a lambda list (URI TITLE LINK &key WIDTH
     HEIGHT DESCRIPTION)

          URI is the URL to an image (GIF, JPEG or PNG) that
          represents the channel.

          TITLE describes the image, it's used in the ALT attribute of
          the HTML <img> tag when the channel is rendered in HTML.

          LINK is the URL of the site, when the channel is rendered,
          the image is a link to the site. (Note, in practice the
          image <title> and <link> should have the same value as the
          channel's <title> and <link>.

          DESCRIPTION contains text that is included in the TITLE
          attribute of the link formed around the image in the HTML
          rendering.

          WIDTH and HEIGHT are integers indicating the dimension of
          the image in pixels.  Maximum value for WIDTH is 144,
          default value is 88. Maximum value for HEIGHT is 400,
          default value is 31.

     LANGUAGE -- An ISO language code for the language the channel is
     written in. This allows aggregators to group all Italian language
     sites, for example, on a single page. List are available from W3C.

     MAX-AGE -- The number of seconds that a channel can be cached
     before refreshing from the source.

     PUBLICATION-DATE -- The publication date in universal time for
     the content in the channel.

     RATING -- a string containing a PICS rating for the channel. This
     may also be a function that writes the string to a stream. The
     W3C Platform for Independent Content Selection (PICS) enables
     labels (metadata) to be associated with Internet content.

     SKIPHOURS -- A list of integers from 0 through 23 denoting an
     hour in GMT time when aggregators may not read the channel. The
     hour beginning at midnight is hour zero.

     SKIPDAYS -- A list of integers from 0 through 6 denoting a
     weekday in GMT time when may not read the channel. Monday is day
     zero.

     WEBMASTER-EMAIL -- A string containing an RFC 822 Email address for
     person responsible for technical issues relating to channel."
  `(with-environment ("channel" :stream ,stream)
     (%write-required-channel-elements ,stream ,title ,link ,description)
     (%write-optional-channel-elements ,stream ,publication-date ,categories ,build-date ,max-age ,copyright ,language 
                                       ,image ,rating ,webmaster-email ,editor-email ,generator ,skipped-hours ,skipped-weekdays)
     ,@body))

;;;------------------------------------------------------------------- 
;;;
;;; ITEM DESCRIPTION
;;;

(defun declare-rss-enclosure (stream uri media-type size)
  "Write XML declaring an enclosure on STREAM,
where URI is the location of the attached resource, MEDIA-TYPE is a
content-type-spec, and SIZE is the length of the resource in bytes."
  (fast-format stream "~&<enclosure url=~I~I />~&"
               (fast-format stream "\"~I\"" (write-uri-string uri stream t))
               (cond-every
                (media-type
                 (fast-format stream " type=\"~I\"" (write-mime-content-type media-type stream)))
                (size
                 (fast-format stream " length=\"~D\"" size)))))

(defun write-enclosure (stream enclosure)
  (flet ((write-encl (encl stream)
           (destructuring-bind (uri media-type . size) encl
             (declare-rss-enclosure stream uri media-type size))))
    (declare (inline write-encl)) 
    (etypecase enclosure
      (function
       (funcall enclosure stream))
      (cons
       (etypecase (car enclosure)
         (function
          (dolist (encl enclosure)
            (funcall encl stream)))
         (atom
          (write-encl enclosure stream))
         (cons
          (dolist (encl enclosure)
            (write-encl encl stream))))))))

(define declare-rss-item (stream &key link title description publication-date categories unique-id author source comments 
                                 enclosure)
  "Declares an entry in the RSS channel.
Each RSS entry is required to specify either a LINK and TITLE or a DESCRIPTION.
All other combinations of fields are optional.
     
     AUTHOR -- A string or function which supplies an RFC 822 Email address of the
     author of the item. For newspapers and magazines syndicating via
     RSS, the author is the person who wrote the article that the item
     describes. For collaborative weblogs, the author of the item
     might be different from the EDITOR or WEBMASTER. For a weblog
     authored by a single individual it would make sense to omit the
     author.

     CATEGORIES -- A list of category specs that declare the entry as
     an instance of the categories. A category spec is a category
     string or a dotted pair (CATEGORY . TAXONOMY).

          CATEGORY is a string that may be forward-slash-separated to
          identify a hierarchic location in the TAXONOMY. Processors
          may establish conventions for the interpretation of
          categories.

          TAXONOMY is either a string or a uri that identifies locates
          the categorization taxonomy.

     DESCRIPTION -- A string containing the item synopsis. This may also be
     a function that writes the string to a stream.

     ENCLOSURE -- A list (URI MEDIA-TYPE . SIZE) where URI is the
     location of the attached resource, MEDIA-TYPE is a
     content-type-spec, and SIZE is the length of the resource in
     bytes. For multiple enclosures, a list of these enclosure specs
     may be provided. Alternatively, a function or a list of functions
     can be supplied that will write the enclosure XML using
     DECLARE-RSS-ENCLOSURE.

     LINK -- A URI to the location where the content is available.
     This may be a URI, a string, or a function that writes the string
     to a stream.

     PUBLICATION-DATE -- The date in universal time when the item was
     published.

     SOURCE -- A URI to the source RSS channel from which the item
     originates.

     UNIQUE-ID -- A string or uri that uniquely and globally
     identifies the item. When present, an aggregator may choose to
     use this string to determine if an item is new. If unique-id is a
     uri, the reader may assume that it is a permalink to the item
     that can be openned in a Web browser. Otherwise, the reader must
     treat it as an opaque string. It is up to the source of the feed
     to guarantee the uniqueness of the string. An MD5 or a strong
     HTTP ETAG are appropriate as unique IDs."
  (unless (or (and title link)
              description)
    (error "Either DESCRIPTION or the TITLE and LINK must be specified."))
  (flet ((permalink-p (x)
           (typecase x
             (string (scheme-prefixed-url-p x))
             (uri t)
             (t nil))))
    (declare (inline permalink-p))
    (with-environment ("item" :stream stream)
      (cond-every
       (title
        (declare-title title stream))
       (link
        (declare-link link stream))
       (description
        (declare-description description stream))
       (publication-date
        (declare-publication-date publication-date stream))
       (categories 
        (declare-categories categories stream))
       (unique-id
        (%with-environment ("guid" :stream stream)
            (when (permalink-p unique-id)
              (%write-command-key-arg stream "isPermaLink" "true"))
          (fast-format stream "~A" unique-id)))
       (author
        (with-environment ("author" :stream stream)
          (write-email-address author stream)))
       (source
        (with-environment ("source" :stream stream)
          (write-uri-string source stream)))
       (comments
        (with-environment ("comments" :stream stream)
          (write-uri-string comments stream)))
       (enclosure
        (write-enclosure stream enclosure))))))
