;;; -*- Mode: lisp; Syntax: common-lisp; Package: http; Base: 10; Patch-File: t -*-
;;; Patch file for CL-HTTP version 70.120
;;; Reason: Patch in multipart form parsing and file upload
;;; 
;;; Add MIME Type text rfc822-headers
;;; Function HTTP:WRITE-TO-ARMOR-PLATED-STRING:  change pad character from @ to * to avoid conflict in *MIME-TSPECIAL-CHARS*
;;; Function HTTP:READ-FROM-ARMOR-PLATED-STRING:  use new pad char and maintain backward compatibility.
;;; Function HTTP::PARSE-MIME-CONTENT-DISPOSITION-HEADER:  use start value for tokenization.
;;; DEFINE-HEADER :CONTENT-DISPOSITION:  update for new parser.
;;; DEFINE-INPUT-TYPE HTML2:FILE:  -
;;; Variable HTML2::*FILE-UPLOAD-DEFAULT-PATHNAME*:  -
;;; Function HTML2::FILE-UPLOAD-DIRECTORY-INDEX:  -
;;; Function HTML2::FILE-UPLOAD-DIRECTORY-FOR-INDEX:  -
;;; Function HTML2::FILE-UPLOAD-MAKE-QUERY:  -
;;; Function HTML2::FILE-UPLOAD-UNPACK-QUERY-BAGGAGE:  -
;;; Function (CLOS:METHOD HTML2:ACCEPT-INPUT (HTML2:FILE T)):  -
;;; Function HTML2:WITH-FILLOUT-FORM:  bind *open-form* to the mime encoding of the form.
;;; Function HTTP::%PARSE-FORM-COLLECT-FORM-VALUE:  abstract the collector.
;;; Function HTTP::PARSE-FORM-RAW-VALUES:  use %parse-form-collect-form-value.
;;; Function HTTP::HANDLING-FORM-PARSING-ERRORS:  -
;;; Function HTTP::%POST-DOCUMENT-HANDLE-URL-CODED-FORM:  -
;;; Function (CLOS:METHOD HTTP::APPLY-FORM-RESPONSE-FUNCTION (T T NULL)):  handle null form values.
;;; Function HTTP::HANDLING-FORM-PARSING-ERRORS: 
;;; Function HTTP::MIME-CONTENT-TYPE-KEYWORD:  -
;;; Function HTTP::MIME-CONTENT-TYPE-PRIMARY-EXTENSION-STRING:  -
;;; DEFINE-CONDITION HTTP::BAD-FORM-DATA:  new.
;;; DEFINE-CONDITION HTTP::BAD-FORM-DATA-POSTED:  -
;;; DEFINE-CONDITION HTTP::BAD-MULTIPART-FORM-DATA-POSTED:  -
;;; Function HTTP::CHAR-POSITION:  fix scoping bug on char.
;;; Function HTTP::MIME-CONTENT-TYPE-SPEC-FOR-PATHNAME-TYPE:  -
;;; Function (CLOS:METHOD HTTP:POST-DOCUMENT (URL:FORM-PROCESSING-MIXIN (EQL :MULTIPART) (EQL :FORM-DATA) T)):  hook up new stuff.
;;; Variable HTTP::*MIME-MULTIPART-BLOCK-MAXIMUM-SIZE*:  -
;;; Function HTTP::SIGNAL-MIME-MULTIPART-BLOCK-MAXIMUM-SIZE-EXCEEDED:  -
;;; Function HTTP::WITH-MIME-MULTIPART-BLOCK-SIZE-LIMIT:  -
;;; Variable HTTP::*FILE-UPLOAD-MAXIMUM-SIZE*:  -
;;; Function HTTP::SIGNAL-FILE-UPLOAD-MAXIMUM-BYTES-EXCEEDED:  -
;;; Function (CLOS:METHOD URL::FILE-UPLOAD-MAXIMUM-BYTES (URL:FORM-PROCESSING-MIXIN)):  -
;;; Written by JCMa, 4/12/01 20:27:37
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5.vlod
;;; with Open Genera 2.0, Genera 8.5, Documentation Database 440.12,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.19, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.4,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.119,
;;; W3 Presentation System 8.1, CL-HTTP Server Interface 53.0,
;;; Symbolics Common Lisp Compatibility 4.0, Comlink Packages 6.0,
;;; Comlink Utilities 10.3, COMLINK Cryptography 2.0, Routing Taxonomy 9.0,
;;; COMLINK Database 11.26, Email Servers 12.0, Comlink Customized LispM Mailer 7.1,
;;; Dynamic Forms 14.5, Communications Linker Server 39.8,
;;; Lambda Information Retrieval System 22.3, Experimental Genera 8 5 Patches 1.0,
;;; Genera 8 5 System Patches 1.26, Genera 8 5 Mailer Patches 1.1,
;;; Genera 8 5 Joshua Patches 1.0, Genera 8 5 Statice Runtime Patches 1.0,
;;; Genera 8 5 Statice Patches 1.0, Genera 8 5 Statice Server Patches 1.0,
;;; Genera 8 5 Statice Documentation Patches 1.0, Genera 8 5 Clim Patches 1.2,
;;; Genera 8 5 Genera Clim Patches 1.0, Genera 8 5 Postscript Clim Patches 1.0,
;;; Genera 8 5 Clim Doc Patches 1.0, Genera 8 5 Lock Simple Patches 1.0, Jcma 43,
;;; HTTP Proxy Server 6.19, HTTP Client Substrate 4.9, Statice Server 466.2,
;;; Ivory Revision 5, VLM Debugger 329, Genera program 8.16,
;;; DEC OSF/1 V4.0 (Rev. 110),
;;; 1280x994 8-bit PSEUDO-COLOR X Screen FUJI:0.0 with 224 Genera fonts (DECWINDOWS Digital Equipment Corporation Digital UNIX V4.0 R1),
;;; Machine serial number -2141189585,
;;; Domain Fixes (from CML:MAILER;DOMAIN-FIXES.LISP.33),
;;; Don't force in the mail-x host (from CML:MAILER;MAILBOX-FORMAT.LISP.24),
;;; Make Mailer More Robust (from CML:MAILER;MAILER-FIXES.LISP.15),
;;; Patch TCP hang on close when client drops connection. (from HTTP:LISPM;SERVER;TCP-PATCH-HANG-ON-CLOSE.LISP.10),
;;; Add CLIM presentation and text style format directives. (from FV:SCLC;FORMAT.LISP.20),
;;; Fix Statice Lossage (from CML:LISPM;STATICE-PATCH.LISP.3),
;;; Make update schema work on set-value attributes with accessor names (from CML:LISPM;STATICE-SET-VALUED-UPDATE.LISP.1),
;;; COMLINK Mailer Patches. (from CML:LISPM;MAILER-PATCH.LISP.107),
;;; Clim patches (from CML:DYNAMIC-FORMS;CLIM-PATCHES.LISP.48),
;;; Domain ad host patch (from W:>reti>domain-ad-host-patch.lisp.21),
;;; Background dns refreshing (from W:>reti>background-dns-refreshing.lisp.7),
;;; Cname level patch (from W:>Reti>cname-level-patch.lisp.10),
;;; Fix FTP Directory List for Periods in Directory Names (from W:>Reti>fix-ftp-directory-list.lisp.7),
;;; TCP-FTP-PARSE-REPLY signal a type error when control connection lost. (from W:>Reti>tcp-ftp-parse-reply-patch.lisp.1).

;;; Patch file for CL-HTTP version 70.120
;;; Written by JCMa, 4/18/01 22:20:21
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5.vlod
;;; with Open Genera 2.0, Genera 8.5, Documentation Database 440.12,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.19, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.4,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.121,
;;; W3 Presentation System 8.1, CL-HTTP Server Interface 53.0,
;;; Symbolics Common Lisp Compatibility 4.0, Comlink Packages 6.0,
;;; Comlink Utilities 10.3, COMLINK Cryptography 2.0, Routing Taxonomy 9.0,
;;; COMLINK Database 11.26, Email Servers 12.0, Comlink Customized LispM Mailer 7.1,
;;; Dynamic Forms 14.5, Communications Linker Server 39.8,
;;; Lambda Information Retrieval System 22.3, Experimental Genera 8 5 Patches 1.0,
;;; Genera 8 5 System Patches 1.26, Genera 8 5 Mailer Patches 1.1,
;;; Genera 8 5 Joshua Patches 1.0, Genera 8 5 Statice Runtime Patches 1.0,
;;; Genera 8 5 Statice Patches 1.0, Genera 8 5 Statice Server Patches 1.0,
;;; Genera 8 5 Statice Documentation Patches 1.0, Genera 8 5 Clim Patches 1.2,
;;; Genera 8 5 Genera Clim Patches 1.0, Genera 8 5 Postscript Clim Patches 1.0,
;;; Genera 8 5 Clim Doc Patches 1.0, Genera 8 5 Images Patches 1.0,
;;; Genera 8 5 Image Substrate Patches 1.0, Genera 8 5 Lock Simple Patches 1.0,
;;; HTTP Proxy Server 6.19, HTTP Client Substrate 4.9, Statice Server 466.2,
;;; Images 431.2, Genera Extensions 16.0, Essential Image Substrate 433.0,
;;; Image Substrate 440.4, Jcma 43, Ivory Revision 5, VLM Debugger 329,
;;; Genera program 8.16, DEC OSF/1 V4.0 (Rev. 110),
;;; 1280x994 8-bit PSEUDO-COLOR X Screen FUJI:0.0 with 224 Genera fonts (DECWINDOWS Digital Equipment Corporation Digital UNIX V4.0 R1),
;;; Machine serial number -2141189585,
;;; Domain Fixes (from CML:MAILER;DOMAIN-FIXES.LISP.33),
;;; Don't force in the mail-x host (from CML:MAILER;MAILBOX-FORMAT.LISP.24),
;;; Make Mailer More Robust (from CML:MAILER;MAILER-FIXES.LISP.15),
;;; Patch TCP hang on close when client drops connection. (from HTTP:LISPM;SERVER;TCP-PATCH-HANG-ON-CLOSE.LISP.10),
;;; Add CLIM presentation and text style format directives. (from FV:SCLC;FORMAT.LISP.20),
;;; Fix Statice Lossage (from CML:LISPM;STATICE-PATCH.LISP.3),
;;; Make update schema work on set-value attributes with accessor names (from CML:LISPM;STATICE-SET-VALUED-UPDATE.LISP.1),
;;; COMLINK Mailer Patches. (from CML:LISPM;MAILER-PATCH.LISP.107),
;;; Clim patches (from CML:DYNAMIC-FORMS;CLIM-PATCHES.LISP.48),
;;; Domain ad host patch (from W:>reti>domain-ad-host-patch.lisp.21),
;;; Background dns refreshing (from W:>reti>background-dns-refreshing.lisp.11),
;;; Cname level patch (from W:>Reti>cname-level-patch.lisp.10),
;;; Fix FTP Directory List for Periods in Directory Names (from W:>Reti>fix-ftp-directory-list.lisp.7),
;;; TCP-FTP-PARSE-REPLY signal a type error when control connection lost. (from W:>Reti>tcp-ftp-parse-reply-patch.lisp.1).


;;; Patch file for CL-HTTP version 70.120
;;; Written by JCMa, 4/18/01 18:31:31
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5.vlod
;;; with Open Genera 2.0, Genera 8.5, Documentation Database 440.12,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.19, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.4,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.121,
;;; W3 Presentation System 8.1, CL-HTTP Server Interface 53.0,
;;; Symbolics Common Lisp Compatibility 4.0, Comlink Packages 6.0,
;;; Comlink Utilities 10.3, COMLINK Cryptography 2.0, Routing Taxonomy 9.0,
;;; COMLINK Database 11.26, Email Servers 12.0, Comlink Customized LispM Mailer 7.1,
;;; Dynamic Forms 14.5, Communications Linker Server 39.8,
;;; Lambda Information Retrieval System 22.3, Experimental Genera 8 5 Patches 1.0,
;;; Genera 8 5 System Patches 1.26, Genera 8 5 Mailer Patches 1.1,
;;; Genera 8 5 Joshua Patches 1.0, Genera 8 5 Statice Runtime Patches 1.0,
;;; Genera 8 5 Statice Patches 1.0, Genera 8 5 Statice Server Patches 1.0,
;;; Genera 8 5 Statice Documentation Patches 1.0, Genera 8 5 Clim Patches 1.2,
;;; Genera 8 5 Genera Clim Patches 1.0, Genera 8 5 Postscript Clim Patches 1.0,
;;; Genera 8 5 Clim Doc Patches 1.0, Genera 8 5 Images Patches 1.0,
;;; Genera 8 5 Image Substrate Patches 1.0, Genera 8 5 Lock Simple Patches 1.0,
;;; HTTP Proxy Server 6.19, HTTP Client Substrate 4.9, Statice Server 466.2,
;;; Images 431.2, Genera Extensions 16.0, Essential Image Substrate 433.0,
;;; Image Substrate 440.4, Jcma 43, Ivory Revision 5, VLM Debugger 329,
;;; Genera program 8.16, DEC OSF/1 V4.0 (Rev. 110),
;;; 1280x994 8-bit PSEUDO-COLOR X Screen FUJI:0.0 with 224 Genera fonts (DECWINDOWS Digital Equipment Corporation Digital UNIX V4.0 R1),
;;; Machine serial number -2141189585,
;;; Domain Fixes (from CML:MAILER;DOMAIN-FIXES.LISP.33),
;;; Don't force in the mail-x host (from CML:MAILER;MAILBOX-FORMAT.LISP.24),
;;; Make Mailer More Robust (from CML:MAILER;MAILER-FIXES.LISP.15),
;;; Patch TCP hang on close when client drops connection. (from HTTP:LISPM;SERVER;TCP-PATCH-HANG-ON-CLOSE.LISP.10),
;;; Add CLIM presentation and text style format directives. (from FV:SCLC;FORMAT.LISP.20),
;;; Fix Statice Lossage (from CML:LISPM;STATICE-PATCH.LISP.3),
;;; Make update schema work on set-value attributes with accessor names (from CML:LISPM;STATICE-SET-VALUED-UPDATE.LISP.1),
;;; COMLINK Mailer Patches. (from CML:LISPM;MAILER-PATCH.LISP.107),
;;; Clim patches (from CML:DYNAMIC-FORMS;CLIM-PATCHES.LISP.48),
;;; Domain ad host patch (from W:>reti>domain-ad-host-patch.lisp.21),
;;; Background dns refreshing (from W:>reti>background-dns-refreshing.lisp.11),
;;; Cname level patch (from W:>Reti>cname-level-patch.lisp.10),
;;; Fix FTP Directory List for Periods in Directory Names (from W:>Reti>fix-ftp-directory-list.lisp.7),
;;; TCP-FTP-PARSE-REPLY signal a type error when control connection lost. (from W:>Reti>tcp-ftp-parse-reply-patch.lisp.1).


;;; Patch file for CL-HTTP version 70.120
;;; Written by JCMa, 4/17/01 23:51:26
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5.vlod
;;; with Open Genera 2.0, Genera 8.5, Documentation Database 440.12,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.19, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.4,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.121,
;;; W3 Presentation System 8.1, CL-HTTP Server Interface 53.0,
;;; Symbolics Common Lisp Compatibility 4.0, Comlink Packages 6.0,
;;; Comlink Utilities 10.3, COMLINK Cryptography 2.0, Routing Taxonomy 9.0,
;;; COMLINK Database 11.26, Email Servers 12.0, Comlink Customized LispM Mailer 7.1,
;;; Dynamic Forms 14.5, Communications Linker Server 39.8,
;;; Lambda Information Retrieval System 22.3, Experimental Genera 8 5 Patches 1.0,
;;; Genera 8 5 System Patches 1.26, Genera 8 5 Mailer Patches 1.1,
;;; Genera 8 5 Joshua Patches 1.0, Genera 8 5 Statice Runtime Patches 1.0,
;;; Genera 8 5 Statice Patches 1.0, Genera 8 5 Statice Server Patches 1.0,
;;; Genera 8 5 Statice Documentation Patches 1.0, Genera 8 5 Clim Patches 1.2,
;;; Genera 8 5 Genera Clim Patches 1.0, Genera 8 5 Postscript Clim Patches 1.0,
;;; Genera 8 5 Clim Doc Patches 1.0, Genera 8 5 Images Patches 1.0,
;;; Genera 8 5 Image Substrate Patches 1.0, Genera 8 5 Lock Simple Patches 1.0,
;;; HTTP Proxy Server 6.19, HTTP Client Substrate 4.9, Statice Server 466.2,
;;; Images 431.2, Genera Extensions 16.0, Essential Image Substrate 433.0,
;;; Image Substrate 440.4, Jcma 43, Ivory Revision 5, VLM Debugger 329,
;;; Genera program 8.16, DEC OSF/1 V4.0 (Rev. 110),
;;; 1280x994 8-bit PSEUDO-COLOR X Screen FUJI:0.0 with 224 Genera fonts (DECWINDOWS Digital Equipment Corporation Digital UNIX V4.0 R1),
;;; Machine serial number -2141189585,
;;; Domain Fixes (from CML:MAILER;DOMAIN-FIXES.LISP.33),
;;; Don't force in the mail-x host (from CML:MAILER;MAILBOX-FORMAT.LISP.24),
;;; Make Mailer More Robust (from CML:MAILER;MAILER-FIXES.LISP.15),
;;; Patch TCP hang on close when client drops connection. (from HTTP:LISPM;SERVER;TCP-PATCH-HANG-ON-CLOSE.LISP.10),
;;; Add CLIM presentation and text style format directives. (from FV:SCLC;FORMAT.LISP.20),
;;; Fix Statice Lossage (from CML:LISPM;STATICE-PATCH.LISP.3),
;;; Make update schema work on set-value attributes with accessor names (from CML:LISPM;STATICE-SET-VALUED-UPDATE.LISP.1),
;;; COMLINK Mailer Patches. (from CML:LISPM;MAILER-PATCH.LISP.107),
;;; Clim patches (from CML:DYNAMIC-FORMS;CLIM-PATCHES.LISP.48),
;;; Domain ad host patch (from W:>reti>domain-ad-host-patch.lisp.21),
;;; Background dns refreshing (from W:>reti>background-dns-refreshing.lisp.11),
;;; Cname level patch (from W:>Reti>cname-level-patch.lisp.10),
;;; Fix FTP Directory List for Periods in Directory Names (from W:>Reti>fix-ftp-directory-list.lisp.7),
;;; TCP-FTP-PARSE-REPLY signal a type error when control connection lost. (from W:>Reti>tcp-ftp-parse-reply-patch.lisp.1).


;;; Patch file for CL-HTTP version 70.120
;;; Written by JCMa, 4/17/01 16:39:42
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5.vlod
;;; with Open Genera 2.0, Genera 8.5, Documentation Database 440.12,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.19, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.4,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.121,
;;; W3 Presentation System 8.1, CL-HTTP Server Interface 53.0,
;;; Symbolics Common Lisp Compatibility 4.0, Comlink Packages 6.0,
;;; Comlink Utilities 10.3, COMLINK Cryptography 2.0, Routing Taxonomy 9.0,
;;; COMLINK Database 11.26, Email Servers 12.0, Comlink Customized LispM Mailer 7.1,
;;; Dynamic Forms 14.5, Communications Linker Server 39.8,
;;; Lambda Information Retrieval System 22.3, Experimental Genera 8 5 Patches 1.0,
;;; Genera 8 5 System Patches 1.26, Genera 8 5 Mailer Patches 1.1,
;;; Genera 8 5 Joshua Patches 1.0, Genera 8 5 Statice Runtime Patches 1.0,
;;; Genera 8 5 Statice Patches 1.0, Genera 8 5 Statice Server Patches 1.0,
;;; Genera 8 5 Statice Documentation Patches 1.0, Genera 8 5 Clim Patches 1.2,
;;; Genera 8 5 Genera Clim Patches 1.0, Genera 8 5 Postscript Clim Patches 1.0,
;;; Genera 8 5 Clim Doc Patches 1.0, Genera 8 5 Images Patches 1.0,
;;; Genera 8 5 Image Substrate Patches 1.0, Genera 8 5 Lock Simple Patches 1.0,
;;; HTTP Proxy Server 6.19, HTTP Client Substrate 4.9, Statice Server 466.2,
;;; Images 431.2, Genera Extensions 16.0, Essential Image Substrate 433.0,
;;; Image Substrate 440.4, Jcma 43, Ivory Revision 5, VLM Debugger 329,
;;; Genera program 8.16, DEC OSF/1 V4.0 (Rev. 110),
;;; 1280x994 8-bit PSEUDO-COLOR X Screen FUJI:0.0 with 224 Genera fonts (DECWINDOWS Digital Equipment Corporation Digital UNIX V4.0 R1),
;;; Machine serial number -2141189585,
;;; Domain Fixes (from CML:MAILER;DOMAIN-FIXES.LISP.33),
;;; Don't force in the mail-x host (from CML:MAILER;MAILBOX-FORMAT.LISP.24),
;;; Make Mailer More Robust (from CML:MAILER;MAILER-FIXES.LISP.15),
;;; Patch TCP hang on close when client drops connection. (from HTTP:LISPM;SERVER;TCP-PATCH-HANG-ON-CLOSE.LISP.10),
;;; Add CLIM presentation and text style format directives. (from FV:SCLC;FORMAT.LISP.20),
;;; Fix Statice Lossage (from CML:LISPM;STATICE-PATCH.LISP.3),
;;; Make update schema work on set-value attributes with accessor names (from CML:LISPM;STATICE-SET-VALUED-UPDATE.LISP.1),
;;; COMLINK Mailer Patches. (from CML:LISPM;MAILER-PATCH.LISP.107),
;;; Clim patches (from CML:DYNAMIC-FORMS;CLIM-PATCHES.LISP.48),
;;; Domain ad host patch (from W:>reti>domain-ad-host-patch.lisp.21),
;;; Background dns refreshing (from W:>reti>background-dns-refreshing.lisp.11),
;;; Cname level patch (from W:>Reti>cname-level-patch.lisp.10),
;;; Fix FTP Directory List for Periods in Directory Names (from W:>Reti>fix-ftp-directory-list.lisp.7),
;;; TCP-FTP-PARSE-REPLY signal a type error when control connection lost. (from W:>Reti>tcp-ftp-parse-reply-patch.lisp.1).


;;; Patch file for CL-HTTP version 70.120
;;; Written by Reti, 4/16/01 17:09:43
;;; while running on RAINIER-VLM from RAINIER:/com/alpha/genera/sys.sct/worlds/kr-with-metaglue-source.vlod
;;; with Open Genera 2.0, Genera 8.5, Documentation Database 440.12,
;;; Logical Pathnames Translation Files NEWEST, MAC 414.0,
;;; Experimental Genera 8 5 Patches 1.0, Genera 8 5 System Patches 1.26,
;;; Genera 8 5 Metering Patches 1.0, Genera 8 5 Joshua Patches 1.0,
;;; Genera 8 5 Jericho Patches 1.0, Genera 8 5 Joshua Doc Patches 1.0,
;;; Genera 8 5 Joshua Metering Patches 1.0, Genera 8 5 Statice Runtime Patches 1.0,
;;; Genera 8 5 Statice Patches 1.0, Genera 8 5 Statice Server Patches 1.0,
;;; Genera 8 5 Statice Documentation Patches 1.0, Genera 8 5 Clim Patches 1.2,
;;; Genera 8 5 Genera Clim Patches 1.0, Genera 8 5 Postscript Clim Patches 1.0,
;;; Genera 8 5 Clx Clim Patches 1.0, Genera 8 5 Clim Doc Patches 1.0,
;;; Genera 8 5 Clim Demo Patches 1.0, Genera 8 5 Color Patches 1.1,
;;; Genera 8 5 Images Patches 1.0, Genera 8 5 Color Demo Patches 1.0,
;;; Genera 8 5 Image Substrate Patches 1.0, Genera 8 5 Lock Simple Patches 1.0,
;;; Genera 8 5 Concordia Patches 1.0, Genera 8 5 Concordia Doc Patches 1.0,
;;; Genera 8 5 C Patches 1.0, Genera 8 5 Pascal Patches 1.0,
;;; Genera 8 5 Fortran Patches 1.0, Color 427.1, Graphics Support 431.0,
;;; Genera Extensions 16.0, Essential Image Substrate 433.0,
;;; Color System Documentation 10.0, SGD Book Design 10.0, Images 431.2,
;;; Image Substrate 440.4, Color Demo 422.0, CLIM 72.0, Genera CLIM 72.0,
;;; CLX CLIM 72.0, PostScript CLIM 72.0, CLIM Demo 72.0, CLIM Documentation 72.0,
;;; Statice Runtime 466.1, Statice 466.0, Statice Browser 466.0,
;;; Statice Server 466.2, Statice Documentation 426.0, Metering 444.0,
;;; Metering Substrate 444.1, Symbolics Concordia 444.0, Graphic Editor 440.0,
;;; Graphic Editing 441.0, Bitmap Editor 441.0, Graphic Editing Documentation 432.0,
;;; Postscript 436.0, Concordia Documentation 432.0, Joshua 237.4,
;;; Joshua Documentation 216.0, Joshua Metering 206.0, Jericho 237.0, C 440.0,
;;; Lexer Runtime 438.0, Lexer Package 438.0, Minimal Lexer Runtime 439.0,
;;; Lalr 1 434.0, Context Free Grammar 439.0, Context Free Grammar Package 439.0,
;;; C Runtime 438.0, Compiler Tools Package 434.0, Compiler Tools Runtime 434.0,
;;; C Packages 436.0, Syntax Editor Runtime 434.0, C Library Headers 434,
;;; Compiler Tools Development 435.0, Compiler Tools Debugger 434.0,
;;; C Documentation 426.0, Syntax Editor Support 434.0, LL-1 support system 438.0,
;;; Fortran 434.0, Fortran Runtime 434.0, Fortran Package 434.0, Fortran Doc 427.0,
;;; Pascal 433.0, Pascal Runtime 434.0, Pascal Package 434.0, Pascal Doc 427.0,
;;; HTTP Server 70.120, Showable Procedures 36.3, Binary Tree 34.0,
;;; W3 Presentation System 8.1, HTTP Client 50.6, HTTP Client Substrate 4.9,
;;; CL-HTTP Server Interface 54.0, HTTP Proxy Server 6.19, Experimental Jpeg Lib 1.0,
;;; Ivory Revision 5, VLM Debugger 329, Genera program 8.16,
;;; DEC OSF/1 V4.0 (Rev. 67),
;;; 1600x1122 24-bit TRUE-COLOR X Screen INTERNET|128.52.54.15:0.0 with 224 Genera fonts (Hummingbird Communications Ltd. R6010),
;;; Machine serial number -2141190824,
;;; New pcxal mapping (from DISTRIBUTION|DIS-W-HOST:>reti>new-pcxal-mapping),
;;; Patch TCP hang on close when client drops connection. (from HTTP:LISPM;SERVER;TCP-PATCH-HANG-ON-CLOSE.LISP.10),
;;; Ansi common lisp as synonym patch (from W:>reti>ansi-common-lisp-as-synonym-patch.lisp.9),
;;; Draw image instance patch (from W:>reti>draw-image-instance-patch.lisp.1),
;;; Test permit patch (from KRI:KRI;TEST-PERMIT-PATCH.LISP.3),
;;; Add clos to mx list methods (from KRI:KRI;ADD-CLOS-TO-MX-LIST-METHODS.LISP.1),
;;; Telnet naws patch (from KRI:KRI;TELNET-NAWS-PATCH.LISP.9),
;;; Its end of line patch (from KRI:KRI;ITS-END-OF-LINE-PATCH.LISP.3),
;;; Unix inbox from spoofing patch (from KRI:KRI;UNIX-INBOX-FROM-SPOOFING-PATCH.LISP.21),
;;; hack to treat namespace as a partial cache of domain (from W:>hes>fixes>partial-namespace-domain.lisp.5),
;;; Popup patch (from KRI:KRI;POPUP-PATCH.LISP.1),
;;; Content type in forward patch (from KRI:KRI;CONTENT-TYPE-IN-FORWARD-PATCH.LISP.4),
;;; Attempt to fix recursive block transport (from KRI:KRI;RBT-PATCH.LISP.1),
;;; Directory attributes patch (from KRI:KRI;DIRECTORY-ATTRIBUTES-PATCH.LISP.6),
;;; Read jfif vogt (from KRI:KRI;READ-JFIF-VOGT.LISP.3),
;;; Domain try harder patch (from KRI:KRI;DOMAIN-TRY-HARDER-PATCH.LISP.6),
;;; Find free ephemeral space patch (from KRI:KRI;FIND-FREE-EPHEMERAL-SPACE-PATCH.LISP.3),
;;; Section name patch (from KRI:KRI;SECTION-NAME-PATCH.LISP.1),
;;; Tape spec patch (from KRI:KRI;TAPE-SPEC-PATCH.LISP.10),
;;; Set dump dates on compare patch (from KRI:KRI;SET-DUMP-DATES-ON-COMPARE-PATCH.LISP.75),
;;; Load file only bin (from KRI:KRI;LOAD-FILE-ONLY-BIN.LISP.1),
;;; Show jpeg pathname (from KRI:KRI;SHOW-JPEG-PATHNAME.LISP.1),
;;; Bullet proof trampoline args (from KRI:KRI;BULLET-PROOF-TRAMPOLINE-ARGS.LISP.1),
;;; More y2k patches (from KRI:KRI;MORE-Y2K-PATCHES.LISP.10),
;;; Vlm disk save patch (from KRI:KRI;VLM-DISK-SAVE-PATCH.LISP.5),
;;; Domain ad host patch (from KRI:KRI;DOMAIN-AD-HOST-PATCH.LISP.21),
;;; Background dns refreshing (from W:>Reti>background-dns-refreshing.lisp.11),
;;; Cname level patch (from KRI:KRI;CNAME-LEVEL-PATCH.LISP.10),
;;; Truename version in eco (from KRI:KRI;TRUENAME-VERSION-IN-ECO.LISP.1),
;;; Zmail patches (from KRI:KRI;ZMAIL-PATCHES.LISP.1),
;;; Better sectionization (from KRI:KRI;BETTER-SECTIONIZATION.LISP.5),
;;; Compile interval patch (from KRI:KRI;COMPILE-INTERVAL-PATCH.LISP.8),
;;; Stealth syn handling (from KRI:KRI;STEALTH-SYN-HANDLING.LISP.4),
;;; Numeric sorted directories (from W:>Reti>numeric-sorted-directories.lisp.2),
;;; Vlm as r (from W:>Reti>vlm-as-s).


;;; Patch file for CL-HTTP version 70.120
;;; Written by JCMa, 4/14/01 03:41:26
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5.vlod
;;; with Open Genera 2.0, Genera 8.5, Documentation Database 440.12,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.19, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.4,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.120,
;;; W3 Presentation System 8.1, CL-HTTP Server Interface 53.0,
;;; Symbolics Common Lisp Compatibility 4.0, Comlink Packages 6.0,
;;; Comlink Utilities 10.3, COMLINK Cryptography 2.0, Routing Taxonomy 9.0,
;;; COMLINK Database 11.26, Email Servers 12.0, Comlink Customized LispM Mailer 7.1,
;;; Dynamic Forms 14.5, Communications Linker Server 39.8,
;;; Lambda Information Retrieval System 22.3, Experimental Genera 8 5 Patches 1.0,
;;; Genera 8 5 System Patches 1.26, Genera 8 5 Mailer Patches 1.1,
;;; Genera 8 5 Joshua Patches 1.0, Genera 8 5 Statice Runtime Patches 1.0,
;;; Genera 8 5 Statice Patches 1.0, Genera 8 5 Statice Server Patches 1.0,
;;; Genera 8 5 Statice Documentation Patches 1.0, Genera 8 5 Clim Patches 1.2,
;;; Genera 8 5 Genera Clim Patches 1.0, Genera 8 5 Postscript Clim Patches 1.0,
;;; Genera 8 5 Clim Doc Patches 1.0, Genera 8 5 Lock Simple Patches 1.0,
;;; HTTP Proxy Server 6.19, HTTP Client Substrate 4.9, Statice Server 466.2,
;;; Ivory Revision 5, VLM Debugger 329, Genera program 8.16,
;;; DEC OSF/1 V4.0 (Rev. 110),
;;; 1280x976 8-bit PSEUDO-COLOR X Screen FUJI:0.0 with 224 Genera fonts (DECWINDOWS Digital Equipment Corporation Digital UNIX V4.0 R1),
;;; Machine serial number -2141189585,
;;; Domain Fixes (from CML:MAILER;DOMAIN-FIXES.LISP.33),
;;; Don't force in the mail-x host (from CML:MAILER;MAILBOX-FORMAT.LISP.24),
;;; Make Mailer More Robust (from CML:MAILER;MAILER-FIXES.LISP.15),
;;; Patch TCP hang on close when client drops connection. (from HTTP:LISPM;SERVER;TCP-PATCH-HANG-ON-CLOSE.LISP.10),
;;; Add CLIM presentation and text style format directives. (from FV:SCLC;FORMAT.LISP.20),
;;; Fix Statice Lossage (from CML:LISPM;STATICE-PATCH.LISP.3),
;;; Make update schema work on set-value attributes with accessor names (from CML:LISPM;STATICE-SET-VALUED-UPDATE.LISP.1),
;;; COMLINK Mailer Patches. (from CML:LISPM;MAILER-PATCH.LISP.107),
;;; Clim patches (from CML:DYNAMIC-FORMS;CLIM-PATCHES.LISP.48),
;;; Domain ad host patch (from W:>reti>domain-ad-host-patch.lisp.21),
;;; Background dns refreshing (from W:>reti>background-dns-refreshing.lisp.11),
;;; Cname level patch (from W:>Reti>cname-level-patch.lisp.10),
;;; Fix FTP Directory List for Periods in Directory Names (from W:>Reti>fix-ftp-directory-list.lisp.7),
;;; TCP-FTP-PARSE-REPLY signal a type error when control connection lost. (from W:>Reti>tcp-ftp-parse-reply-patch.lisp.1).


;;; Patch file for CL-HTTP version 70.120
;;; Written by JCMa, 4/14/01 03:04:52
;;; while running on FUJI-VLM from FUJI:/usr/lib/symbolics/ComLink-39-8-F-MIT-8-5.vlod
;;; with Open Genera 2.0, Genera 8.5, Documentation Database 440.12,
;;; Logical Pathnames Translation Files NEWEST, CLIM 72.0, Genera CLIM 72.0,
;;; PostScript CLIM 72.0, MAC 414.0, 8-5-Patches 2.19, Statice Runtime 466.1,
;;; Statice 466.0, Statice Browser 466.0, Statice Documentation 426.0, Joshua 237.4,
;;; CLIM Documentation 72.0, Showable Procedures 36.3, Binary Tree 34.0,
;;; Mailer 438.0, Working LispM Mailer 8.0, HTTP Server 70.119,
;;; W3 Presentation System 8.1, CL-HTTP Server Interface 53.0,
;;; Symbolics Common Lisp Compatibility 4.0, Comlink Packages 6.0,
;;; Comlink Utilities 10.3, COMLINK Cryptography 2.0, Routing Taxonomy 9.0,
;;; COMLINK Database 11.26, Email Servers 12.0, Comlink Customized LispM Mailer 7.1,
;;; Dynamic Forms 14.5, Communications Linker Server 39.8,
;;; Lambda Information Retrieval System 22.3, Experimental Genera 8 5 Patches 1.0,
;;; Genera 8 5 System Patches 1.26, Genera 8 5 Mailer Patches 1.1,
;;; Genera 8 5 Joshua Patches 1.0, Genera 8 5 Statice Runtime Patches 1.0,
;;; Genera 8 5 Statice Patches 1.0, Genera 8 5 Statice Server Patches 1.0,
;;; Genera 8 5 Statice Documentation Patches 1.0, Genera 8 5 Clim Patches 1.2,
;;; Genera 8 5 Genera Clim Patches 1.0, Genera 8 5 Postscript Clim Patches 1.0,
;;; Genera 8 5 Clim Doc Patches 1.0, Genera 8 5 Lock Simple Patches 1.0,
;;; HTTP Proxy Server 6.19, HTTP Client Substrate 4.9, Statice Server 466.2,
;;; Ivory Revision 5, VLM Debugger 329, Genera program 8.16,
;;; DEC OSF/1 V4.0 (Rev. 110),
;;; 1280x976 8-bit PSEUDO-COLOR X Screen FUJI:0.0 with 224 Genera fonts (DECWINDOWS Digital Equipment Corporation Digital UNIX V4.0 R1),
;;; Machine serial number -2141189585,
;;; Domain Fixes (from CML:MAILER;DOMAIN-FIXES.LISP.33),
;;; Don't force in the mail-x host (from CML:MAILER;MAILBOX-FORMAT.LISP.24),
;;; Make Mailer More Robust (from CML:MAILER;MAILER-FIXES.LISP.15),
;;; Patch TCP hang on close when client drops connection. (from HTTP:LISPM;SERVER;TCP-PATCH-HANG-ON-CLOSE.LISP.10),
;;; Add CLIM presentation and text style format directives. (from FV:SCLC;FORMAT.LISP.20),
;;; Fix Statice Lossage (from CML:LISPM;STATICE-PATCH.LISP.3),
;;; Make update schema work on set-value attributes with accessor names (from CML:LISPM;STATICE-SET-VALUED-UPDATE.LISP.1),
;;; COMLINK Mailer Patches. (from CML:LISPM;MAILER-PATCH.LISP.107),
;;; Clim patches (from CML:DYNAMIC-FORMS;CLIM-PATCHES.LISP.48),
;;; Domain ad host patch (from W:>reti>domain-ad-host-patch.lisp.21),
;;; Background dns refreshing (from W:>reti>background-dns-refreshing.lisp.11),
;;; Cname level patch (from W:>Reti>cname-level-patch.lisp.10),
;;; Fix FTP Directory List for Periods in Directory Names (from W:>Reti>fix-ftp-directory-list.lisp.7),
;;; TCP-FTP-PARSE-REPLY signal a type error when control connection lost. (from W:>Reti>tcp-ftp-parse-reply-patch.lisp.1).




(SCT:FILES-PATCHED-IN-THIS-PATCH-FILE 
  "HTTP:SERVER;SERVER.LISP.890"
  "HTTP:SERVER;UTILS.LISP.483"
  "HTTP:SERVER;HTML2.LISP.295"
  "HTTP:SERVER;HTML2.LISP.296"
  "HTTP:SERVER;SERVER.LISP.891"
  "HTTP:SERVER;SERVER.LISP.892"
  "HTTP:SERVER;HTML2.LISP.297"
  "HTTP:SERVER;HEADERS.LISP.491"
  "HTTP:SERVER;SERVER.LISP.893"
  "HTTP:SERVER;HTTP-CONDITIONS.LISP.189"
  "HTTP:SERVER;UTILS.LISP.484"
  "HTTP:SERVER;HEADERS.LISP.493"
  "HTTP:SERVER;UTILS.LISP.485"
  "HTTP:SERVER;HEADERS.LISP.494"
  "HTTP:SERVER;UTILS.LISP.487"
  "HTTP:SERVER;HEADERS.LISP.495"
  "HTTP:SERVER;SERVER.LISP.894"
  "HTTP:SERVER;HTML2.LISP.299"
  "HTTP:SERVER;UTILS.LISP.491"
  "HTTP:SERVER;VARIABLES.LISP.193"
  "HTTP:SERVER;HTTP-CONDITIONS.LISP.191"
  "HTTP:SERVER;SERVER.LISP.899")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.890")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

;;;------------------------------------------------------------------- 
;;;
;;;  STATIC FILE EXPORT-TYPE, PLUS METHODS FOR :GET AND :HEAD
;;;

;; IANA maintains a list of assigned content types per RFC 1590 that can be found at
;;; ftp://ftp.isi.edu/in-notes/iana/assignments/media-types/media-types
;;; See www.iana.org for new interface   8/11/98 -- JCMa.

(define-url-export-types
  ;; RFC 822 headers
  (:headers-file :rfc822-headers (:text :rfc822-headers) :copy-mode :text :alternate-extensions (:headers)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;UTILS.LISP.483")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: lisp -*-")

(define write-to-armor-plated-string (form &optional (line-length *armor-plated-string-line-length*))
  "Writes FORM to an armor plated string that travels through HTML and LISP in tact.
LINE-LENGTH controls the number of characters per line of encoded output.
In cases where RETURN causes truncation, this number should be high enough so
no line breaks occur."
  (let ((string (write-to-string form :escape t :base 10.)))
    (declare (dynamic-extent string))
    (base64:with-encoding-vector (#\$ #\! #\*)
      (base64:base64-encode-vector string :max-line-length line-length))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;UTILS.LISP.483")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: lisp -*-")

;; *read-eval* is NIL in all server transactions.
(define read-from-armor-plated-string (string &optional (eof-error-p t) eof (start 0) (end (length string)))
  "Read from an armor plated string that travels through HTML in tact.
When EOF-ERROR-P is null, base 64 encoding errors and read errors are caught and the value of EOF returned."
  (declare (fixnum end))
  (handler-case-if (null eof-error-p)
     (let* ((pad-char (if (char= (schar string (1- end)) #\@) #\@ #\*))	;backward compatible with old #@ pad char 4/12/2001 -- JCMa.
	    (decoded-string (base64:with-encoding-vector (#\$ #\! pad-char)
			      (base64:base64-decode-vector string :start start :end end
							   :decoded-byte-type *standard-character-type*
							   :bogus-character-action :error))))
       (declare (dynamic-extent decoded-string))
       (read-from-string decoded-string eof-error-p eof))
    (base64:base64-decode-error () eof)))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTML2.LISP.295")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- mode: lisp; syntax: ansi-common-lisp; package: html2; base: 10 -*-")

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTML2.LISP.295")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- mode: lisp; syntax: ansi-common-lisp; package: html2; base: 10 -*-")

(eval-when (:load-toplevel :compile-toplevel :execute)
  (mapc #'(lambda (x) (import (intern x :html2) :http)) '("*FILE-UPLOAD-DEFAULT-DIRECTORY*" "FILE-UPLOAD-UNPACK-QUERY-BAGGAGE"))
  (export (intern "*FILE-UPLOAD-DEFAULT-DIRECTORY*" :http) :http)
  )


(PROGN

(define-input-type
  file
  :superclass string+
  :type-arg "FILE"
  :lisp-type 'string)

(defparameter *file-upload-default-directory* #p"http:uploads;"
	      "The default directory to which files are uploaded files when posted via file upload.")

(defvar *file-upload-directory-index-table* (make-hash-table :test #'equal))
(defvar *file-upload-directory-index-lock* (make-lock "File-Upload-Index" :type :multiple-reader-single-writer))

;; these won't work across boots 4/12/2001 -- JCMa.
(defun file-upload-directory-index (upload-directory)
  (declare (values directory-map-key new-p))
  (let ((directory (pathname upload-directory)))
    (cond ((www-utils:with-lock-held (*file-upload-directory-index-lock* :read "File Upload Index")
	     (gethash directory *file-upload-directory-index-table*)))
	  (t (let ((table *file-upload-directory-index-table*))
	       (www-utils:with-lock-held (*file-upload-directory-index-lock* :write "File Upload index")
		 (or (gethash directory table)
		     (loop with table = *file-upload-directory-index-table*
			   for index = (random 100000)
			   while (gethash index table)
			   finally (setf (gethash directory table) index
					 (gethash index table) directory)
				   (http::pathname-create-directory-if-needed upload-directory)
				   (return (values index t))))))))))

(defun file-upload-make-query (query-name &optional (upload-directory *file-upload-default-directory*))
  (let ((query-baggage (list query-name (file-upload-directory-index upload-directory))))
    (declare (dynamic-extent query-baggage))
    (http:write-to-armor-plated-string query-baggage)))

)

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTML2.LISP.296")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- mode: lisp; syntax: ansi-common-lisp; package: html2; base: 10 -*-")

;;; extra error checking AND encoding the directory into the query-name.
(defmethod accept-input ((file file) query-name &rest args &key (stream *output-stream*) &allow-other-keys)
  (cond ((equal *open-form* '(:multipart :form-data)))
	((null *open-form*)
	 (error "~S ~S should be within a ~S." 'accept-input 'file 'with-fillout-form))
	(t (error "~S ~S requires the :ENCODING-TYPE of ~S to be ~S."
		  'accept-input 'file 'with-fillout-form '(:multipart :form-data) )))
  (with-slots (default-size default-max-size) file
    (destructuring-bind (&key size max-length content-type (directory *file-upload-default-directory*) &allow-other-keys) args
      (let ((directory-query-name (file-upload-make-query query-name directory)))
	(declare (dynamic-extent directory-query-name))
	(%issue-command ("INPUT" stream)
	  (html2::write-standard-input-type-args (stream directory-query-name file args :bind-default t)
	    (let ((local-size (or size default-size))
		  (max (or max-length default-max-size)))
	      (cond-every
		(local-size
		  (%write-command-key-arg stream "SIZE" local-size))
		(max
		  (unless (< max 1024.)
		    (error "String fields cannot exceed 1024 characters in HMTL 2.0"))
		  (%write-command-key-arg stream "MAXLENGTH" max t))
		(content-type
		  (fast-format stream " ACCEPT=")
		  (write-char #\" stream)
		  (etypecase content-type
		    (cons
		      (etypecase (car content-type)
			(keyword (http::print-mime-content-type-header content-type stream))
			(cons (http::print-mime-content-type-sequence-header content-type stream))))
		    (keyword
		      (http::print-mime-content-type-header (http::%mime-content-type-spec content-type) stream))
		    (string
		      (write-string content-type stream)))
		  (write-char #\" stream)))))))))) 

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTML2.LISP.296")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- mode: lisp; syntax: ansi-common-lisp; package: html2; base: 10 -*-")

;;;------------------------------------------------------------------- 
;;;
;;; FILL-OUT FORMS
;;;

(defvar *open-form* nil
  "Bound to a null-null value within the context of a fill-out form.
The value is the MIME type for form encoding.")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTML2.LISP.296")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- mode: lisp; syntax: ansi-common-lisp; package: html2; base: 10 -*-")

(define-macro with-fillout-form ((action target &key name (stream '*output-stream*)
                                         (encoding-type ''(:application :x-www-form-urlencoded))
                                         events) &body body)
  "Establishes an fillout-form environment.  
ACTION is either :POST, :MAIL, or :GET, or :NONE.
NAME is a name identifying the form element.
TARGET is the URL to which the form values are returned.
If ACTION is :NONE, TARGET can be NIL.
EVENTS is a list of client-side events processed when the form is submitted.

ENCODING-TYPE is MIME content type to use when return the form values to TARGET.
ENCODING-TYPE defaults to application/x-www-form-urlencoded.
See ACCEPT-INPUT for documentation on client-side events.             
:GET should only be used in exceptional circumstances as not only is
it considered obsolete but it also is limited to 1024 characters 
including the rest of the the Target URL."
  `(cond (*open-form*
          (error "HTML does not allow nesting of forms."))
         (t (let ((*open-form* ,encoding-type))
	      (%with-environment ("FORM" :fresh-line t :stream ,stream)
				 (write-form-command-args ,stream ,action ,target *open-form* ,name ,events)
                ,@body)))))




;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.891")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(define parse-form-raw-values (stream buffer bytes &optional (durable-values-p *durable-form-values*))
  "Function that parses bytes with of form values from STREAM.
  It returns an ALIST of (QUERY-ID VALUE COLLECTION-FLAG QUERY-BAGGAGE).  When
  multiple values for QUERY-ID are collected into VALUE, COLLECTION-FLAG
  is non-null.  Otherwise, there is no third value.
  QUERY-BAGGAGE is an overloading field on the query-id that can carry additional information.
  When present it is an atom when COLLECTION-FLAG is null and a list when COLLECTION-FLAG is
  non-null."
  (declare (values raw-query-value-alist))
  (labels ((get-raw-string (buffer start end)
	     (declare (fixnum start end))
	     (multiple-value-bind (string new-end)
		 (nstring-translate-chars buffer start end #\space)
	       (cond ((= start new-end) nil)
		     (durable-values-p (subseq string start new-end))
		     (t (make-array (- (the fixnum new-end) start) :element-type (array-element-type string)
				    :displaced-to string :displaced-index-offset start)))))
	   (unpacked-query-indices (string start end)
	     (declare (fixnum start end))
	     (let ((pos2 (1- end))
		   pos1)
	       (if (and (eql (aref string pos2) #\))
			(setq pos1 (char-position #\( string start pos2)))
		   (values pos1 (1+ (the fixnum pos1)) pos2)
		   (values end))))
	   (get-keyword (buffer start end)
	     (multiple-value-bind (string new-end)
		 (nstring-translate-chars buffer start end #\space)
	       (multiple-value-bind (key-end baggage-start baggage-end)
		   (unpacked-query-indices string start new-end)
		 (values (%tokenize-form-query-keyword string start key-end)
			 (when (and baggage-start baggage-end)
			   (get-raw-string string baggage-start baggage-end)))))))
    (declare (inline unpacked-query-indices get-raw-string get-keyword maybe-push-entry))
    ;; collect the form data over the HTTP connection by reading bytes of characters.
    (multiple-value-bind (string end)
	(crlf-stream-copy-into-string stream bytes 0 buffer)
      (loop with query and baggage and value and alist and ptr 
	    with start fixnum = 0
	    while (< start end)
	    for key-end fixnum = (or (char-position #\= string start end)
				     (error 'bad-form-data-posted
					    :format-string "Bad Form Data Posted: No Query delimiter found in ~S."
					    :format-args (list (subseq string start end))))
	    do (multiple-value-setq (query baggage)
		 (get-keyword string start key-end))
	       ;; get the value
	       (let* ((val-start (1+ key-end))
		      (val-end (or (char-position #\& string val-start end) end)))
		 (declare (fixnum val-start val-end))
		 (setq value (get-raw-string string val-start val-end)	;get the query value
		       start (1+ val-end)))
	    when (and query value)		;maintain query order
	      do (setq ptr (%parse-form-collect-form-value query value baggage alist ptr))
		 (or alist (setq alist ptr))
		 #+ignore (format t "~&QUERY: ~S~:[~;~&Baggage: ~:*~S~]~&VALUE: ~S" query baggage value)
	    finally (return alist)))))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.891")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defmethod apply-form-response-function (url stream (form-alist null))
  (declare (ignore stream))
  (error 'bad-syntax-provided :format-string "No form values were returned for ~A."
	 :format-args (list (url:name-string url)) :url url))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.891")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defmacro handling-form-parsing-errors ((url) &body body)
  `(flet ((%handle-error-reading-form-values (error)
	    (typecase error
	      (network-error nil)		;Pass through network errors for logging higher up
	      (t (setf (server-status *server*) 500)
		 (error 'error-handling-post-method :url ,url :server-error error
			:headers (header-plist)
			:format-string "POST Error reading form values for ~A."
			:format-args (list (url:name-string ,url))
			:stack-backtrace (when *stack-backtraces-in-bug-reports*
					   (stack-backtrace-string error)))))))
     (declare (dynamic-extent #'%handle-error-reading-form-values))
     (handler-bind-if (not *debug-server*)
	((error #'%handle-error-reading-form-values))
       . ,body)))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.891")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defmacro handling-form-response-errors ((url form-alist) &body body)
  `(flet ((%handle-error-computing-form-response (error)
	    (typecase error
	      (network-error nil)		;Pass through network errors for logging higher up
	      (t (setf (server-status *server*) 500)
		 (error 'error-handling-post-method :url ,url :server-error error
			:headers (header-plist)
			:form-alist (and ,form-alist (durable-form-values-p ,url))
			:format-string "POST Error computing form response for ~A."
			:format-args (list (url:name-string ,url))
			:stack-backtrace (when *stack-backtraces-in-bug-reports*
					   (stack-backtrace-string error)))))))
     (declare (dynamic-extent #'%handle-error-computing-form-response))
     (handler-bind-if (not *debug-server*)
	((error #'%handle-error-computing-form-response))
       . ,body)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.891")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defmethod apply-form-response-function ((url url::form-processing-mixin) stream form-alist)
  (let ((fctn (url:response-function url)))
    (unless fctn
      (error 'server-internal-error :url url
	     :format-string "No Response function was found for ~A."
	     :format-args (list (url:name-string url))))
    (handling-form-response-errors (url form-alist)
      (funcall fctn url stream form-alist))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.892")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defun %post-document-handle-url-coded-form (url stream)
  (let ((bytes (get-header :content-length))
	(durable-values-p (durable-form-values-p url))
	form-alist)
    (with-post-form-context (bytes)
      (handling-form-parsing-errors (url)
	(setq form-alist (parse-form-raw-values stream buffer bytes durable-values-p))
	;; Only when values are durable, keep track of form values for
	;; possible logging or subsequent access
	(when durable-values-p
	  (setf (server-form-data *server*) form-alist))
	(apply-form-response-function url stream form-alist)))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTML2.LISP.297")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- mode: lisp; syntax: ansi-common-lisp; package: html2; base: 10 -*-")

(defun file-upload-unpack-query-baggage (encoded-query-name)
  (declare (values query-name directory))
  (destructuring-bind (query-name directory-index)
      (http:read-from-armor-plated-string encoded-query-name)
    (values query-name (file-upload-directory-for-index directory-index))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.491")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(define mime-content-type-keyword (content-type-spec &optional (error-p t))
  "Returns the keyword for the content-type-spec."
  (destructuring-bind (major minor &rest parameters) content-type-spec
    (declare (ignore parameters))
    (loop for item in *mime-content-type-alist*
          when (and (eq (second item) major)
                    (eq (third item) minor))
            return (first item)
          finally (if error-p
		      (error "Unknown content type, ~S." content-type-spec)
		      (return nil)))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.491")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(define mime-content-type-primary-extension-string (content-type-spec &optional (error-p t))
  "Returns the primary pathname extension string for the MIME content type, CONTENT-TYPE-SPEC."
  (let ((keyword (mime-content-type-keyword content-type-spec error-p)))
    (cond ((null keyword) nil)
	  ((get keyword 'primary-extension-string))
	  (t (setf (get keyword 'primary-extension-string) 
		   (string-downcase (symbol-name (primary-pathname-extension keyword t))))))))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.893")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defun %parse-form-collect-form-value (query value baggage alist ptr &aux entry)
  (declare (values new-ptr))
  (macrolet ((make-entry (query value baggage)
	       `(if ,baggage (list (list ,query ,value nil ,baggage)) (list (list ,query ,value)))))
    ;; legend (keyword value collapsed-value-p query-name-baggage)
    ;; (break "Query: ~S~&Value: ~S" query value)
    (cond ((and alist (setq entry (assoc query alist :TEST #'EQ)))
	   (cond
	     ((eql t (third entry))		;collecting values?
	      (setf (second entry) `(,.(second entry) ,value))
	      (etypecase (fourth entry)
		(cons (setf (fourth entry) `(,.(fourth entry) ,baggage)))
		(null (when baggage
			(setf (cdddr entry) `((,baggage)))))))
	     ((fourth entry)			;first collect with prior baggage
	      (setf (cdr entry) `((,(second entry) ,value) t ,(if baggage `(,(fourth entry) ,baggage) `(,(fourth entry))))))
	     ;; first collect with no prior baggage
	     (t (setf (cdr entry) `((,(second entry) ,value) t))))
	   ptr)
	  (ptr (prog1 (setq entry (make-entry query value baggage))
		      (setf (cdr ptr) entry)))
	  (t (make-entry query value baggage)))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTTP-CONDITIONS.LISP.189")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: http; Base: 10; Default-character-style: (:fix :roman :normal) -*-")

(define-condition bad-form-data
                  (bad-syntax-provided)
  ((status-code :initform 400)
   (reason :initform "Bad Form Data Posted: Client provided unparsable form data.")))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTTP-CONDITIONS.LISP.189")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: http; Base: 10; Default-character-style: (:fix :roman :normal) -*-")

(define-condition bad-form-data-posted
                  (bad-form-data)
  ((reason :initform "Bad Form Data Posted: Client provided unparsable URL-encoded data.")))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTTP-CONDITIONS.LISP.189")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: http; Base: 10; Default-character-style: (:fix :roman :normal) -*-")

(define-condition bad-multipart-form-data-posted
                  (bad-form-data)
  ((reason :initform "Bad Form Data Posted: Client provided unparsable MIME Multipart encoded data.")))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;UTILS.LISP.484")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: lisp -*-")

(PROGN
(declaim (inline %string-equal))

(defun %string-equal (string1 string2 &optional (start1 0) (end1 (length string1)) (start2 0) (end2 (length string2)))
  "Returns non-null if string1 is equal to string2 using char-equal as the test."
  (declare (fixnum start1 end1 start2 end2))
  (when (= (- end1 start1) (- end2 start2))
    (check-type string1 string)
    (check-type string2 string)
    (with-fast-array-references ((string1 string1 string)
				 (string2 string2 string))
      (loop for idx1 fixnum upfrom start1 below end1
	    for idx2 fixnum upfrom start2
	    unless (char-equal (aref string1 idx1) (aref string2 idx2))
	      return nil
	    finally (return t)))))
)

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;UTILS.LISP.484")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: lisp -*-")

(PROGN
;;;------------------------------------------------------------------- 
;;;
;;; MIME MULTIPART STREAM DECODING
;;;

(defun mime-boundary-line-p (line boundary &optional (line-length (length line)) (boundary-length (length boundary))
                                  &aux pos pos2)
  "Returns non-null when line matches the MIME multipart boundary, BOUNDARY."
  (declare (values boundary-p last-block-p)
	   (fixnum line-length boundary-length))
  (if (and (> line-length boundary-length)
           (%string-equal "--" line 0 2 0 2)
           (%string-equal boundary line 0 boundary-length 2 (setq pos (+ 2 boundary-length))))
      (values t (and (>= line-length (setq pos2 (+ 2 (the fixnum pos))))
		     (%string-equal "--" line 0 2 pos pos2)))
      (values nil)))

(defgeneric mime-stream-flush-until-boundary (mime-stream boundary mode &optional buffer)
  (declare (values decoded-string last-block-p))
  (:documentation "Flushes MIME multipart data from mime-stream upto the next boundary.
MODE can be any of :TEXT, CRLF, :BINARY."))

(defgeneric mime-stream-decode-until-boundary (mime-stream to-stream boundary mode &optional buffer)
  (declare (values last-block-p))
  (:documentation "Decodes MIME multipart data from MIME-STREAM and copies it to TO-STREAM upto BOUNDARY
according to MODE. MODE can be any of :TEXT, CRLF, :BINARY.
Specialize these methods for higher performance on specific platforms."))

(defmethod mime-stream-decode-until-boundary (mime-stream to-stream boundary (mode (eql :crlf)) &optional buffer)
  (mime-stream-decode-until-boundary mime-stream to-stream boundary :binary buffer))

(defun mime-stream-decode-into-file-until-boundary (mime-stream pathname boundary mode &optional buffer &aux last-block-p)
  "Captures MIME multipart data from MIME-STREAM into the file, pathname, upto the next BOUNDARY
according to MODE. MODE can be any of :TEXT, CRLF, :BINARY."
  (declare (values pathname last-block-p))
  (let ((capture-mode (ecase mode
			((:text :crlf) :text)	;local file should normally be in native charset
			(:binary :binary))))
    (with-open-file (file-stream pathname :direction :output
				 :element-type (copy-mode-element-type capture-mode) :if-exists :supersede)
      (setq last-block-p (mime-stream-decode-until-boundary mime-stream file-stream boundary capture-mode buffer)))
    #+MCL(autotype-file pathname)
    (values pathname last-block-p)))

(define-generic mime-stream-decode-into-string-until-boundary (mime-stream boundary &optional string buffer)
  (declare (values decoded-string last-block-p))
  (:documentation "Captures MIME multipart data from MIME-STREAM into STRING upto the next BOUNDARY.
If STRING is provided, data is copied into STRING with size adjustment as needed.
BUFFER is a temporary line buffer."))

)

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.493")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(defun parse-mime-content-disposition-header (string &optional (start 0) (end (length string))
                                                     &aux parameters)
  (with-string-trim-bounds (*white-space-chars* string start end)
    (let* ((parameter-index (mime-header-parameter-index string start end))
	   (disposition (%tokenize-header-keyword string start (or parameter-index end))))
      (when parameter-index
	(setq parameters (parse-mime-header-parameters string (the fixnum (1+ (the fixnum parameter-index))) end)))
      (list* disposition parameters))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.493")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(define-header :content-disposition             ;not in 1.1
               (:header :entity)
  :print-string "Content-Disposition"
  :parse-function 'parse-mime-content-disposition-header
  :print-function 'print-mime-content-disposition-header)


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;UTILS.LISP.485")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: lisp -*-")

(defmethod mime-stream-decode-into-string-until-boundary (mime-stream boundary &optional string buffer &aux last-block-p)
  (peek-char nil mime-stream)
  (cond (string
	 (values (with-output-to-string (stream string)
		   (setq last-block-p (mime-stream-decode-until-boundary mime-stream stream boundary :text buffer)))
		 last-block-p))
	(t (values (with-output-to-string (stream)
		     (setq last-block-p (mime-stream-decode-until-boundary mime-stream stream boundary :text buffer)))
		   last-block-p))))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.494")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(defun parse-mime-header-parameters (string &optional (start 0) (end (length string)))
  (flet ((delimiter-p (char)
           (member char '(#\; #\space #\tab) :test #'eql))
	 (intern-value (key string s e)
	   (declare (fixnum s e))
	   (case key
	     ((:boundary :name :filename)	;parameters used in content-disposition for file upload 4/13/2001 -- JCMa.
	      (subseq string s e))		;some parameters are strings
	     (t (with-fast-array-references ((string string string))
		  (loop with quality-value-p
			for idx fixnum upfrom s below e
			for ch = (aref string idx)
			unless (or (digit-char-p ch)
				   (setq quality-value-p (eql ch #\.)))
			  return (%tokenize-header-keyword string s e)
			finally (return (parse-number string s e quality-value-p))))))))
    (declare (inline intern-value))
    (loop with param-key and param-value
	  for s = (position-if-not* #'delimiter-p string :start start :end end) then (the fixnum (1+ idx))
	  while (and s (< s end))
	  for idx fixnum = (or (char-position #\; string (1+ s) end) end)
	  for delim fixnum = (char-position #\= string (1+ s) idx)
	  for p1 = (position-valid-mime-char string (1+ delim) idx)
	  for p2 = (position-if* #'mime-valid-char-for-token-p string :start p1 :end idx :from-end t)
	  when p2
	    do (setq param-key (%tokenize-header-keyword string (position-valid-mime-char string s delim) delim))
	       (setq param-value (intern-value param-key string p1 (1+ (the fixnum p2))))
	    and collect param-key
	    and collect param-value)))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;UTILS.LISP.487")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: lisp -*-")

(define-macro char-position (char string  &optional (start 0) end from-end)
  "Returns the position of CHAR in string from START upto END.
when FROM-END is non-null, the string is scanned backward."
  (case from-end
    ((t)
     `(let ((ch ,char))
	(with-fast-array-references ((string ,string string))
	  (loop for idx fixnum downfrom (1- (the fixnum ,(or end '(length string)))) to (the fixnum ,start)
		when (eql ch (aref string idx))
		  return idx
		finally (return nil)))))
    ((nil)
     `(let ((ch ,char))
	(with-fast-array-references ((string ,string string))
	  (loop for idx fixnum upfrom (the fixnum ,start) below (the fixnum ,(or end '(length string)))
		when (eql ch (aref string idx))
		  return idx
		finally (return nil)))))
    (t (if end
	   `(char-position-2-case ,char ,string ,start ,end ,from-end)
	   `(let ((string ,string))
	      (char-position-2-case ,char ,string ,start (length string) ,from-end))))))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HEADERS.LISP.495")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10.; Syntax: ANSI-COMMON-LISP; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")

(defun mime-content-type-spec-for-pathname-type (keyword-or-string &optional (error-p t))
  "Returns the MIME content type for the pathname type KEYWORD-OR-STRING." 
  (let ((export-type (export-type-for-pathname-type keyword-or-string error-p)))
    (when export-type
      (%mime-content-type-spec (primary-pathname-extension-for-export-type export-type t)))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.894")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(PROGN
(defparameter *file-upload-default-pathname-name* "temp"
  "The default filename used when no filename is supplied by file upload.")

(defparameter *file-upload-default-pathname-name-maximum-size* 30
  "The default size for filename used when no filename is supplied by file upload.")

(defun file-upload-generate-file-name ()
  (let* ((server *server*)
	 (name *file-upload-default-pathname-name*)
	 (client-address (server-host-ip-address server))
	 (request-time (write-to-string (server-request-time server)))
	 (hash (concatenate 'string client-address request-time))
	 (digest (sha:sha-digest-vector hash))
	 (integer-chars (with-fast-array-references ((array digest vector))
			  (loop with len = (length array)
				and limit = (- *file-upload-default-pathname-name-maximum-size* (length name)  1)
				for i fixnum upfrom 0 below len by 2
				for integer = (aref array i)
				sum (the fixnum (integer-length integer)) into size
				while (< size limit)
				nconc (digit-chars integer 8)))))
    (declare (dynamic-extent request-time hash digest integer-chars))
    (concatenate 'string name "-" integer-chars)))

;; why not use URL encoding to make directory delimiters go away? 4/13/2001 -- JCMa.
(defun file-upload-parse-filename (filename directory content-type)
  (flet ((strip-directory-info (string)
	   (loop with end fixnum = (length string)
		 with last-idx fixnum = (1- end)
		 for idx fixnum downfrom last-idx to 0
		 when (char-position (aref string idx) "\\/:>" 0 4)	;directory delimiters
		   return (if (= idx last-idx)
			      (error 'bad-multipart-form-data-posted
				     :format-string "File Upload: Bad file name, ~S."
				     :format-args (list string) :url (server-url *server*))
			      (subseq filename (1+ idx) end))
		 finally (return filename)))
	 (trim-characters (string trim-chars)
	   (when string
	     (let* ((start 0)
		    (end (length string))
		    (chars (- end start))
		    n-chars)
	       (declare (fixnum start end))
	       (unless (zerop chars)
		 ;; removes version numbers and random emacs ~'s
		 (with-string-trim-bounds (trim-chars string start end)
		   (setq n-chars (- end start))
		   (cond ((= chars (setq n-chars (- end start)))
			  string)
			 ((zerop n-chars) nil)
			 (t (subseq string start end)))))))))
    (let* ((path (pathname (strip-directory-info filename)))
	   (p-name (or (trim-characters (pathname-name path) '#.(coerce "~" 'list))
		       (file-upload-generate-file-name)))
	   (p-type (trim-characters (pathname-type path) '#.(coerce "~0123456789" 'list))))
      (cond (p-type
	     (let ((content-type-from-p-type (mime-content-type-spec-for-pathname-type p-type nil)))
	       (cond ((and content-type content-type-from-p-type)
		      (unless (MIME-CONTENT-TYPE-SPEC-EQUAL-P content-type content-type-from-p-type nil)
			(setq content-type content-type-from-p-type)))
		     (content-type-from-p-type
		      (setq content-type content-type-from-p-type))
		     (t (setq content-type '(:application :octet-stream))))))	;default binary
	    (content-type
	     (setq p-type (or (mime-content-type-primary-extension-string content-type nil)
			      (and (second content-type) (string-downcase (symbol-name (second content-type))))
			      "rdm")))		;random extension
	    (t (setq content-type '(:application :octet-stream))))	;default binary
      (values (make-pathname :defaults directory :name p-name :type p-type)
	      (if content-type (mime-content-type-copy-mode content-type) :binary)))))

(defun parse-mime-multipart-form-data-block (mime-stream boundary &optional buffer)
  "Parses and returns a mime multipart form data block from MIME-STREAM upto BOUNDARY.
BUFFER is a line buffer."
  (declare (values query-name value baggage last-block-p))
  (with-header-values (content-disposition content-type) *headers*
    (destructuring-bind (&key name filename &allow-other-keys)
	(cdr content-disposition)
      (cond ((null name)
	     (if content-disposition
		 (error 'bad-multipart-form-data-posted  :url (server-url *server*)
			:format-string "Multipart Form: No name provided for the form element.")
		 (error 'bad-multipart-form-data-posted :url (server-url *server*)
			:format-string "Multipart Form: No Content-Disposition header.")))
	    ((or (null filename) (null-string-p filename))
	     (multiple-value-bind (raw-value last-block-p)
		 (mime-stream-decode-into-string-until-boundary mime-stream boundary nil buffer)
	       (values (%tokenize-form-query-keyword name) raw-value nil last-block-p)))
	    (t (multiple-value-bind (query-name directory)
		   (html2::file-upload-unpack-query-baggage name) 
		 (multiple-value-bind (destination copy-mode)
		     (file-upload-parse-filename filename directory content-type)
		   (multiple-value-bind (pathname last-block-p)
		       (mime-stream-decode-into-file-until-boundary mime-stream destination boundary copy-mode buffer)
		     ;; Record the pathname used to save the file as the value
		     ;; of the input element.  Include the original filename as
		     ;; an additional keyword value pair.
		     (values (%tokenize-form-query-keyword query-name)
			     pathname
			     `(:upload-filename ,filename :content-type ,content-type :copy-mode ,copy-mode )
			     last-block-p)))))))))

)

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.894")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defmethod post-document ((url url:form-processing-mixin) (type (eql :multipart)) (subtype (eql :form-data)) stream)
  (%post-document-handle-mime-multipart-form-data url stream))

 ;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTML2.LISP.299")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- mode: lisp; syntax: ansi-common-lisp; package: html2; base: 10 -*-")

(defun file-upload-directory-for-index (index &optional (error-p t))
  (cond ((gethash index *file-upload-directory-index-table*))
	(error-p
	 (error 'http::bad-multipart-form-data-posted
		:format-string "File Upload: Unknown directory index ~S.~
                              ~&The client may have submitted an obsolete file upload form with stale directory information.~
                              ~&Please use the back button on your browser, reload the form your filled in, resubmit the refreshed form ~
                              ~&and see if the problem goes away."
		:format-args (list index) :url (http::server-url http::*server*)))
	(t nil)))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;UTILS.LISP.491")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: lisp -*-")

(defmethod mime-stream-flush-until-boundary (mime-stream boundary mode &optional buffer)
  (declare (values end-p))
  (with-null-stream (null-stream)
    (mime-stream-decode-until-boundary mime-stream null-stream boundary mode buffer)))
 
;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;UTILS.LISP.491")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: lisp -*-")

(PROGN

(defparameter *mime-multipart-block-maximum-size* nil
  "When non-null, this controls the maximum number of bytes that will be copied from a MIME multipart block.")

(defparameter *mime-multipart-block-maximum-size-exceeded-error-function* 'default-signal-mime-multipart-block-maximum-size-exceeded
  "The function used to signal an error when the number of bytes in a MIME multipart block exceeds a maximum threshold.")

(defun default-signal-mime-multipart-block-maximum-size-exceeded (bytes maximum-bytes)
  (error "MIME Multipart Block Size Exceeded: Attempt to decode ~D bytes which is more than the allowed maximum, ~D bytes."
	 bytes maximum-bytes))

(defmacro signal-mime-multipart-block-maximum-size-exceeded (bytes maximum-bytes)
  `(funcall *mime-multipart-block-maximum-size-exceeded-error-function* ,bytes ,maximum-bytes))

(define-macro with-mime-multipart-block-size-limit ((maximum-bytes &key (error-function 'default-signal-mime-multipart-block-maximum-size-exceeded))
						    &body body)
  "Controls the maximum bytes than a MIME multipart block may contain witn the scope of BODY.
ERROR-FUNCTION is a function that receives the arguments (BYTES MAXIMUM-BYTES) and signals an appropriate error."
  `(let ((*mime-multipart-block-maximum-size* ,maximum-bytes)
	 (*mime-multipart-block-maximum-size-exceeded-error-function* ,error-function))
     ,@body))
)


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;UTILS.LISP.491")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: lisp -*-")

(defmethod mime-stream-decode-until-boundary (mime-stream to-stream boundary (mode (eql :binary)) &optional buffer
							  &aux (max-bytes *mime-multipart-block-maximum-size*) (total-bytes 0))
  (declare (values last-block-p)
	   (ignore buffer)
	   (integer total-bytes))
  (macrolet ((binary-char-code (char)
	       (typecase char
		 (#.*standard-character-type*
		  #+Genera (si:ascii-code char)
		  #-Genera (char-code char))
		 (t #+Genera `(si:ascii-code ,char)
		    #-Genera `(char-code ,char)))))
    (labels ((fill-boundary-array (array array-length boundary boundary-length)
	       (with-fast-array-references ((array array vector)
					    (boundary boundary string))
		 (loop initially (setf (aref array 0) (binary-char-code #\Return)
				       (aref array 1) (binary-char-code #\Linefeed) )
		       for i fixnum below boundary-length
		       do (setf (aref array (+ 4 i)) (char-code (aref boundary i))))))
	     (protected-write-byte (byte stream)
	       (and max-bytes
		    (> (incf total-bytes) (the integer max-bytes))
		    (signal-mime-multipart-block-maximum-size-exceeded total-bytes max-bytes))
	       (write-byte byte stream))
	     (write-array-portion (to-stream array n &rest extra-bytes)
	       (declare (dynamic-extent extra-bytes))
	       (with-fast-array-references ((array array vector))
		 (loop for idx fixnum upfrom 0 below n
		       do (protected-write-byte (aref array idx) to-stream)))
	       (dolist (byte extra-bytes)
		 (protected-write-byte byte to-stream)))
	     (check-for-mime-boundry (mime-stream to-stream boundary-array boundary-array-length)
	       (let ((last 1))
		 ;; start with 1 because the CR matched to get us here!
		 (with-fast-array-references ((boundary-array boundary-array vector))
		   (loop for idx fixnum upfrom 1 below boundary-array-length
			 for next-byte = (read-byte mime-stream)
			 while (eql next-byte (aref boundary-array idx))
			 do (setq last idx)
			 finally (when (< last (1- boundary-array-length))
				   (write-array-portion to-stream boundary-array last next-byte)
				   (return-from check-for-mime-boundry nil))))
		 (let ((byte1 (read-byte mime-stream))
		       (byte2 (read-byte mime-stream)) )
		   (cond ((and (= byte1 (binary-char-code #\-)) (= byte2 (binary-char-code #\-)))
			  (setq byte1 (read-byte mime-stream)
				byte2 (read-byte mime-stream))
			  (cond ((and (eql byte1 (binary-char-code #\Return)) (eql byte2 (binary-char-code #\Linefeed)))
				 (return-from mime-stream-decode-until-boundary t))
				(t (write-array-portion to-stream boundary-array boundary-array-length
							(binary-char-code #\-) (binary-char-code #\-) byte1 byte2))))
			 ((and (eql byte1 (binary-char-code #\Return)) (eql byte2 (binary-char-code #\Linefeed)))
			  (return-from mime-stream-decode-until-boundary nil))
			 (t (write-array-portion to-stream boundary-array boundary-array-length byte1 byte2)))))))
      (declare (inline fill-boundary-array)
	       (dynamic-extent #'protected-write-byte))
      (let* ((boundary-length (length boundary) )
	     (boundary-array-length (+ 4 boundary-length)) ;; len + CR + LF + 2 #\-'s
	     (boundary-array  (make-array boundary-array-length :initial-element #.(char-code #\-))))
	(declare (dynamic-extent boundary-array)
		 (fixnum boundary-length boundary-array-length))
	(with-binary-stream (mime-stream :input)
	  (loop initially (fill-boundary-array boundary-array boundary-array-length boundary boundary-length)
		for byte = (read-byte mime-stream)
		do (if (eql byte (binary-char-code #\Return))
		       (check-for-mime-boundry mime-stream to-stream boundary-array boundary-array-length)
		       (protected-write-byte byte to-stream))))))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;UTILS.LISP.491")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Syntax: Ansi-common-lisp; Package: HTTP; Base: 10; Mode: lisp -*-")

(defmethod mime-stream-decode-until-boundary (mime-stream to-stream boundary (mode (eql :text)) &optional buffer
							  &aux (max-bytes *mime-multipart-block-maximum-size*) (total-bytes 0))
  (declare (values last-block-p)
	   (integer total-bytes))
  (macrolet ((check-size-limit (n-chars total-bytes max-bytes &optional crlf-p)
	       `(when (and ,max-bytes (> (setq ,total-bytes (+ ,total-bytes (the fixnum ,n-chars)
							       ,@(when crlf-p `((if ,crlf-p 2 0)))))
					 (the integer ,max-bytes)))
		  (signal-mime-multipart-block-maximum-size-exceeded ,total-bytes ,max-bytes))))
    (with-text-stream (mime-stream :input)
      (loop with subsequent-line-p and  boundary-length = (length boundary)
	    doing (multiple-value-bind (line error-p delmiter length)
		      (read-delimited-line mime-stream '(#\Return #\Linefeed) nil buffer)
		    (when error-p
		      (return nil))
		    (multiple-value-bind (finish-p last-block-p)
			(mime-boundary-line-p line boundary length boundary-length)
		      (cond (finish-p (return (values last-block-p)))
			    ((member delmiter '(#\Return #\Linefeed))
			     (check-size-limit length total-bytes max-bytes subsequent-line-p)
			     (if subsequent-line-p (terpri to-stream) (setq subsequent-line-p t))
			     (write-string line to-stream :start 0 :end length))
			    (t (check-size-limit length total-bytes max-bytes subsequent-line-p)			     
			       (write-string line to-stream :start 0 :end length)))))))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;VARIABLES.LISP.193")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10; Syntax: ANSI-Common-Lisp;-*-")

(define-parameter *file-upload-maximum-size* 10000000
		  "The maximum number of bytes accepted for a single file upload.
No multipart MIME block may exceed this threshold during file upload.")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;VARIABLES.LISP.193")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: HTTP; BASE: 10; Syntax: ANSI-Common-Lisp;-*-")

(eval-when (:load-toplevel :compile-toplevel :execute) (export (intern "*FILE-UPLOAD-MAXIMUM-SIZE*" :http) :http))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;HTTP-CONDITIONS.LISP.191")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: http; Base: 10; Default-character-style: (:fix :roman :normal) -*-")

(define-condition file-upload-maximum-size-exceeded
                  (bad-multipart-form-data-posted)
  ((reason :initform "File Upload: Maximum Upload File Size Exceeded")))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.899")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defun signal-file-upload-maximum-size-exceeded (bytes maximum-size)
  (declare (ignore bytes))
  (error 'file-upload-maximum-size-exceeded :url (server-url *server*)
	 :format-string "File Upload Error: Attempt to upload more than the allowed maximum file size of ~D bytes."
	 :format-args (list maximum-size)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.899")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defun parse-mime-multipart-form-data (mime-stream maximum-file-size &optional durable-values-p)
  (declare (values raw-query-value-alist)
	   (ignore durable-values-p))
  "Top-level method for parsing multipart form data from mime-stream.
  It returns an ALIST of (QUERY-ID VALUE COLLECTION-FLAG QUERY-BAGGAGE).  When
  multiple values for QUERY-ID are collected into VALUE, COLLECTION-FLAG
  is non-null.  Otherwise, there is no third value.
  QUERY-BAGGAGE is an overloading field on the query-id that can carry additional information.
  When present it is an atom when COLLECTION-FLAG is null and a list when COLLECTION-FLAG is
  non-null."
  (destructuring-bind (&key boundary &allow-other-keys) (cddr (get-header :content-type))
    (unless boundary
      (error 'bad-multipart-form-data-posted
	     :format-string "Multipart Form: No boundary provided with header, Content-Type: ~A"
	     :format-args (list (get-raw-header :content-type *headers* t)) :url (server-url *server*)))
    (with-mime-multipart-block-size-limit (maximum-file-size :error-function 'signal-file-upload-maximum-size-exceeded)
      (using-resource (line-buffer line-buffer *line-buffer-size*)
	(let ((boundary-length (length boundary))
	      alist ptr)
	  (flet ((parse-multipart-block (mime-stream)
		   (with-headers (mime-stream)
		     (with-header-values (content-disposition) *headers*
		       (unless content-disposition
			 (error 'bad-multipart-form-data-posted
				:format-string "Multipart Form: No Content-Dispostion header in data block" :url (server-url *server*)))
		       (destructuring-bind (disposition &rest plist) content-disposition
			 (declare (ignore plist))
			 (case disposition
			   (:form-data
			     (multiple-value-bind (query value baggage last-block-p)
				 (parse-mime-multipart-form-data-block mime-stream boundary line-buffer)
			       (setq ptr (%parse-form-collect-form-value query value baggage alist ptr))
			       (or alist (setq alist ptr))
			       last-block-p))
			   (t (error 'bad-multipart-form-data-posted
				     :format-string "Multipart Form: No handler to parse block with Content Dispostion: ~A"
				     :format-args (list (get-raw-header :content-disposition *headers* t)) :url (server-url *server*)))))))))
	    (declare (inline parse-multipart-block))
	    (multiple-value-bind (line error-p delimiter length)
		(read-delimited-line mime-stream '(#\Return #\Linefeed) nil line-buffer)
	      (declare (ignore delimiter))
	      (when error-p
		(return-from parse-mime-multipart-form-data nil))
	      (multiple-value-bind (multipart-boundry-p last-block-p)
		  (mime-boundary-line-p line boundary length boundary-length)
		(when (and multipart-boundry-p (not last-block-p))
		  (loop until (parse-multipart-block mime-stream)))))
	    alist))))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.899")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defun %post-document-handle-mime-multipart-form-data (url stream)
  (let ((durable-values-p (durable-form-values-p url))
	(maximum-size (file-upload-maximum-size url))
	form-alist)
    (setq form-alist (handling-form-parsing-errors (url)
		       (parse-mime-multipart-form-data stream maximum-size durable-values-p) ))
    (when durable-values-p
      (setf (server-form-data *server*) form-alist))
    (apply-form-response-function url stream form-alist)))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.899")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defmethod  export-url ((url url:http-object) (translation (eql :html-form)) &rest args)
  (destructuring-bind (&key response-function header-function pathname server character-set
			    (durable-form-values nil durable-form-values-p-supplied)
			    (maximum-upload-file-size nil maximum-upload-file-size-supplied-p)
			    &allow-other-keys) args
    (cond (pathname
           (setf (translated-pathname  url) pathname))
          (t (error "No PATHNAME was provided while exporting the URL, ~S, with translation, ~S"
                    url translation)))
    (unless (and response-function (good-response-function-p response-function))
      (error "RESPONSE-FUNCTION, ~S, must be a defined function when exporting the URL, ~S, with translation, ~S"
             response-function url translation))
    (setf (translation-method url) translation
          (character-set url) character-set)
    (if durable-form-values-p-supplied
	(setf (get-value url :durable-form-values-p) durable-form-values)
	(remove-value url :durable-form-values-p))
    (if maximum-upload-file-size-supplied-p
	(setf (get-value url :maximum-upload-file-size) maximum-upload-file-size)
	(remove-value url :maximum-upload-file-size))
    (let ((init-args `(,server ,response-function ,@header-function)))
      (declare (dynamic-extent init-args))
      (url:initialize-specialization url 'url:http-form init-args))
    url))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.899")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defun %export-computed-form (url translation args)
  (destructuring-bind (&key form-function response-function header-function
			    (durable-form-values nil durable-form-values-p-supplied)
			    (maximum-upload-file-size nil maximum-upload-file-size-supplied-p)
			    &allow-other-keys) args
    (unless (and response-function (good-response-function-p response-function))
      (error "RESPONSE-FUNCTION, ~S, must be a defined function when exporting the URL, ~S, with translation, ~S"
             response-function url translation))
    (unless (and form-function (good-response-function-p form-function))
      (error "FORM-FUNCTION, ~S, must be a defined function when exporting the URL, ~S, with translation, ~S"
             form-function url translation))
    (setf (translation-method url) translation)
    (if durable-form-values-p-supplied
	(setf (get-value url :durable-form-values-p) durable-form-values)
	(remove-value url :durable-form-values-p))
    (if maximum-upload-file-size-supplied-p
	(setf (get-value url :maximum-upload-file-size) maximum-upload-file-size)
	(remove-value url :maximum-upload-file-size))
    (let ((init-args `(,form-function ,response-function ,@header-function)))
      (declare (dynamic-extent init-args))
      (url:initialize-specialization url 'url:http-computed-form init-args))
    url))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.899")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(setf (documentation 'export-url 'function)

      "HTTP:EXPORT-URL is the primary method that exports URLS to make them
accessible via the http server. URL is either a string or an interned URL to
be exported.  EXPORT-TYPE is the method to use in exporting URL.


I. Basic Export Types: These involve making the contents of a file accessible via
a URL. These types require URLs that are object (i.e., have a name and extension).

        :HTML-FILE (&key pathname)
        :TEXT-FILE (&key pathname)
        :RTF-FILE (&key pathname)

        :GIF-IMAGE (&key pathname)
        :JPEG-IMAGE (&key pathname)
        :X-BITMAP-IMAGE (&key pathname)
        :PICT-IMAGE (&key pathname)
        :TIFF-IMAGE (&key pathname)

        :BASIC-AUDIO (&key pathname)
        :AIFF-AUDIO (&key pathname)
        :WAV-AUDIO (&key pathname)

        :MPEG-VIDEO (&key pathname)
        :QUICKTIME-VIDEO (&key pathname)

        :VRML-WORLD (&key pathname)

        :DIRECTORY (&key pathname immediate-export recache recursive-p)
        :HTML-DIRECTORY (&key pathname immediate-export recache recursive-p)
        :TEXT-DIRECTORY (&key pathname immediate-export recache recursive-p)
        :LISP-DIRECTORY (&key pathname immediate-export recache recursive-p)
        :IMAGE-DIRECTORY (&key pathname immediate-export recache recursive-p)
        :AUDIO-DIRECTORY (&key pathname immediate-export recache recursive-p)
        :VIDEO-DIRECTORY (&key pathname immediate-export recache recursive-p)
        :WORLD-DIRECTORY (&key pathname immediate-export recache recursive-p)
        :APPLICATION-DIRECTORY (&key pathname immediate-export recache recursive-p)

:DIRECTORY exports all files whose resource type is known.
Others export a specific content type, and ignore other file types.
When recursive-p is non-null, all subdirectories are also exported.
Otherwise, subdirectories are ignored.

When *AUTO-EXPORT* is non-null, new files are automatically exported when they
are scoped by one of these directory export types. Auto-export occurs on
demand for the GET and HEAD methods. If *AUTO-EXPORT* is :ON-DEMAND,
files are exported when they are first requested rather than at the
time the directory is exported. When exporting a directory, a non-null
argument to :IMMEDIATE-EXPORT overrides lazy directory exporting. In general,
on-demand directory exports trade faster server start up for a slightly slower
first access to a file URL within the directory. When :RECACHE is non-null,
a directory export updates the export parameters for every inferior. This
parameter forces traversal of the entire structure, like a non-null
:IMMEDIATE-EXPORT would.

A directory-style list in HTML is the default content returned for the
get method on a URL directory path. This behavior is customized by
providing a response function via the :DIRECTORY-WRITER keyword.  This
response function is called with the arguments (URL STREAM) and must
return non-null when it handles the response. If it declines to handle
the response, it may return NIL, and the standard method will list the
directory as usual. HTTP:WRITE-INDEXED-DIRECTORY-LISTING is a directory
writer that will serve the contents of an index.html file found in
the directory. Other computed returns are possible.

Note that the presence in file or directory names of escaped characters 
(see *ESCAPED-CHARACTERS*) will lead to inconsistent results, and possibly
errors. Space and question mark are examples.

        :PDF-FILE  (&key pathname)
        :POSTSCRIPT-FILE (&key pathname)

        :BINHEX-FILE (&key pathname)
        :STUFFIT-FILE  (&key pathname)
        :COMPRESSED-FILE (&key pathname)
        :MAC-BINARY-FILE (&key pathname)

        :WORD-FILE  (&key pathname)
        :POWER-POINT-FILE (&key pathname)
        :EXCEL-FILE  (&key pathname)

The following export types support inline plug-ins on the client side.
Plug-ins can be referenced using NS2.0:WITH-EMBEDDED-SCRIPT

        Inline speech synthesis is available using a macintalk plug-in
        from http://www.mvpsolutions.com/PlugInSite/Talker.html

        :TALK-AUDIO (&key pathname)

The Java language provides applets that execute on the client.  This kind of
mobile code is supported with the following export types and the HMTL
generation macro WITH-JAVA-APPLET.
        
        :JAVA-FILE (&key pathname)
        :JAVA-BINARY (&key pathname)
        :JAVA-SCRIPT-FILE (&key pathname)

:JAVA-FILE exports the source code whereas :JAVA-BINARY provides the byte
compiled binary to the client running the applet. :JAVA-SCRIPT-FILE exports
the browser scripting language, JavaScript, which is simpler and easier to use
than Java itself.

II. Redirect Export Types: These export types inform the client to
look elsewhere for a URL.  They work for the GET and HEAD operations.
The exported URL can be either an HTTP object or an HTTP path.

        :REDIRECT (&key alternate-urls pathname)
        :TEMPORARY-REDIRECT (&key alternate-urls pathname)

Alternatively, a computed response may call REDIRECT-REQUEST to issue a
redirect rather than serving content itself.

III. Search Export Types: these involve performing searches using the search
index or map search facilities in HTTP. Search URLs must end with ? so that
the system can composed the right combination of classes. In all cases, the
response function must compute the answer and return HTML over the http stream
to the client.

General Purpose Searches

        :SEARCH (&key response-function search-parser search-database)

        This exports a URL that performs searches by calling RESPONSE-FUNCTION with
        the arguments URL and STREAM. The search database and search parameters are
        cached on the URL and accessible via URL:SEARCH-DATABASE and URL:SEARCH-KEYS.

        The optional export argument SEARCH-PARSER is the parser that obtains URL:SEARCH-KEYS
        from the suffix of a search URL. Several parsers are predefined:

                URL:STANDARD-PARSE-SEARCH-INFO: This standard parser for
                search URLs. It tests whether the search info encodes form or
                list data and calls the appropriate of the next two parsers.

                URL:PARSE-SEARCH-INFO: This normal parser for search URLs
                produces a list of search parameters using + as the delimiter.

                URL:PARSE-SEARCH-INFO-AS-QUERY-ALIST: This parser for URL
                encoded form values returns an alist just like posting a form
                would. This parser should be used when an HTML form is
                returned to a search URL.  However, this method of returning
                form values is limited to 1024 characters total in the URL,
                and therefore, it's use in new code is deprecated.

        Users may provide other specialized parsers. They should accept
        the arguments (url-string start end) and need not located the ?
        suffix delimiter.

        The export argument, SEARCH-WRITER, allows a URL to specialize how the
        parsed presentation on URL:SEARCH-KEYS is written. Several writers are
        predefined.

                URL:STANDARD-WRITE-SEARCH-INFO: This standard writer for
                search URLs. It tests whether the search info encodes form or
                list data and calls the appropriate of the next two writers.

                URL:WRITE-SEARCH-INFO: This normal writer for search URLs
                produces a list of search parameters using + as the delimiter.

                URL:WRITE-SEARCH-INFO-AS-QUERY-ALIST: This writer for URL
                encoded form values that prints alist values as name value pairs
                using the urlencoding syntax.

        The export argument, HEADER-FUNCTION, allows a search URL to specialize
        how it responds to the HEAD method based on runtime knowledge.
        HEADER-FUNCTION is called with the argument URL  and returns values
        used to determine respoonse to HEAD. The returned values are:
        (CONTENT-TYPE CONTENT-LENGTH LAST-MODIFICATION VERSION CHARSET PUBLIC ALLOW)
        
                CONTENT-TYPE (required) is a keyword or content-type. Default
                is HTML.

                CONTENT-LENGTH (optional) is the length in bytes of the entity
                body. Default is none.

                LAST-MODIFICATION (optional) is a universal time indicating
                when the entity was last changed. Default is now.
 
                VERSION (optional) is a number or string that distinguishes
                different versions of the entity. Default is none.

                CHARSET (optional) is a keyword indicating the character set
                of the entity. Default is :ISO-8859-1.

                PUBLIC (optional) is a boolean indicating whether the resource
                is available to the public. Default is none.

                ALLOW (optional) is a list of HTTP method keywords available
                on the resource. Default is none.

Image Searches

   Image Maps

        :IMAGE-MAP (&key pathname export-type map-format map-pathname
                         search-parser search-writer header-function)

        This exports the image in PATHNAME as IMAGE-EXPORT-TYPE and
        establishes a response function based on the image map in MAP-PATHNAME
        whose MAP-FORMAT is either :CERN or :NCSA. EXPORT-TYPE is the
        appropriate image search export type (see below).

   Direct Searches

        These provide direct control over the response function for image searches.

        :GIF-IMAGE (&key pathname response-function search-database
                         search-parser search-writer header-function)
        :JPEG-IMAGE (&key pathname response-function search-database
                          search-parser search-writer header-function)
        :PNG-IMAGE (&key pathname response-function search-database
                         search-parser search-writer header-function)
        :X-BITMAP-IMAGE (&key pathname response-function search-database
                              search-parser search-writer header-function)
        :PICT-IMAGE (&key pathname response-function search-database)
                          search-parser search-writer header-function)
        :TIFF-IMAGE (&key pathname response-function search-database
                          search-parser search-writer header-function)

        These export types allow the client's user to click on images and
        invoke a response from the server.  These URL are both objects and
        searchable.  When they are requested without the ? suffix, the
        contents of their associate image file is returned.  When the ? suffix
        appears, their RESPONSE-FUNCTION is called on the arguments URL and
        STREAM. See the macro HTTP:WITH-IMAGE-COORDINATES automatically binds
        the X and Y coordinates.

IV. Computed Export Types: These compute responses returned to clients.

        :COMPUTED (&key response-function header-function) RESPONSE-FUNCTION
        is called with the arguments URL and STREAM and is responsible for
        returning HTML to the client. :COMPUTED has an optional pathname so
        that the computation may reference a file, if necessary.
        HEADER-FUNCTION is documented section III.
        
        :HTML-FORM (&key response-function pathname durable-form-values
        server) :HTML-FORM returns the contents of PATHNAME when it is
        requested via GET.  When there is a POST, its RESPONSE-FUNCTION is
        called with URL, STREAM, and FORM-ALIST.  FORM-ALIST is an alist of
        (QUERY RAW-VALUE) for all the queries in the form that the client
        returns. QUERY is a keyword.  When a client returns multiple
        raw-values for a QUERY, these are aggregated into a list of the values
        associated with the query in a single, order-preserving entry.
        DURABLE-FORM-VALUES is a boolean and controls whether query values
	are new, copied strings or indirect arrays pointing into a volitile
	form data buffer. High-volume applications or forms large query values
	can gain efficiency by using durable form values, all operations on
	these values must be completed within the context of the form's
	response function. When the form uses the MIME multipart encoding,
        the MAXIMUM-UPLOAD-FILE-SIZE may supply the maximum allowable file size,
        if it diverges from the default HTTP:*FILE-UPLOAD-MAXIMUM-SIZE*.

        :COMPUTED-FORM (&key form-function response-function header-function
        durable-form-values server) :COMPUTED-FORM is a cross between
        :COMPUTED and :HTML-FORM that provides FORM-FUNCTION to generate html
        just like :COMPUTED and RESPONSE-FUNCTION to handle the post method
        when form values are returned. FORM-FUNCTION is called with the
        arguments URL and STREAM and is responsible for returning HTML to the
        client. RESPONSE-FUNCTION takes the same arguments as :HTML-FORM.

        :SCRIPT (&key script header-function) Script is a client-side script
        defined with NS2.0:DEFINE-SCRIPT. These scripts may merely deliver a
        static copy of the client-side script, or they may perform a
        computation that emits an appropriate script.

        :SHTML-FILE (&key pathname header-function) This is a computed
        response that is inserted in a static HTML file containing
        server-parsed HTML. When an SHTML element is found by the server, it
        inserts the result of a computation in place of the SHTML element.
        SHTML elements are delimted by <!--# and --> and look like:

                <!--#include file=\"insert.text\"-->

        INCLUDE is an SHTML operation that requires a FILE. For security
        reasons, FILE must be in the same directory as the STHML file. Access
        may be controlled by providing the optional SUBNETS parameter, which
        is a comma-separated string of IP addresses without whitespace.

         <!--#include file=\"insert.text\" subnets=\"128.52.0.0,18.21.0.0\"-->
        
        EVAL is a more general SHTML operation that requires an ACTION
        parameter. ACTION is a string denoting an SHTML action. Specific
        parameters may be required by individual actions. Here, DATE-TIME
        is the operation and FORMAT is a parameter.

              <!--#eval action=\"date-time\" format=\"iso\"-->

        Predefined actions are documented by HTTP:SHOW-SHTML-ACTIONS. New
        SHTML actions are defined with HTTP:DEFINE-SHTML-ACTION. Files with
        the extention SHTML are autoexported by HTML directory export types.


V. Reverse Proxy Export Type

This provides a reverse proxying facility for servers, the reverse of normal
client proxying. In a reverse proxy, when a client requests a specific URL,
the server proxies the request to an origin server and returns the resulting
HTTP message to the client. Thus, the reverse proxy behaves as if it was the
origin server.

To perform this task, the reverse proxy needs to know the origin server to
which a particular incoming client HTTP request is mapped. This information is
provided at URL export time by the application. Although all requests on a
particular port may be reverse proxied, this facility allows different
segments of the URL space to be delegated to different origin servers by
according to the local URL provided to EXPORT-URL. Applications may choose to
either run with local proxy caching or not, depending on the value of
*PROXY-CACHING-P*. Normally, local proxy caching will improve performance.
Note that this facility is only available when the proxy server is loaded.

        :REVERSE-PROXY (&key proxy-forwardings) proxy-forwardings is a list of
        (DOMAIN-NAME PORT PROTOCOL). If more than one host is provided,
        forwarding distributed over the set of origin servers. Only the most
        general URL needs to be exported because the facility automatically
        forwards requests to inferior URLs. Although the reverse proxy
        enforces all security parameters on the root reverse-proxy URL,
	all other issues concerning the URL are delegated to the origin server,
	for example, method availability and URL existence. When desired, a 
	proxy may be placed between the reverse proxy and the origin server using
	DEFINE-CLIENT-PROXY-MAPPING.


VI. Export Parameters and Issues

1. Portable Exports: The #u reader macro merges a partial URL specification
against the default for the local host. Use this when you be able to load the
same exports file on different hosts. The #u reader macro has an extended syntax
that allows you to overview the default host and port specified by
the server configuration. The syntax is 

     #u(url-string :host host-string :port port-number)

URL-STRING is a relative URI. HOST-STRING is the domain name or
IP string for the host. PORT-NUMBER is an integer.


2. Security: 

  A. Subnet Security: Secure subnets are a list of IP addresses, where 0 is a
  wildcard. For example, 128.52.0.0 matches all the subnets at the AI lab.
  The follow export arguments allow control over which clients can perform
  HTTP methods such as GET, HEAD, POST, :OPTIONS, or :TRACE (read access)
  versus PUT or DELETE (write access).

     :READ-SUBNETS allows read access to be specified at the level of
     URLs as they are exported.

     :WRITE-SUBNETS allows write access to be specified at the level of
     URLs as they are exported.

     DEFINE-READ-SUBNETS restricts read access globally to the server.

     DEFINE-WRITE-SUBNETS restricts write access globally to the server.

  Write access presumes read access, and consequently, IP addresses from the
  write subnets need not be included in the read subnets. To select the global
  authentication policy for write methods, HTTP:*ACCEPT-WRITE-METHODS*.

  DEFINE-SUBNET can be used to specify named subnet for use in subnet 
  specifications. Note that named subnets are resolved when they are used,
  for example by a call to EXPORT-URL, and therefore, changes to named
  subnets require re-export of URL referring to them.

  B. User Authentication: URL authentication can be specified using the
  following export arguments.

     :AUTHENTICATION-REALM a realm obtainable via INTERN-REALM.
     These can be created with ADD-REALM.

     :CAPABILITIES a URL access control obtainable via INTERN-ACCESS-CONTROL.
     These can be created with ADD-ACCESS-CONTROL-GROUP.

  See also: ADD-GROUP, ADD-USER, and SAVE-AUTHENTICATION-DATA.

  C. Connection Timeouts: URL-specific timeouts can override the global
  timeouts controlling when an HTTP connection can be kill

     :LIFE-TIME is the number of milliseconds that an HTTP connection can live
     before it is scavengeable.  The gloabl default is *SERVER-LIFE-TIME*.

     :TIMEOUT is 60ths of a second and controls the amount idle time before an
     HTTP connection is scavengeable. The gloabl default is *SERVER-TIMEOUT*.

  These value supplied for :LIFE-TIME or :TIMEOUT must be an integer, null or
  a function.  An integer is interpreted as timeout value in the appropriate
  units.  When the the value is null, the field is is cleared on the URL and
  the global default applies. If the value is a function, the function is
  called with the argument (URL) and must return an integer.

3. Expiration: The expiration time for a url is issued as an EXPIRES header so
that proxy servers can determine when they need to refresh their cache.

Expiration is controlled by providing the :EXPIRATION when exporting any URL.
If expiration is not provided, the default is no expiration date.

The :EXPIRATION keyword takes one argument, which is either keyword or a list
of (keyword &rest arguments).

     Arguments                       Meaning

 :NO-EXPIRATION-HEADER        --  No EXPIREs header is issued.
 :NEVER                       --  EXPIRES header indicates one year from now.
 (:INTERVAL <universal-time>) --  EXPIRES header indicates now + <universal-time>.
 (:TIME <universal-time>)     --  EXPIRES header indicates an <universal-time>.
 (:FUNCTION <function-spec>)  --  EXPIRES header indicates universal time computed by
                                  applying <function-spec> to URL.  <function-spec>
                                  should return a universal for use in the EXPIRES header
                                  or NIL, in which case no EXPIRES header is issued.

4. Character Sets: The :CHARACTER-SET keyword allows any URLs whose content
type is TEXT (e.g., text/plain, text/html) to be exported with character sets
other than the HTTP default :ISO-8859-1, or subsets. The valid character sets
for HTTP are:  :US-ASCII, :ISO-8859-1, :ISO-8859-2, :ISO-8859-3, :ISO-8859-4,
:ISO-8859-5, :ISO-8859-6, :ISO-8859-7, :ISO-8859-8, :ISO-8859-9, :ISO-2022-JP,
:ISO-2022-JP, :ISO-2022-KR, :UNICODE-1-1, :UNICODE-2-2-UTF-7,
:UNICODE-2-2-UTF-7.  Whenever TEXT content types use a character set other
than :ISO-8859-1, the HTTP requires explicit specification via this export
keyword.

6. Languages:  The :LANGUAGE keyword may be supplied for any exported
URL. The value is a sequence of keywords denoting the language(s) in which the
resource is written. HTTP 1.1 defines these keywords in section 3.10 to
conform with RFC 1766. They can be a two-letter ISO 639 language abbreviation,
optionally with a two-letter ISO 3166 country code as a subtag.

7. Documentation: Keywords and a descriptive string can be attached to URLs at
export time.  For directory exports, note that these arguments apply to ALL
URLs exported during the directory walk.

     :KEYWORDS                  A list of keywords.
     :DOCUMENTATION             A string describing the URL.


8. Virtual Hosts: HTTP 1.1 requires a virtual hosting facility,
which this server implements. You can define a virtual host (or
vanity name) that will be served by the physical server from
the same IP address. Any URIs served by a virtual host must be
exported by specifying the absolute URI, including host and port
number.  The #u reader macro may be useful here.  The following
operators are available for use with virtual hosts:

     ADD-VIRTUAL-HOST: Adds a virtual host on a virtual port and
                       and makes URLs served by that host available
                       to HTTP 1.1 or greater clients.

     REMOVE-VIRTUAL-HOST: Makes the virtual host unavailable, but does
                          does not remove any URLs it exports.

9. New static export types for data stored in files can be defined with
DEFINE-URL-EXPORT-TYPE.

10. HTTP 1.1 Cache Control: The keywords below may be supplied when exporting
any URL in order to statically control how downstream proxies and caches
handle the content associated with a URL.

        :PUBLIC -- If the value is T, the entire message is cachable by any
        cache even if it would not normally be cached.

        :PRIVATE -- If the value is T, the entire message is intended for a
        single user and must not be cached by a shared cache. If the value is
        a list of keywords denoting specific header, then only these headers
        should be considered private.

        :NO-CACHE -- If the value is T, the entire message must not be cached
        by any cache along the request chain.  If the value is a list of
        keywords denoting specific headers, then only these headers should be
        discarded before caching.

        :NO-STORE -- If the value is T, the entire message must not be stored
        in any non-volatile storage by any cache along the request chain.

        :MAX-AGE -- The value is the number of seconds for which the response
        is valid, after which it should be revalidated.  This directive
        overrides the expiration header, thus allowing HTTP 1.1 server to
        provide a longer expiration time to HTTP 1.1 servers and proxies.
        This defaults to a value derived from the expiration time.

        :MUST-REVALIDATE -- If the value is T, caches and proxies must not
        serve the resource without revalidating it once it becomes stale, even
        if configured to operate with state data. Servers should send this
        directive if and only if failure to revalidate will result in
        incorrect operation. Recipients must not take any automated action
        that violates this directive.

        :PROXY-REVALIDATE -- If the value is T, this directive is the same as
        :MUST-REVALIDATE except that it applies only to proxies and not
        non-shared user caches. It can be used on response to an authenticated
        request to permit the user's cache to store and later return the
        response without needing to revalidate it.

        :NO-TRANSFORM -- If the value is T, caches and proxies must not change
        the body of the message or change headers describing the content.

11. Property List: The PROPERTY-LIST keyword adds a property list of
alternating keywords and value to a URL.  These properties can be read
and set via GET-VALUE.")

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "HTTP:SERVER;SERVER.LISP.899")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Syntax: ANSI-Common-Lisp; Package: http; Base: 10 -*-")

(defmethod file-upload-maximum-size ((url url::form-processing-mixin))
  (multiple-value-bind (value found-p)
      (get-value url :maximum-upload-file-size)
    (if found-p value *file-upload-maximum-size*)))