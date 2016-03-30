"http"
`("cl-http;*.*"       ,(http-pathname))
`("http;*.*"          ,(http-pathname))
`("acl;**;*.*"        ,(http-pathname "acl" "obc"))	;5/8/2001 -- JCMa.
`("docs;**;*.*"       ,(http-pathname "docs"))
`("logs;**;*.*"       ,(http-pathname "log"))
`("sources;**;*.*"    ,(http-pathname))
`("www;**;*.*"        ,(http-pathname "www"))
`("root;**;*.*"       ,(rooted-pathname))
`("**;*.*"            ,(http-pathname))

"minp"
`("**;*.*.*" ,(translate-logical-pathname "HTTP:clim;clim-sys;"))
`("*.*.*" ,(translate-logical-pathname "HTTP:clim;clim-sys;"))

"aclpc"
`("**;*.*.*" ,(translate-logical-pathname "HTTP:acl;aclpc;"))
`("*.*.*" ,(translate-logical-pathname "HTTP:acl;aclpc;"))

"html-parser"
`("html-parser;*.*.*" ,(translate-logical-pathname "HTTP:html-parser;v7;"))
`("**;*.*.*" ,(translate-logical-pathname "HTTP:html-parser;v7;"))
`("*.*.*" ,(translate-logical-pathname "HTTP:html-parser;v7;"))
