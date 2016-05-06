(use uri-common sha1 message-digest srfi-19 dot-locking)
(define (env-ref str #!optional (default #f))
  (or (get-environment-variable str) default))

(define-syntax cgi-debug
  (syntax-rules ()
    [(_ expr ...)
     (handle-exceptions exn
	 (begin
	   (header-set! Status: 200 'OK)
	   (header-set! Content-Type: 'text/html)
	   (header-send)
	   (<body> (<h1> (display "DEBUG"))
		   (<pre> (pp (condition->list exn)))))
       expr ...)]))

(define-syntax <tag>
  (syntax-rules (@)
    [(_ name (@ (attr value) ...)
        body ...)
     (begin
       (display (conc "<" 'name " "
                      (conc 'attr "=\"" value "\" ") ...
                      ">"))
       body ...
       (display (conc "</" 'name ">")))]
    [(_ name (@ ())
	body ...)
     (<tag> name (@)
	    body ...)]
    [(_ name body ...)
     (<tag> name (@)
            body ...)]))

(define-syntax <tag/>
  (syntax-rules ()
    [(_ name (@ (attr value) ...))
     (display (conc "<" 'name " "
                    (conc  'attr "=\"" value "\" ") ...
                    "/>"))]
    [(_ name (@ ()))
     (<tag/> name (@))]
    [(_ name)
     (<tag/> name (@))]))


;;; header body プリーズ
(define-syntax html!
  (syntax-rules ()
    [(_ expr ...)
     (begin (header-set! Content-type: 'text/html)
	    (header-send)
	    (html5 expr ...))]))

(define-syntax html5
  (syntax-rules ()
    [(_ expr ...)
     (begin (display "<!doctype html>\n")
	    (<html> (@ [lang: "ja"])
		    expr ...))]))

(define-syntax define-tags
  (ir-macro-transformer
   (lambda (expr inject compare)
     `(begin
        ,@(map (lambda (sym)
                 (let* ([sym  (inject sym)]
                        [symstr (symbol->string sym)]
                        [name (string->symbol  (conc "<" sym ">"))]
                        [body (gensym 'body)]
                        [attr (gensym 'attr)]
                        [value (gensym 'value)])
                   (cond [(irregex-match ".*/$" symstr)
                          (let ([newsym (string->symbol (string-drop-right symstr 1))])
                            `(define-syntax ,name
                               (syntax-rules ()
                                 [(_ expr ...)
                                  (<tag/> ,(inject newsym) expr ...)])))]
                         [else
                          `(define-syntax ,name
                             (syntax-rules ()
                               [(_ expr ...)
                                (<tag> ,sym expr ...)]))])))
               (cdr expr))))))


(define-tags
  html
  ;; metadata
  head title style
  base/ link/ meta/
  
  ;; script
  script noscript
  
  ;; section
  body section nav article aside h1 h2 h3 h4 h5 h6
  header footer address
  
  ;; grouping
  p hr pre blockquote ol ul li dl dt dd figure figcaption div main
  
  ;; text semantics
  a em strong small s cite q dfn abbr time code var samp kbd sub sup i b mark
  ruby rt rp bdo span
  br/ wbr/
  
  ;; edits
  ins del
  
  ;; embedded
  iframe object img/ embed/
  
  ;; table
  table caption tbody thead tfoot tr td th colgroup

  ;; form
  form fieldset legend label input/ button select datalist optgroup
  option textarea keygen/ output progress meter

  ;; other
  menu
  )

;;; html escape
;;; エスケープさせた方が良い文字列には echo を使え
(define (html-escape str)
  (with-output-to-string
      (lambda ()
        (string-for-each (lambda (ch)
                           (case ch
                             [(#\") (display "&quot;")]
                             [(#\') (display "&#39;")]
                             [(#\&) (display "&amp;")]
                             [(#\<) (display "&lt;")]
                             [(#\>) (display "&gt;")]
                             [(#\ ) (display "&nbsp;")]
                             [(#\newline) (display "<br />")]
                             [else (write-char ch)]))
                         str))))
(define (newline->br str)
  (irregex-replace #\newline str "<br />"))

(define (echo . args)
  (for-each
   (compose display html-escape ->string)
   args))
;;; getによるパラメタ
(define (get-alist)
  (form-urldecode
   (env-ref "QUERY_STRING")))
;;; postによるパラメタ
(define post-alist
  (let ([cache #f])
    (lambda ()
      (unless cache
        (set! cache (form-urldecode
                     (read-string (string->number (env-ref "CONTENT_LENGTH"))))))
      cache)))
;;; get、postまとめ
(define (query-alist)
  (let ([method (env-ref "REQUEST_METHOD")])
    (cond [(string=? method "GET") (get-alist)]
          [(string=? method "POST") (post-alist)]
          [else #f])))

(define (query-ref key #!optional (default #f))
  (alist-ref key (query-alist) eqv? default))

(define-values (header-set! header-delete! header-send)
  (let ([header '()])
    (define (h-set! name . args)
      (set! header
	    ;(alist-update! name args header)
	    (cons (cons name args) header)))
    (define (h-delete! name)
      (set! header (alist-delete name header)))
    (define (h-send)
      (for-each (lambda (item)
		  (display (conc (car item) ": "))
		  (for-each (lambda (x) (display (conc " " x))) (cdr item))
		  (newline))
		(reverse! header))
      (newline))
    (values h-set! h-delete! h-send)))

(define (jump-to-url url)
  (header-set! Status: 302 'Ridirect)
  (header-set! Location: url)
  (header-send))

(define-syntax link-to-sub
  (syntax-rules ()
    [(_ str url (pair ...))
     (<a> (@ [href: url]
	     pair ...)
	  (display str))]
    [(_ str url (pair ...) key value rest ...)
     (link-to-sub str url (pair ... [key value]) rest ...)]))
;;; (link-to "hello" "hello.com" class: "foo" target: "_blank" ...)
(define-syntax link-to
  (syntax-rules ()
    [(_ name url)
     (link-to-sub name url ())]
    [(_ name url key value rest ...)
     (link-to-sub name url ([key value]) rest ...)]))

;;; COOKIE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (cookie-alist)
  (let ([cookie (env-ref "HTTP_COOKIE")])
    (if cookie
        (map (lambda (str) (let ([pair (irregex-split "=" str)])
			(cons (string->symbol
			       (uri-decode-string (first pair)))
			      (uri-decode-string (second pair)))))
             (irregex-split "; *" cookie))
        '())))

(define (cookie-ref key #!optional (default #f))
  (alist-ref key (cookie-alist) default))

;;; expiresは秒で指定
(define (cookie-set! name value #!key expires domain path secure (httponly #t))
  (header-set! Set-Cookie:
	       (conc (uri-encode-string (->string name))
		     "="
		     (uri-encode-string (->string value))
		     ";"
		     (if expires (conc " expires="
				       (format-date				       
					"~a, ~d ~b ~Y ~H:~M:~S GMT"
					(date-add-duration
					 (current-date (utc-timezone-locale))
					 (seconds->time expires))) ";")
			 "")
		     (if domain  (conc " domain="  domain  ";") "")
		     (if path    (conc " path="    path    "; ") "")
		     (if secure " Secure;" "")
		     (if httponly " Httponly;" ""))))


;;; session ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 無効なsession idが送られてきた場合、セッションを作成しないで
;;; #fを返す
(define-record _session_
  name id filepath alist writer expires)
(define *session* #f)


(define (session-file-reader filename)
  (lambda () (first (read-file filename))))

(define (session-file-writer filename)
  (lambda (data) (with-output-to-file filename
	      (lambda () (write data)))))

(define (session-start name session-root #!key expires lock)
  (unless (directory-exists? session-root)
    (create-directory session-root))
  (let ([session
	 (cond [(alist-ref name (cookie-alist)) =>
		(cut restore-session name session-root <> expires)]
	       [else
		(init-session name session-root expires)])])
    (when lock (obtain-dot-lock (_session_-filepath session)))
    (set! *session* session)
    #t))

(define (session-unlock)
  (release-dot-lock (_session_-filepath *session*)))

(define (valid-session-path? path)
  (file-exists? path))

(define (restore-session name session-root id expires)
  (cookie-set! name id #:expires expires)
  (let ([filename (make-pathname session-root id)])
    (if (valid-session-path? filename)
	(begin
	  (make-_session_ name
			    id
			    filename
			    (first (read-file filename))
			    (session-file-writer filename)
			    expires))
	(init-session name session-root expires))))

(define (init-session name session-root expires)
  (let* ([id (message-digest-string
	      (sha1-primitive)
	      (string-append (seconds->string)
			     (env-ref "REMOTE_PORT")
			     (env-ref "REMOTE_ADDR")
			     (env-ref "QUERY_STRING")
			     (env-ref "HTTP_USER_AGENT")))]
	 [filename (make-pathname session-root id)])
    (cookie-set! name id #:expires expires)
    (with-output-to-file filename
      (lambda () (write '())))
    (make-_session_ name
		      id
		      filename
		      '()
		      (session-file-writer filename)
		      expires)))

(define (session-id)
  (_session_-id *session*))

(define (session-set! key value)
  (let ([new-alist (alist-update key value (_session_-alist *session*))])
    (_session_-alist-set! *session* new-alist)
    ((_session_-writer *session*) new-alist)))

(define (session-ref key #!optional (default #f))
  (alist-ref key (_session_-alist *session*) eqv? default))

(define (session-delete! key)
  (let ([new-alist (alist-delete key (_session_-alist *session*))])
    (_session_-alist-set! *session* new-alist)
    ((_session_-writer *session*) new-alist)))

;;; header-sendより先に呼ばないとダメ
(define (session-destroy!)
  (delete-file* (_session_-filepath *session*))
  (cookie-set! (_session_-name *session*) "hoge" #:expires -86400))
