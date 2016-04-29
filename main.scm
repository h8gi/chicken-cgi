(use uri-common sha1 message-digest srfi-19)

(define get-env get-environment-variable)

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
    [(_ name body ...)
     (<tag> name (@)
            body ...)]))
(define-syntax <tag/>
  (syntax-rules ()
    [(_ name (@ (attr value) ...))
     (display (conc "<" 'name " "
                    (conc  'attr "=\"" value "\" ") ...
                    "/>"))]
    [(_ name)
     (<tag/> name (@))]))


;;; header body プリーズ
(define-syntax html!
  (syntax-rules ()
    [(_ expr ...)
     (begin (header-set! Content-type: 'text/html)
	    (html5 expr ...)
	    (quit))]))

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
   (get-env "QUERY_STRING")))
;;; postによるパラメタ
(define post-alist
  (let ([cache #f])
    (lambda ()
      (unless cache
        (set! cache (form-urldecode
                     (read-string (string->number (get-env "CONTENT_LENGTH"))))))
      cache)))
;;; get、postまとめ
(define (query-alist)
  (let ([method (get-env "REQUEST_METHOD")])
    (cond [(string=? method "GET") (get-alist)]
          [(string=? method "POST") (post-alist)]
          [else #f])))

(define (cookie-alist)
  (let ([cookie (get-env "HTTP_COOKIE")])
    (if cookie
        (map (lambda (str) (let ([pair (irregex-split "=" str)])
			(cons (string->symbol
			       (uri-decode-string (first pair)))
			      (uri-decode-string (second pair)))))
             (irregex-split "; *" cookie))
        '())))

(define (query-ref key)
  (alist-ref key (query-alist)))
(define (cookie-ref key)
  (alist-ref key (cookie-alist)))

;;; expiresは秒で指定
(define (set-cookie name value #!key expires domain path)
  (display (conc "Set-Cookie: "
		 (uri-encode-string (->string name)) "=" (uri-encode-string (->string value)) ";"
		 (if expires (conc " expires="
				   (format-date				       
				    "~a, ~d ~b ~Y ~H:~M:~S GMT"
				    (date-add-duration
				     (current-date (utc-timezone-locale))
				     (seconds->time expires))) ";")
		     "")
		 (if domain  (conc " domain="  domain  ";") "")
		 (if path    (conc " path="    path    "; ") "")))
  (display "\r\n"))

(define-values (header-set! header-delete! header-send)
  (let ([header '()])
    (define (h-set! name . args)
      (set! header (alist-update! name args header)))
    (define (h-delete! name)
      (set! header (alist-delete name header)))
    (define (h-send)
      (for-each (lambda (item)
		  (display (conc (car item) ": "))
		  (for-each (lambda (x) (display (conc " " x))) (cdr item))
		  (display "\r\n"))
		(reverse! header))
      (display "\r\n"))
    (values h-set! h-delete! h-send)))

(define (http-header . lines)
  (for-each (lambda (line)
	      (display (string-chomp line))
	      (display "\r\n"))
	    lines) 
  (display "\r\n"))

(define (jump-to-file url)
  (header-set! Location: url)
  (header-send)
  (quit))

;;; session ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 無効なsession idが送られてきた場合、セッションを作成しないで
;;; #fを返す

(define-record __session__
  name id filepath alist writer expires)

(define (session-file-reader filename)
  (lambda () (first (read-file filename))))

(define (session-file-writer filename)
  (lambda (data) (with-output-to-file filename
	      (lambda () (write data)))))

(define (session-start name session-root #!key expires)
  (cond [(alist-ref name (cookie-alist)) =>
	 (cut restore-session name session-root <> expires)]
	[else
	 (init-session name session-root expires)]))
(define (valid-session-path? path)
  (file-exists? path))
(define (restore-session name session-root id expires)
  (set-cookie name id #:expires expires)
  (let ([filename (string-append session-root id)])
    (if (valid-session-path? filename)
	(make-__session__ name
			  id
			  filename
			  (first (read-file filename))
			  (session-file-writer filename)
			  expires)
	(init-session name session-root expires))))

(define (init-session name session-root expires)
  (let* ([id (message-digest-string
	      (sha1-primitive)
	      (string-append (seconds->string)
			     (get-env "REMOTE_PORT")
			     (get-env "REMOTE_ADDR")
			     (get-env "QUERY_STRING")
			     (get-env "HTTP_USER_AGENT")))]
	 [filename (string-append session-root id)])
    (set-cookie name id #:expires expires)
    (with-output-to-file filename
      (lambda () (write '())))
    (make-__session__ name
		      id
		      filename
		      '()
		      (session-file-writer filename)
		      expires)))

(define (session-update! session key value)
  (let ([new-alist (alist-update! key value (__session__-alist session))])
    (__session__-alist-set! session new-alist)
    ((__session__-writer session) new-alist)
    new-alist))

(define (session-ref session key)
  (alist-ref key (__session__-alist session)))

;;; header-sendより先に呼ばないとダメ
(define (delete-session! session)
  (delete-file* (__session__-filepath session))
  (set-cookie (__session__-name session) "hoge" #:expires -86400))

