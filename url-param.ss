(module url-param mzscheme
  (require (lib "servlet.ss" "web-server")
           (lib "struct.ss")
           (lib "plt-match.ss")
           (lib "url.ss" "net")
           (lib "list.ss")
           (lib "etc.ss"))
  (require (planet "list.ss" ("jaymccarthy" "mmss.plt" 1))
           (planet "maybe.ss" ("jaymccarthy" "mmss.plt" 1)))
  
  ;; Interface  
  (provide make-instance-expiration-handler
           make-start-reconstruction-handler
           make-continuation-expiration-handler
           with-url-parameters)
  
  ; make-instance-expiration-handler : (request? -> a) -> request? -> (U a void)
  (define (make-instance-expiration-handler fail)
    (define params (web-cell-ref *current-url-params*))
    (lambda (failed-request)
      (parameterize ([current-url-param-request failed-request])
        (with-url-parameters
         (lambda ()
           (define the-servlet-url (request->servlet-url failed-request))
           (if (reconstructable? params failed-request)
               (redirect-to (servlet-url->url-string/no-continuation the-servlet-url)
                            temporarily)
               (fail failed-request)))))))
  
  ; make-start-reconstruction-handler : (-> a) (request? -> a) -> request? -> a
  (define (make-start-reconstruction-handler continue restart)
    (define params (web-cell-ref *current-url-params*))
    (lambda (new-request)
      (parameterize ([current-url-param-request new-request])
        (with-url-parameters
         (lambda ()
           (if (reconstructable? params new-request)
               (begin (reconstruct params new-request)
                      (continue))
               (restart new-request)))))))
  
  ; make-continuation-expiration-handler
  (define make-continuation-expiration-handler make-instance-expiration-handler)
  
  ; with-url-parameters : (-> a) -> a  
  (define (with-url-parameters continue)
    (define params (web-cell-ref *current-url-params*))
    (define old (current-url-transform))
    (parameterize ([current-url-transform
                    (lambda (url)
                      (embed-params params (old url)))])
      (continue)))
  
  ;; Mid-tier  
  ; embed-params : (listof param-def?) string? -> string?
  (define (embed-params params base-url/s)
    (define base-url (string->url base-url/s))
    (define rpath (reverse (url-path base-url)))
    (url->string
     (copy-struct url base-url
                  [url-path (append (reverse (rest rpath))
                                    (list (copy-struct path/param (first rpath)
                                                       [path/param-param
                                                        ((current-url-param-encode) params)])))])))
  
  ; encode : (listof param-def?) -> (listof string?)
  (define (encode params)
    (map (lambda (the-pd)
           (format "~a=~a"
                   (param-def-id the-pd)
                   ((param-def-write the-pd)
                    (web-cell-ref (param-def-cell the-pd)))))
         params))
  
  ; decode : (listof string?) -> (listof (cons symbol? string?))
  (define (decode uri-params)
    (map just-value
         (filter just?
                 (map (match-lambda
                        [(regexp "^([^=]+)=(.*)$"
                                 (list s k v))
                         (make-just (cons (string->symbol k) v))]
                        [else
                         (make-nothing)])
                      uri-params))))
  
  ; recover : (listof param-def?) request? -> (listof (maybe (cons param-def? a)))
  (define (recover params a-request)
    (define a-url (request-uri a-request))
    (define last-param
      (path/param-param (first (reverse (url-path a-url)))))
    (define present-params
      ((current-url-param-decode) last-param))
    (define (select k)
      (first-in-list (lambda (k*v)
                       (eq? (car k*v) k))
                     present-params))
    (map (lambda (the-pd)
           (define k*v (select (param-def-id the-pd)))
           (with-handlers ([exn:fail? (lambda _ (make-nothing))])
             (make-just (cons the-pd
                              ((param-def-read the-pd) (cdr k*v))))))
         params))
  
  ; reconstructable? : (listof param-def?) request? -> boolean?
  (define (reconstructable? params req)
    (empty? (filter nothing? (recover params req))))
  
  ; reconstruct : (listof param-def?) request? -> void
  (define (reconstruct params req)
    (for-each (match-lambda
                [(struct just ((list-rest pd v)))
                 (web-cell-shadow (param-def-cell pd) v)])
              (recover params req)))
  
  (provide encode
           decode)
  
  ;; Internal parameters
  ; param-def a : symbol? * (string? -> a) * (a -> string?) * (local-cell (param a))
  (define-struct param-def (id read write cell))
  ; param a : a
  (define-struct param (value))
  
  ;; URL parameters
  (provide bind-url-parameter)
  
  ; bind-url-parameter : (cell:local a) symbol? (string? -> a) (a -> string?) -> void
  (define (bind-url-parameter cell id read write)
    (web-cell-shadow *current-url-params*
                         (list* (make-param-def id read write cell)
                                (web-cell-ref *current-url-params*))))
  
  ;; Top-level
  (provide current-url-param-request
           current-url-param-decode
           current-url-param-encode)
  
  ; *current-url-params* : (local-cell (listof param-def?))
  (define *current-url-params* (make-web-cell empty))
  
  ; current-url-param-request : request
  (define current-url-param-request (make-parameter #f))
  
  ; current-url-param-encode : (listof param-def?) -> (listof string?)
  (define current-url-param-encode (make-parameter encode))
  
  ; current-url-param-decode : (listof string?) -> (listof (cons symbol? string?))
  (define current-url-param-decode (make-parameter decode)))