(module hashed-url-param mzscheme
  (require (lib "plt-match.ss")
           (lib "list.ss"))
  (require (prefix hash-store: (planet "hash-store.ss" ("jaymccarthy" "hash-store.plt" 1)))
           (planet "string.ss" ("jaymccarthy" "mmss.plt" 1)))
  (require (prefix orig: "url-param.ss"))
  
  (define current-url-param-hash-store (make-parameter #f))
  
  (define (encode params)
    (define orig (orig:encode params))
    (define id (hash-store:store! (current-url-param-hash-store) (string->bytes/utf-8 (write/string orig))))
    (list (format "~a=~a" 'HASH id)))
  
  (define (decode params)
    (define hashed (orig:decode params))
    (match hashed
      [(list (list-rest 'HASH strid))
       (define id (string->bytes/utf-8 strid))
       (define orig
         (read/string
          (bytes->string/utf-8
           (hash-store:lookup (current-url-param-hash-store) id))))
       (orig:decode orig)]
      [_
       empty]))
  
  (define (with-url-parameters hs-path continue)
    (parameterize ([orig:current-url-param-encode encode]
                   [current-url-param-hash-store (hash-store:create hs-path)])
      (orig:with-url-parameters continue)))
  
  (define (make-instance-expiration-handler hs-path fail)
    (define handler (orig:make-instance-expiration-handler fail))
    (lambda (failed-request)
      (parameterize ([orig:current-url-param-decode decode]
                     [orig:current-url-param-encode encode]
                     [current-url-param-hash-store (hash-store:create hs-path)])
        (handler failed-request))))
  
  (define (make-start-reconstruction-handler hs-path continue restart)
    (define handler (orig:make-start-reconstruction-handler continue restart))
    (lambda (new-request)
      (parameterize ([orig:current-url-param-decode decode]
                     [orig:current-url-param-encode encode]
                     [current-url-param-hash-store (hash-store:create hs-path)])
        (handler new-request))))
  
  (define make-continuation-expiration-handler
    make-instance-expiration-handler)
  
  (provide make-instance-expiration-handler
           make-start-reconstruction-handler
           make-continuation-expiration-handler
           with-url-parameters)
  (provide encode
           decode)
  (provide (rename orig:bind-url-parameter bind-url-parameter))
  (provide (rename orig:current-url-param-request current-url-param-request)
           (rename orig:current-url-param-decode current-url-param-decode)
           (rename orig:current-url-param-encode current-url-param-encode)))