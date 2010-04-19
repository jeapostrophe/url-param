(module secure mzscheme
  (require (lib "servlet.ss" "web-server")
           (lib "plt-match.ss"))
  (require (planet "string.ss" ("jaymccarthy" "mmss.plt" 1))
           (planet "maybe.ss" ("jaymccarthy" "mmss.plt" 1))           
           (planet "hmac-sha1.ss" ("jaymccarthy" "hmac-sha1.plt" 1)))
  (require "url-param.ss")
  (provide (all-defined))
      
  (define (bind-secure-url-parameter original-cell id original-read original-write gen-secret-key)
    (let ([secure-hash
           (lambda (data)
             (HMAC-SHA1 (gen-secret-key) (string->bytes/utf-8 (string-append data (request-client-ip (current-url-param-request))))))])
      (bind-url-parameter original-cell id
                          (lambda (s)
                            (match (read/string s)
                              [(list data hash)
                               (let ([rehash (secure-hash data)])
                                 (if (bytes=? hash rehash)
                                     (make-just (original-read data))
                                     (make-nothing)))]
                              [#f
                               (make-nothing)]))
                          (lambda (a)
                            (if (just? a)
                                (let ([data (original-write (just-value a))])
                                  (write/string (list data (secure-hash data))))
                                (write/string #f)))))))