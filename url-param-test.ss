(module url-param-test mzscheme
  (require (lib "servlet.ss" "web-server"))
  (require "url-param.ss")
  (provide interface-version timeout start
           instance-expiration-handler)
    
  (define interface-version 'v2-transitional)
  (define timeout (* 60 15))  
  (define instance-expiration-handler 
    (make-instance-expiration-handler
     (lambda (failed-request)
       `(html (body "Failed to detect restorable.")))))
  
  (define counter (make-web-cell 0))
  (bind-url-parameter counter 'i string->number number->string)
  
  (define (interface message)
    (redirect/get)
    (send/suspend/dispatch
     (lambda (embed/url)
       `(html (head (title ,message))
              (body (h2 ,message)
                    (p ,(number->string (web-cell-ref counter)))
                    (ul (li (a ([href ,(embed/url
                                        (lambda _
                                          (web-cell-shadow counter (add1 (web-cell-ref counter)))
                                          (interface "Incremented.")))])
                               "add1"))
                        (li (a ([href ,(embed/url
                                        (lambda _
                                          (web-cell-shadow counter (sub1 (web-cell-ref counter)))
                                          (interface "Decremented.")))])
                               "sub1"))
                        (li (a ([href ,(embed/url
                                        (lambda _
                                          (clear-continuation-table!)
                                          (interface "Forgotton old continuations")))])
                               "forget"))))))))
  
  (define start
    (make-start-reconstruction-handler
     (lambda ()
       (interface "Countinued."))
     (lambda (initial-request)
       (interface "New.")))))