#lang racket

(require (only-in "chat-server.rkt" HOST PORT))

(module+ main

  (define-values (in-p out-p)
    (tcp-connect HOST PORT))

  (file-stream-buffer-mode out-p 'line)

  (define (shutdown!)
    (close-input-port in-p)
    (close-output-port out-p))

  ;; read input from the user and send to the server
  (void
   (thread
    (thunk
     (with-handlers ([(const #t) (lambda (e) (shutdown!) (exit))])
       (let loop ()
         (define in (read-line))
         (cond
           [(eof-object? in)
            (shutdown!)]
           [else
            (displayln in out-p)
            (loop)]))))))

  ;; read input from the server and display to the user
  (thread-wait
   (thread
    (thunk
     (with-handlers ([(const #t) (lambda (e) (shutdown!))])
       (let loop ()
         (define in (read-line in-p))
         (cond
           [(eof-object? in)
            (displayln "Connection terminated by server")
            (shutdown!)]
           [else
            (displayln in)
            (loop)])))))))