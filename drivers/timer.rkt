#lang racket

(provide sync-timer%)

(require "../monitor.rkt")

(define sync-timer%
  (class monitor%
    (inherit notify-all
             wait)
    (super-new)
    (init period-ms)
    (define period-s (/ period-ms 1000))
    
    (define/synchronized (awaken-sleepers)
      (notify-all))
    
    (define/synchronized (wait-for-tick)
      (wait))
    (define worker-thread
      (thread
       (thunk
        (let loop ()
          (sleep period-s)
          (awaken-sleepers)
          (loop)))))))