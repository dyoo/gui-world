(module calm-evt mzscheme
  (require (lib "contract.ss"))
  
  ;; calm-evt: acts as an event that fires off after a delay, if the event
  ;; doesn't do anything else by the time the delay times out.
  
  
  (define-values (struct:calm-evt make-calm-evt calm-evt? calm-evt-ref calm-evt-set!)
    (make-struct-type 'calm-evt #f 4 0 #f
                      (list
                       (cons prop:evt
                             (lambda (a-calm-evt)
                               (thread-resume (calm-evt-helper-thread a-calm-evt)
                                              (current-thread))
                               (calm-evt-out-ch a-calm-evt))))
                      (current-inspector)
                      #f))
  
  
  (define calm-evt-wrapped-evt (make-struct-field-accessor calm-evt-ref 0))
  (define calm-evt-delay-in-milliseconds (make-struct-field-accessor calm-evt-ref 1))
  (define calm-evt-out-ch (make-struct-field-accessor calm-evt-ref 2))
  (define calm-evt-helper-thread (make-struct-field-accessor calm-evt-ref 3))
  
  
  ;; The default delay is a second.
  (define default-delay 1000)
  
  
  ;; -make-calm-evt: evt -> calm-evt
  ;; -make-calm-evt: evt natural-number -> calm-evt
  ;; Creates a calm-evt.
  (define -make-calm-evt
    (case-lambda
      [(wrapped-evt)
       (-make-calm-evt wrapped-evt default-delay)]
      [(wrapped-evt a-delay)
       (letrec ([a-calm-evt
                 (make-calm-evt wrapped-evt
                                a-delay
                                (make-channel)
                                (thread
                                 (lambda ()
                                   (helper-loop a-calm-evt))))])
         a-calm-evt)]))
  
  
  ;; helper-loop: calm-evt -> void
  ;; The helper loop waits for events to come off the wrapped event.
  ;; Once it does so, it squirrels away the value, and makes it ready for
  ;; delivery once timeout fires.
  ;; 
  ;; The finite state machine looks like:
  #|

                                                           -----------+ pick up value
               pick up value                               V          |
   --> start --------------> waiting-for-timeout-or-another-value ----+
          ^                                 |                  ^
          |                         timeout |                  |
          |                                 V                  |
          +---------------- allowing-value-to-be-delivered ----+ pick up value
            deliver value                   
  |#
  (define (helper-loop a-calm-evt)
    (let loop ([wait-for-timeout? #f]
               [value-box #f])
      
      (define (start?)
        (and (not wait-for-timeout?)
             (not value-box)))
      (define (goto-start)
        (loop #f #f))
      
      (define (waiting-for-timeout-or-another-value?)
        (and wait-for-timeout?
             value-box))
      
      (define (goto-waiting-for-timeout-or-another-value v)
        (loop #t (box v)))
      
      (define (allowing-value-to-be-delivered?)
        (and (not wait-for-timeout?)
             value-box))
      
      (define (goto-allowing-value-to-be-delivered)
        (loop #f value-box))
            
      (let* ([handle:pick-up-value
              (handle-evt (calm-evt-wrapped-evt a-calm-evt)
                          (lambda (v)
                            (goto-waiting-for-timeout-or-another-value v)))]
             
             [handle:timeout
              (handle-evt (alarm-evt (+ (current-inexact-milliseconds)
                                        (calm-evt-delay-in-milliseconds a-calm-evt)))
                          (lambda (_)
                            (goto-allowing-value-to-be-delivered)))]

             [make-handle:deliver-value
              (lambda ()
                (handle-evt (channel-put-evt (calm-evt-out-ch a-calm-evt) (unbox value-box))
                            (lambda (_)
                              (goto-start))))])
        
        (cond [(start?)
               (sync handle:pick-up-value)]
              [(waiting-for-timeout-or-another-value?)
               (sync handle:pick-up-value
                     handle:timeout)]
              [(allowing-value-to-be-delivered?)
               (sync handle:pick-up-value
                     (make-handle:deliver-value))]))))
  
  
  
  
  (provide/contract
   [rename -make-calm-evt
           make-calm-evt
           (case-> (evt? . -> . calm-evt?)
                   (evt? natural-number/c . -> . calm-evt?))]
   [calm-evt? (any/c . -> . boolean?)]))