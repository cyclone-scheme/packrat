;; Copyright (c) 2004, 2005 Tony Garnock-Jones <tonyg@kcbbs.gen.nz>
;; Copyright (c) 2005 LShift Ltd. <query@lshift.net>
;; Copyright © 2019 Göran Weinholt <goran@weinholt.se>
;; SPDX-License-Identifier: MIT

(import (scheme base)
	(cyclone test)
	(cyclone packrat))

(define (run-tests)
  (test-begin "expr")

  (define (generator tokens)
    (let ((stream tokens))
      (lambda ()
	(if (null? stream)
	    (values #f #f)
	    (let ((base-token (car stream)))
	      (set! stream (cdr stream))
	      (values #f base-token))))))

  (define calc (packrat-parser expr
			       (expr ((a <- mulexp '+ b <- mulexp)
				      (+ a b))
				     ((a <- mulexp) a))
			       (mulexp ((a <- simple '* b <- simple)
					(* a b))
				       ((a <- simple) a))
			       (simple ((a <- 'num) a)
				       (('oparen a <- expr 'cparen) a))))

  (define g (generator
	     '((oparen) (num . 1) (+) (num . 2) (cparen) (*) (num . 3))))

  (define r (calc (base-generator->results g)))

  (test-assert (parse-result-successful? r))
  (test 9 (parse-result-semantic-value r))
  
  (test-end))

(run-tests)
