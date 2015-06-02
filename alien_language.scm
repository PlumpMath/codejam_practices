;;; https://code.google.com/codejam/contest/90101/dashboard#s=p0
(use-modules (ice-9 rdelim)
             (ice-9 regex))


(define (string-gsub str from to)
  (regexp-substitute/global
   #f (string-append "[" from "]+")  str 'pre to 'post))

(define (solve file-in file-out)
  (let* ((in (open-input-file file-in))
         (out (open-output-file file-out))
         (numbers (map string->number (string-split (read-line in) #\space)))
         (L (car numbers))
         (D (car (cdr numbers)))
         (N (car (cddr numbers)))
         (dictionary (let loop ((i 1) (result '()))
                       (if (> i D) result
                           (loop (+ i 1) (cons (read-line in) result)))))
         (line (lambda () (string-gsub
                      (string-gsub
                       (read-line in) "(" "[") ")" "]"))))
    (let loop ((i 1))
      (if (<= i N)
          (let* ((l (line)))
            (display (format #f "Case #~a: ~a\n"
                             i
                             (length    ;FIXME: it is very slow
                              (filter (lambda (x)
                                        (string-match l x))
                                      dictionary))) out)
            (loop (+ i 1)))))))


(unless (> (length (command-line)) 1) (exit 1))

(let ((path (cadr (command-line))))
  (if (string-match ".in$" path)
      (solve path (regexp-substitute/global #f ".in$" path 'pre ".out" 'post))))
