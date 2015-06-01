;;; See https://code.google.com/codejam/contest/32016/dashboard#s=p0
(use-modules (ice-9 rdelim)
             (ice-9 regex))

(define (solve file-in file-out)
  (let* ((in (open-input-file file-in))
         (out (open-output-file file-out))
         (x (string->number (read-line in)))
         (read-list (lambda () (map string->number (string-split (read-line in) #\space)))))
    (let loop ((i 1))
      (if (<= i x)
          (begin
            (read-line in)
            (let ((l1 (sort (read-list) <))
                  (l2 (sort (read-list) >)))
              (display (format #f "Case #~a: ~a\n" i (apply + (map * l1 l2))) out))
            (loop (+ i 1)))))))

(unless (> (length (command-line)) 1) (exit 1))

(let ((path (cadr (command-line))))
  (if (string-match ".in$" path)
      (solve path (regexp-substitute/global #f ".in$" path 'pre ".out" 'post))))
