;;;
;;;
;;;
(use cairo)
(use math.const)
(use file.util)
(use text.html-lite)
(use srfi-13)

(define-constant SNIPPET_H 256)
(define-constant SNIPPET_W 256)

;;;
;;;
;;;
(define (warnf . x)
  (apply (cut format (current-error-port) <...>) x))

;;;
;;;
;;;
(define-syntax define-snippet
  (syntax-rules (end-snippet)
    ((_ sym x end-snippet)
     (push! *snippets* (cons 'sym x)))))

(define *snippets* '())
(define (snippet->name s)     (car s))
(define (snippet->func s)     (cdr s))
(define (snippet->png-name s)  #`"snippet-,(snippet->name s).png")

(define (get-snippet n) 
  (let ((s (assq n *snippets*)))
    (if s s (error "Something went wrong"))))

(define (snippet-create-png s w h)
  (let* ((sf (cairo_image_surface_create CAIRO_FORMAT_ARGB32 w h))
         (cr (cairo_create sf))
         (fn (snippet->png-name s)))
    (warnf "creating ~a~%" fn)
    (cairo_save cr)
    (cairo_scale cr w h)
    (cairo_set_line_width cr 0.04)
    ((snippet->func s) cr)
    (cairo_restore cr)
    (cairo_surface_write_to_png sf fn)
    (cairo_destroy cr)
    (cairo_surface_destroy sf)
    fn))

;;;
;;;
;;;
(define (snippets->html)

  (define (comment? l) (string-prefix? "#|" l))
  (define (do-comment l)
    (cond ((eof-object? l)
           (error "file ends within comment"))
          ((string-prefix? "|#" l) #t)
          (else
           (print l)
           (do-comment (read-line)))))
          
  (define (snippet? l) (string-prefix? "(define-snippet " l))
  (define (get-snippet-name l) 
    (get-snippet (string->symbol 
                  (list-ref (string-tokenize l) 
                            1))))
  (define (do-snippet s)
    (let ((png (snippet-create-png s SNIPPET_W SNIPPET_H)))
      (print #`"<h2>,(snippet->name s)</h2>")
      (print "<table><tr>")
      (print #`"<td><img src=\",png\">")
      (begin
        (display "<td><pre><code>")
        (let loop ((l (read-line)))
          (cond ((eof-object? l)
                 (error "file ends within define-snippet"))
                ((string-contains l "end-snippet") #t)
                (else
                 (print (html-escape-string l))
                 (loop (read-line)))))
        (print "</code></pre>"))
      (print "</tr></table>")))

  (let loop ((l (read-line)))
    (cond ((eof-object? l) #t)
          ((comment? l) 
           (do-comment (read-line))
           (loop (read-line)))
          ((snippet? l)
           (do-snippet (get-snippet-name l))
           (loop (read-line)))
          (else 
           (loop (read-line))))))

;;;
;;;
;;;
(define (doit-test f)
  (load f :paths '("."))
  (for-each (lambda (s)
              (snippet-create-png s SNIPPET_W SNIPPET_H))
            *snippets*))

(define (doit f)
  (load f :paths '("."))
  (with-input-from-file f
    (lambda ()
      (with-output-to-file 
          #`",(path-sans-extension f).html" 
        snippets->html))))

(define (main args)
  (if (not (null? (cdr args)))
      (for-each doit (cdr args))
      (errorf "snippets-html snippets.scm ...~%"))
  0)

;;; EOF
