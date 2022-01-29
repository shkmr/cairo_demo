;;;
;;;  cairo-demo/PS/basket.c
;;;
(use cairo)

(define-constant X_INCHES  8)
(define-constant Y_INCHES  3)

(define (main args)
  (let* ((surface (cairo_ps_surface_create "basket.ps"
                                           (* X_INCHES 72.0)
                                           (* Y_INCHES 72.0)))
         (cr (cairo_create surface)))
    (cairo_surface_destroy surface)
    (draw cr)
    (cairo_show_page cr)
    (cairo_destroy cr)
    0))

(define (draw cr)
  (cairo_move_to cr 50  50)
  (cairo_line_to cr 550 50)
  (cairo_curve_to cr 450 240 150 240 50 50)
  (cairo_close_path cr)
  (cairo_save cr)
  (cairo_set_source_rgb cr 0.8 0.1 0.1)
  (cairo_fill_preserve cr)
  (cairo_restore cr)
  (cairo_set_line_width cr 6)
  (cairo_set_source_rgb cr  0.0 0.0 0.0)
  (cairo_stroke cr))

;;; EOF
