;;;
;;; cairo-demo/png/caps_joins.c
;;;
(use gauche.process)
(use cairo)

(define-constant WIDTH  600)
(define-constant HEIGHT 600)
(define-constant STRIDE (* WIDTH 4))

(define image (cairo/g-make-image STRIDE HEIGHT 1))

(define (main args)
  (let* ((surface (cairo_image_surface_create_for_data image
                                                       CAIRO_FORMAT_ARGB32 
                                                       WIDTH HEIGHT STRIDE))
         (cr (cairo_create surface)))

    (cairo_rectangle cr 0 0 WIDTH HEIGHT)
    (cairo_set_source_rgb cr 1 1 1)
    (cairo_fill cr)
    
    (draw-caps-joins cr WIDTH HEIGHT)

    (cairo_surface_write_to_png surface "caps_joins.png")
    (cairo_destroy cr)
    (cairo_surface_destroy surface)
    0))

(define (stroke-v-twice cr width height)
  (cairo_move_to cr 0 0)
  (cairo_rel_line_to cr (/ width 2) (/ height 2))
  (cairo_rel_line_to cr (/ width 2) (- (/ height 2)))

  (cairo_save cr)
  (cairo_stroke cr)
  (cairo_restore cr)
  
  (cairo_save cr)
  (begin
    (cairo_set_line_width cr 2.0)
    (cairo_set_line_cap cr CAIRO_LINE_CAP_BUTT)
    (cairo_set_source_rgb cr 1 1 1)
    (cairo_stroke cr))
  (cairo_restore cr)

  (cairo_new_path cr)
  )

(define (draw-caps-joins cr width height)
  (let ((line-width (logand (round->exact (/ height 16))
                            (lognot 1))))
    (cairo_set_line_width cr line-width)
    (cairo_set_source_rgb cr 0 0 0)

    (cairo_translate cr line-width line-width)

    (dec! width (* 2 line-width))

    (cairo_set_line_join cr CAIRO_LINE_JOIN_BEVEL)
    (cairo_set_line_cap cr CAIRO_LINE_CAP_BUTT)
    (stroke-v-twice cr width height)

    (cairo_translate cr 0 (- (/ height 4) line-width))
    (cairo_set_line_join cr CAIRO_LINE_JOIN_MITER)
    (cairo_set_line_cap cr CAIRO_LINE_CAP_SQUARE)
    (stroke-v-twice cr width height)

    (cairo_translate cr 0 (- (/ height 4) line-width))
    (cairo_set_line_join cr CAIRO_LINE_JOIN_ROUND)
    (cairo_set_line_cap cr CAIRO_LINE_CAP_ROUND)
    (stroke-v-twice cr width height)
    ))