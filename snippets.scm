#|
<html>
<head><title>cairo: snippets by Gauche/c-wrapper</title></head>
<body>
<h1>Cairo samples by Gauche/c-wrapper</h1>

<p>This page is created by using
<a href="http://practical-scheme.net/gauche/index.html">Gauche</a> and
<a href="http://homepage.mac.com/naoki.koguro/prog/c-wrapper/index.html">
c-wrapper</a>.</p>

<p>Most of the code snippets shown here are direct translations of
the <a href="http://cairographics.org/samples/">original</a> snippets
in C.</p>

<p>Complete source code to reproduce this page as well as other
examples can be found in this <a href="../cairo.tgz">tgz</a>.
|#
(define-snippet arc
  (lambda (cr)
    (let ((xc 0.5)
          (yc 0.5)
          (radius 0.4)
          (angle1  (* 45.0  pi/180))
          (angle2  (* 180.0 pi/180)))
      
      (cairo_arc cr xc yc radius angle1 angle2)
      (cairo_stroke cr)
      
      ;; draw helping lines
      (cairo_set_source_rgba cr 1 0.2 0.2 0.6)
      (cairo_arc cr xc yc 0.05 0 (* 2 pi))
      (cairo_fill cr)
      (cairo_set_line_width cr 0.03)
      (cairo_arc cr xc yc radius angle1 angle1)
      (cairo_line_to cr xc yc)
      (cairo_arc cr xc yc radius angle2 angle2)
      (cairo_line_to cr xc yc)
      (cairo_stroke cr)))
  end-snippet
  )

(define-snippet clip
  (lambda (cr)
    (cairo_arc cr 0.5 0.5 0.3 0 (* 2 pi))
    (cairo_clip cr)

    (cairo_new_path cr)   ;  current path is not
                          ;  consumed by cairo_clip()
    (cairo_rectangle cr 0 0 1 1)
    (cairo_fill cr)
    (cairo_set_source_rgb cr 0 1 0)
    (cairo_move_to cr 0 0)
    (cairo_line_to cr 1 1)
    (cairo_move_to cr 1 0)
    (cairo_line_to cr 0 1)
    (cairo_stroke cr))
  end-snippet
  )

(define-snippet clip_image
  (lambda (cr)
    (cairo_arc cr 0.5 0.5 0.3 0 (* 2 pi))
    (cairo_clip cr)
    (cairo_new_path cr)   ;  path not consumed by clip()

    (let* ((image (cairo_image_surface_create_from_png
                                    "data/romedalen.png"))
           (w     (cairo_image_surface_get_width image))
           (h     (cairo_image_surface_get_height image)))

      (cairo_scale cr (/ 1.0 w) (/ 1.0 h))
      (cairo_set_source_surface cr image 0 0)
      (cairo_paint cr)

      (cairo_surface_destroy image)))
  end-snippet
  )

(define-snippet curve_rectangle
  (lambda (cr)
    (let ((x  0.1) (y  0.5)
          (x1 0.4) (y1 0.9)
          (x2 0.6) (y2 0.1)
          (x3 0.9) (y3 0.5))

      (cairo_move_to cr x y)
      (cairo_curve_to cr x1 y1 x2 y2 x3 y3)
      (cairo_stroke cr)

      (cairo_set_source_rgba cr 1 0.2 0.2 0.6)
      (cairo_set_line_width cr 0.03)
      (cairo_move_to cr  x  y) (cairo_line_to cr x1 y1)
      (cairo_move_to cr x2 y2) (cairo_line_to cr x3 y3)
      (cairo_stroke cr)))
  end-snippet)

(define-snippet dash
  (lambda (cr)
    ;; you can also use f64vector
    (let* ((dashes (vector 0.20   ;  ink 
                           0.04   ;  skip
                           0.04   ;  ink
                           0.04)) ;  skip
           (ndash (vector-length dashes))
           (offset -0.2))

      (cairo_set_dash cr dashes ndash offset)

      (cairo_move_to cr 0.5 0.1)
      (cairo_line_to cr 0.9 0.9)
      (cairo_rel_line_to cr -0.4 0.0)
      (cairo_curve_to cr 0.2 0.9 0.2 0.5 0.5 0.5)

      (cairo_stroke cr)))
  end-snippet)

(define-snippet dash_list
  (lambda (cr)
    ;; You can pass list where array is expected.
    (let ((dashes '(0.20 0.04 0.04 0.04)) 
          (offset -0.2))
      (cairo_set_dash cr dashes (length dashes) offset)
      (cairo_move_to cr 0.5 0.1)
      (cairo_line_to cr 0.9 0.9)
      (cairo_rel_line_to cr -0.4 0.0)
      (cairo_curve_to cr 0.2 0.9 0.2 0.5 0.5 0.5)
      (cairo_stroke cr)))
  end-snippet)

(define-snippet gradient
  (lambda (cr)
    (let ((pat (cairo_pattern_create_linear 0.0 0.0 0.0 1.0)))
      (cairo_pattern_add_color_stop_rgba pat 1 0 0 0 1)
      (cairo_pattern_add_color_stop_rgba pat 0 1 1 1 1)
      (cairo_rectangle cr 0 0 1 1)
      (cairo_set_source cr pat)
      (cairo_fill cr)
      (cairo_pattern_destroy pat))

    (let ((pat (cairo_pattern_create_radial 0.45 0.4 0.1 0.4 0.4 0.5)))
      (cairo_pattern_add_color_stop_rgba pat 0 1 1 1 1)
      (cairo_pattern_add_color_stop_rgba pat 1 0 0 0 1)
      (cairo_set_source cr pat)
      (cairo_arc cr 0.5 0.5 0.3 0 (* 2 pi))
      (cairo_fill cr)
      (cairo_pattern_destroy pat)))
  end-snippet)

(define-snippet imagepattern
  (lambda (cr)
    (let* ((image (cairo_image_surface_create_from_png "data/romedalen.png"))
           (w     (cairo_image_surface_get_width image))
           (h     (cairo_image_surface_get_height image))
           (matrix  (make <cairo_matrix_t>))
           (pattern (cairo_pattern_create_for_surface image)))

      (cairo_pattern_set_extend pattern CAIRO_EXTEND_REPEAT)
      (cairo_translate cr 0.5 0.5)
      (cairo_rotate cr pi/4)
      (cairo_scale cr (/ 1 (sqrt 2)) (/ 1 (sqrt 2)))
      (cairo_translate cr -0.5 -0.5)

      (cairo-matrix-init-scale matrix (* w 5.0) (* h 5.0))
      (cairo-pattern-set-matrix pattern matrix)

      (cairo_set_source cr pattern)

      (cairo_rectangle cr 0 0 1.0 1.0)
      (cairo_fill cr)

      (cairo_pattern_destroy pattern)
      (cairo_surface_destroy image)))
  end-snippet)

(define-snippet imagepattern_gauche
  (lambda (cr)
    (let* ((image (cairo_image_surface_create_from_png 
                   "data/gauche_logo_square.png"))
           (w     (cairo_image_surface_get_width image))
           (h     (cairo_image_surface_get_height image))
           (matrix  (make <cairo_matrix_t>))
           (pattern (cairo_pattern_create_for_surface image)))

      (cairo_pattern_set_extend pattern CAIRO_EXTEND_REPEAT)
      (cairo_translate cr 0.5 0.5)
      (cairo_rotate cr pi/4)
      (cairo_scale cr (/ 1 (sqrt 2)) (/ 1 (sqrt 2)))
      (cairo_translate cr -0.5 -0.5)

      (cairo-matrix-init-scale matrix (* w 2.0) (* h 2.0))
      (cairo-pattern-set-matrix pattern matrix)

      (cairo_set_source cr pattern)

      (cairo_rectangle cr 0 0 1.0 1.0)
      (cairo_fill cr)

      (cairo_pattern_destroy pattern)
      (cairo_surface_destroy image)))
  end-snippet)

(define-snippet text
  (lambda (cr)
    (cairo_select_font_face cr "Sans" 
                            CAIRO_FONT_SLANT_NORMAL
                            CAIRO_FONT_WEIGHT_BOLD)
    (cairo_set_font_size cr 0.35)

    (cairo_move_to cr 0.04 0.53)
    (cairo_show_text cr "Hello")

    (cairo_move_to cr 0.27 0.65)
    (cairo_text_path cr "void")
    (cairo_set_source_rgb cr 0.5 0.5 1)
    (cairo_fill_preserve cr)
    (cairo_set_source_rgb cr 0 0 0)
    (cairo_set_line_width cr 0.01)
    (cairo_stroke cr)

    ;; draw helping lines
    (cairo_set_source_rgba cr 1 0.2 0.2 0.6)
    (cairo_arc cr 0.04 0.53 0.02 0 (*  2 pi))
    (cairo_close_path cr)
    (cairo_arc cr 0.27 0.65 0.02 0 (* 2 pi))
    (cairo_fill cr))
  end-snippet)

(define-snippet text_extents
  (lambda (cr)
    (let ((extents (make <cairo_text_extents_t>))
          (utf8    "cairo")
          (x       0.1)
          (y       0.6))

      (cairo_select_font_face cr "Sans"
                              CAIRO_FONT_SLANT_NORMAL
                              CAIRO_FONT_WEIGHT_NORMAL)
      (cairo_set_font_size cr 0.4)
      (cairo-text-extents cr utf8 extents)

      (cairo_move_to cr x y)
      (cairo_show_text cr utf8)

      ;;  draw helping lines
      (cairo_set_source_rgba cr 1 0.2 0.2 0.6)
      (cairo_arc cr x y 0.05 0 (* 2 pi))
      (cairo_fill cr)
      (cairo_move_to cr x y)
      (cairo_rel_line_to cr 0 (- (ref extents 'height)))
      (cairo_rel_line_to cr (ref extents 'width) 0)
      (cairo_rel_line_to cr (ref extents 'x_bearing)
                         (- (ref extents 'y_bearing)))
      (cairo_stroke cr)))
  end-snippet)

#;(define-snippet text_extents_kanji
  (lambda (cr)
    (let ((extents (make <cairo_text_extents_t>))
          (utf8    "漢字")
          (x       0.1)
          (y       0.6))

      (cairo_select_font_face cr "Osaka"
                              CAIRO_FONT_SLANT_NORMAL
                              CAIRO_FONT_WEIGHT_NORMAL)
      (cairo_set_font_size cr 0.4)
      (cairo-text-extents cr utf8 extents)

      (cairo_move_to cr x y)
      (cairo_show_text cr utf8)

      ;;  draw helping lines
      (cairo_set_source_rgba cr 1 0.2 0.2 0.6)
      (cairo_arc cr x y 0.05 0 (* 2 pi))
      (cairo_fill cr)
      (cairo_move_to cr x y)
      (cairo_rel_line_to cr 0 (- (ref extents 'height)))
      (cairo_rel_line_to cr (ref extents 'width) 0)
      (cairo_rel_line_to cr (ref extents 'x_bearing)
                         (- (ref extents 'y_bearing)))
      (cairo_stroke cr)))
  end-snippet)

#;(define-snippet dragon
  (lambda (cr)
    (define u 1.0)
    (define r (/ 1.0 (sqrt 2)))
    (define (rec x) 
      (cairo_save cr) (x) (cairo_restore cr))

    (define (dragon n)
      (cond ((> n 0) 
             (rec (lambda ()
                    (cairo_scale cr r r)
                    (cairo_rotate cr pi/4)
                    (dragon (- n 1))))
             (rec (lambda ()
                    (cairo_translate cr u 0)
                    (cairo_scale cr r r)
                    (cairo_rotate cr pi/4)
                    (cairo_translate cr (- u) 0)
                    (dragon (- n 1)))))
            (else
             (cairo_rectangle cr 0.2 0.2 0.8 0.8)
             (cairo_fill cr))))
    (cairo_scale cr 0.5 0.5)
    (cairo_translate cr 0.5 0.7)
    (dragon 9))
  end-snippet)

(define-snippet levy
  (lambda (cr)
    (define u 1.0)
    (define r (/ 1.0 (sqrt 2)))
    (define (rec x) 
      (cairo_save cr) (x) (cairo_restore cr))

    (define (levy n)
      (cond ((> n 0) 
             (rec (lambda ()
                    (cairo_scale cr r r)
                    (cairo_rotate cr pi/4)
                    (cairo_scale cr 1.0 -1.0)
                    (levy (- n 1))))
             (rec (lambda ()
                    (cairo_translate cr u 0)
                    (cairo_scale cr r r)
                    (cairo_rotate cr pi/4)
                    (cairo_translate cr (- u) 0)
                    (levy (- n 1)))))
            (else
             (cairo_move_to cr 0 0)
             (cairo_line_to cr 1 1)
             (cairo_stroke cr))))
    (cairo_scale cr 0.7 0.7)
    (cairo_translate cr 0.1 0.5)
    (cairo_set_line_width cr 0.2)
    (levy 8))
  end-snippet)
#|
<h4>END OF SAMPLES</h4>
</body>
</html>
|#
;;; EOF
