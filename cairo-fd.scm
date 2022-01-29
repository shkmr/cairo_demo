;;;
;;;  fd.scm : fdface fdhand fdlogo
;;;
(define-module cairo-fd
  (use math.const)
  (use cairo)
  (export fdface-draw
          fdhand-draw-now
          fdlogo-draw))
(select-module cairo-fd)
;;;
;;; cairo-demo/quatz/fdface.c
;;;
(define-constant pi*2 (* pi 2))
(define-constant pi/6 (/ pi 6))

(define (fdface-draw cr width height)

  (define (draw-fancy-tick cr radius)
    (cairo_save cr)
    (cairo_arc cr 0 0 radius 0 pi*2)
    (cairo_set_source_rgb cr 0.231 0.502 0.682)
    (cairo_fill cr)
    (cairo_set_source_rgb cr 0.73 0.73 0.73)
    (cairo_set_line_width cr (/ (* 2 radius) 3))
    (cairo_arc cr 0 0 (* radius 2) 0 pi*2)
    (cairo_stroke cr)
    (cairo_restore cr))

  (define (draw-plain-tick cr radius)
    (cairo_save cr)
    (cairo_arc cr 0 0 radius 0 pi*2)
    (cairo_set_source_rgb cr 0.73 0.73 0.73)
    (cairo_fill cr)
    (cairo_restore cr))

  (cairo_save cr)
  (begin
    (cairo_scale cr width height)
    (cairo_save cr)
    (begin
      (cairo_translate cr 0.15 0.15)
      (cairo_scale cr 0.7 0.7)
      (fdlogo-draw cr 1 1))
    (cairo_restore cr)
    (cairo_translate cr 0.5 0.5)
    (cairo_scale cr 0.93 0.93)
    (do ((minute  0 (+ minute 1))
         (degrees 0 (+ degrees 6)))
        ((= minute 60) #t)
      (define (deg->rad deg) (* pi/180 deg))
      (cairo_save cr)
      (cairo_rotate cr (deg->rad degrees))
      (cairo_translate cr 0 0.5)
      (cond ((zero? (modulo minute 15))
             (draw-fancy-tick cr 0.015))
            ((zero? (modulo minute 5))
             (draw-fancy-tick cr 0.01))
            (else
             (draw-plain-tick cr 0.01)))
      (cairo_restore cr)))
  (cairo_restore cr))

;;;
;;; cairo-demo/quartz/fdhand.c
;;;
(define (draw-hour cr width length)
  (let ((r (/ width 2)))
    (cairo_move_to cr  length             (- r))
    (cairo_arc     cr  length             0  r      (- pi/2) pi/2)
    (cairo_line_to cr  (* width (sqrt 2)) r)
    (cairo_arc     cr  0                  0  (* r 2) pi/6 (- pi/6))
    (cairo_close_path cr)))

(define (draw-minute cr width length)
  (let ((r (/ width 2)))
    (cairo_move_to cr  length             (- r))
    (cairo_arc     cr  length             0          r (- pi/2) pi/2)
    (cairo_line_to cr  0                  r)
    (cairo_line_to cr  0                  (- r))
    (cairo_close_path cr)))

(define (draw-second cr width length)
  (let ((r         (/ width 2))
        (thick     width)
        (back      (- (/ length 3)))
        (back-thin (- (/ length 10))))
    (cairo_move_to cr  length           (- r))
    (cairo_arc     cr  length           0          r (- pi/2) pi/2)
    (cairo_line_to cr back-thin        r)
    (cairo_line_to cr back-thin        thick)
    (cairo_line_to cr back             thick)
    (cairo_line_to cr back             (- thick))
    (cairo_line_to cr back-thin        (- thick))
    (cairo_line_to cr back-thin        (- r))
    (cairo_close_path cr)))

(define (draw-hand cr noon_deg width length alt draw)
  (let ((m (make <cairo_matrix_t>)))
    (cairo-get-matrix cr m)
    (begin
      (cairo_translate cr (/ alt 2) alt)
      (cairo_rotate cr (- (* noon_deg pi/180) pi/2))
      (draw cr width length))
    (cairo-set-matrix cr m)))

(define-constant HOUR_WIDTH      0.03)
(define-constant HOUR_LENGTH     0.25)
(define-constant HOUR_ALT        0.010)
(define-constant MINUTE_WIDTH    0.015)
(define-constant MINUTE_LENGTH   0.39)
(define-constant MINUTE_ALT      0.020)
(define-constant SECOND_WIDTH    0.0075)
(define-constant SECOND_LENGTH   0.32)
(define-constant SECOND_ALT      0.026)

(define (draw-time cr width height tm seconds?)
  (let* ((second-angle (* (slot-ref tm 'sec) 6.0))
         (minute-angle (+ (* (slot-ref tm 'min) 6.0)
                          (/ second-angle 60.0)))
         (hour-angle   (+ (* (slot-ref tm 'hour) 30.0)
                          (/ minute-angle 12.0))))

    (cairo_save cr)
    (begin
      (cairo_scale cr width height)
      (cairo_translate cr 0.5 0.5)
      (draw-hand cr hour-angle   HOUR_WIDTH HOUR_LENGTH HOUR_ALT draw-hour)
      (draw-hand cr minute-angle MINUTE_WIDTH MINUTE_LENGTH MINUTE_ALT draw-minute)
      (if seconds?
          (draw-hand cr second-angle SECOND_WIDTH SECOND_LENGTH SECOND_ALT draw-second))
      (cairo_set_source_rgba cr 0 0 0 0.3)
      (cairo_fill cr)
      (cairo_set_source_rgba cr 0 0 0 1)
      (draw-hand cr hour-angle HOUR_WIDTH HOUR_LENGTH 0.0 draw-hour)
      (cairo_fill cr)
      (draw-hand cr minute-angle MINUTE_WIDTH MINUTE_LENGTH 0.0 draw-minute)
      (cairo_fill cr)
      (if seconds?
          (begin
            (draw-hand cr second-angle SECOND_WIDTH SECOND_LENGTH 0.0 draw-second)
            (cairo_fill cr))))
    (cairo_restore cr)))

(define (fdhand-draw-now cr width height seconds?)
  (receive (sec usec) (sys-gettimeofday)
    (draw-time cr width height (sys-localtime sec) seconds?)))

;;;
;;; cairo-demo/quartz/fdlogo.c
;;;    
(define (draw-boundary cr)
  (cairo_move_to   cr 63.000 36.000)
  (cairo_curve_to  cr 63.000 43.000 
                      58.000 47.000
                      51.000 47.000)
  (cairo_line_to   cr 13.000 47.000)
  (cairo_curve_to  cr  6.000 47.000
                       1.000 43.000
                       1.000 36.000)
  (cairo_line_to   cr  1.000 12.000)
  (cairo_curve_to  cr  1.000  5.000
                       6.000  1.000
                      13.000  1.000)
  (cairo_line_to   cr 51.000  1.000)
  (cairo_curve_to  cr 58.000  1.000
                      63.000  5.000
                      63.000 12.000)
  (cairo_close_path cr))

(define (draw-outline cr)
  (cairo_set_source_rgb cr 0.73 0.73 0.73)
  (cairo_set_line_width cr 2)
  (draw-boundary cr)
  (cairo_stroke cr))

(define (draw-background cr)
  (cairo_save cr)
  (cairo_set_source_rgb cr 0.231 0.502 0.682)
  (cairo_translate cr 3.5 3.5)
  (cairo_scale cr 0.887 0.848)
  (draw-boundary cr)
  (cairo_fill cr)
  (cairo_restore cr))


(define (draw-window cr)

  (cairo_move_to  cr -6.00 -7.125)

  (cairo_line_to  cr  6.00 -7.125)

  (cairo_curve_to cr  8.00 -7.125
                      9.00 -6.125
                      9.00 -4.125)

  (cairo_line_to  cr  9.00  4.125)

  (cairo_curve_to cr  9.00  6.125
                      8.00  7.125
                      6.00  7.125)

  (cairo_line_to  cr -6.00  7.125)

  (cairo_curve_to cr -8.00  7.125
                     -9.00  6.125
                     -9.00  4.125)

  (cairo_line_to  cr -9.00 -4.125)

  (cairo_curve_to cr -9.00 -6.125
                     -8.00 -7.125
                     -6.00 -7.125)

  (cairo_close_path cr))

(define (draw-window-at cr x y scale)
  (cairo_save cr)
  (begin
    (cairo_translate cr x y)
    (cairo_scale cr scale scale)
    (draw-window cr)
    (cairo_save cr)
    (begin
      (cairo_set_source_rgb cr 1 1 1)
      (cairo_fill cr))
    (cairo_restore cr)
    (cairo_set_source_rgb cr 0.231 0.502 0.682)
    (cairo_scale cr (/ 1 scale) (/ 1 scale))
    (cairo_stroke cr))
  (cairo_restore cr))

(define (draw-windows cr)
  (cairo_save cr)
  (begin
    (cairo_move_to cr 18.00 16.125)
    (cairo_line_to cr 48.25 20.375)
    (cairo_line_to cr 30.25 35.825)
    (cairo_close_path cr)
    (cairo_set_source_rgba cr 1 1 1 0.5)
    (cairo_stroke cr))
  (cairo_restore cr)
  (draw-window-at cr 18.00 16.125 1)
  (draw-window-at cr 48.25 20.375 0.8)
  (draw-window-at cr 30.25 35.825 0.5))

(define-constant FDLOGO_ROT_X_FACTOR     1.086)
(define-constant FDLOGO_ROT_Y_FACTOR     1.213)
(define-constant FDLOGO_WIDTH            (* 64 FDLOGO_ROT_X_FACTOR))
(define-constant FDLOGO_HEIGHT           (* 48 FDLOGO_ROT_Y_FACTOR))

(define (fdlogo-draw cr width height)
  (let* ((x-scale (/ width FDLOGO_WIDTH))
         (y-scale (/ height FDLOGO_HEIGHT))
         (scale   (min x-scale y-scale))
         (x-off   (/ (- width (* scale FDLOGO_WIDTH)) 2))
         (y-off   (/ (- height(* scale FDLOGO_HEIGHT)) 2)))
    (cairo_save cr)
    (cairo_translate cr x-off y-off)
    (cairo_scale cr scale scale)
    (cairo_translate cr -2.5 14.75)
    (cairo_rotate cr -0.274990703529840)
    (draw-outline cr)
    (draw-background cr)
    (draw-windows cr)
    (cairo_restore cr)))

(provide "cairo-fd")
#|
;;; translated into Scheme by Shigenobu Kimura <skimu@mac.com>
;;;  $Id: cairo-fd.scm,v 1.2 2008/04/07 03:52:35 skimu Exp $
/*
 * Id: fdface.c,v 1.2 2005-05-11 22:40:18 tor Exp 
 * Id: fdhand.c,v 1.2 2005-05-11 22:40:18 tor Exp 
 * Id: fdlogo.c,v 1.2 2005-05-11 22:40:18 tor Exp
 *
 * Copyright © 2003 Keith Packard
 *
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of Keith Packard not be used in
 * advertising or publicity pertaining to distribution of the software without
 * specific, written prior permission.  Keith Packard makes no
 * representations about the suitability of this software for any purpose.  It
 * is provided "as is" without express or implied warranty.
 *
 * KEITH PACKARD DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
 * INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO
 * EVENT SHALL KEITH PACKARD BE LIABLE FOR ANY SPECIAL, INDIRECT OR
 * CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE,
 * DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
 * TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
 * PERFORMANCE OF THIS SOFTWARE.
 */
|#