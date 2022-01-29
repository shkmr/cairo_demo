;;;
;;;   cairo module
;;;
(define-module cairo
  (use math.const)
  (use gauche.process)
  (use c-wrapper)
  (export-all)
  )
(select-module cairo)

(c-load '("cairo.h" "cairo-xlib.h" "cairo-pdf.h" "cairo-ps.h")
        :cppflags-cmd  "pkg-config cairo --cflags-only-I"
        :cflags-cmd    "pkg-config cairo --cflags-only-other"
        :libs-cmd      "pkg-config cairo --libs"
        :compiled-lib  "cairolib"
        )

;;;
;;;  APIs which require c-wrapper function is not usually
;;;  ``Schemer's Way'', should be encapsulated here
;;;

;;; Cairo
(define (cairo/g-make-image width height bpp)
  (make (make-c-array <c-uchar> (* width height bpp))))

(define (cairo-device-to-user cr x y)
  (let ((x (cast <c-double> x))
        (y (cast <c-double> y)))
    (cairo_device_to_user cr (ptr x) (ptr y))))

(define (cairo-get-matrix cr m) (cairo_get_matrix cr (ptr m)))
(define (cairo-set-matrix cr m) (cairo_set_matrix cr (ptr m)))

(define (cairo-matrix-init-scale m w h)
  (cairo_matrix_init_scale (ptr m) w h))

(define (cairo-pattern-set-matrix p m)
  (cairo_pattern_set_matrix p (ptr m)))

(define (cairo-text-extents cr str extents)
  (cairo_text_extents cr str (ptr extents)))

;;; X11
(define (x-open-display arg)
  (let ((dpy (XOpenDisplay arg)))
    (cond ((null-ptr? dpy) #f)
          (else dpy))))

(define (x-pending? dpy)
  (not (zero? (XPending dpy))))

(define (x-create-gc dpy pixmap fore gcv)
  (XCreateGC dpy pixmap fore (ptr gcv)))

(define (x-next-event dpy xev)
  (XNextEvent dpy (ptr xev)))

(provide "cairo")
;;; EOF
