;;;
;;;
;;;
(use cairo)
(use cairo-fd)

(define-class <window> ()
  ((dpy             :accessor dpy-of)
   (scr             :accessor scr-of)
   (win             :accessor win-of)
   (pix             :accessor pix-of)
   (gc              :accessor gc-of)
   (width           :accessor width-of)
   (height          :accessor height-of)
   (xev             :accessor xev-of)
   (event-mask      :accessor event-mask-of)
   (needs-refresh?  :accessor needs-refresh?
                    :init-value #t)
   ))

(define (make-window dpy width height)
  (let* ((gcv  (make <XGCValues>))
         (root (XDefaultRootWindow dpy))
         (scr  (XDefaultScreen dpy))
         (win  (XCreateSimpleWindow dpy root 0 0
                                    width
                                    height
                                    0
                                    (XWhitePixel dpy scr)
                                    (XWhitePixel dpy scr)))
         (pix  (XCreatePixmap dpy win width height (XDefaultDepth dpy scr)))
         (dum  (set! (ref gcv 'foreground) (XWhitePixel dpy scr)))
         (gc   (x-create-gc dpy pix GCForeground gcv))
         (w    (make <window>)))
    (set! (dpy-of w)    dpy)
    (set! (win-of w)    win)
    (set! (scr-of w)    scr)
    (set! (gc-of w)     gc)
    (set! (pix-of w)    pix)
    (set! (width-of w)  width)
    (set! (height-of w) height)
    (XFillRectangle dpy pix gc 0 0 width height)
    (win-select-events w)
    (XMapWindow dpy win)
    (set! (xev-of w) (make <XEvent>))
    w))

(define (win-refresh win)
  (XFillRectangle (dpy-of win) (pix-of win)  (gc-of win) 0 0
                  (width-of win) (height-of win))
  (let* ((surface (cairo_xlib_surface_create (dpy-of win)
                                             (pix-of win)
                                             (XDefaultVisual (dpy-of win) (scr-of win))
                                             (width-of win) (height-of win)))
         (cr (cairo_create surface)))

    (fdface-draw cr (width-of win) (height-of win))
    (fdhand-draw-now cr (width-of win) (height-of win) #t)

    (if (not (zero? (cairo_status cr)))
        (format (current-error-port)
                "Cairo is unhappy: ~a~%"
                (cairo_status_to_string status)))

    (cairo_destroy cr)
    (cairo_surface_destroy surface)

    (XCopyArea (dpy-of win) (pix-of win) (win-of win) (gc-of win)
               0 0 (width-of win) (height-of win) 0 0)))

(define (win-select-events win)
  (set! (event-mask-of win) (logior ButtonPressMask
                                    ButtonReleaseMask
                                    PointerMotionMask
                                    KeyPressMask
                                    StructureNotifyMask
                                    ExposureMask))
  (XSelectInput (dpy-of win) (win-of win) (event-mask-of win)))

(define (win-handle-event win)

  (let loop ()
    ; XXX BUSY WAIT!!
    (cond ((x-pending? (dpy-of win)) #t)
          ((needs-refresh? win)
           (win-refresh win)
           (set! (needs-refresh? win) #f)
           (loop))
          (else
           (loop))))

  (x-next-event (dpy-of win) (xev-of win))
  (let ((xxx (ref (xev-of win) 'type)))
    (cond ((= xxx ConfigureNotify)
           (win-handle-configure win (ref (xev-of win) 'xconfigure)))
          ((= xxx Expose)
           (win-handle-expose win (ref (xev-of win) 'xexpose)))
          ((= xxx KeyPress)
           (win-handle-key-press win (ref (xev-of win) 'xkey)))
          (else
           ;;(format #t "Unknown Event (~d)~%" xxx)
           )))
  (win-handle-event win))
             
(define (win-handle-configure win cev)
  (let ((has-grown #f))
    (if (or (> (ref cev 'width) (width-of win))
            (> (ref cev 'height) (height-of win)))
        (set! has-grown #t))
    (set! (width-of win) (ref cev 'width))
    (set! (height-of win) (ref cev 'height))
    (if has-grown
        (win-grow-pixmap win))))

(define (win-grow-pixmap win)
  (let ((new (XCreatePixmap (dpy-of win) (win-of win) (width-of win) (height-of win)
                            (XDefaultDepth (dpy-of win) (scr-of win)))))
    (XFillRectangle (dpy-of win) new (gc-of win) 0 0 (width-of win) (height-of win))
    (XCopyArea (dpy-of win) (pix-of win) new (gc-of win) 0 0 (width-of win) (height-of win) 0 0)
    (XFreePixmap (dpy-of win) (pix-of win))
    (set! (pix-of win) new)
    (set! (needs-refresh? win) #t)))

(define (win-handle-expose win eev)
  (XCopyArea (dpy-of win) (pix-of win) (win-of win) (gc-of win)
             (ref eev 'x) (ref eev 'y)
             (ref eev 'width) (ref eev 'height)
             (ref eev 'x) (ref eev 'y)))

(define (win-handle-key-press win kev)
  (let ((cmd (hash-table-get *bindings* (ref kev 'keycode) #f)))
    (if cmd
        ((proc-of cmd) win))))

(define (fd-clock dpy width height)
  (let ((window (make-window dpy width height)))
    (define (alarm-handler x)
      (set! (needs-refresh? window) #t)
      (sys-alarm 1))
    (set-signal-handler! SIGALRM alarm-handler)
    (init-binding dpy)
    (sys-alarm 1)
    (win-handle-event window)))

(define (main args)
  (cond ((x-open-display 0) 
         => (lambda (dpy)
              (fd-clock dpy 400 400)))
        (else
         (error "Could not open display"))))

(define (save-to-png win)
  (let* ((bpp 4) ; byte-per-pixel
         (image   (cairo/g-make-image (width-of win) (height-of win) bpp))
         (surface (cairo_image_surface_create_for_data image
                                                       CAIRO_FORMAT_ARGB32 
                                                       (width-of win) 
                                                       (height-of win) 
                                                       (* bpp (width-of win))))
         (cr (cairo_create surface)))

    (cairo_rectangle cr 0 0 (width-of win) (height-of win))
    (cairo_set_source_rgb cr 1 1 1)
    (cairo_fill cr)

    (fdface-draw cr (width-of win) (height-of win))
    (fdhand-draw-now cr (width-of win) (height-of win) #t)

    (cairo_surface_write_to_png surface "fdclock.png")
    (cairo_destroy cr)
    (cairo_surface_destroy surface)
    0))

(define (save-to-pdf win)
  (let* ((points/dot 1.0)
         (surface (cairo_pdf_surface_create "fdclock.pdf"
                                            (* (width-of win) points/dot)
                                            (* (height-of win) points/dot)))
         (cr (cairo_create surface)))

    (cairo_rectangle cr 0 0 (width-of win) (height-of win))
    (cairo_set_source_rgb cr 1 1 1)
    (cairo_fill cr)

    (fdface-draw cr (width-of win) (height-of win))
    (fdhand-draw-now cr (width-of win) (height-of win) #t)

    (cairo_show_page cr)
    (cairo_destroy cr)
    (cairo_surface_destroy surface)
    0))

(define (save-to-ps win)
  (let* ((points/dot 1.0)
         (surface (cairo_ps_surface_create "fdclock.ps"
                                           (* (width-of win) points/dot)
                                           (* (height-of win) points/dot)))
         (cr (cairo_create surface)))

    (cairo_rectangle cr 0 0 (width-of win) (height-of win))
    (cairo_set_source_rgb cr 1 1 1)
    (cairo_fill cr)

    (fdface-draw cr (width-of win) (height-of win))
    (fdhand-draw-now cr (width-of win) (height-of win) #t)

    (cairo_show_page cr)
    (cairo_destroy cr)
    (cairo_surface_destroy surface)
    0))

(define-class <command> ()
  ((key     :accessor key-of
            :init-keyword :key)
   (keycode :accessor keycode-of
            :init-keyword :keycode)
   (proc    :accessor proc-of
            :init-keyword :proc)
   (doc     :accessor doc-of
            :init-keyword :doc)))

(define *bindings* (make-hash-table))

(define (bind-key dpy key proc doc)
  (let* ((keysym  (XStringToKeysym key))
         (keycode (XKeysymToKeycode dpy keysym)))
    (if (= keysym NoSymbol)
        (errorf "No keysym for ~s~%" key))
    (hash-table-put! *bindings* keycode
                     (make <command> :key key :keycode keycode
                           :proc proc :doc doc))))

(define (quit-cb win) 
  (XCloseDisplay (dpy-of win))
  (exit 0))

(define (init-binding dpy)
  (bind-key dpy "Q"     quit-cb     "Exit the program")
  (bind-key dpy "S"     save-to-png "Save to png file (fdclock.png)")
  (bind-key dpy "P"     save-to-pdf "Save to pdf file (fdclock.pdf)")
  (bind-key dpy "O"     save-to-ps  "Save to ps file (fdclock.ps)")
  )


;;;