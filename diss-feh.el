;; diss-feh.el --- Diss integration with feh        -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Ian Eure

;; Author: Ian Eure <ian@retrospec.tv>
;; Keywords: multimedia

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; One of the issues with diss is that Emacs' image handling code is
;; very inefficient.  This is an attempt to work around that by using
;; feh to display the images instead of Emacs.

;;; Code:

(defvar diss-feh-mode--capture nil
  "When non-nil, captures the next feh window.")

(defvar diss-feh-mode--process nil
  "The feh process associated with this slideshow.")
(make-variable-buffer-local 'diss-feh-mode--process)

 ;; Class

(defclass diss-feh-slideshow (diss-slideshow)
  ((major-mode :initform 'diss-feh-mode
               :documentation "The major mode to control the diss buffer.")
   (feh-buffer :initarg :feh-buffer
               :documentation "The buffer where Feh is being displayed.")))

(cl-defmethod diss--slideshow-begin ((this diss-feh-slideshow))
  "Start the slideshow."
  ;; Side-effect: sets current file.
  (unless (diss-mode--navigate this)
    (error "Found no file in DISS buffer?!"))

  (diss-feh--spawn))

(cl-defmethod diss-slideshow-pause! ((this diss-feh-slideshow))
  "Pause slideshow THIS."
  (with-slots (feh-buffer) diss-image-mode--slideshow
    (with-current-buffer feh-buffer
      (unless (diss-feh--title->paused?)
        (diss-slideshow-toggle-pause! this)))))

(cl-defmethod diss-slideshow-toggle-pause! ((this diss-feh-slideshow))
  "Pause or resume slideshow THIS."
  (with-slots (feh-buffer) diss-image-mode--slideshow
    (with-current-buffer feh-buffer
      (setf paused (not(diss-feh--title->paused?)))
      (exwm-input--fake-key ?h))))

(cl-defmethod diss--move ((this diss-feh-slideshow) &optional arg find-function)
  (let ((key (if (> (signum (or arg 1)) 0) ?n ?p)))
    (exwm-input--fake-key key)))

 ;; Diss-feh-mode

(defvar diss-feh-mode-map
  (let ((km (make-composed-keymap '(dired-mode-map))))
    (define-key km "\C-c\C-r" 'diss-feh-resume)
    (define-key km (kbd "RET") 'diss-feh-move-here)
    (define-key km "C" 'diss-configure)
    km)
  "Keymap for DISS-FEH-MODE.")

(define-derived-mode diss-feh-mode diss-mode "DISS/feh"
  "Major mode for Dired Image Slideshows with feh backend."
  (setq-local diss--slideshow-class 'diss-feh-slideshow))

(defun diss-feh--make-list ()
  "Create the list of files for the slideshow.  Returns path to file list."
  (let ((save-silently t))
    (make-temp-file "fehdiss" nil nil
                    (with-output-to-string
                      (save-match-data
                        (dired-map-dired-file-lines
                         (lambda (file)
                           (when (string-match diss-mode--image-regexp file)
                             (princ (concat file "\n"))))))))))

(defun diss-feh--args (filelist)
  "Return list of command-line arguments to pass to feh."
  (with-slots (current step delay loop paused) diss-mode--slideshow
    (let ((delay (if (and delay paused) (* -1 delay) delay)))
    `("--scale-down"
      "--auto-zoom"
      "--title" "fehdiss %f"
      "--image-bg" "#444"
      ,@(when delay `("--slideshow-delay" ,(number-to-string delay)))
      "--on-last-slide" ,(if loop "resume" "hold")
      "--filelist" ,filelist
      "--no-recursive"
      "--draw-filename"
      "--start-at" ,current
      ,@(when (<= step -1) '("--reverse"))))))

(defun diss-feh--spawn ()
  "Spawn the feh process.  Returns process object."
  (when (and diss-feh-mode--process
             (processp diss-feh-mode--process)
             (process-live-p diss-feh-mode--process))
    (delete-process diss-feh-mode--process))

  (setq diss-feh-mode--capture diss-mode--slideshow)
  (add-to-list 'exwm-manage-finish-hook 'diss-feh--exwm-capture)
  (let* ((filelist (diss-feh--make-list))
         (args (diss-feh--args filelist)))
    (setq diss-feh-mode--process
          (make-process
           :name "diss/feh"
           :buffer nil
           :command (cons "feh" args)
           :sentinel (lambda (process _message)
                       (when (eq 'exit (process-status process))
                         (delete-file filelist nil))
                       (with-slots (paused) diss-mode--slideshow
                         (setf paused t)))))))

(defun diss-feh-resume ()
  "Resume a paused slideshow."
  (interactive)
  (diss-feh--spawn))

(defun diss-feh-move-here ()
  "Move current position to the selected image."
  (interactive)
  (with-slots (current) diss-mode--slideshow
    (setf current (diss-mode--dired-expanded-filename))
    (diss-feh-resume)))

 ;; diss-feh-image-mode

(defvar diss-feh-image-mode-map
  (let ((km (make-sparse-keymap)))
    (define-key km "s" 'diss-image-mode-pop-to-list)
    (define-key km "q" 'diss-image-mode-quit)
    (define-key km "e" 'diss-image-mode-set-prefix-name-and-next)
    (define-key km "p" 'diss-image-mode-previous)
    (define-key km "r" 'diss-feh-image-mode-rotate-cw)
    (define-key km "R" 'diss-feh-image-mode-rotate-ccw)

    (define-key km "x" 'diss-feh-image-mode-fit-to-width)
    (define-key km "y" 'diss-feh-image-mode-fit-to-height)
    (define-key km "0" 'diss-feh-image-mode-transform-reset)

    (define-key km "u" 'diss-image-mode-unmark-and-next)
    (define-key km "d" 'diss-image-mode-delete-and-next)
    (define-key km "m" 'diss-image-mode-mark-and-next)
    (define-key km "j" 'diss-image-mode-junk-and-next)
    (define-key km "c" 'diss-image-mode-categorize-and-next)
    (define-key km "v" 'diss-image-mode-rename-file-and-advance)
    (define-key km (kbd "SPC") 'diss-image-mode-toggle-paused)
    (define-key km (kbd "<mouse-3>") 'diss-image-mode-toggle-paused)
    km)
  "Keymap for DISS-FEH-IMAGE-MODE.")

(define-minor-mode diss-feh-image-mode
  "Minor mode for Dired Image Slideshow with feh backend." nil " DISS/feh"
  diss-feh-image-mode-map

  (when diss-feh-image-mode
    (add-hook 'exwm-update-title-hook #'diss-feh--update-title-hook nil t)
    (use-local-map diss-feh-image-mode-map)))

(defun diss-feh--exwm-capture ()
  "Capture feh when it starts up."
  (when (and diss-feh-mode--capture (string= exwm-class-name "feh"))
    (setq diss-image-mode--slideshow diss-feh-mode--capture
          diss-feh-mode--capture nil)
    (with-slots (feh-buffer) diss-image-mode--slideshow
      (setq feh-buffer (current-buffer)))

    ;; Enable fixed geometry mode, this seems to be impossible via the
    ;; commandline.
    (exwm-input--fake-key ?g)

    (diss-feh-image-mode)))

(defun diss-feh--title->state ()
  "Return (FILENAME . PAUSED) state, based on the feh window title."
  (let ((raw (substring exwm-title 8)))
    (if (s-ends-with? " [Paused]" raw)
        (cons (substring raw 0 -9) t)
      (cons raw nil))))

(defun diss-feh--update-title-hook ()
  "Handle title updated events from feh."
  (with-slots (mark paused current feh-buffer) diss-image-mode--slideshow
    ;; Update state based on the title.
    (cl-destructuring-bind (ncurrent . npaused) (diss-feh--title->state)
      (when (and mark (buffer-live-p feh-buffer) (not (string= current ncurrent)))
        ;; Mark the image that was being displayed before the update.
        (diss-mode--mark diss-image-mode--slideshow current mark))
      (setf current ncurrent
            paused npaused))))

(defun diss-feh-image-mode-rotate-cw ()
  "Rotate image clockwise."
  (interactive)
  (exwm-input--fake-key ?>))

(defun diss-feh-image-mode-rotate-ccw ()
  "Rotate image counterclockwise."
  (interactive)
  (exwm-input--fake-key ?<))

(defun diss-feh-image-mode-rotate-ccw ()
  "Rotate image counterclockwise."
  (interactive)
  (exwm-input--fake-key ?<))

(defun diss-feh-image-mode-fit-to-width ()
  (interactive)
  (exwm-input--fake-key ?!))

(defun diss-feh-image-mode-fit-to-height ()
  (interactive)
  (exwm-input--fake-key ?/))

(defun diss-feh-image-mode-transform-reset ()
  (interactive)
  (exwm-input--fake-key ?*))

(provide 'diss-feh)
;;; diss-feh.el ends here
