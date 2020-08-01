;;; diss-feh.el --- Diss integration with feh        -*- lexical-binding: t; -*-

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
  ((feh-buffer :initarg :feh-buffer
               :documentation "The buffer where Feh is being displayed.")))

(cl-defmethod diss-slideshow-pause! ((this diss-feh-slideshow))
  "Pause DISS/Feh slideshow THIS."
  (with-slots (feh-buffer) diss-feh-image-mode--slideshow
    (with-current-buffer feh-buffer
      (unless (diss-feh--title->paused?)
        (diss-slideshow-toggle-pause!)))))

(cl-defmethod diss-slideshow-toggle-pause! ((this diss-feh-slideshow))
  "Pause or resume DISS/Feh slideshow THIS."
  (with-slots (feh-buffer) diss-feh-image-mode--slideshow
    (with-current-buffer feh-buffer
      (exwm-input--fake-key ?h)
      (setf paused (diss-feh--title->paused?)))))

 ;; Diss-feh-mode

(defvar diss-feh-mode-map
  (let ((km (make-composed-keymap '(dired-mode-map))))
    (define-key km "\C-c\C-r" 'diss-feh-resume)
    (define-key km (kbd "RET") 'diss-feh-move-here)
    km)
  "Keymap for DISS-FEH-MODE.")

(define-derived-mode diss-feh-mode diss-mode "DISS/feh"
  "Major mode for Dired Image Slideshows (feh backend)."
  (setq-local diss--slideshow-class 'diss-feh-slideshow))

(defun diss-feh--make-list ()
  "Create the list of files for the slideshow.  Returns path to filelist."
  (make-temp-file "fehdiss" nil nil
                  (with-output-to-string
                    (save-match-data
                      (dired-map-dired-file-lines
                       (lambda (file)
                         (when (string-match diss-mode--image-regexp file)
                           (princ (concat file "\n")))))))))

(defun diss-feh-start (config-name)
  "Start a slideshow from a Dired buffer, using params from CONFIG-NAME."
  (interactive
   (list
    (setq diss-last-config
          (completing-read
           "Configuration: "
           (mapcar #'car diss-saved-configurations)
           nil nil diss--last-config))))

  (apply #'diss-start* #'diss-feh-mode
         (or (cdr (assoc config-name diss-saved-configurations))
             (cdr (push (cons config-name (diss-configure)) diss-saved-configurations))))

  (diss-feh--begin))

(defun diss-feh--args ()
  "Return list of arguments to feh."
  (with-slots (current step delay loop) diss-mode--slideshow
    `("-."
      "-Z"
      "-^" "fehdiss %f"
      "-B" "#444"
      ,@(when delay `("-D" ,(number-to-string delay)))
      "--on-last-slide" ,(if loop "resume" "hold")
      "-f" ,(diss-feh--make-list)
      "--start-at" ,current
      ,@(when (<= step -1) '("-n")))))

(defun diss-feh--spawn ()
  (when (and diss-feh-mode--process
             (processp diss-feh-mode--process)
             (process-live-p diss-feh-mode--process))
    (delete-process diss-feh-mode--process))

  (setq diss-feh-mode--capture diss-mode--slideshow)
  (add-to-list 'exwm-manage-finish-hook 'diss-feh--exwm-capture)
  (let ((args (diss-feh--args)))
    (setq diss-feh-mode--process
          (apply #'start-process "feh" nil "feh" args))))

(defun diss-feh--begin ()
  "Start the DISS/feh slideshow."

  ;; Side-effect: sets current file.
  (unless (diss-mode--navigate diss-mode--slideshow)
    (error "Found no file in DISS buffer?!"))

  (diss-feh--spawn))

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

(defvar diss-feh-image-mode--slideshow nil
  "The slideshow object this `DISS-FEH-IMAGE-MODE' buffer is associated with.")
(make-variable-buffer-local 'diss-feh-image-mode--slideshow)

(defvar diss-feh-image-mode-map
  (let ((km (make-sparse-keymap)))
    (define-key km "q" 'diss-feh-image-mode-quit)
    (define-key km "e" 'diss-feh-image-mode-set-prefix-name-and-next)
    (define-key km "u" 'diss-feh-image-mode-unmark-and-next)
    (define-key km "d" 'diss-feh-image-mode-delete-and-next)
    (define-key km "m" 'diss-feh-image-mode-mark-and-next)
    (define-key km "c" 'diss-feh-image-mode-categorize-and-next)
    (define-key km (kbd "SPC") 'diss-feh-image-mode-toggle-paused)
    (define-key km (kbd "<mouse-3>") 'diss-feh-image-mode-toggle-paused)
    km)
  "Keymap for DISS-IMAGE-MODE.")

(define-minor-mode diss-feh-image-mode
  "Minor mode for Dired Image Slideshow, feh edition." nil "fehDISS"
  diss-feh-image-mode-map

  (when diss-feh-image-mode
    (add-hook 'exwm-update-title-hook #'diss-feh--update-title-hook nil t)
    (use-local-map diss-feh-image-mode-map)))

(defun diss-feh--exwm-capture ()
  "Capture feh when it starts up."
  (when (and diss-feh-mode--capture (string= exwm-class-name "feh"))
    (setq diss-feh-image-mode--slideshow diss-feh-mode--capture
          diss-feh-mode--capture nil)
    (with-slots (feh-buffer) diss-feh-image-mode--slideshow
      (setq feh-buffer (current-buffer)))

    (diss-feh-image-mode)))

(defun diss-feh--title->filename ()
  (let ((raw (substring exwm-title 8)))
    (if (s-ends-with? " [Paused]" raw)
        (substring raw 0 -9)
      raw)))

(defun diss-feh--title->paused? ()
  (s-ends-with? "[Paused]" exwm-title))

(defun diss-feh--update-title-hook ()
  (with-slots (mark paused) diss-feh-image-mode--slideshow
    (when mark
      (diss-mode--mark diss-feh-image-mode--slideshow (diss-feh--title->filename) mark))
    (setf paused (diss-feh--title->paused?))))

(defun diss-feh-image-mode--automatic-move ()
  "Automatically advance buf IMAGE-BUFFER to the next image in slideshow SS."
  (with-slots (paused mark) ss
    (when (and (diss-slideshow-active? ss)
               (not paused)
               (not (minibufferp)))
      (if-let ((window (display-buffer-reuse-window image-buffer nil)))
          (with-current-buffer image-buffer
            (when mark
              (diss-mode--mark ss (buffer-file-name) mark))
            (with-selected-window window
              (diss--move diss-feh-image-mode--slideshow (oref ss step))))
        ;; If no window is displaying the buffer anymore, pause.
        (diss-slideshow-pause! ss)))))

(defun diss-feh-image-mode-next (&optional arg)
  "Move ARG images forward in the slideshow."
  (interactive "p")
  (exwm-input--fake-key ?n))

(defun diss-feh-image-mode-categorize (char)
  "Mark the current image with CHAR."
  (interactive (list (read-char-exclusive "Category char: " t)))
  (when char
    (diss-mode--mark diss-feh-image-mode--slideshow (diss-feh--title->filename) char t)))

(defun diss-feh-image-mode-mark-and-next ()
  "Mark the current image and advance."
  (interactive)
  (diss-feh-image-mode-categorize ?*)
  (diss-feh-image-mode-next))

(defun diss-image-mode-unmark-and-next ()
  "Remove mark from the current image and advance."
  (interactive)
  (diss-mode--unmark diss-feh-image-mode--slideshow (diss-feh--title->filename))
  (diss-feh-image-mode-next))

(defun diss-feh-image-mode-categorize-and-next (char)
  "Mark the current image with CHAR and advance."
  (interactive (list (read-char-exclusive "Category char: " t)))
  (when char
    (diss-feh-image-mode-categorize char)
    (diss-feh-image-mode-next)))

(defun diss-feh-image-mode-delete-and-next ()
  "Flag the current image for deletion and advance."
  (interactive)
  (diss-feh-image-mode-categorize ?D)
  (diss-feh-image-mode-next))

(defun diss-feh-image-mode-quit ()
  "Flag the current image for deletion and advance."
  (interactive)
  (bury-buffer))

(defun diss-feh-image-mode-toggle-paused ()
  "Toggle whether the current slideshow is paused."
  (interactive)
  (diss-slideshow-toggle-pause! diss-feh-image-mode--slideshow))

(provide 'diss-feh)
;;; diss-feh.el ends here
