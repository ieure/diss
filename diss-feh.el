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

(defvar diss-feh--capture nil
  "When non-nil, captures the next feh window.")

(defvar diss-feh-mode--slideshow nil
  "The slideshow object this `DISS-FEH-MODE' buffer is associated with.")
(make-variable-buffer-local 'diss-feh-mode--slideshow)

(defvar diss-feh-mode-map (make-sparse-keymap)
  "Keymap for DISS-IMAGE-MODE.")

(define-key diss-feh-mode-map "q" 'diss-feh-mode-quit)

(define-key diss-feh-mode-map "e" 'diss-feh-mode-set-prefix-name-and-next)

(define-key diss-feh-mode-map "u" 'diss-feh-mode-unmark-and-next)
(define-key diss-feh-mode-map "d" 'diss-feh-mode-delete-and-next)
(define-key diss-feh-mode-map "m" 'diss-feh-mode-mark-and-next)
(define-key diss-feh-mode-map "c" 'diss-feh-mode-categorize-and-next)

(define-minor-mode diss-feh-mode
  "Minor mode for Dired Image Slideshow, feh edition." nil "fehDISS"
  diss-feh-mode-map

  (when diss-feh-mode
    (setq-local exwm-update-title-hook
                (cons 'diss-feh--update-title-hook exwm-update-title-hook))))

(defun diss-feh--exwm-capture ()
  "Capture feh when it starts up."
  (when (and diss-feh--capture (string= exwm-class-name "feh"))
    (setq diss-feh-mode--slideshow diss-feh--capture
          diss-feh--capture nil)
    (diss-feh-mode)))

(defun diss-feh--update-title-hook ()
  (with-slots (mark) diss-feh-mode--slideshow
    (when mark
      (diss-mode--mark diss-feh-mode--slideshow (substring exwm-title 8) mark))))

(defun diss-feh-mode--automatic-move ()
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
              (diss--move diss-image-mode--slideshow (oref ss step))))
        ;; If no window is displaying the buffer anymore, pause.
        (diss-slideshow-pause! ss)))))

(defun diss-feh--make-list ()
  "Create the list of files for the slideshow.  Returns path to filelist."
  (make-temp-file "fehdiss" nil nil
                  (with-output-to-string
                    (dired-map-dired-file-lines
                     (lambda (file)
                       (princ (concat file "\n")))))))

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

(defun diss-feh--begin ()
  "Start the DISS/feh slideshow."

  ;; Side-effect: sets current file.
  (unless (diss-mode--navigate diss-mode--slideshow)
    (error "Found no file in DISS buffer?!"))

  (setq diss-feh--capture diss-mode--slideshow)
  (add-to-list 'exwm-manage-finish-hook 'diss-feh--exwm-capture)
  (apply #'start-process "feh" nil "feh" (diss-feh--args)))

(provide 'diss-feh)
;;; diss-feh.el ends here
