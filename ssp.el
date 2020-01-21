;;; ssp.el --- Slideshow+                            -*- lexical-binding: t; -*-

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

;;

;;; Code:

(require 'dired)

(defvar ssp-mode-default-auto-delay .5)

(defvar ssp-mode--dired-buffer nil
  "Dired buffer containing images to display in the slideshow.")

(defvar ssp-mode--position nil
  "Current position in the dired buffer.")

(defvar ssp--starting nil
  "Non-nil if this is the first image to be displayed.")

(defvar ssp-mode--auto nil
  "How long to wait until automatically advancing to the next image.")

(defvar ssp-mode--loop nil
  "When non-NIL, start over at the beginning when the end is reached.")

(defvar ssp-mode--timer nil
  "Timer for advancing to the next image.")

(define-minor-mode ssp-mode
  "Minor mode for dired-based image slideshow."
  nil " SSP"
  nil
  :global t

  (if ssp-mode
      (progn (setq ssp-mode--dired-buffer (current-buffer)
                   ssp-mode--position nil
                   ssp--starting t)

             (add-hook 'image-mode-hook 'ssp-image-mode))

    (ssp--cancel-automatic)
    (setq ssp-mode--dired-buffer nil
          ssp-mode--position nil)
    (ssp-image-mode -1)
    (remove-hook 'image-mode-hook 'ssp-image-mode)
    (remove-hook 'image-mode-hook 'ssp--flag-file-deletion)))



(defvar ssp-image-mode-map
  (let ((km (make-sparse-keymap)))
    (define-key km "p" 'ssp-find-previous)
    (define-key km "n" 'ssp-find-next)

    (define-key km "u" 'ssp-unmark)
    (define-key km "d" 'ssp-flag-file-deletion-and-next)
    (define-key km "m" 'ssp-mark-and-next)

    (define-key km "c a" 'ssp--categorize)
    (define-key km "c b" 'ssp--categorize)
    (define-key km "c c" 'ssp--categorize)
    (define-key km "c d" 'ssp--categorize)
    (define-key km "c e" 'ssp--categorize)
    (define-key km "c f" 'ssp--categorize)
    (define-key km "c g" 'ssp--categorize)
    (define-key km "c h" 'ssp--categorize)
    (define-key km "c i" 'ssp--categorize)
    (define-key km "c j" 'ssp--categorize)
    (define-key km "c k" 'ssp--categorize)
    (define-key km "c l" 'ssp--categorize)
    (define-key km "c m" 'ssp--categorize)
    (define-key km "c n" 'ssp--categorize)
    (define-key km "c o" 'ssp--categorize)
    (define-key km "c p" 'ssp--categorize)
    (define-key km "c q" 'ssp--categorize)
    (define-key km "c r" 'ssp--categorize)
    (define-key km "c s" 'ssp--categorize)
    (define-key km "c t" 'ssp--categorize)
    (define-key km "c u" 'ssp--categorize)
    (define-key km "c v" 'ssp--categorize)
    (define-key km "c w" 'ssp--categorize)
    (define-key km "c x" 'ssp--categorize)
    (define-key km "c y" 'ssp--categorize)
    (define-key km "c z" 'ssp--categorize)

    (define-key km "\C-c\C-q" 'ssp-mode-quit)
    (define-key km "\C-c\C-r" 'ssp-resume-automatic)

    km)
  "Keymap for SSP-IMAGE-MODE.")

(define-minor-mode ssp-image-mode
  "Minor mode for dired-based image slideshow."
  nil nil
  ssp-image-mode-map

  (when ssp-mode--auto
    (let ((image-buffer (current-buffer)))
      (setq ssp-mode--timer
            (run-with-timer ssp-mode--auto nil
                            (lambda ()
                              (if (buffer-live-p image-buffer)
                                  (with-current-buffer image-buffer
                                    (ssp-find-next))))))))
  (when ssp--starting
    (delete-other-windows)
    (setq ssp--starting nil)))



(defun ssp-start ()
  "Begin slideshow."
  (interactive)
  ;; Stop any active slideshows.
  (when ssp-mode
    (ssp-mode -1))
  (when (called-interactively-p)
    (setq ssp-mode--auto nil
          ssp-mode--loop nil))
  (ssp-mode 1)
  (find-file (ssp--navigate 1)))

(defun ssp-mode-quit ()
  "Stop SSP mode."
  (interactive)
  (ssp-mode -1))

(defun ssp--prefix-to-delay (arg)
  "Compute decimal delay for integer ARG."
  (cond
   ((null arg) ssp-mode-default-auto-delay)
   ((< arg 10) arg)                     ; seconds
   (t (/ arg 1000.0))))                 ; milliseconds


(defun ssp-start-automatic (&optional arg)
  "Begin automatic slideshow with ARG delay between images.

If ARG is between 0-9, delay that many seconds.
If ARG is greater than 9, delay that many milliseconds."
  (interactive "P")
  (setq ssp-mode--auto (ssp--prefix-to-delay arg))
  (ssp-start))

(defun ssp-start-looping (&optional arg)
  "Begin automatic slideshow with ARG delay between images.

If ARG is between 0-9, delay that many seconds.
If ARG is greater than 9, delay that many milliseconds."
  (interactive "P")
  (setq ssp-mode--auto (ssp--prefix-to-delay arg)
        ssp-mode--loop t)
  (ssp-start))

(defun ssp-start-deleting (&optional arg)
  "Begin automatic deleting slideshow with ARG delay between images.

If ARG is between 0-9, delay that many seconds.
If ARG is greater than 9, delay that many milliseconds."
  (interactive "P")
  (setq ssp-mode--auto (ssp--prefix-to-delay arg))
  (add-hook 'image-mode-hook 'ssp--flag-file-deletion)
  (ssp-start))

(defun ssp-resume-automatic (&optional arg)
  "Resume automatic slideshow."
  (interactive "P")
  (setq ssp-mode--auto (ssp--prefix-to-delay arg))
  (ssp-find-next))

(defun ssp--cancel-automatic ()
  "Stop automatic slideshow."
  (when ssp-mode--timer (cancel-timer ssp-mode--timer))
  (setq ssp-mode--auto nil))

(defun ssp-find-next ()
  "Replace the current image with the next one."
  (interactive)
  (when (called-interactively-p) (ssp--cancel-automatic))

  (if-let ((next (ssp--navigate 1)))
      (find-alternate-file next)
    (message "End of images.")
    (ssp-mode -1)))

(defun ssp--navigate* (arg)
  "Move forwards or backwards ARG images in `ssp-mode--dired-buffer'."
  (when ssp-mode--dired-buffer
    (let ((image-file-name (buffer-file-name)))
      (with-current-buffer ssp-mode--dired-buffer
        (save-excursion
          (goto-char (or ssp-mode--position (point-min)))

          (when (string= image-file-name
                         (or (ignore-errors (dired-get-file-for-visit)) ""))
            (dired-next-line arg))

          (save-match-data
            (let ((image))
              (while (not (or image (if (< arg 0) (bobp) (eobp))))
                (if (string-match (image-file-name-regexp) (or (dired-get-filename t t) ""))
                    (if-let ((extant-file (ignore-errors (dired-get-file-for-visit))))
                        (setq image extant-file
                              ssp-mode--position (point)))
                  (dired-next-line arg)))
              image)))))))

(defun ssp--navigate (arg)
  "Move forwards or backwards ARG images in `ssp-mode--dired-buffer'."
  (if-let ((file (ssp--navigate* arg)))
      file
    (when ssp-mode--loop
      (setq ssp-mode--position (if (< arg 0) (point-max) (point-min)))
      (ssp--navigate* arg))))

(defun ssp-find-previous ()
  "Replace the current image with the previous one."
  (interactive)
  (when (called-interactively-p) (ssp--cancel-automatic))

  (if-let ((image (ssp--navigate -1)))
      (find-alternate-file image)
    (message "No previous image.")
    (ssp-mode -1)))

(defun ssp--mark-with-char (char &optional force)
  "Mark an image with CHAR.  Doesn't replace marks, unless FORCE is non-nil."
  (with-current-buffer ssp-mode--dired-buffer
    (save-excursion
      (goto-char ssp-mode--position)
      (beginning-of-line)
      (when (equal ?\040 (following-char))
        (let ((dired-marker-char char))
          (dired-mark nil nil))))))

(defun ssp--flag-file-deletion ()
  "Flag the current file for deletion."
  (ssp--mark-with-char "D"))

(defun ssp-flag-file-deletion-and-next ()
  "Flag the current image for deletion in its dired buffer.

Advances to the next image, unless an automatic slideshow is
currently playing."
  (interactive)
  (ssp--mark-with-char "D" t)
  (unless ssp-mode--auto (ssp-find-next)))

(defun ssp-mark ()
  "Mark the current image in its dired buffer.

Advances to the next image, unless an automatic slideshow is
currently playing."
  (interactive)
  (ssp--mark-with-char "*" t)
  (unless ssp-mode--auto (ssp-find-next)))

(defun ssp-unmark ()
  "Mark the current image in its dired buffer.

Advances to the next image, unless an automatic slideshow is
currently playing."
  (interactive)
  (with-current-buffer ssp-mode--dired-buffer
    (goto-char ssp-mode--position)
    (dired-unmark nil nil))
  (unless ssp-mode--auto (ssp-find-next)))

(defun ssp-categorize ()
  "Mark this image, using the current input command as the character.

Advances to the next image, unless an automatic slideshow is
currently playing."
  (interactive)
  (ssp--mark-with-char last-command-event t)
  (unless ssp-mode--auto (ssp-find-next)))

(define-key dired-mode-map "\C-c\C-s" 'ssp-start)
(define-key dired-mode-map "\C-c\C-a" 'ssp-start-automatic)
(define-key dired-mode-map "\C-c\C-d" 'ssp-start-deleting)
(define-key dired-mode-map "\C-c\C-l" 'ssp-start-looping)

(provide 'ssp)
;;; ssp.el ends here
