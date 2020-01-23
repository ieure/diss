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

(defvar ssp-mode--dired-buffer nil
  "Dired buffer containing images to display in the slideshow.")

(defvar ssp-mode--position nil
  "Current position in the dired buffer.")

(defvar ssp-mode--loop nil
  "When non-NIL, start over at the beginning when the end is reached.")

 ;; Minor mode for controlling the slideshow.

(define-minor-mode ssp-mode
  "Minor mode for dired-based image slideshow."
  nil " SSP"
  nil
  :global t

  (if ssp-mode
      ;; Enabling
      (progn
        (setq ssp-mode--dired-buffer (current-buffer)
              ssp-mode--position (point))

        (find-file (ssp--navigate 1))
        ;; (delete-other-windows)
        (add-hook 'image-mode-hook 'ssp-image-mode))

    ;; Disabling
    (setq ssp-mode--dired-buffer nil
          ssp-mode--position nil)
    (ssp-image-mode -1)
    (remove-hook 'image-mode-hook 'ssp-image-mode)
    (remove-hook 'image-mode-hook 'ssp--flag-file-deletion)))

;; Minor mode for images in the slideshow.

(defvar ssp-image-mode-map
  (let ((km (make-sparse-keymap)))
    (define-key km "p" 'ssp-find-previous)
    (define-key km "n" 'ssp-find-next)

    (define-key km "u" 'ssp-unmark)
    (define-key km "d" 'ssp-flag-file-deletion-and-next)
    (define-key km "m" 'ssp-mark-and-next)
    (define-key km "q" 'ssp-pause)

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
    (define-key km "\C-c\C-r" 'ssp-automatic-mode-resume)
    (define-key km "\\" 'ssp-automatic-mode-resume)

    km)
  "Keymap for SSP-IMAGE-MODE.")

(define-minor-mode ssp-image-mode
  "Minor mode for dired-based image slideshow."
  nil nil
  ssp-image-mode-map)

 ;; Automatic mode.

(defvar ssp-automatic-mode-default-delay .5)

(defun ssp-automatic-mode--prefix-to-delay (arg)
  "Compute decimal delay for integer ARG."
  (cond
   ((null arg) ssp-automatic-mode-default-delay)
   ((< arg 10) arg)                     ; seconds
   (t (/ arg 1000.0))))                 ; milliseconds

(defvar ssp-automatic-image-mode-map
  (let ((km (copy-keymap ssp-image-mode-map)))
    (define-key km "q" 'ssp-pause)

    (define-key km "n" 'ssp-automatic-mode-next)
    (define-key km "p" 'ssp-automatic-mode-previous)
    (define-key km "\C-c\C-q" 'ssp-mode-quit)
    (define-key km "\C-c\C-r" 'ssp-automatic-mode-resume)
    (define-key km "\\" 'ssp-automatic-mode-resume)

    km)
  "Keymap for SSP-AUTOMATIC-IMAGE-MODE.")

(define-minor-mode ssp-automatic-mode
  "Minor mode for dired-based image slideshow."
  nil nil nil
  :global t

  (if ssp-automatic-mode
      ;; Enabling
      (progn
        (setq ssp-automatic-mode--paused nil)
        (add-hook 'image-mode-hook 'ssp-automatic-image-mode)
        (ssp-mode 1))

    ;; Disabling
    (ssp-mode -1)
    (remove-hook 'image-mode-hook 'ssp-automatic-image-mode)))

(defvar ssp-automatic-image-mode--timer nil
  "Timer for advancing to the next image.")

(defvar ssp-automatic-image-mode--delay ssp-automatic-mode-default-delay)

(defvar ssp-automatic-mode--paused nil)

(define-minor-mode ssp-automatic-image-mode
  "Minor mode for dired-based image slideshow."
  nil nil
  ssp-automatic-image-mode-map

  (if ssp-automatic-image-mode
      ;; Enable
      (unless ssp-automatic-mode--paused
        (let ((image-buffer (current-buffer)))
          (setq ssp-automatic-image-mode--timer
                (run-with-timer ssp-automatic-image-mode--delay nil
                                (lambda ()
                                  (if (buffer-live-p image-buffer)
                                      (with-current-buffer image-buffer
                                        (ssp-find-next))))))))

    ;; Disable
    (ssp-automatic-image-mode--cancel)))

(defun ssp-automatic-image-mode--cancel ()
  (when ssp-automatic-image-mode--timer
    (cancel-timer ssp-automatic-image-mode--timer)))

(defun ssp-automatic-mode-resume (&optional arg)
  "Resume automatic slideshow."
  (interactive "P")
  (setq ssp-automatic-mode--paused nil)
  (ssp-find-next))

(defun ssp-automatic-mode-next ()
  (interactive)
  (ssp-pause*)
  (ssp-find-next))

(defun ssp-automatic-mode-previous ()
  (interactive)
  (ssp-pause*)
  (ssp-find-previous))

(defun ssp-automatic-start (&optional arg)
  "Begin automatic slideshow with ARG delay between images.

If ARG is between 0-9, delay that many seconds.
If ARG is greater than 9, delay that many milliseconds."
  (interactive "P")
  (ssp-automatic-mode -1)
  (when (or arg (= ssp-automatic-image-mode--delay 0))
    (setq ssp-automatic-image-mode--delay
          (ssp-automatic-mode--prefix-to-delay arg)))
  (ssp-automatic-mode 1))

 ;; User helper functions

(defun ssp-start (arg)
  "Begin slideshow from dired."
  (interactive "p")
  (ssp-mode -1)
  (when (called-interactively-p)
    (setq ssp-mode--auto nil
          ssp-mode--loop nil))
  (save-excursion
    (when arg (goto-char (point-min)))
    (ssp-mode 1)))

(defun ssp-image-start ()
  "Begin slideshow from an image."
  (interactive)
  (let* ((bfn (buffer-file-name))
         (barename (file-name-nondirectory bfn)))
    (with-current-buffer (dired bfn)
      (save-excursion
        (goto-char (point-min))
        (unless (search-forward barename)
          (error "Can't find image?!"))
        (ssp-start t)))))

(defun ssp-mode-quit ()
  "Stop SSP mode."
  (interactive)
  (ssp-mode -1))

(defun ssp-start-looping (&optional arg)
  "Begin automatic slideshow with ARG delay between images.

If ARG is between 0-9, delay that many seconds.
If ARG is greater than 9, delay that many milliseconds."
  (interactive "P")
  (ssp-automatic-mode -1)
  (when (or arg (= ssp-automatic-image-mode--delay 0))
    (setq ssp-automatic-image-mode--delay
          (ssp-automatic-mode--prefix-to-delay arg)))
  (setq ssp-mode--loop t)
  (ssp-automatic-mode 1))

(defun ssp-start-deleting (&optional arg)
  "Begin automatic deleting slideshow with ARG delay between images.

If ARG is between 0-9, delay that many seconds.
If ARG is greater than 9, delay that many milliseconds."
  (interactive "P")

  (ssp-automatic-mode -1)
  (when (or arg (= ssp-automatic-image-mode--delay 0))
    (setq ssp-automatic-image-mode--delay
          (ssp-automatic-mode--prefix-to-delay arg)))
  (add-hook 'image-mode-hook 'ssp--flag-file-deletion)
  (ssp-automatic-mode 1))

(defun ssp-find-next ()
  "Replace the current image with the next one."
  (interactive)

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

(defun ssp-pause* ()
  (setq ssp-automatic-mode--paused t)
  (ssp-automatic-image-mode--cancel))

(defun ssp-pause ()
  "Stop the automatic slideshow and bury the current buffer."
  (interactive)
  (ssp-pause*)
  (bury-buffer))

(defun ssp-find-previous ()
  "Replace the current image with the previous one."
  (interactive)

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
(define-key dired-mode-map "\C-c\C-a" 'ssp-automatic-start)
(define-key dired-mode-map "\C-c\C-d" 'ssp-start-deleting)
(define-key dired-mode-map "\C-c\C-l" 'ssp-start-looping)

(provide 'ssp)
;;; ssp.el ends here
