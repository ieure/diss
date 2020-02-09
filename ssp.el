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

(defvar ssp--active nil
  "List of active slideshows.  Each element is a cons cell
  of (IMAGE . SSP-BUFFER).")

(defvar ssp-image-mode--slideshow nil
  "The slideshow this image buffer is associated with.")
(make-variable-buffer-local 'ssp-image-mode--slideshow)

(defvar ssp-mode--slideshow nil
  "The slideshow in this SSP-MODE buffer.")
(make-variable-buffer-local 'ssp-mode--slideshow)

;; (defvar ssp-mode--loop nil
;;   "When non-NIL, start over at the beginning when the end is reached.")
;; (make-variable-buffer-local 'ssp-mode--loop)

;; (defvar ssp-mode--automark nil
;;   "When non-NIL, mark files with this char when automatically advancing.")
;; (make-variable-buffer-local 'ssp-mode--automark)

(defvar ssp-mode--step 1
  "How many images to advance at a time.")
(make-variable-buffer-local 'ssp-mode--step)

(defvar ssp-mode--paused nil
  "Whether the slideshow is paused")
(make-variable-buffer-local 'ssp-mode--paused)

;; (defvar ssp-mode--automatic-delay nil
;;   "When non-nil, delay this long before going to the next image.")
;; (make-variable-buffer-local 'ssp-mode--loop)

(defvar ssp-mode--image-regexp (image-file-name-regexp)
  "Regular expression matching images.")
(make-variable-buffer-local 'ssp-mode--image-regexp)

(defun ssp--cleanup ()
  "Clean up ended slideshows."
  (setq ssp--active
        (cl-loop for ss in ssp--active
                 when (buffer-live-p (cdr ss))
                 collect ss)))

(define-derived-mode ssp-mode dired-mode "SSP"
  "Major mode for dired slideshows."

  (setq ssp-mode--loop nil
        ssp-mode--automatic-delay nil
        ssp-mode--image-regexp (image-file-name-regexp))
  (add-hook 'image-mode-hook 'ssp-image-mode--maybe-enable)
  (add-to-list 'ssp--active (setq ssp-mode--slideshow (cons nil (current-buffer)))))

(defun ssp-start (arg)
  (interactive "p")
  (unless (eq major-mode 'dired-mode)
    (error "Must be started from Dired."))

  (ssp--cleanup)

  (let ((dsal dired-subdir-alist))
    (switch-to-buffer (clone-indirect-buffer (format "*ssp %s*" dired-directory) nil))
    (ssp-mode)
    (setq dired-subdir-alist dsal)
    (goto-char (point-min))
    (ssp--move arg ssp-mode--slideshow)))

(define-key dired-mode-map "\C-c\C-s" 'ssp-start)
;; (define-key dired-mode-map "\C-c\C-a" 'ssp-automatic-start)
;; (define-key dired-mode-map "\C-c\C-d" 'ssp-start-deleting)
;; (define-key dired-mode-map "\C-c\C-l" 'ssp-start-looping)

(defun ssp-mode--map-windows-displaying (buffer fn)
  (cl-loop for frame in (frame-list)
           do (cl-loop for window in (window-list frame)
                       when (eq buffer (window-buffer window))
                       do (funcall fn window))))

(defun ssp-mode--navigate** (arg)
  "Move forwards or backwards ARG images."
  (save-match-data
    (cl-loop with n = (abs arg)
             with a = (if (< arg 0) -1 1)
             until (or (if (< arg 0) (bobp) (eobp))
                       (zerop n))
             do (dired-next-line a)
             for fn = (dired-get-filename t t)
             when (string-match ssp-mode--image-regexp (or fn ""))
             do (decf n)
             finally return fn)))

(defun ssp-mode--navigate* (arg)
  "Move forwards or backwards ARG images."
  (if-let ((file (ssp-mode--navigate** arg)))
      file
    (when ssp-mode--loop
      (goto-char (if (< arg 0) (point-max) (point-min)))
      (ssp-mode--navigate** arg))))

(defun ssp-mode--navigate (arg ss)
  "Move forwards or backwards ARG images."
  (when-let ((file (with-current-buffer (cdr ss)
                     (ssp-mode--navigate* arg))))
    ;; Update state
    (setf (car ss) (expand-file-name file))

    ;; Adjust window point
    (ssp-mode--map-windows-displaying
     (current-buffer) (lambda (window)
                        (set-window-point window (point))))
    ;; Return file
    file))

 ;; Minor mode for images in the slideshow.

(setq ssp-image-mode-map
      (let ((km (make-sparse-keymap)))
        (define-key km "p" 'ssp-image-mode-previous)
        (define-key km "n" 'ssp-image-mode-next)

        ;; (define-key km "u" 'ssp-unmark)
        ;; (define-key km "d" 'ssp-flag-file-deletion-and-next)
        ;; (define-key km "m" 'ssp-mark)
        ;; (define-key km "q" 'ssp-pause)

        ;; (define-key km "ca" 'ssp-categorize-and-next)
        ;; (define-key km "cb" 'ssp-categorize-and-next)
        ;; (define-key km "cc" 'ssp-categorize-and-next)
        ;; (define-key km "cd" 'ssp-categorize-and-next)
        ;; (define-key km "ce" 'ssp-categorize-and-next)
        ;; (define-key km "cf" 'ssp-categorize-and-next)
        ;; (define-key km "cg" 'ssp-categorize-and-next)
        ;; (define-key km "ch" 'ssp-categorize-and-next)
        ;; (define-key km "ci" 'ssp-categorize-and-next)
        ;; (define-key km "cj" 'ssp-categorize-and-next)
        ;; (define-key km "ck" 'ssp-categorize-and-next)
        ;; (define-key km "cl" 'ssp-categorize-and-next)
        ;; (define-key km "cm" 'ssp-categorize-and-next)
        ;; (define-key km "cn" 'ssp-categorize-and-next)
        ;; (define-key km "co" 'ssp-categorize-and-next)
        ;; (define-key km "cp" 'ssp-categorize-and-next)
        ;; (define-key km "cq" 'ssp-categorize-and-next)
        ;; (define-key km "cr" 'ssp-categorize-and-next)
        ;; (define-key km "cs" 'ssp-categorize-and-next)
        ;; (define-key km "ct" 'ssp-categorize-and-next)
        ;; (define-key km "cu" 'ssp-categorize-and-next)
        ;; (define-key km "cv" 'ssp-categorize-and-next)
        ;; (define-key km "cw" 'ssp-categorize-and-next)
        ;; (define-key km "cx" 'ssp-categorize-and-next)
        ;; (define-key km "cy" 'ssp-categorize-and-next)
        ;; (define-key km "cz" 'ssp-categorize-and-next)

        ;; (define-key km "\C-c\C-q" 'ssp-mode-quit)

        km)
      ;; "Keymap for SSP-IMAGE-MODE."
      )

(define-minor-mode ssp-image-mode
  "Minor mode for dired-based image slideshow."
  nil "SSPi"
  ssp-image-mode-map

  (when ssp-image-mode
    (let ((image-buffer (current-buffer)))
      (with-current-buffer (cdr ssp-image-mode--slideshow)
        (when (and ssp-mode--automatic-delay (not ssp-mode--paused))
          (setq ssp--timer
                (run-with-timer
                 ssp-mode--automatic-delay nil
                 (lambda ()
                   (if (and (not ssp-mode--paused)
                            (buffer-live-p image-buffer))
                       (with-current-buffer image-buffer
                         (ssp-image-mode-next)))))))))))

(defun ssp-image-mode--maybe-enable ()
  "Enable SSP-IMAGE-MODE if this image is part of a current slideshow."
  (when-let ((slideshow (assoc (expand-file-name (buffer-file-name))
                               ssp--active)))
    (when (buffer-live-p (cdr slideshow))
      (setq ssp-image-mode--slideshow slideshow)
      (ssp-image-mode 1))))

(defun ssp--move (arg &optional ss)
  (let ((ss (or ss ssp-image-mode--slideshow)))
    (if-let ((next-file (ssp-mode--navigate arg ss)))
        (find-alternate-file next-file)
      (pop-to-buffer ss))))

(defun ssp-image-mode-next (&optional arg)
  "Move ARG images forward in the slideshow."
  (interactive "p")
  (ssp--move arg))

(defun ssp-image-mode-previous (&optional arg)
  "Move ARG images back in the slideshow."
  (interactive "p")
  (ssp--move (* -1 arg)))



;; (define-minor-mode ssp-mode
;;   "Minor mode for dired-based image slideshow."
;;   nil " SSP"
;;   nil
;;   :global t

;;   (if ssp-mode
;;       ;; Enabling
;;       (progn
;;         (setq ssp-mode--dired-buffer (current-buffer)
;;               ssp-mode--position (point))

;;         (find-file (ssp-mode--navigate 1))
;;         ;; (delete-other-windows)
;;         (add-hook 'image-mode-hook 'ssp-image-mode))

;;     ;; Disabling
;;     (setq ssp-mode--dired-buffer nil
;;           ssp-mode--position nil)
;;     (ssp-image-mode -1)
;;     (remove-hook 'image-mode-hook 'ssp-image-mode)
;;     (remove-hook 'image-mode-hook 'ssp--flag-file-deletion)))

;;  ;; Automatic mode.

;; (defvar ssp-automatic-mode-default-delay .5)

;; (defun ssp-automatic-mode--prefix-to-delay (arg)
;;   "Compute decimal delay for integer ARG."
;;   (cond
;;    ((null arg) ssp-automatic-mode-default-delay)
;;    ((< arg 10) arg)                     ; seconds
;;    (t (/ arg 1000.0))))                 ; milliseconds

;; (setq ssp-automatic-image-mode-map
;;   (let ((km (copy-keymap ssp-image-mode-map)))
;;     (define-key km "q" 'ssp-pause)

;;     (define-key km "n" 'ssp-automatic-mode-next)
;;     (define-key km "p" 'ssp-automatic-mode-previous)
;;     (define-key km "\C-c\C-q" 'ssp-mode-quit)
;;     (define-key km "\C-c\C-r" 'ssp-automatic-mode-pause-resume)
;;     (define-key km "\\" 'ssp-automatic-mode-pause-resume)
;;     (define-key km "]" 'ssp-automatic-mode-pause-resume)
;;     (define-key km (kbd "<mouse-3>") 'ssp-automatic-mode-pause-resume)

;;     km)
;;   ;; "Keymap for SSP-AUTOMATIC-IMAGE-MODE."
;;   )

;; (define-minor-mode ssp-automatic-mode
;;   "Minor mode for dired-based image slideshow."
;;   nil nil nil
;;   :global t

;;   (if ssp-automatic-mode
;;       ;; Enabling
;;       (progn
;;         (setq ssp-automatic-mode--paused nil)
;;         (add-hook 'image-mode-hook 'ssp-automatic-image-mode)
;;         (ssp-mode 1))

;;     ;; Disabling
;;     (ssp-mode -1)
;;     (remove-hook 'image-mode-hook 'ssp-automatic-image-mode)))

;; (defvar ssp-automatic-image-mode--timer nil
;;   "Timer for advancing to the next image.")

;; (defvar ssp-automatic-image-mode--delay ssp-automatic-mode-default-delay)

;; (defvar ssp-automatic-mode--paused nil)

;; (define-minor-mode ssp-automatic-image-mode
;;   "Minor mode for dired-based image slideshow."
;;   nil nil
;;   ssp-automatic-image-mode-map

;;   (if ssp-automatic-image-mode
;;       ;; Enable
;;       (unless ssp-automatic-mode--paused
;;         (let ((image-buffer (current-buffer)))
;;           (setq ssp-automatic-image-mode--timer
;;                 (run-with-timer ssp-automatic-image-mode--delay nil
;;                                 (lambda ()
;;                                   (if (and (not ssp-automatic-mode--paused) (buffer-live-p image-buffer))
;;                                       (with-current-buffer image-buffer
;;                                         (ssp-find-next))))))))

;;     ;; Disable
;;     (ssp-automatic-image-mode--cancel)))

;; (defun ssp-automatic-image-mode--cancel ()
;;   (when ssp-automatic-image-mode--timer
;;     (cancel-timer ssp-automatic-image-mode--timer)))

;; (defun ssp-automatic-mode-pause-resume (&optional arg)
;;   "Pause or resume automatic slideshow."
;;   (interactive "P")
;;   (setq ssp-automatic-mode--paused (not ssp-automatic-mode--paused))
;;   (unless ssp-automatic-mode--paused
;;     (ssp-find-next)))

;; (defun ssp-automatic-mode-next ()
;;   (interactive)
;;   (ssp-pause*)
;;   (ssp-find-next))

;; (defun ssp-automatic-mode-previous ()
;;   (interactive)
;;   (ssp-pause*)
;;   (ssp-find-previous))

;; (defun ssp-automatic-start (&optional arg)
;;   "Begin automatic slideshow with ARG delay between images.

;; If ARG is between 0-9, delay that many seconds.
;; If ARG is greater than 9, delay that many milliseconds."
;;   (interactive "P")
;;   (ssp-automatic-mode -1)
;;   (when (or arg (= ssp-automatic-image-mode--delay 0))
;;     (setq ssp-automatic-image-mode--delay
;;           (ssp-automatic-mode--prefix-to-delay arg)))
;;   (ssp-automatic-mode 1))

;;  ;; User helper functions

;; (defun ssp-start (arg)
;;   "Begin slideshow from dired."
;;   (interactive "p")
;;   (ssp-mode -1)
;;   (when (called-interactively-p)
;;     (setq ssp-mode--auto nil
;;           ssp-mode--loop nil))
;;   (save-excursion
;;     (when arg (goto-char (point-min)))
;;     (ssp-mode 1)))

;; (defun ssp-image-start ()
;;   "Begin slideshow from an image."
;;   (interactive)
;;   (let* ((bfn (buffer-file-name))
;;          (barename (file-name-nondirectory bfn)))
;;     (with-current-buffer (dired bfn)
;;       (save-excursion
;;         (goto-char (point-min))
;;         (unless (search-forward barename)
;;           (error "Can't find image?!"))
;;         (ssp-start t)))))

;; (defun ssp-mode-quit ()
;;   "Stop SSP mode."
;;   (interactive)
;;   (ssp-mode -1))

;; (defun ssp-start-looping (&optional arg)
;;   "Begin automatic slideshow with ARG delay between images.

;; If ARG is between 0-9, delay that many seconds.
;; If ARG is greater than 9, delay that many milliseconds."
;;   (interactive "P")
;;   (ssp-automatic-mode -1)
;;   (when (or arg (= ssp-automatic-image-mode--delay 0))
;;     (setq ssp-automatic-image-mode--delay
;;           (ssp-automatic-mode--prefix-to-delay arg)))
;;   (setq ssp-mode--loop t)
;;   (ssp-automatic-mode 1))

;; (defun ssp-start-deleting (&optional arg)
;;   "Begin automatic deleting slideshow with ARG delay between images.

;; If ARG is between 0-9, delay that many seconds.
;; If ARG is greater than 9, delay that many milliseconds."
;;   (interactive "P")

;;   (ssp-automatic-mode -1)
;;   (when (or arg (= ssp-automatic-image-mode--delay 0))
;;     (setq ssp-automatic-image-mode--delay
;;           (ssp-automatic-mode--prefix-to-delay arg)))
;;   (add-hook 'image-mode-hook 'ssp--flag-file-deletion)
;;   (ssp-automatic-mode 1))

;; (defun ssp-find-next ()
;;   "Replace the current image with the next one."
;;   (interactive)

;;   (if-let ((next (ssp-mode--navigate 1)))
;;       (find-alternate-file next)
;;     (message "End of images.")
;;     (pop-to-buffer (get-buffer ssp-mode--dired-buffer))
;;     (if ssp-automatic-image-mode
;;         (ssp-automatic-image-mode -1)
;;       (ssp-mode -1))))

;; (defun ssp-mode--navigate (arg)
;;   "Move forwards or backwards ARG images in `ssp-mode--dired-buffer'."
;;   (if-let ((file (ssp-mode--navigate* arg)))
;;       file
;;     (when ssp-mode--loop
;;       (setq ssp-mode--position (if (< arg 0) (point-max) (point-min)))
;;       (ssp-mode--navigate* arg))))

;; (defun ssp-pause* ()
;;   (setq ssp-automatic-mode--paused t)
;;   (ssp-automatic-image-mode--cancel))

;; (defun ssp-pause ()
;;   "Stop the automatic slideshow and bury the current buffer."
;;   (interactive)
;;   (ssp-pause*)
;;   (bury-buffer))

;; (defun ssp-find-previous ()
;;   "Replace the current image with the previous one."
;;   (interactive)

;;   (if-let ((image (ssp-mode--navigate -1)))
;;       (find-alternate-file image)
;;     (message "No previous image.")
;;     (ssp-mode -1)))

;; (defun ssp--mark-with-char (char &optional force)
;;   "Mark an image with CHAR.  Doesn't replace marks, unless FORCE is non-nil."
;;   (with-current-buffer ssp-mode--dired-buffer
;;     (save-excursion
;;       (goto-char ssp-mode--position)
;;       (beginning-of-line)
;;       (when (equal ?\040 (following-char))
;;         (let ((dired-marker-char char))
;;           (dired-mark nil nil))))))

;; (defun ssp--flag-file-deletion ()
;;   "Flag the current file for deletion."
;;   (ssp--mark-with-char "D"))

;; (defun ssp-flag-file-deletion-and-next ()
;;   "Flag the current image for deletion in its dired buffer.

;; Advances to the next image, unless an automatic slideshow is
;; currently playing."
;;   (interactive)
;;   (ssp--mark-with-char "D" t)
;;   (unless (and ssp-automatic-mode (not ssp-automatic-mode--paused))
;;     (ssp-find-next)))

;; (defun ssp-mark-and-next ()
;;   "Mark the current image in its dired buffer.

;; Advances to the next image, unless an automatic slideshow is
;; currently playing."
;;   (interactive)
;;   (ssp--mark-with-char "*" t)
;;   (unless ssp-automatic-mode (ssp-find-next)))

;; (defun ssp-unmark-and-next ()
;;   "Mark the current image in its dired buffer.

;; Advances to the next image, unless an automatic slideshow is
;; currently playing."
;;   (interactive)
;;   (with-current-buffer ssp-mode--dired-buffer
;;     (goto-char ssp-mode--position)
;;     (dired-unmark nil nil))
;;   (unless ssp-automatic-mode (ssp-find-next)))

;; (defun ssp-categorize-and-next ()
;;   "Mark this image, using the current input command as the character.

;; Advances to the next image, unless an automatic slideshow is
;; currently playing."
;;   (interactive)
;;   (ssp--mark-with-char last-command-event t)
;;   (unless ssp-automatic-mode (ssp-find-next)))

(provide 'ssp)
;;; ssp.el ends here
