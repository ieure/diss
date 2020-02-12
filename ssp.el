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

(defclass ssp-slideshow nil
  ((step :initform 1
         :initarg :step
         :documentation "How many images to advance by when navigating.")
   (loop :initform nil
         :initarg :loop
         :documentation "When non-NIL, slideshow will start over when the end is reached.")
   (mark :initform nil
         :initarg :mark
         :documentation "When non-NIL, mark images with this character when automatically advancing.")
   (delay :initform nil
          :initarg :delay
          :documentation "How long to display an image for before advancing to the next.")
   (paused :initform t
           :initarg :paused
           :documentation "When NIL, images will advance automatically after DELAY.")

   (current :initform nil
            :initarg nil
            :documentation "The image currently being shown.")
   (buffer :initform nil
           :initarg :buffer
           :documentation "Dired buffer used as the source of images."))

  :allow-nil-initform t)

(cl-defmethod ssp-slideshow-active? ((this ssp-slideshow))
  (buffer-live-p (oref this buffer)))

(defvar ssp--active nil
  "List of active slideshows.  Each element is a cons cell
  of (IMAGE . SSP-BUFFER).")

(defvar ssp--timer nil)
(make-variable-buffer-local 'ssp--timer)

(defvar ssp-image-mode--slideshow nil
  "The slideshow this image buffer is associated with.")
(make-variable-buffer-local 'ssp-image-mode--slideshow)

(defvar ssp-mode--slideshow nil
  "The slideshow in this SSP-MODE buffer.")
(make-variable-buffer-local 'ssp-mode--slideshow)

(defvar ssp-mode--image-regexp (image-file-name-regexp)
  "Regular expression matching images.")
(make-variable-buffer-local 'ssp-mode--image-regexp)

(defun ssp--cleanup ()
  "Clean up ended slideshows."
  (setq ssp--active
        (cl-loop for ss in ssp--active
                 when (ssp-slideshow-active? ss)
                 collect ss)))

(defvar ssp-mode-map
  (let ((km (make-composed-keymap '(dired-mode-map))))
    (define-key km "x" 'ssp-do-flagged-delete)
    (define-key km "\C-c\C-r" 'ssp-resume)))


(define-derived-mode ssp-mode dired-mode "SSP"
  "Major mode for dired slideshows."
  (add-hook 'image-mode-hook 'ssp-image-mode--maybe-enable))

(defun ssp-mode--dired-expanded-filename ()
  (if-let ((fn (dired-get-filename t t)))
      (expand-file-name fn) ""))

(defun ssp-mode--ensure (ss filename)
  "Ensure that the dired buffer is pointing at FILENAME."
  (with-current-buffer (oref ss buffer)
    (cl-loop with bfn = (file-name-nondirectory filename)
             with efn = (expand-file-name filename)
             with searching = nil
             with not-found = nil
             for found = (string= efn (ssp-mode--dired-expanded-filename))
             until (or found not-found)
             unless searching
             do (progn (goto-char (point-min)) (setf searching t))
             do (setq not-found (not (search-forward bfn nil t)))
             finally return found)))

(defun ssp-mode--mark (ss filename char &optional force)
  "Mark FILENAME with CHAR."
  (ssp-mode--ensure ss filename)
  (with-current-buffer (oref ss buffer)

    (when (or force                     ; Replace mark
              (save-excursion
                (beginning-of-line)
                (equal ?\040 (following-char)))) ; File is unmarked

      (let ((dired-marker-char char))
        (dired-mark nil nil)))))

(defun ssp-mode--unmark (ss filename)
  "Unmark FILENAME."
  (ssp-mode--ensure ss filename)
  (with-current-buffer (oref ss buffer)
    (dired-unmark nil nil)))

(defun ssp-start* (&rest args)
  (unless (eq major-mode 'dired-mode)
    (error "Must be started from Dired."))

  (ssp--cleanup)

  (let ((dsal dired-subdir-alist))
    (switch-to-buffer (clone-indirect-buffer (format "*ssp %s*" dired-directory) nil))
    (ssp-mode)
    (setq dired-subdir-alist dsal)
    (goto-char (point-min))
    (add-to-list 'ssp--active (setq ssp-mode--slideshow (apply 'ssp-slideshow :buffer (current-buffer) args)))
    (if-let ((file (ssp-mode--navigate ssp-mode--slideshow)))
        (find-file file)
      (error "Found no file in SSP buffer?!"))))

(defun ssp-start ()
  (interactive)
  (ssp-start* :step 1))

(defun ssp-start-looping ()
  (interactive)
  (ssp-start* :step 1 :delay .5 :paused nil :loop t))

(defun ssp-start-automatic ()
  (interactive)
  (ssp-start* :step 1 :delay .5 :paused nil))

(defun ssp-start-deleting ()
  (interactive)
  (ssp-start* :step 1 :delay .5 :paused nil :mark ?D))

(define-key dired-mode-map "\C-c\C-s" 'ssp-start)
(define-key dired-mode-map "\C-c\C-a" 'ssp-start-automatic)
(define-key dired-mode-map "\C-c\C-l" 'ssp-start-looping)
(define-key dired-mode-map "\C-c\C-d" 'ssp-start-deleting)

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
    (when (oref ssp-mode--slideshow loop)
      (goto-char (if (< arg 0) (point-max) (point-min)))
      (ssp-mode--navigate** arg))))

(defun ssp-mode--navigate (ss &optional arg)
  "Move forwards or backwards ARG images."
  (let ((arg (or arg (oref ss step))))
    (with-slots (current buffer) ss
      (when-let ((file (with-current-buffer buffer
                         (ssp-mode--navigate* arg))))
        ;; Update state
        (setf current (expand-file-name file))

        ;; Adjust window point
        (ssp-mode--map-windows-displaying
         (current-buffer)
         (lambda (window) (set-window-point window (point))))

        ;; Return file
        file))))

(defun ssp-do-flagged-delete ()
  (interactive)
  (let ((delete-by-moving-to-trash (if current-prefix-arg (not delete-by-moving-to-trash)
                                     delete-by-moving-to-trash)))
    (dired-do-flagged-delete)))

(defun ssp-resume ()
  (interactive)
  (with-slots (current) ssp-mode--slideshow
    (if-let ((buf (get-file-buffer current)))
        (switch-to-buffer buf)
      (find-file current))))

(defun ssp-move-here ()
  (interactive)
  (with-slots (current) ssp-mode--slideshow
    (setf current (ssp-mode--dired-expanded-filename))
    (ssp-resume)))

 ;; Minor mode for images in the slideshow.

(defvar ssp-image-mode-map
  (let ((km (make-sparse-keymap)))
    (define-key km "p" 'ssp-image-mode-previous)
    (define-key km (kbd "<mouse-1>") 'ssp-image-mode-previous)
    (define-key km "n" 'ssp-image-mode-next)
    (define-key km "q" 'ssp-image-mode-quit)

    (define-key km "\\" 'ssp-image-mode-toggle-paused)
    (define-key km (kbd "<mouse-3>") 'ssp-image-mode-toggle-paused)

    (define-key km "u" 'ssp-image-mode-unmark-and-next)
    (define-key km "d" 'ssp-image-mode-delete-and-next)
    (define-key km "m" 'ssp-image-mode-mark-and-next)

    (define-key km "ca" 'ssp-image-mode-categorize-and-next)
    (define-key km "cb" 'ssp-image-mode-categorize-and-next)
    (define-key km "cc" 'ssp-image-mode-categorize-and-next)
    (define-key km "cd" 'ssp-image-mode-categorize-and-next)
    (define-key km "ce" 'ssp-image-mode-categorize-and-next)
    (define-key km "cf" 'ssp-image-mode-categorize-and-next)
    (define-key km "cg" 'ssp-image-mode-categorize-and-next)
    (define-key km "ch" 'ssp-image-mode-categorize-and-next)
    (define-key km "ci" 'ssp-image-mode-categorize-and-next)
    (define-key km "cj" 'ssp-image-mode-categorize-and-next)
    (define-key km "ck" 'ssp-image-mode-categorize-and-next)
    (define-key km "cl" 'ssp-image-mode-categorize-and-next)
    (define-key km "cm" 'ssp-image-mode-categorize-and-next)
    (define-key km "cn" 'ssp-image-mode-categorize-and-next)
    (define-key km "co" 'ssp-image-mode-categorize-and-next)
    (define-key km "cp" 'ssp-image-mode-categorize-and-next)
    (define-key km "cq" 'ssp-image-mode-categorize-and-next)
    (define-key km "cr" 'ssp-image-mode-categorize-and-next)
    (define-key km "cs" 'ssp-image-mode-categorize-and-next)
    (define-key km "ct" 'ssp-image-mode-categorize-and-next)
    (define-key km "cu" 'ssp-image-mode-categorize-and-next)
    (define-key km "cv" 'ssp-image-mode-categorize-and-next)
    (define-key km "cw" 'ssp-image-mode-categorize-and-next)
    (define-key km "cx" 'ssp-image-mode-categorize-and-next)
    (define-key km "cy" 'ssp-image-mode-categorize-and-next)
    (define-key km "cz" 'ssp-image-mode-categorize-and-next)

    km)
  "Keymap for SSP-IMAGE-MODE.")


(defun ssp-image-mode--automatic (ss image-buffer)
  "Automatically advance to the next image."
  (with-slots (paused mark) ss
    (when (and (ssp-slideshow-active? ss)
               (not paused))
      (when mark
        (ssp-mode--mark ss (buffer-file-name) mark))
      (with-current-buffer image-buffer
        (ssp--move ssp-image-mode--slideshow (oref ss step))))))

(define-minor-mode ssp-image-mode
  "Minor mode for dired-based image slideshow."
  nil "SSPi"
  ssp-image-mode-map

  (when ssp-image-mode
    (let ((image-buffer (current-buffer))
          (ss ssp-image-mode--slideshow))
      (with-slots (paused delay) ss
        (when (and delay (not paused))
          (thread-last (apply-partially 'ssp-image-mode--automatic ss image-buffer)
            (run-with-timer delay nil)
            (setq ssp--timer)))))))

(defun ssp-image-mode--slideshow-for (file)
  (cl-loop for ss in ssp--active
           with file = (expand-file-name file)
           if (string= file (oref ss current))
           return ss))

(defun ssp-image-mode--maybe-enable ()
  "Enable SSP-IMAGE-MODE if this image is part of a current slideshow."
  (when-let ((slideshow (ssp-image-mode--slideshow-for (buffer-file-name))))
    (when (ssp-slideshow-active? slideshow)
      (setq ssp-image-mode--slideshow slideshow)
      (ssp-image-mode 1))))

(defun ssp--move (ss &optional arg)
  (ssp-mode--ensure ss (buffer-file-name))
  (let ((arg (or arg (oref ss step))))
    (if-let ((next-file (ssp-mode--navigate ss arg)))
        (find-alternate-file next-file)
      (pop-to-buffer (oref ss buffer)))))

(defun ssp-image-mode-next (&optional arg)
  "Move ARG images forward in the slideshow."
  (interactive "p")
  (oset ssp-image-mode--slideshow paused t)
  (ssp--move ssp-image-mode--slideshow arg))

(defun ssp-image-mode-previous (&optional arg)
  "Move ARG images back in the slideshow."
  (interactive "p")
  (oset ssp-image-mode--slideshow paused t)
  (ssp--move ssp-image-mode--slideshow (* -1 arg)))

(defun ssp-image-mode-categorize-and-next ()
  (interactive)
  (ssp-mode--mark last-command-event t)
  (ssp-image-mode-next))

(defun ssp-image-mode-mark-and-next ()
  (interactive)
  (ssp-mode--mark ssp-image-mode--slideshow (buffer-file-name) ?m t)
  (ssp-image-mode-next))

(defun ssp-image-mode-unmark-and-next ()
  (interactive)
  (ssp-mode--unmark ssp-image-mode--slideshow (buffer-file-name))
  (ssp-image-mode-next))

(defun ssp-image-mode-delete-and-next ()
  (interactive)
  (ssp-mode--mark ssp-image-mode--slideshow (buffer-file-name) ?D t)
  (ssp-image-mode-next))

(defun ssp-image-mode-quit (&optional arg)
  (interactive "p")
  (oset ssp-image-mode--slideshow paused t)
  (bury-buffer))

(defun ssp-image-mode-categorize-and-next ()
  (interactive)
  (ssp-mode--mark ssp-image-mode--slideshow (buffer-file-name) last-command-event t)
  (ssp-image-mode-next))

(defun ssp-image-mode-toggle-paused ()
  (interactive)
  (with-slots (paused step) ssp-image-mode--slideshow
    (if (setf paused (not paused))
        (cancel-timer ssp--timer)
      (ssp-image-mode--automatic ssp-image-mode--slideshow (current-buffer)))))

(provide 'ssp)
;;; ssp.el ends here
