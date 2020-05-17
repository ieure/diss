;;; diss.el --- Dired Image Slideshow                 -*- lexical-binding: t; -*-

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
(require 'dired-aux)

(defgroup diss nil "Dired Image Slideshow")

(defcustom diss-sort-destinations nil
  "Diss image sorting destinations.

Alist of Dired marker character to directory.  When `DISS-SORT' is called, images with the corresponding mark are moved to that directory."
  :group 'diss
  :type '(alist :key-type character :value-type directory))

(defclass diss-slideshow nil
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

  :allow-nil-initform t
  :documentation "Class representing an in-progress slideshow.")

(cl-defmethod diss-slideshow-active? ((this diss-slideshow))
  "Returns non-NIL if THIS is an active slideshow."
  (buffer-live-p (oref this buffer)))

(cl-defmethod diss-slideshow-pause! ((this diss-slideshow))
  "Pause slideshow THIS."
  (with-slots (paused) diss-image-mode--slideshow
    (setf paused t)
    (when diss--timer
      (cancel-timer diss--timer))))

(defvar diss--active nil
  "List of active slideshows.  Each element is a cons cell
  of (IMAGE . DISS-BUFFER).")

(defconst diss--prefix-name-regexp "^\\([^_]+\\)_"
  "Regular expression which matches prefix names.")

(defvar diss--timer nil
  "Timer used to automatically advance images.")
(make-variable-buffer-local 'diss--timer)

(defvar diss-image-mode--slideshow nil
  "The slideshow object this `DISS-IMAGE-MODE' buffer is associated with.")
(make-variable-buffer-local 'diss-image-mode--slideshow)

(defvar diss-mode--slideshow nil
  "The slideshow in this `DISS-MODE' buffer.")
(make-variable-buffer-local 'diss-mode--slideshow)

(defvar diss-mode--image-regexp (image-file-name-regexp)
  "Regular expression matching images.")
(make-variable-buffer-local 'diss-mode--image-regexp)

(defun diss--cleanup ()
  "Clean up inactive slideshows."
  (setq diss--active
        (cl-loop for ss in diss--active
                 when (diss-slideshow-active? ss)
                 collect ss)))

(defvar diss-mode-map
  (let ((km (make-composed-keymap '(dired-mode-map))))
    (define-key km "x" 'diss-do-flagged-delete)
    (define-key km "\C-c\C-r" 'diss-resume)
    (define-key km (kbd "RET") 'diss-move-here)
    (define-key km "\C-c\C-c" 'diss-sort)
    km)
  "Keymap for DISS-MODE.")

(define-derived-mode diss-mode dired-mode "DISS"
  "Major mode for Dired Image Slideshows."
  (add-hook 'image-mode-hook 'diss-image-mode--maybe-enable))

(defun diss-mode--dired-expanded-filename ()
  "Return the expanded filename of the current dired item."
  (if-let ((fn (dired-get-filename t t)))
      (expand-file-name fn) ""))

(defun diss-mode--ensure (ss filename)
  "Ensure that slideshow SS's dired buffer has FILENAME selected."
  (with-current-buffer (oref ss buffer)
    (cl-loop with bfn = (file-name-nondirectory filename)
             with efn = (expand-file-name filename)
             with searching = nil
             with not-found = nil
             for found = (string= efn (diss-mode--dired-expanded-filename))
             until (or found not-found)
             unless searching
             do (progn (goto-char (point-min)) (setf searching t))
             do (setq not-found (not (search-forward bfn nil t)))
             finally return found)))

(defun diss-mode--mark (ss filename char &optional force)
  "Mark slideshow FILENAME in slideshow SS with CHAR.

If the file already has a mark, do nothing, unless FORCE is
non-NIL."
  (diss-mode--ensure ss filename)
  (with-current-buffer (oref ss buffer)
    (when (or force                     ; Replace mark
              (save-mark-and-excursion
                (beginning-of-line)
                (equal ?\040 (following-char)))) ; File is unmarked

      (let ((dired-marker-char char)
            (mark-active nil))          ; Never mark a region
        (dired-mark nil nil)))))

(defun diss-mode--unmark (ss filename)
  "Unmark FILENAME in slideshow SS."
  (diss-mode--ensure ss filename)
  (with-current-buffer (oref ss buffer)
    (dired-unmark nil nil)))

(defun diss-start* (&rest args)
  "Start a slideshow."

  (unless (eq major-mode 'dired-mode)
    (error "Must be started from Dired."))

  (diss--cleanup)

  (let ((dsal dired-subdir-alist))
    (switch-to-buffer (clone-indirect-buffer (format "*diss %s*" dired-directory) nil))
    (diss-mode)
    (setq-local dired-subdir-alist dsal)
    (goto-char (point-min))
    (add-to-list 'diss--active (setq diss-mode--slideshow (apply 'diss-slideshow :buffer (current-buffer) args)))
    (if-let ((file (diss-mode--navigate diss-mode--slideshow)))
        (find-file file)
      (error "Found no file in DISS buffer?!"))))

(defun diss-start (&optional arg)
  "Start a slideshow from a Dired buffer."
  (interactive "p")
  (diss-start* :step (or arg 1)))

(defun diss-start-automatic ()
  "Start an automatically advancing slideshow from a Dired buffer."
  (interactive)
  (diss-start* :step 1 :delay .35 :paused nil))

(defun diss-start-looping ()
  "Start an automatically advancing looping slideshow from a Dired buffer."
  (interactive)
  (diss-start* :step 1 :delay .35 :paused nil :loop t))

(defun diss-start-deleting ()
  "Start an automatically advancing slideshow that deletes files from a Dired buffer."
  (interactive)
  (diss-start* :step 1 :delay .35 :paused nil :mark ?D))

(define-key dired-mode-map "\C-c\C-s" 'diss-start)
(define-key dired-mode-map "\C-c\C-a" 'diss-start-automatic)
(define-key dired-mode-map "\C-c\C-l" 'diss-start-looping)
(define-key dired-mode-map "\C-c\C-d" 'diss-start-deleting)

(defun diss-mode--map-windows-displaying (buffer fn)
  "Apply FN to every window displaying BUFFER."
  (cl-loop for frame in (frame-list)
           do (cl-loop for window in (window-list frame)
                       when (eq buffer (window-buffer window))
                       do (funcall fn window))))

(defun diss-mode--navigate** (arg)
  "Move forwards or backwards by ARG images (helper)."
  (save-match-data
    (cl-loop with n = (abs arg)
             with a = (if (< arg 0) -1 1)
             until (or (if (< arg 0) (bobp) (eobp))
                       (zerop n))
             do (dired-next-line a)
             for fn = (dired-get-filename t t)
             when (string-match diss-mode--image-regexp (or fn ""))
             do (decf n)
             finally return fn)))

(defun diss-mode--navigate* (arg)
  "Move forwards or backwards by ARG images (helper)."
  (if-let ((file (diss-mode--navigate** arg)))
      (expand-file-name file)
    (when (oref diss-mode--slideshow loop)
      (goto-char (if (< arg 0) (point-max) (point-min)))
      (diss-mode--navigate** arg))))

(defun diss-mode--navigate (ss &optional arg)
  "Move forwards or backwards by ARG images."
  (let ((arg (or arg (oref ss step))))
    (with-slots (current buffer) ss
      (when-let ((file (with-current-buffer buffer
                         (diss-mode--navigate* arg))))
        ;; Update state
        (setf current file)

        ;; Adjust window point
        (diss-mode--map-windows-displaying
         (current-buffer)
         (lambda (window) (set-window-point window (point))))

        ;; Return file
        file))))

(defun diss-mode--prefix-names ()
  (save-excursion
    (save-match-data
      (let ((prefixes)
            (re (format "%s.*%s" diss--prefix-name-regexp diss-mode--image-regexp)))
        (dired-map-dired-file-lines
         (lambda (file)
           (let ((file (file-name-nondirectory file)))
             (when (string-match re file)
               (let ((pn (substring-no-properties (match-string 1 file))))
                 (add-to-list 'prefixes pn))))))
        prefixes))))

(defun diss-mode--prefix-name (file)
  "Return the prefix name of FILE."
  (save-match-data
    (let ((s (file-name-nondirectory file)))
      (or (when (string-match diss--prefix-name-regexp s)
            (match-string 1 s))
          ""))))

(defun diss-do-flagged-delete (arg)
  "Delete files."
  (interactive "P")

  ;; Handle cases where the current file is flagged for deletion.
  (save-excursion
    (goto-char (point-max))
    (condition-case nil
        (progn
          (dired-prev-marked-file 1)
          (dired-next-line 1)
          (oset diss-mode--slideshow current (diss-mode--dired-expanded-filename)))
      (error (diss-mode--navigate diss-mode--slideshow))))

  (let ((delete-by-moving-to-trash (if current-prefix-arg (not delete-by-moving-to-trash)
                                     delete-by-moving-to-trash)))
    (dired-do-flagged-delete)))

(defun diss-resume ()
  "Resume a paused slideshow."
  (interactive)
  (with-slots (current) diss-mode--slideshow
    (if-let ((buf (get-file-buffer current)))
        (switch-to-buffer buf)
      (find-file current))))

(defun diss-move-here ()
  "Move current position to the selected image."
  (interactive)
  (with-slots (current) diss-mode--slideshow
    (setf current (diss-mode--dired-expanded-filename))
    (diss-resume)))

(defun diss--sort* ()
  (let ((dests-files)
        (num-files 0))
    (dired-map-dired-file-lines
     (lambda (file)
       (let ((mark (save-mark-and-excursion
                     (beginning-of-line)
                     (following-char))))
         (when-let ((dest (cdr (assoc mark diss-sort-destinations))))
           (unless (assoc dest dests-files)
             (push (list dest) dests-files))
           (push file (cdr (assoc dest dests-files)))
           (incf num-files)))))
    (cons dests-files num-files)))

(defun diss-sort ()
  (interactive)
  (cl-destructuring-bind (dests-files . num-files) (diss--sort*)
    (when (and (> num-files 0)
               (y-or-n-p (format "Sort %d file%s? " num-files (if (> num-files 1) "s" ""))))
      (thread-first
          (lambda (dest-files)
            (cl-destructuring-bind (dest &rest files) dest-files
              (unless (and (file-exists-p dest) (eq t (file-attribute-type (file-attributes dest))))
                (make-directory dest t))

              (mapc
               (lambda (file)
                 (dired-rename-file file (concat dest "/" (file-name-nondirectory file)) nil))
               files)))

        (mapc dests-files)))))

 ;; Minor mode for images in the slideshow.

(defvar diss-image-mode-map
  (let ((km (make-sparse-keymap)))
    (define-key km "p" 'diss-image-mode-previous)
    (define-key km (kbd "<mouse-1>") 'diss-image-mode-previous)
    (define-key km "n" 'diss-image-mode-next)
    (define-key km "q" 'diss-image-mode-quit)

    (define-key km "e" 'diss-image-mode-set-prefix-name-and-advance)

    (define-key km "\\" 'diss-image-mode-toggle-paused)
    (define-key km (kbd "<mouse-3>") 'diss-image-mode-toggle-paused)

    (define-key km "u" 'diss-image-mode-unmark-and-next)
    (define-key km "d" 'diss-image-mode-delete-and-next)
    (define-key km "m" 'diss-image-mode-mark-and-next)

    (define-key km "x" 'diss-image-mode-fit-to-width)
    (define-key km "y" 'diss-image-mode-fit-to-height)
    (define-key km "f" 'diss-image-mode-smart-fit)
    (define-key km "r" 'diss-image-mode-rotate-cw)
    (define-key km "R" 'diss-image-mode-rotate-ccw)

    (define-key km "0" 'image-transform-reset)

    (define-key km "c" 'diss-image-mode-categorize-and-next)
    km)
  "Keymap for DISS-IMAGE-MODE.")

(defun diss-image-mode--window-size ()
  "Returns (WIDTH . HEIGHT) of the current selected window."
  (cl-destructuring-bind (left top right bottom) (window-edges nil t nil t)
    (cons (- right left) (- bottom top))))

(defun diss-image-mode--rotation (direction)
  "Return rotation angle, in degrees.

If DIRECTION is positive, rotates clockwise, otherwise rotates
counterclockwise."
  (thread-first
      (signum direction)
    (* 90)
    (+ image-transform-rotation )
    (truncate)
    (% 360)
    (float)))

(defun diss-image-mode--rotate (direction)
  "Rotate image (helper)."
  (setq-local image-transform-rotation (diss-image-mode--rotation direction))
  (image-toggle-display-image))

(defun diss-image-mode-rotate-cw ()
  "Rotate image clockwise."
  (interactive)
  (diss-image-mode--rotate 1))

(defun diss-image-mode-rotate-ccw ()
  "Rotate image counterclockwise."
  (interactive)
  (diss-image-mode--rotate -1))

(defun diss-image-mode-fit-to-height ()
  "Fit the current image to the height of the current window."
  (interactive)
  (setq-local image-transform-resize 'fit-height)
  (image-toggle-display-image))

(defun diss-image-mode-fit-to-width ()
  "Fit the current image to the width of the current window."
  (interactive)
  (setq-local image-transform-resize 'fit-width)
  (image-toggle-display-image))

(defun diss-image-mode-smart-fit ()
  "Fit the image to the window it's displayed in."
  (interactive)
  (cl-destructuring-bind ((iw . ih) (ww . wh))
      (list (image-size (image-get-display-property) t)
            (diss-image-mode--window-size))
    (let ((img-aspect (/ iw (float ih)))
          (win-aspect (/ ww (float wh))))
      (if (>= img-aspect win-aspect)
          (diss-image-mode-fit-to-width)
        (diss-image-mode-fit-to-height)))))

(defun diss-image-mode-set-prefix-name-and-advance (prefix-name &optional marker)
  "Apply PREFIX-NAME to current file.

With prefix arg, prompt for marker char, and mark file."
  (interactive
   (list
    (completing-read
     "Name: "
     (with-current-buffer (oref diss-image-mode--slideshow buffer)
       (diss-mode--prefix-names))
     nil
     nil
     (diss-mode--prefix-name (buffer-file-name)))
    (when current-prefix-arg (read-char-exclusive "Category char: " t))))

  (let* ((bfn (buffer-file-name))
         (new-name (concat prefix-name "_" (file-name-nondirectory bfn))))
    (diss-image-mode-next)
    (with-current-buffer (oref diss-image-mode--slideshow buffer)
      (dired-rename-file bfn new-name nil)
      (dired-add-entry new-name (or marker (dired-file-marker bfn)) t))))

(defun diss-image-mode--automatic (ss image-buffer)
  "Automatically advance buf IMAGE-BUFFER to the next image in slideshow SS."
  (with-slots (paused mark) ss
    (when (and (diss-slideshow-active? ss)
               (not paused)
               (not (minibufferp)))
      (with-current-buffer image-buffer
        (when mark
          (diss-mode--mark ss (buffer-file-name) mark))
        (diss--move diss-image-mode--slideshow (oref ss step))))))

(defun diss-image-mode--automatic-move (ss image-buffer)
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
        (setf paused t)))))

(define-minor-mode diss-image-mode
  "Minor mode for Dired Image Slideshow."
  nil "DISS"
  diss-image-mode-map

  (when diss-image-mode
    (let ((image-buffer (current-buffer))
          (ss diss-image-mode--slideshow))
      (diss-image-mode-smart-fit)
      (with-slots (paused delay) ss
        (when (and delay (not paused))
          (setq diss--timer
                (run-with-timer
                 delay nil
                 (lambda ()
                   (diss-image-mode--automatic-move ss image-buffer)))))))))

(defun diss-image-mode--slideshow-for (file)
  "Return the slideshow instance with FILE as its current image."
  (cl-loop for ss in diss--active
           with file = (expand-file-name file)
           if (string= file (oref ss current))
           return ss))

(defun diss-image-mode--maybe-enable ()
  "Enable DISS-IMAGE-MODE if this image is part of a current slideshow."
  (when-let ((slideshow (diss-image-mode--slideshow-for (buffer-file-name))))
    (when (diss-slideshow-active? slideshow)
      (setq diss-image-mode--slideshow slideshow)
      (diss-image-mode 1))))

(defun diss--move (ss &optional arg)
  (diss-mode--ensure ss (buffer-file-name))
  (let ((arg (or arg (oref ss step)))
        (image-transform-resize nil))   ; Don't resize on open
    (if-let ((next-file (diss-mode--navigate ss arg)))
        (progn
          (when-let ((buf (find-buffer-visiting next-file)))
            (kill-buffer buf))
          (find-alternate-file next-file))
      (pop-to-buffer (oref ss buffer)))))

(defun diss-image-mode-next (&optional arg)
  "Move ARG images forward in the slideshow."
  (interactive "p")
  (diss--move diss-image-mode--slideshow arg))

(defun diss-image-mode-previous (&optional arg)
  "Move ARG images back in the slideshow."
  (interactive "p")
  (diss-slideshow-pause! diss-image-mode--slideshow)
  (diss--move diss-image-mode--slideshow (* -1 arg)))

(defun diss-image-mode-categorize (char)
  "Mark the current image with CHAR."
  (interactive (list (read-char-exclusive "Category char: " t)))
  (when char
    (diss-mode--mark diss-image-mode--slideshow (buffer-file-name) char t)))

(defun diss-image-mode-categorize-and-next (char)
  "Mark the current image with CHAR and advance."
  (interactive (list (read-char-exclusive "Category char: " t)))
  (when char
    (diss-mode--mark diss-image-mode--slideshow (buffer-file-name) char t)
    (diss-image-mode-next)))

(defun diss-image-mode-mark-and-next ()
  "Mark the current image and advance."
  (interactive)
  (diss-mode--mark diss-image-mode--slideshow (buffer-file-name) ?* t)
  (diss-image-mode-next))

(defun diss-image-mode-unmark-and-next ()
  "Remove marks from the current image and advance."
  (interactive)
  (diss-mode--unmark diss-image-mode--slideshow (buffer-file-name))
  (diss-image-mode-next))

(defun diss-image-mode-delete-and-next ()
  "Flag the current image for deletion and advance."
  (interactive)
  (diss-mode--mark diss-image-mode--slideshow (buffer-file-name) ?D t)
  (diss-image-mode-next))

(defun diss-image-mode-quit (&optional arg)
  "Pause the slideshot and bury the current buffer."
  (interactive "p")
  (oset diss-image-mode--slideshow paused t)
  (bury-buffer))

(defun diss-image-mode-toggle-paused ()
  "Toggle whether the current slideshow is paused."
  (interactive)
  (with-slots (paused step) diss-image-mode--slideshow
    (if (setf paused (not paused))
        (cancel-timer diss--timer)
      (diss-image-mode--automatic diss-image-mode--slideshow (current-buffer)))))

(provide 'diss)
;;; diss.el ends here
