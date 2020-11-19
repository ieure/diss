;;; diss.el --- Dired Image Slideshow                 -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Ian Eure

;; Author: Ian Eure
;; Version: 1.0.0
;; Package-Requires: ((emacs "26.1"))
;; URL: https://github.com/ieure/diss
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

;; Diss is a dired-based image slideshow.  Press C-c C-s in a Dired
;; buffer to start it.  See README.org for more.

;;; Code:

(require 'dired)
(require 'dired-aux)

(defgroup diss nil
  "Dired Image Slideshow"
  :group 'multimedia)

(defcustom diss-sort-destinations nil
  "Diss image sorting destinations.

Alist of Dired marker character to directory.  When `DISS-SORT' is called, images with the corresponding mark are moved to that directory."
  :group 'diss
  :type '(alist :key-type character :value-type directory))

(defcustom diss-saved-configurations
  '(("standard" ((:step 1) (:paused t) (:loop nil)))
    ("autolooping" ((:step 1) (:paused nil) (:delay 0.25) (:loop t)))
    ("deleting" ((:step 1) (:paused nil) (:delay 0.25) (:mark 68) (:loop nil))))
  "List of saved configurations.

The car of each configuration is its name.  The cdr is the list
of arguments passed to constructor function `diss-slideshow'."
  :group 'diss
  :type '(alist :key-type (string :tag "Name")
                :value-type
                (list
                 (set
                  (group (const :tag "Image step" :value 1 :step) integer)
                  (group (const :tag "Loop at end" :loop) boolean)
                  (group (const :tag "Start paused" :value t :paused) boolean)
                  (group (const :tag "Delay between images" :delay) number)
                  (group (const :tag "Mark images with" :mark) character)))))

(defvar diss--last-config "standard"
  "Last used Diss configuration.")

(defvar diss--slideshow-class 'diss-slideshow
  "The class to use for the slideshow object.")

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

(cl-defmethod diss--slideshow-begin ((this diss-slideshow))
  "Begin the slideshow."
  (diss--move this nil #'find-file))

(cl-defmethod diss-slideshow-active? ((this diss-slideshow))
  "Returns non-NIL if THIS is an active slideshow."
  (buffer-live-p (oref this buffer)))

(cl-defmethod diss-slideshow-pause! ((this diss-slideshow))
  "Pause slideshow THIS."
  (with-slots (paused) diss-image-mode--slideshow
    (setf paused t)
    (when diss--timer
      (cancel-timer diss--timer))))

(cl-defmethod diss-slideshow-toggle-pause! ((this diss-slideshow))
  "Pause or resume slideshow THIS."
  (with-slots (paused) this
    (when (and (setf paused (not paused)) diss--timer)
      (cancel-timer diss--timer))
    paused))

(defvar diss--active nil
  "The list of all currently active slideshows.

Each element is a cons cell of (IMAGE . DISS-BUFFER).")

(defconst diss--prefix-name-regexp "^\\([^_]+\\)_\\(.*\\)"
  "Regular expression which matches prefix names.")

(defvar diss--prefix-name-cache nil
  "Cache of recent prefix names.")

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
    (define-key km "C" 'diss-configure)
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
  (when (diss-mode--ensure ss filename)
    (with-current-buffer (oref ss buffer)
      (when (or force                     ; Replace mark
                (save-mark-and-excursion
                  (beginning-of-line)
                  (equal ?\040 (following-char)))) ; File is unmarked

        (let ((dired-marker-char char)
              (mark-active nil))          ; Never mark a region
          (dired-mark nil nil))))))

(defun diss-mode--unmark (ss filename)
  "Unmark FILENAME in slideshow SS."
  (diss-mode--ensure ss filename)
  (with-current-buffer (oref ss buffer)
    (dired-unmark nil nil)))

(defun diss-start* (args)
  "Start a slideshow (internal helper).

Passes ARGS to the function specified in `diss--slideshow-class'."

  (unless (eq major-mode 'dired-mode)
    (error "Must be started from Dired"))

  (diss--cleanup)

  (unless diss--prefix-name-cache
    (setq diss--prefix-name-cache
          (cl-remove-duplicates (append diss--prefix-name-cache (diss--all-prefix-names)))))

  (let ((dsal dired-subdir-alist))
    (switch-to-buffer (clone-indirect-buffer (format "*diss %s*" dired-directory) nil))
    (funcall diss--slideshow-class)
    (setq-local dired-subdir-alist dsal)
    (goto-char (point-min))
    (add-to-list 'diss--active (setq diss-mode--slideshow (apply diss--slideshow-class :buffer (current-buffer) args)))

    (diss--slideshow-begin diss-mode--slideshow)))

(defun diss--slideshow->args (ss)
  "Return arguments for current configutation of slideshow SS."
  (cl-loop for slot in '(step paused delay mark loop)
           append (list (intern (format ":%s" slot))
                         (slot-value ss slot))))

(cl-defun diss-read-config (&key (step 1) (paused t) (delay .35) mark loop)
  "Read slideshow parameters."
  (interactive)
  (setf step (read-number "Step by: " step))
  (unless (setf paused (not (y-or-n-p "Advance automatically? ")))
    (setf delay (read-number "Delay between images: " delay))
    (setf mark (read-char "Mark with: ")))
  (setf loop (y-or-n-p "Loop? "))

  `(((:step ,step)
     (:paused ,paused)
     (:delay ,delay)
     (:mark ,mark)
     (:loop ,loop))))

(defun diss-configure ()
  "Re/configure the current slideshow."
  (interactive)
  (cl-loop for (k v) in (car (apply #'diss-read-config (diss--slideshow->args diss-mode--slideshow)))
           for slot = (intern (substring (symbol-name k) 1))
           do (setf (slot-value diss-mode--slideshow slot) v)))

(defun diss--maybe-configure (config-name)
  (unless (assoc config-name diss-saved-configurations)
    (push (cons config-name (diss-read-config)) diss-saved-configurations))

  (cl-reduce #'append (cadr (assoc config-name diss-saved-configurations))))

(defun diss-start (config-name)
  "Start a slideshow from a Dired buffer, using params from CONFIG-NAME."
  (interactive
   (list (completing-read
          "Configuration: "
          (mapcar #'car diss-saved-configurations) nil nil diss--last-config)))
  (setq diss--last-config config-name)
  (diss-start* (diss--maybe-configure config-name)))

(defun diss--all-prefix-names ()
  "Return a list of all prefix names Diss knows about."
  (cl-remove-duplicates
   (cl-loop for dir in (mapcar 'cdr diss-sort-destinations)
            append (mapcar 'diss-mode--prefix-name (ignore-errors (directory-files dir nil diss--prefix-name-regexp))))
   :test 'string=))

(define-key dired-mode-map "\C-c\C-s" 'diss-start)

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
  "Move forwards or backwards by ARG images in slideshow SS."
  (let ((arg (or arg (oref ss step))))
    (with-slots (current buffer) ss
      (if (zerop arg) current
        (when-let ((file (with-current-buffer buffer
                           (diss-mode--navigate* arg))))
          ;; Update state
          (setf current file)

          ;; Adjust window point
          (diss-mode--map-windows-displaying
           (current-buffer)
           (lambda (window) (set-window-point window (point))))

          ;; Return file
          file)))))

(defun diss-mode--prefix-name (file)
  "Return the prefix name of FILE."
  (car (diss-mode--prefix-and-name file)))

(defun diss-mode--prefix-and-name (file)
  "Return the prefix and filename of FILE."
  (save-match-data
    (let ((s (file-name-nondirectory file)))
      (or (when (string-match diss--prefix-name-regexp s)
            (cons (match-string 1 s) (match-string 2 s)))
          (cons "" s)))))

(defun diss--move-to-first-unmarked ()
  "Move to the first file without a marker."
  (save-excursion
    (goto-char (point-max))
    (condition-case nil
        (progn
          (dired-prev-marked-file 1)
          (dired-next-line 1)
          (oset diss-mode--slideshow current (diss-mode--dired-expanded-filename)))
      (error (diss-mode--navigate diss-mode--slideshow)))))

(defun diss-do-flagged-delete (arg)
  "Delete files.

With prefix ARG, inverts the value of var `delete-by-moving-to-trash'."
  (interactive "P")
  (diss--move-to-first-unmarked)
  (let ((delete-by-moving-to-trash (if current-prefix-arg (not delete-by-moving-to-trash)
                                     delete-by-moving-to-trash)))
    (dired-do-flagged-delete)))

(defun diss-resume ()
  "Resume a paused slideshow."
  (interactive)
  (diss--move diss-mode--slideshow 0 #'find-file))

(defun diss-move-here ()
  "Move current position to the selected image."
  (interactive)
  (with-slots (current) diss-mode--slideshow
    (setf current (diss-mode--dired-expanded-filename))
    (diss-resume)))

(defun diss--sort-destination-prefix (filename)
  "Determine the sort destination based on FILENAME's prefix name."
  (when-let ((prefix (car (diss-mode--prefix-and-name filename))))
    (cl-loop for dest in diss-sort-destinations
             if (string= prefix (file-name-nondirectory (cdr dest)))
             return (cdr dest))))

(defun diss--sort-destination-mark (mark)
  "Determine the sort destination based on MARK."
  (cdr (assoc mark diss-sort-destinations)))

(defun diss--sort-destination (mark filename)
  "Determine the sort destination based on MARK and FILENAME."
  (if-let ((mark-dest (diss--sort-destination-mark mark)))
      mark-dest
    (when (= ?* mark) (diss--sort-destination-prefix filename))))

(defun diss--sort* ()
  "Return file sorting information.

Returns a cons cell of (DESTS-FILES . NUM-FILES).

DESTS-FILES is a list of lists.  The car of each list is the
destination directory, and the cdr is the files to be moved
there."
  (let ((dests-files)
        (num-files 0))
    (dired-map-dired-file-lines
     (lambda (file)
       (let ((mark (save-mark-and-excursion
                     (beginning-of-line)
                     (following-char))))
         (when-let ((dest (diss--sort-destination mark file)))
           (unless (assoc dest dests-files)
             (push (list dest) dests-files))
           (push file (cdr (assoc dest dests-files)))
           (incf num-files)))))
    (cons dests-files num-files)))

(cl-defun diss--rename-to ((dest &rest files))
  "Rename FILES, moving them to DEST."
  (unless (and (file-exists-p dest)
               (eq t (file-attribute-type (file-attributes dest))))
    (make-directory dest t))

  (cl-loop
   for file in files
   do (dired-rename-file file
                         (concat dest "/" (file-name-nondirectory file)) t)))

(defun diss--narrow-to-marked ()
  "Narrow the buffer to the section containing marked files."
  (save-excursion
    (goto-char (point-min))
    (dired-next-marked-file 1)
    (let ((beg (line-beginning-position)))
      (goto-char (point-max))
      (dired-prev-marked-file 1)
      (narrow-to-region beg (line-end-position)))))

(defun diss-sort ()
  "Sort marked images.

Renames according to `diss-sort-destinations'."
  (interactive)
  (save-restriction
    (widen)
    (diss--narrow-to-marked)
    (cl-destructuring-bind (dests-files . num-files) (diss--sort*)
      (when (and (> num-files 0)
                 (y-or-n-p (format "Sort %d file%s? " num-files (if (> num-files 1) "s" ""))))
        (mapc #'diss--rename-to dests-files)))))

 ;; Minor mode for images in the slideshow.

(defvar diss-image-mode-map
  (let ((km (make-sparse-keymap)))
    (define-key km "p" 'diss-image-mode-previous)
    (define-key km (kbd "<mouse-1>") 'diss-image-mode-previous)
    (define-key km "n" 'diss-image-mode-next)
    (define-key km "P" 'diss-image-mode-previous-other-window)
    (define-key km "N" 'diss-image-mode-next-other-window)
    (define-key km "q" 'diss-image-mode-quit)

    (define-key km "e" 'diss-image-mode-set-prefix-name-and-next)

    (define-key km (kbd "SPC") 'diss-image-mode-toggle-paused)
    (define-key km (kbd "<mouse-3>") 'diss-image-mode-toggle-paused)

    (define-key km "u" 'diss-image-mode-unmark-and-next)
    (define-key km "d" 'diss-image-mode-delete-and-next)
    (define-key km "m" 'diss-image-mode-mark-and-next)
    (define-key km "j" 'diss-image-mode-junk-and-next)

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
  "Return (WIDTH . HEIGHT) of the current selected window."
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

(defmacro diss--without-scaling (&rest body)
  "Evaluate BODY with IMAGE-SCALING-FACTOR set to 1.0."
  `(let ((image-scaling-factor 1.0))
     ,@body))

(defun diss--toggle-display-image ()
  "Toggle image display, with scaling disabled."
  (diss--without-scaling
   (image-toggle-display-image)))

(defun diss-image-mode--rotate (direction)
  "Rotate image in DIRECTION."
  (setq-local image-transform-rotation (diss-image-mode--rotation direction))
  (diss--toggle-display-image))

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
  (diss--toggle-display-image))

(defun diss-image-mode-fit-to-width ()
  "Fit the current image to the width of the current window."
  (interactive)
  (setq-local image-transform-resize 'fit-width)
  (diss--toggle-display-image))

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

(defun diss-image-mode-set-prefix-name-and-next (prefix-name &optional marker)
  "Apply PREFIX-NAME to current file, then advance.

With a prefix arg, prompt for marker char MARKER, and mark file
with it."
  (interactive
   (list
    (completing-read
     "Name: "
     (with-current-buffer (oref diss-image-mode--slideshow buffer)
       diss--prefix-name-cache)
     nil
     nil
     (diss-mode--prefix-name (buffer-file-name)))
    (when current-prefix-arg (read-char-exclusive "Category char: " t))))

  (add-to-list 'diss--prefix-name-cache prefix-name)
  (let* ((bfn (buffer-file-name))
         (new-name (concat prefix-name "_" (cdr (diss-mode--prefix-and-name bfn)))))
    (with-current-buffer (oref diss-image-mode--slideshow buffer)
      (let ((marker (or marker (dired-file-marker bfn))))
        (dired-rename-file bfn new-name nil)
        (dired-add-entry new-name marker t)))
    (diss-image-mode-next)))

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
        (diss-slideshow-pause! ss)))))

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

(defun diss--move (ss &optional arg find-function)
  "Find the ARG-th file in slideshow SS with FIND-FUNCTION.

If ARG is omitted, use the slideshow's step.
If the end of the slideshow is reached, display the Diss buffer."
  (when-let ((current (or (buffer-file-name) (oref ss current))))
    (diss-mode--ensure ss current))
  (let ((arg (or arg (oref ss step)))
        (image-transform-resize nil))   ; Don't resize on open
    (if-let ((next-file (diss-mode--navigate ss arg)))
        (progn
          ;; If a buffer is showing the file already, kill it.
          (when-let ((buf (find-buffer-visiting next-file)))
            (kill-buffer buf))
          (diss--without-scaling
           (funcall (or find-function #'find-alternate-file) next-file)))
      ;; Slideshow is over, show the Diss buffer
      (pop-to-buffer (oref ss buffer)))))

(defun diss-image-mode-next (&optional arg)
  "Move ARG images forward in the slideshow."
  (interactive "p")
  (diss-slideshow-pause! diss-image-mode--slideshow)
  (diss--move diss-image-mode--slideshow arg))

(defun diss-image-mode-previous (&optional arg)
  "Move ARG images back in the slideshow."
  (interactive "p")
  (diss-slideshow-pause! diss-image-mode--slideshow)
  (diss--move diss-image-mode--slideshow (* -1 arg)))

(defun diss-image-mode-next-other-window (&optional arg)
  "Move ARG images forward in the slideshow, displaying in another window."
  (interactive "p")
  (diss-slideshow-pause! diss-image-mode--slideshow)
  (diss--move diss-image-mode--slideshow arg #'find-file-other-window))

(defun diss-image-mode-previous-other-window (&optional arg)
  "Move ARG images back in the slideshow, displaying in another window."
  (interactive "p")
  (diss-slideshow-pause! diss-image-mode--slideshow)
  (diss--move diss-image-mode--slideshow (* -1 arg) #'find-file-other-window))

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

(defun diss-image-mode-junk-and-next ()
  "Mark the current image as junk and advance."
  (interactive)
  (diss-mode--mark diss-image-mode--slideshow (buffer-file-name) ?j t)
  (diss-image-mode-next))

(defun diss-image-mode-unmark-and-next ()
  "Remove mark from the current image and advance."
  (interactive)
  (diss-mode--unmark diss-image-mode--slideshow (buffer-file-name))
  (diss-image-mode-next))

(defun diss-image-mode-delete-and-next ()
  "Flag the current image for deletion and advance."
  (interactive)
  (diss-mode--mark diss-image-mode--slideshow (buffer-file-name) ?D t)
  (diss-image-mode-next))

(defun diss-image-mode-quit ()
  "Pause the slideshow and bury the current buffer."
  (interactive)
  (diss-slideshow-pause! diss-image-mode--slideshow)
  (bury-buffer))

(defun diss-image-mode-toggle-paused ()
  "Toggle whether the current slideshow is paused."
  (interactive)
  (unless (diss-slideshow-toggle-pause! diss-image-mode--slideshow)
    (diss-image-mode--automatic diss-image-mode--slideshow (current-buffer))))

(provide 'diss)
;;; diss.el ends here
