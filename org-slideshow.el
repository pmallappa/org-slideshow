;;; org-show.el --- Summary
;; Copyright(C) 2014-2021 John Kitchin

;; Author: John Kitchin <jkitchin@andrew.cmu.edu>
;; Contributions from Sacha Chua.
;; This file is not currently part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program ; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;; A simple mode for presenting org-files as slide-shows. A slide is a headline
;; with a :slide: tag. See file:org-slideshow.org for usage.

;;; Code:
(require 'animate)
(require 'easymenu)

(defcustom org-slideshow-org-meta-line-background 'unspecified
  "Background for org-meta-line face during a show."
  :group 'org-slideshow)

(defcustom org-slideshow-org-meta-line-height 100
  "Height of org-meta-line face during show."
  :group 'org-slideshow
  :type 'number)

(defcustom org-slideshow-text-scale 4
  "Scale for text in presentation."
  :group 'org-slideshow
  :type 'number)

(defcustom org-slideshow-latex-scale 4.0
  "Scale for latex preview."
  :group 'org-slideshow
  :type 'number)

(defcustom org-slideshow-hide-tags 'slide
  "If 'slide only hide slide tags.
If 'all hide all tags."
  :group 'org-slideshow
  :type 'symbol)

;;* Variables

(defvar org-slideshow-presentation-file nil
  "File containing the presentation.")

(defvar org-slideshow-slide-tag "slide"
  "Tag that marks slides.")

(defvar org-slideshow-slide-tag-regexp
  (concat ":" (regexp-quote org-slideshow-slide-tag) ":")
  "Regex to identify slide tags.")



(defvar org-slideshow-original-latex-scale
  (if (boundp 'org-format-latex-options)
      (plist-get org-format-latex-options :scale)
    nil)
  "Original scale for latex preview, so we can reset it.")

(defvar org-slideshow-current-slide-number 1
  "Holds current slide number.")

(defvar org-slideshow-mogrify-p
  (executable-find "mogrify")
  "Determines if images are mogrified (changed size in presentation mode.")

(when org-slideshow-mogrify-p
  (ignore-errors (require 'eimp)))

(defvar org-slideshow-tags-column -60
  "Column position to move tags to in slide mode.")

(defvar org-slideshow-original-tags-column org-tags-column
  "Save value so we can change back to it.")

(defvar *org-slideshow-flyspell-mode* (when (boundp flyspell-mode)
                                   flyspell-mode)
  "Whether flyspell mode is enabled at beginning of show.
Used to reset the state after the show.")

(defvar *org-slideshow-running* nil
  "Flag for if the show is running.")

(defvar org-slideshow-slide-list '()
  "List of slide numbers and markers to each slide.")

(defvar org-slideshow-slide-titles '()
  "List of titles and slide numbers for each slide.")

(defvar org-meta-line-background (face-attribute 'org-meta-line :background)
  "Stores original value so we can restore it.")

(defvar org-meta-line-height (face-attribute 'org-meta-line :height)
  "Stores original value so we can restore it.")


;;* Functions
(defvar org-slideshow-temp-images '() "List of temporary images.")

(defun org-slideshow-execute-slide ()
  "Process slide at point.
If it contains an Emacs Lisp source block, evaluate it.
  If it contains an image, view it in a split buffer
  Else, focus on that buffer.
  Hide all drawers."
  (interactive)
  (setq org-slideshow-presentation-file (expand-file-name (buffer-name)))
  (delete-other-windows)

  ;; make sure nothing is folded. This seems to be necessary to
  ;; prevent an error on narrowing then trying to make latex fragments
  ;; I think.
  (org-cycle '(64))

  (org-narrow-to-subtree)
  (visual-line-mode 1)
  (let ((heading-text (nth 4 (org-heading-components)))
        (org-format-latex-options (plist-put org-format-latex-options
                                             :scale org-slideshow-latex-scale)))

    (set-frame-name (format "%-180s%15s%s"
                            heading-text
                            "slide "
                            (cdr (assoc heading-text org-slideshow-slide-titles))))

    ;; preview equations in the current subtree
    (org-latex-preview)

    ;; setup the text
    (switch-to-buffer (current-buffer))
    (text-scale-set org-slideshow-text-scale)
    (org-slideshow-subtree)
    (org-cycle-hide-drawers t)
    (org-display-inline-images)
    (delete-other-windows)


    ;; evaluate special code blocks last as they may change the arrangement
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward org-babel-src-block-regexp nil t)
        (save-excursion
          (goto-char (match-beginning 0))
          (let* ((src (org-element-context))
                 (start (org-element-property :begin src))
                 (end (org-element-property :end src))
                 (info (save-excursion
                         (org-babel-get-src-block-info))))
            (when (string= "emacs-lisp-slide" (car info))
              ;; fold code
              (org-cycle)
              (unwind-protect
                  (eval (read (concat "(progn " (nth 1 info) ")")))))))))
    ;; clear the minibuffer
    (message "")))

    (defun org-slideshow-next-slide ()
      "Goto next slide in presentation."
      (interactive)
      (find-file org-slideshow-presentation-file)
      (widen)
      (if (<= (+ org-slideshow-current-slide-number 1) (length org-slideshow-slide-titles))
          (progn
            (setq org-slideshow-current-slide-number (+ org-slideshow-current-slide-number 1))
            (org-slideshow-goto-slide org-slideshow-current-slide-number))
        (org-slideshow-goto-slide org-slideshow-current-slide-number)
        (message "This is the end. My only friend the end.  Jim Morrison.")))


(defun org-slideshow-previous-slide ()
  "Goto previous slide in the list."
  (interactive)
  (find-file org-slideshow-presentation-file)
  (widen)
  (if (> (- org-slideshow-current-slide-number 1) 0)
      (progn
        (setq org-slideshow-current-slide-number (- org-slideshow-current-slide-number 1))
        (org-slideshow-goto-slide org-slideshow-current-slide-number))
    (org-slideshow-goto-slide org-slideshow-current-slide-number)
    (message "Once upon a time...")))


(defun org-slideshow-open-slide ()
  "Start show at this slide."
  (setq org-slideshow-presentation-file (expand-file-name (buffer-name)))
  (org-slideshow-initialize)
  (let ((n (cdr (assoc (nth 4 (org-heading-components)) org-slideshow-slide-titles))))
    (setq org-slideshow-current-slide-number n)
    (org-slideshow-goto-slide n)))


(defun org-slideshow-initialize ()
  "Initialize the org-slideshow.
Make slide lists for future navigation. Rerun this if you change
slide order."
  (setq  org-slideshow-slide-titles '()
         org-slideshow-temp-images '()
         org-slideshow-slide-list '())

  (org-with-wide-buffer
   (let ((n 0))
     (org-map-entries
      (lambda ()
        (when (string-match-p ":slide:" (or (nth 5 (org-heading-components)) ""))
          (setq n (+ n 1))

          (add-to-list 'org-slideshow-slide-titles
                       (cons (nth 4 (org-heading-components)) n) t)

          (add-to-list 'org-slideshow-slide-list
                       (cons n (set-marker (make-marker) (point))) t)))))))


(defun org-slideshow-start-slideshow ()
  "Start the slide show, at the beginning."
  (interactive)
  (setq *org-slideshow-running* t)
  (setq org-slideshow-presentation-file (expand-file-name (buffer-name)))
  (beginning-of-buffer)
  (setq org-tags-column org-slideshow-tags-column)
  (org-set-tags-command '(4))

  (set-face-attribute 'org-meta-line nil :background org-slideshow-org-meta-line-background)
  (set-face-attribute 'org-meta-line nil :height org-slideshow-org-meta-line-height)

  (org-slideshow-initialize)
  ;; hide slide tags
  (save-excursion
    (cond
     ((equal org-slideshow-hide-tags 'all)
      (org-map-entries
       (lambda ()
         (let ((tag-string (cl-sixth (org-heading-components))))
           (when tag-string
             (re-search-forward tag-string (line-end-position) t)
             (overlay-put
              (make-overlay (match-beginning 0) (match-end 0))
              'invisible 'slide))))))

     ((equal org-slideshow-hide-tags 'slide)
      (while (re-search-forward ":slide:" nil t)
        (overlay-put
         (make-overlay (match-beginning 0) (match-end 0))
         'invisible 'slide)))))
  ;; hide emacs-lisp-slide blocks
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward org-babel-src-block-regexp nil t)
      (save-excursion
        (goto-char (match-beginning 0))
        (let* ((src (org-element-context))
               (start (org-element-property :begin src))
               (end (org-element-property :end src))
               (info (save-excursion
                       (org-babel-get-src-block-info))))
          (when (string= "emacs-lisp-slide" (car info))
            (save-restriction
              (overlay-put
               (make-overlay start end)
               'invisibility 'slide)))))))
  (add-to-invisibility-spec 'slide)
  (beginning-of-buffer)
  (delete-other-windows)
  ;; (when (not org-slideshow-mode) (org-slideshow-mode 1))
  (setq org-slideshow-current-slide-number 1)
  (org-slideshow-goto-slide 1))


(defun org-slideshow-stop-slideshow ()
  "Stop the org-slideshow.
Try to reset the state of your Emacs. It isn't perfect ;)"
  (interactive)
  ;; make slide tag visible again
  (remove-from-invisibility-spec 'slide)

  (set-face-attribute 'org-meta-line nil :background org-meta-line-background)
  (set-face-attribute 'org-meta-line nil :height org-meta-line-height)


  ;; Redisplay inline images
  (org-display-inline-images)

  ;; reset latex scale
  (plist-put org-format-latex-options :scale org-slideshow-original-latex-scale)

  ;; clean up temp images
  (mapcar (lambda (x)
            (let ((bname (file-name-nondirectory x)))
              (when (get-buffer bname)
                (set-buffer bname)
                (save-buffer)
                (kill-buffer bname)))

            (when (file-exists-p x)
              (delete-file x)))
          org-slideshow-temp-images)
  (setq org-slideshow-temp-images '())

  ;; ;; clean up miscellaneous buffers
  (when (get-buffer "*Animation*") (kill-buffer "*Animation*"))

  (when org-slideshow-presentation-file (find-file org-slideshow-presentation-file))
  (widen)
  (text-scale-set 0)
  (delete-other-windows)
  (setq org-slideshow-presentation-file nil)
  (setq org-slideshow-current-slide-number 1)
  (set-frame-name (if (buffer-file-name)
                      (abbreviate-file-name (buffer-file-name))))
  (setq org-tags-column org-slideshow-original-tags-column)
  (org-set-tags-command '(4))
  (setq *org-slideshow-running* nil)
  (org-slideshow-mode -1))


(defun org-slideshow-goto-slide (n)
  "Goto slide N."
  (interactive "nSlide number: ")
  (message "Going to slide %s" n)
  (find-file org-slideshow-presentation-file)
  (setq org-slideshow-current-slide-number n)
  (widen)
  (goto-char (cdr (assoc n org-slideshow-slide-list)))
  (org-slideshow-execute-slide))


(defun org-slideshow-toc ()
  "Show a table of contents for the slideshow."
  (interactive)
  (let ((links) (c-b (buffer-name)) (n))
    (save-excursion
      (widen)
      (mapcar
       (lambda (x)
         (setq n (car x))
         (goto-char (cdr x))
         (add-to-list
          'links
          (format " [[elisp:(progn (switch-to-buffer \"%s\")(goto-char %s)(org-slideshow-execute-slide))][%2s %s]]\n\n"
                  (marker-buffer (cdr x))
                  (marker-position (cdr x))
                  (car x)
                  (nth 4 (org-heading-components))) t))
       org-slideshow-slide-list))

    (switch-to-buffer "*List of Slides*")
    (org-mode)
    (erase-buffer)

    (insert (mapconcat 'identity links ""))

    (use-local-map (copy-keymap org-mode-map))
    (local-set-key "q" #'(lambda () (interactive) (kill-buffer)))))


(defun org-slideshow-animate (strings)
  "Animate STRINGS in an *Animation* buffer."
  (switch-to-buffer (get-buffer-create
                     (or animation-buffer-name
                         "*Animation*")))
  (erase-buffer)
  (text-scale-set 6)
  (let* ((vpos (/ (- 20
                     1 ;; For the mode-line
                     (1- (length strings))
                     (length strings))
                  2))
         (width 43)
         hpos)
    (while strings
      (setq hpos (/ (- width (length (car strings))) 2))
      (when (> 0 hpos) (setq hpos 0))
      (when (> 0 vpos) (setq vpos 0))
      (animate-string (car strings) vpos hpos)
      (setq vpos (1+ vpos))
      (setq strings (cdr strings)))))


(defun org-slideshow-increase-text-size (&optional arg)
  "Increase text size. Bound to \\[org-slideshow-increase-text-size].
With prefix ARG, set `org-slideshow-text-scale' so subsquent slides
are the same text size."
  (interactive "P")
  (text-scale-increase 1.5)
  (when arg
    (setq org-slideshow-text-scale (* org-slideshow-text-scale 1.5))))


(defun org-slideshow-decrease-text-size (&optional arg)
  "Increase text size. Bound to \\[org-slideshow-decrease-text-size].
With prefix ARG, set `org-slideshow-text-scale' so subsquent slides
are the same text size."
  (interactive "P")
  (text-scale-decrease 1.5)
  (when arg
    (setq org-slideshow-text-scale (/ org-slideshow-text-scale 1.5))))

;;* Menu and org-slideshow-mode

(defvar org-slideshow-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [next] 'org-slideshow-next-slide)
    (define-key map [prior] 'org-slideshow-previous-slide)

    (define-key map [f5] 'org-slideshow-start-slideshow)
    (define-key map [f6] 'org-slideshow-execute-slide)
    (define-key map (kbd "C--") 'org-slideshow-decrease-text-size)
    (define-key map (kbd "C-=") 'org-slideshow-increase-text-size)
    (define-key map (kbd "\e\eg") 'org-slideshow-goto-slide)
    (define-key map (kbd "\e\et") 'org-slideshow-toc)
    (define-key map (kbd "\e\eq") 'org-slideshow-stop-slideshow)
    map)
  "Keymap for function ‘org-slideshow-mode’.")


(define-minor-mode org-slideshow-mode
  "Minor mode for org-slideshow

\\{org-slideshow-mode-map}"
  :init-value nil
  :lighter " org-slideshow"
  :global t
  :keymap org-slideshow-mode-map
  ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Minor-Mode-Conventions.html
  (let ((enable org-slideshow-mode))
    (if enable
        ;; do-enable
        (progn
          (when flyspell-mode
            (setq *org-slideshow-flyspell-mode* t)
            (flyspell-mode-off)
            (setq *org-slideshow-flyspell-mode* nil))

          (easy-menu-define my-menu org-slideshow-mode-map "My own menu"
            '("org-slideshow"
              ["Start slide show" org-slideshow-start-slideshow t]
              ["Next slide" org-slideshow-next-slide t]
              ["Previous slide" org-slideshow-previous-slide t]
              ["Open this slide" org-slideshow-open-slide t]
              ["Goto slide" org-slideshow-goto-slide t]
              ["Table of contents" org-slideshow-toc t]
              ["Stop slide show"  org-slideshow-stop-slideshow t])))
      ;; restore flyspell
      (when  *org-slideshow-flyspell-mode*
        (flyspell-mode-on))

      ;; close the show.
      (when *org-slideshow-running*
        (org-slideshow-stop-slideshow)))))

;;* Make emacs-lisp-slide blocks executable

;; this is tricker than I thought. It seems babel usually runs in some
;; sub-process and I need the code to be executed in the current buffer.
(defun org-babel-execute:emacs-lisp-slide (body params)
  (message "%S" body)
  (let ((src (org-element-context)))
    (save-excursion
      (goto-char (org-element-property :begin src))
      (re-search-forward (org-element-property :value src))
      (eval-region (match-beginning 0) (match-end 0)))))

;; * help
(defun org-slideshow-help ()
  "Open the help file."
  (interactive)
  (find-file (expand-file-name "org-slideshow.org"
                               (file-name-directory
                                (locate-library "org-slideshow")))))



;;* The end

(provide 'org-slideshow)

;;; org-slideshow.el ends here
