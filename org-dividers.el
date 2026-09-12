;;; org-dividers.el --- Org Dividers  -*- lexical-binding: t -*-
;;
;; Copyright (C) 2025-2026 Taro Sato
;;
;; Author: Taro Sato <okomestudio@gmail.com>
;; URL: https://github.com/okomestudio/org-dividers
;; Version: 0.6.4
;; Keywords: org
;; Package-Requires: ((emacs "31.1") (org "9.7"))
;;
;;; License:
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; A minor mode to add decoration to Org section dividers.
;;
;;; Code:

(require 'org)
(require 'org-element-ast)

(defgroup org-dividers nil
  "Org Dividers group."
  :prefix "org-dividers-"
  :group 'org)

(defcustom org-dividers-hl-title nil
  "Match regexp for headline title to be styled."
  :type 'string
  :group 'org-dividers)

(defcustom org-dividers-hl-match nil
  "Org elements match for filtering by `org-map-entries'.
Only headlines matches by this specification will be styled. See the
documentation for `org-map-entires' for what MATCH means."
  :type 'string
  :group 'org-dividers)

(defcustom org-dividers-hl-padding-outer 1
  "Outer padding for headline."
  :type 'integer
  :group 'org-dividers)

(defcustom org-dividers-hl-padding-inner 1
  "Inner padding for headline."
  :type 'integer
  :group 'org-dividers)

(defcustom org-dividers-hl-text-position -2
  "Headline text position from the left.
If negative, the position is from the right."
  :type 'number
  :group 'org-dividers)

(defcustom org-dividers-hl-char ?―
  "Headline divider character."
  :type 'character
  :group 'org-dividers)

(defcustom org-dividers-hr-styles
  '((t . ("divider-decorative.svg" . 0.15)))
  "Horizontal rule styles."
  :type '(alist :key-type (choice (integer :tag "Dash count")
                                  (const :tag "Default fallback" t))
                :value-type (cons (file :tag "SVG file path")
                                  (float :tag "Scale factor")))
  :group 'org-dividers)

(defface org-dividers-hl '((t :inherit default))
  "Face used for headlines.")

(defconst org-dividers--dir
  (file-name-directory (or load-file-name
                           (bound-and-true-p byte-compile-current-file)
                           buffer-file-name))
  "Package directory path.")

;;; Horizontal Rules

(defun org-dividers-hr--display (hr-len)
  (when-let* ((hr-sub (or (alist-get hr-len org-dividers-hr-styles)
                          (alist-get t org-dividers-hr-styles)))
              (im-file (let ((f (car hr-sub)))
                         (or (and (file-exists-p f) f)
                             (let ((f (file-name-concat org-dividers--dir "images" f)))
                               (and (file-exists-p f) f)))))
              (im-scale (cdr hr-sub))
	      (level-1 (1- (or (org-current-level) 1)))
              (win-width (- (window-body-width nil t)
                            (if (bound-and-true-p org-indent-mode)
                                (* level-1
                                   (window-font-width nil) ; default-font-width (?)
                                   org-indent-indentation-per-level)
                              0)))
              (image (create-image im-file nil nil :scale im-scale))
              (margin (round (/ (- win-width (car (image-size image t))) 2))))
    (append image `(:margin (,margin . 0)))))

(defun org-dividers-hr--on-modification (ov after-p beg end &optional len)
  (when after-p
    (let* ((l-beg (save-excursion (goto-char beg) (line-beginning-position)))
           (l-end (save-excursion (goto-char end) (line-end-position)))
           (l-len (length (string-trim (buffer-substring-no-properties l-beg l-end)))))
      (if (< l-len 5)
          (delete-overlay ov)
        (when-let* ((display (org-dividers-hr--display l-len)))
          (overlay-put ov 'display display))))))

(defun org-dividers-hr--redraw (beg end &optional len)
  "Redraw horizontal rules within a region from BEG to END.
For the meaning of BEG, END, and LEN, see `after-change-functions'."
  (when (and len (> len 0))
    (dolist (ov (overlays-in beg (+ end len)))
      (when (and (eq (overlay-get ov 'category) 'org-dividers-hr-ov)
                 (< (- (overlay-end ov) (overlay-start ov) 5)))
        (delete-overlay ov))))
  (save-excursion
    (save-restriction
      (widen)
      (let ((r-beg (save-excursion (goto-char beg) (line-beginning-position)))
            (r-end (save-excursion (goto-char end) (line-end-position))))
        (narrow-to-region r-beg r-end)
        (goto-char (point-min))
        (org-element-map (org-element-parse-buffer) 'horizontal-rule
          (lambda (el)
            (when-let*
                ((hr-beg (org-element-property :begin el))
                 (hr-end (save-excursion (goto-char hr-beg) (line-end-position)))
                 (hr-len (length (string-trim (buffer-substring-no-properties hr-beg hr-end))))
                 (display (when (> hr-len 4)
                            (org-dividers-hr--display hr-len))))
              (if-let* ((ov (seq-find
                             (lambda (ov)
                               (when (and (eq (overlay-get ov 'category) 'org-dividers-hl-ov)
                                          (eq (overlay-start ov) hr-beg)
                                          (eq (overlay-end ov) hr-end))
                                 ov))
                             (overlays-in hr-beg hr-end))))
                  (overlay-put ov 'display display)
                (when-let* ((ov (make-overlay hr-beg hr-end nil t t)))
                  (overlay-put ov 'category 'org-dividers-hr-ov)
                  (overlay-put ov 'priority -100)
                  (overlay-put ov 'display display)
                  (overlay-put ov 'modification-hooks '(org-dividers-hr--on-modification))
                  (overlay-put ov 'insert-in-front-hooks '(org-dividers-hr--on-modification))
                  (overlay-put ov 'insert-behind-hooks '(org-dividers-hr--on-modification))))))
          nil nil nil)))))

(defun org-dividers-hr--remove-all (beg end)
  "Remove horizontal rule overlays in region from BEG to END."
  (remove-overlays beg end 'category 'org-dividers-hr-ov))

;;; Headlines

(defun org-dividers-hl--total-width (win face)
  "Compute the total character width of line at point given FACE in WIN."
  (let* ((level-1 (1- (or (org-current-level) 1)))
         (indent-width (if (bound-and-true-p org-indent-mode)
                           (* level-1 org-indent-indentation-per-level)
                         0)))
    (max 0 (- (window-max-chars-per-line win face)
              level-1
              indent-width))))

(defun org-dividers-hl--update (ov win title)
  "Update the display of overlay OV with headline TITLE in window WIN.
Update is not performed when no change is detected in display property."
  (let* ((face 'org-dividers-hl)
         (total-width (org-dividers-hl--total-width win face)))
    (unless (and (string= title (overlay-get ov 'title))
                 (= (length (substring-no-properties (or (overlay-get ov 'display) "")))
                    total-width))
      (let ((display
             (with-selected-window win
               (let* ((dash-count (max 0 (- total-width
                                            (* 2 org-dividers-hl-padding-outer)
                                            (* 2 org-dividers-hl-padding-inner)
                                            (string-width title))))
                      (pos org-dividers-hl-text-position)
                      (dash-left (max 0 (if (> pos 0) pos (+ dash-count pos))))
                      (dash-right (max 0 (if (> pos 0) (- dash-count pos) (abs pos)))))
                 (propertize
                  (concat (make-string (1- (or (org-current-level) 1)) ?\s)
                          (make-string org-dividers-hl-padding-outer ?\s)
                          (make-string dash-left org-dividers-hl-char)
                          (make-string org-dividers-hl-padding-inner ?\s)
                          title
                          (make-string org-dividers-hl-padding-inner ?\s)
                          (make-string dash-right org-dividers-hl-char)
                          (make-string org-dividers-hl-padding-outer ?\s))
                  'face face)))))
        (overlay-put ov 'display display)
        (overlay-put ov 'title title))))
  ov)

(defun org-dividers-hl--create (win beg end title)
  "Create a TITLE overlay for headline spanning BEG to END in window WIN."
  (let ((ov (make-overlay beg end nil nil t)))
    (overlay-put ov 'category 'org-dividers-hl-ov)
    (overlay-put ov 'window win)
    (overlay-put ov 'priority -100)
    (org-dividers-hl--update ov win title)
    (overlay-put ov 'isearch-open-invisible t)
    ov))

(defun org-dividers-hl--redraw (beg end &optional len)
  "Redraw headlines in region from BEG to END.
See `after-change-functions' for what BEG, END, and LEN means."
  (when (and len (> len 0))
    (dolist (win (get-buffer-window-list (current-buffer) nil t))
      (dolist (ov (overlays-in beg (+ end len)))
        (when (and (eq (overlay-get ov 'window) win)
                   (eq (overlay-get ov 'category) 'org-dividers-hl-ov)
                   (= (overlay-start ov) (overlay-end ov)))
          (delete-overlay ov)))))
  (save-excursion
    (save-restriction
      (widen)
      (let ((r-beg (save-excursion (goto-char beg) (line-beginning-position)))
            (r-end (save-excursion (goto-char end) (line-end-position))))
        (narrow-to-region r-beg r-end)
        (dolist (win (get-buffer-window-list (current-buffer) nil t))
          (goto-char (point-min))
          (org-map-entries
           (lambda ()
             (let* ((el (org-element-at-point))
                    (title (org-element-property :title el))
                    (beg (org-element-property :begin el))
                    (end (save-excursion (goto-char beg) (line-end-position))))
               (if-let* ((ov (seq-find
                              (lambda (ov)
                                (when (and (eq (overlay-get ov 'window) win)
                                           (eq (overlay-get ov 'category) 'org-dividers-hl-ov)
                                           (eq (overlay-start ov) beg)
                                           (eq (overlay-end ov) end))
                                  ov))
                              (overlays-in beg end))))
                   (if (and org-dividers-hl-title (string-match-p org-dividers-hl-title title))
                       (org-dividers-hl--update ov win title)
                     (delete-overlay ov))
                 (when (and org-dividers-hl-title (string-match-p org-dividers-hl-title title))
                   (org-dividers-hl--create win beg end title)))))
           org-dividers-hl-match))))))

(defun org-dividers-hl--remove-all (beg end)
  "Remove all headline overlays in region from BEG to END."
  (remove-overlays beg end 'category 'org-dividers-hl-ov))

;;; Minor Mode

;;;###autoload
(define-minor-mode org-dividers-mode
  "A minor mode for styling Org headlines and horizontal rules."
  :group 'org-dividers
  :lighter "OrgD"
  (pcase org-dividers-mode
    ('t
     (when (boundp 'org-modern-horizontal-rule)
       (setq-local org-modern-horizontal-rule nil))
     (add-hook 'after-change-functions #'org-dividers-mode--on-after-change nil t)
     (add-hook 'window-configuration-change-hook
               #'org-dividers-mode--on-window-configuration-change nil t))
    (_
     (let ((beg (point-min)) (end (point-max)))
       (org-dividers-hr--remove-all beg end)
       (org-dividers-hl--remove-all beg end))
     (remove-hook 'window-configuration-change-hook
                  #'org-dividers-mode--on-window-configuration-change t)
     (remove-hook 'after-change-functions #'org-dividers-mode--on-after-change t))))

(defun org-dividers-mode--on-after-change (beg end len)
  "A hook function for `after-change-functions'.
On insertion, LEN is 0. BEG is at the first char and END is after the end
of last char of inserted text.

On deletion, LEN is the character count of deleted text. Both BEG and
END are at the first char of deleted text."
  (org-dividers-hr--redraw beg end len)
  (org-dividers-hl--redraw beg end len))

(defun org-dividers-mode--on-window-configuration-change ()
  (when-let* ((win (selected-window))
              (beg (window-start win))
              (end (window-end win t)))
    (org-dividers-hr--redraw beg end nil)
    (org-dividers-hl--redraw beg end nil)))

(provide 'org-dividers)
;;; org-dividers.el ends here
