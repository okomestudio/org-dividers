;;; org-dividers.el --- Org Dividers  -*- lexical-binding: t -*-
;;
;; Copyright (C) 2025-2026 Taro Sato
;;
;; Author: Taro Sato <okomestudio@gmail.com>
;; URL: https://github.com/okomestudio/org-dividers
;; Version: 0.4.2
;; Keywords: org
;; Package-Requires: ((emacs "30.1") (org "9.7"))
;;
;;; License:
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; Add decorations to Org section dividers.
;;
;;; Code:

(require 'org)
(require 'org-element-ast)

(defgroup org-dividers nil
  "Org Dividers group."
  :prefix "org-dividers-"
  :group 'org)

;;; Dividers

(defcustom org-dividers-horizontal-rules nil
  "Cons of the form (image-file . scale) or nil."
  :group 'org-dividers
  :type '(choice (const nil) (cons string number)))

(defvar org-dividers-horizontal-rules-before-style-hook nil
  "Normal hook running before styling horizontal rules.")

(defun org-dividers-horizontal-rules-remove (beg end)
  "Remove horizontal rule overlays in region between BEG and END."
  (remove-overlays beg end 'category 'org-dividers-hr))

(defun org-dividers-horizontal-rules-style (beg end len)
  "Style horizontal rule overlays with image.
See `after-change-functions' for what BEG, END, and LEN mean."
  (when (and (derived-mode-p 'org-mode)
             org-dividers-horizontal-rules
             (or (null len) (> len 0)))
    (run-hooks 'org-dividers-horizontal-rules-before-style-hook)
    (save-restriction
      (narrow-to-region beg end)
      (org-element-map (org-element-parse-buffer) 'horizontal-rule
        (lambda (hr)
          (let* ((hr-beg (org-element-property :begin hr))
                 (hr-end (save-excursion
                           (goto-char hr-beg) (line-end-position)))
                 (win-width (- (window-width nil t)
                               (if (bound-and-true-p org-indent-mode)
                                   (* (or (org-current-level) 0)
                                      org-indent-indentation-per-level
                                      (default-font-width))
                                 0)))
                 (image (create-image (car org-dividers-horizontal-rules) nil nil
                                      :scale (cdr org-dividers-horizontal-rules)))
                 (image-width (car (image-size image t)))
                 (margin (- (/ win-width 2) (/ image-width 2)))
                 (ov (make-overlay hr-beg hr-end nil 'front-adv nil)))
            (overlay-put ov 'category 'org-dividers-hr)
            (overlay-put ov 'display (append image `(:margin (,margin . 0))))
            (overlay-put ov 'evaporate t)
            (overlay-put ov 'priority 90)
            nil))
        nil nil nil))))

;;; Headlines

(defcustom org-dividers-headline-regexp nil
  "Match regexp for headline texts to be styled."
  :group 'org-dividers
  :type 'string)

(defcustom org-dividers-headline-match nil
  "Value for MATCH in `org-map-entries'.
See the documentation for `org-map-entires' for what MATCH means."
  :group 'org-dividers
  :type 'string)

(defcustom org-dividers-headline-padding-outer 1
  "Outer padding for headline dividers."
  :group 'org-dividers
  :type 'number)

(defcustom org-dividers-headline-padding-inner 1
  "Inner padding for headline dividers."
  :group 'org-dividers
  :type 'number)

(defcustom org-dividers-headline-text-position -2
  "Headline text position from the left.
If negative, the position is from the right."
  :group 'org-dividers
  :type 'number)

(defface org-dividers-headline '((t :inherit default))
  "Face used for headline dividers.")

(defun org-dividers-headline--format (text face)
  "Format divider given headline TEXT and FACE."
  (let* ((total-width (window-max-chars-per-line nil face))
         (dash-count (max 0 (- total-width
                               (* 2 org-dividers-headline-padding-outer)
                               (* 2 org-dividers-headline-padding-inner)
                               (string-width text))))
         (dash-left (if (> org-dividers-headline-text-position 0)
                        org-dividers-headline-text-position
                      (+ dash-count org-dividers-headline-text-position)))
         (dash-right (if (> org-dividers-headline-text-position 0)
                         (- dash-count org-dividers-headline-text-position)
                       (abs org-dividers-headline-text-position))))
    (concat (make-string org-dividers-headline-padding-outer ?\s)
            (make-string dash-left ?⎯)
            (make-string org-dividers-headline-padding-inner ?\s)
            text
            (make-string org-dividers-headline-padding-inner ?\s)
            (make-string dash-right ?⎯)
            (make-string org-dividers-headline-padding-outer ?\s))))

(defun org-dividers-headline--draw (hl)
  "Style headline element HL as divider overlay."
  (let* ((beg (org-element-property :begin hl))
         (end (and (save-excursion (goto-char beg) (line-end-position))))
         (face 'org-dividers-headline)
         (title (org-element-property :title hl))
         (text (org-dividers-headline--format title face))
         (ov (make-overlay beg end nil 'front-adv nil)))
    (overlay-put ov 'category 'org-dividers)
    (overlay-put ov 'face face)
    (overlay-put ov 'display text)
    (overlay-put ov 'evaporate t)
    (overlay-put ov 'isearch-open-invisible t)))

(defun org-dividers-headline-draw (beg end len)
  "Draw headline dividers in region.
See `after-change-functions' for what BEG, END, and LEN means."
  (when (or (null len) (> len 0))
    (save-restriction
      (narrow-to-region beg end)
      (org-map-entries
       (lambda ()
         (when-let*
             ((el (org-element-at-point))
              (hl (and (eq (org-element-type el) 'headline) el))
              (title (org-element-property :title hl)))
           (when (and org-dividers-headline-regexp
                      (string-match org-dividers-headline-regexp title))
             (org-dividers-headline--draw hl))))
       org-dividers-headline-match))))

(defun org-dividers-headline-remove (beg end)
  "Remove all headline dividers in region between BEG and END."
  (remove-overlays beg end 'category 'org-dividers))

;;; Minor Mode Configuration

(defun org-dividers-mode--on-before-change (beg end)
  (org-dividers-horizontal-rules-remove beg end)
  (org-dividers-headline-remove beg end))

(defun org-dividers-mode--on-after-change (beg end len)
  (org-dividers-horizontal-rules-style beg end len)
  (org-dividers-headline-draw beg end len))

(defun org-dividers-mode--on-window-scroll (win beg)
  (let ((end (window-end win t))
        (len nil))
    (org-dividers-horizontal-rules-remove beg end)
    (org-dividers-horizontal-rules-style beg end len)
    (org-dividers-headline-remove beg end)
    (org-dividers-headline-draw beg end len)))

(defun org-dividers-mode--on-window-buffer-change (win)
  (let ((beg (window-start win))
        (end (window-end win t)))
    (org-dividers-horizontal-rules-remove beg end)
    (org-dividers-horizontal-rules-style beg end len)
    (org-dividers-headline-remove beg end)
    (org-dividers-headline-draw beg end len)))

(defun org-dividers-mode--on-window-configuration-change ()
  (when-let* ((win (selected-window))
              (beg (window-start win))
              (end (window-end win t)))
    (org-dividers-horizontal-rules-remove beg end)
    (org-dividers-horizontal-rules-style beg end nil)
    (org-dividers-headline-remove beg end)
    (org-dividers-headline-draw beg end nil)))

;;;###autoload
(define-minor-mode org-dividers-mode
  "A minor mode for styling Org headlines as dividers."
  :group 'org-dividers
  :lighter "OrgD"
  (pcase org-dividers-mode
    ('t
     (add-hook 'before-change-functions #'org-dividers-mode--on-before-change nil t)
     (add-hook 'after-change-functions #'org-dividers-mode--on-after-change nil t)
     (add-hook 'window-configuration-change-hook #'org-dividers-mode--on-window-configuration-change nil t)
     ;; (add-hook 'window-scroll-functions #'org-dividers-mode--on-window-scroll nil t)
     ;; (add-hook 'window-buffer-change-functions #'org-dividers-mode--on-window-buffer-change nil t)
     ;; (add-hook 'after-change-major-mode-hook #'org-dividers-headline--redraw nil t)
     )
    (_
     (let ((beg (point-min)) (end (point-max)))
       (org-dividers-horizontal-rules-remove beg end)
       (org-dividers-headline-remove beg end))
     ;; (remove-hook 'after-change-major-mode-hook #'org-dividers-headline--redraw t)
     ;; (remove-hook 'window-buffer-change-functions #'org-dividers-mode--on-window-buffer-change t)
     ;; (remove-hook 'window-scroll-functions #'org-dividers-mode--on-window-scroll t)
     (remove-hook 'window-configuration-change-hook #'org-dividers-mode--on-window-configuration-change t)
     (remove-hook 'before-change-functions #'org-dividers-mode--on-before-change t)
     (remove-hook 'after-change-functions #'org-dividers-mode--on-after-change t))))

(provide 'org-dividers)
;;; org-dividers.el ends here
