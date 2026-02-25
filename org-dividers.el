;;; org-dividers.el --- Org Dividers  -*- lexical-binding: t -*-
;;
;; Copyright (C) 2025-2026 Taro Sato
;;
;; Author: Taro Sato <okomestudio@gmail.com>
;; URL: https://github.com/okomestudio/org-dividers
;; Version: 0.3.1
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

;;; Dividers (WIP)

(defcustom org-dividers-horizontal-rules nil
  "Cons of the form (image-file . scale)."
  :type '(cons string number))

(defface org-dividers-horizontal-rule '((t :inherit default))
  "Face used for horizontal rules.")

(defun org-dividers-horizontal-rules-apply-style ()
  "Replace horizontal rules with an image using an overlay."
  (interactive)
  ;; TODO(2025-07-01): Compute the padding correctly.
  (when (and (derived-mode-p 'org-mode) org-dividers-horizontal-rules)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^-\\{5,\\}$" nil t)
        (let ((pt (point)))
          (beginning-of-line)
          (let* ((face 'org-dividers-horizontal-rule)
                 (window-width (window-max-chars-per-line nil face))
                 (image (create-image (car org-dividers-horizontal-rules)
                                      nil
                                      nil
                                      :scale (cdr org-dividers-horizontal-rules)))
                 (image-width (/ (car (image-size image t)) (frame-char-width)))
                 (padding (max 0 (/ (- window-width image-width) 2)))
                 (centered-text (concat (make-string padding ? ) " "))
                 (ov (make-overlay (match-beginning 0) (match-end 0))))
            (message "DEBUG: %d %d %d" window-width image-width padding)
            (overlay-put ov 'category 'org-dividers)
            (overlay-put ov 'face face)
            (overlay-put ov 'before-string centered-text)
            (overlay-put ov 'display image)
            (overlay-put ov 'evaporate t))
          (goto-char pt))))))

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
         (ov (make-overlay beg end nil 'front-adavnce nil)))
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

(defun org-dividers-headline--redraw ()
  "Redraw all headlines dividers."
  (let ((beg (point-min)) (end (point-max)))
    (org-dividers-headline-remove beg end)
    (org-dividers-headline-draw beg end (- end beg))))

;;;###autoload
(define-minor-mode org-dividers-mode
  "A minor mode for styling Org headlines as dividers."
  :group 'org-dividers
  :lighter "OrgD"
  (pcase org-dividers-mode
    ('t
     (add-hook 'before-change-functions #'org-dividers-headline-remove nil t)
     (add-hook 'after-change-functions #'org-dividers-headline-draw nil t)
     (add-hook 'after-change-major-mode-hook #'org-dividers-headline--redraw nil t)
     (add-hook 'window-configuration-change-hook #'org-dividers-headline--redraw nil t))
    (_
     (org-dividers-headline-remove (point-min) (point-max))
     (remove-hook 'after-change-major-mode-hook #'org-dividers-headline--redraw t)
     (remove-hook 'window-configuration-change-hook #'org-dividers-headline--redraw t)
     (remove-hook 'after-change-functions #'org-dividers-headline-draw t)
     (remove-hook 'before-change-functions #'org-dividers-headline-remove t))))

(provide 'org-dividers)
;;; org-dividers.el ends here
