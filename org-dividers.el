;;; org-dividers.el --- Org Dividers  -*- lexical-binding: t -*-
;;
;; Copyright (C) 2025-2026 Taro Sato
;;
;; Author: Taro Sato <okomestudio@gmail.com>
;; URL: https://github.com/okomestudio/org-dividers
;; Version: 0.2.1
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

;;; Headings

(defface org-dividers-heading '((t :inherit default))
  "Face used as heading.")

(defun org-dividers-heading--draw (title)
  "Style heading with TITLE at point."
  (save-excursion
    (org-back-to-heading t)
    (let* ((beg (point))
           (end (line-end-position))
           (face 'org-dividers-heading)
           (window-width (window-max-chars-per-line nil face))
           (suffix " ⎯⎯")
           (dash-count (max 0 (- window-width 1
                                 (string-width title)
                                 (string-width suffix))))
           (text (concat (make-string dash-count ?⎯) " " title suffix))
           (ov (make-overlay beg end)))
      (overlay-put ov 'category 'org-dividers)
      (overlay-put ov 'face face)
      (overlay-put ov 'display text)
      (overlay-put ov 'evaporate t)
      (overlay-put ov 'isearch-open-invisible t))))

(defcustom org-dividers-heading-regexp nil
  "Match regexp for heading texts to be styled."
  :group 'org-dividers
  :type 'string)

(defcustom org-dividers-heading-match nil
  "Value for MATCH in `org-map-entries'.
See the documentation for `org-map-entires' for what MATCH means."
  :group 'org-dividers
  :type 'string)

(defun org-dividers-heading-draw (&optional beg end len)
  "Apply styling to all headings with region.
When given, BEG and END specify a region, and LEN is the length of content being
removed. When not given, the region will be the entire buffer."
  (setq beg (or beg (point-min))
        end (or end (point-max)))
  (save-restriction
    (narrow-to-region beg end)
    (org-dividers-heading-remove beg end len)
    (remove-overlays beg end 'category 'org-dividers)
    (org-map-entries
     (lambda ()
       (when-let*
           ((s (org-element-property :title (org-element-at-point)))
            (_ (string-match org-dividers-heading-regexp s)))
         (org-dividers-heading--draw s)))
     org-dividers-heading-match)))

(defun org-dividers-heading-remove (&optional beg end len)
  "Remove all styles from heading.
When given, BEG and END specify a region, and LEN is the length of content being
removed. When not given, the region will be the entire buffer."
  (setq beg (or beg (point-min))
        end (or end (point-max)))
  (remove-overlays beg end 'category 'org-dividers))

;;;###autoload
(define-minor-mode org-dividers-mode
  "A minor mode for styling Org headings and dividers."
  :group 'org-dividers
  :lighter "OrgD"
  (pcase org-dividers-mode
    ('t
     (add-hook 'after-change-major-mode-hook #'org-dividers-heading-draw nil t)
     (add-hook 'after-change-functions #'org-dividers-heading-draw nil t)
     (add-hook 'window-configuration-change-hook #'org-dividers-heading-draw nil t))
    (_
     (org-dividers-heading-remove (point-min) (point-max))
     (remove-hook 'after-change-major-mode-hook #'org-dividers-heading-draw t)
     (remove-hook 'after-change-functions #'org-dividers-heading-draw t)
     (remove-hook 'window-configuration-change-hook #'org-dividers-heading-draw t))))

(provide 'org-dividers)
;;; org-dividers.el ends here
