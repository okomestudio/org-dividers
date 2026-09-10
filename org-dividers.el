;;; org-dividers.el --- Org Dividers  -*- lexical-binding: t -*-
;;
;; Copyright (C) 2025-2026 Taro Sato
;;
;; Author: Taro Sato <okomestudio@gmail.com>
;; URL: https://github.com/okomestudio/org-dividers
;; Version: 0.6.2
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

(defun org-dividers-hr--remove-all (beg end)
  "Remove horizontal rule overlays in region from BEG to END."
  (remove-overlays beg end 'category 'org-dividers-hr))

(defun org-dividers-hr--remove (ov after-p _beg _end &optional _len)
  "Remove overlay OV when AFTER-P is non-nil."
  (unless after-p
    (delete-overlay ov)))

(defun org-dividers-hr--redraw (beg end len)
  "Redraw horizontal rules within a region from BEG to END.
For the meaning of BEG, END, and LEN, see `after-change-functions'."
  (save-excursion
    (save-restriction
      (widen)
      (let ((l-beg (save-excursion (goto-char beg) (line-beginning-position)))
            (l-end (save-excursion (goto-char end) (line-end-position))))
        (org-dividers-hr--remove-all l-beg l-end)
        (narrow-to-region l-beg l-end)
        (org-element-map (org-element-parse-buffer) 'horizontal-rule
          (lambda (hr)
            (when-let*
                ((hr-beg (org-element-property :begin hr))
                 (hr-end (save-excursion (goto-char hr-beg) (line-end-position)))
                 (hr-len (length (string-trim (buffer-substring-no-properties hr-beg hr-end))))
                 (hr-sub (or (alist-get hr-len org-dividers-hr-styles)
                             (alist-get t org-dividers-hr-styles)))
                 (im-file (let ((f (car hr-sub)))
                            (or (and (file-exists-p f) f)
                                (let ((f (file-name-concat org-dividers--dir "images" f)))
                                  (and (file-exists-p f) f)))))
                 (im-scale (cdr hr-sub)))
              (let* ((win-width (- (window-width nil t)
                                   (if (bound-and-true-p org-indent-mode)
                                       (* (or (org-current-level) 0)
                                          org-indent-indentation-per-level
                                          (default-font-width))
                                     0)))
                     (image (create-image im-file nil nil :scale im-scale))
                     (margin (- (/ win-width 2) (/ (car (image-size image t)) 2)))
                     (ov (make-overlay hr-beg hr-end nil 'front-adv nil)))
                (overlay-put ov 'category 'org-dividers-hr)
                (overlay-put ov 'display (append image `(:margin (,margin . 0))))
                (overlay-put ov 'evaporate t)
                (overlay-put ov 'modification-hooks '(org-dividers-hr--remove))
                (overlay-put ov 'insert-in-front-hooks '(org-dividers-hr--remove))
                (overlay-put ov 'insert-behind-hooks '(org-dividers-hr--remove))
                (overlay-put ov 'priority 90)
                nil)))
          nil nil nil)))))

;;; Headlines

(defun org-dividers-hl--format (win text face)
  "Format headline  given headline TEXT and FACE in window WIN."
  (let ((win (or win
                 (get-buffer-window (current-buffer) t)
                 (selected-window))))
    (with-selected-window win
      (let* ((level-1 (max 0 (1- (or (org-current-level) 1))))
             (indent-width (if (bound-and-true-p org-indent-mode)
                               (* level-1 org-indent-indentation-per-level)
                             0))
             (total-width (max 0 (- (window-max-chars-per-line win face)
                                    level-1
                                    indent-width)))
             (dash-count (max 0 (- total-width
                                   (* 2 org-dividers-hl-padding-outer)
                                   (* 2 org-dividers-hl-padding-inner)
                                   (string-width text))))
             (pos org-dividers-hl-text-position)
             (dash-left (max 0 (if (> pos 0) pos (+ dash-count pos))))
             (dash-right (max 0 (if (> pos 0) (- dash-count pos) (abs pos)))))
        (propertize
         (concat (make-string level-1 ?\s)
                 (make-string org-dividers-hl-padding-outer ?\s)
                 (make-string dash-left org-dividers-hl-char)
                 (make-string org-dividers-hl-padding-inner ?\s)
                 text
                 (make-string org-dividers-hl-padding-inner ?\s)
                 (make-string dash-right org-dividers-hl-char)
                 (make-string org-dividers-hl-padding-outer ?\s))
         'face face)))))

(defun org-dividers-hl--draw (win hl)
  "Draw headline element HL as overlay in window WIN."
  (let* ((beg (org-element-property :begin hl))
         (end (save-excursion (goto-char beg) (line-end-position)))
         (title (org-element-property :title hl))
         (text (org-dividers-hl--format win title 'org-dividers-hl))
         (ov (make-overlay beg end nil 'front-adv nil)))
    (overlay-put ov 'category 'org-dividers-hl)
    (overlay-put ov 'window win)
    (overlay-put ov 'display text)
    (overlay-put ov 'evaporate t)
    (overlay-put ov 'isearch-open-invisible t)))

(defun org-dividers-hl--remove-all (beg end)
  "Remove all headline overlays in region from BEG to END."
  (remove-overlays beg end 'category 'org-dividers-hl))

(defun org-dividers-hl--redraw (beg end len)
  "Redraw headlines in region from BEG to END.
See `after-change-functions' for what BEG, END, and LEN means."
  (when (or (null len) (> (1- end) beg))
    (save-excursion
      (save-restriction
        (goto-char beg)
        (setq beg (line-beginning-position))
        (goto-char end)
        (setq end (line-end-position))
        (org-dividers-hl--remove-all beg end)

        (narrow-to-region beg end)
        (dolist (win (get-buffer-window-list (current-buffer) nil t))
          (goto-char (point-min))
          (org-map-entries
           (lambda ()
             (when-let*
                 ((el (org-element-at-point))
                  (_ (and (eq (org-element-type el) 'headline)
                          org-dividers-hl-title
                          (string-match-p org-dividers-hl-title
                                          (org-element-property :title el)))))
               (org-dividers-hl--draw win el)))
           org-dividers-hl-match))))))

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
