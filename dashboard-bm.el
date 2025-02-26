;;; dashboard-bm.el --- Visual Bookmarks for Dashboard  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Jeffrey Phillips
;; Copyright (C) 2024-2025  emacs-dashboard maintainers

;; Author: Jeffrey Phillips
;; Maintainer: Jeffrey Phillips
;;             Shen, Jen-Chieh <jcs090218@gmail.com>
;; URL: https://github.com/emacs-dashboard/dashboard-bm
;; Version: 0.0.1
;; Package-Requires: ((emacs "27.1") (dashboard "1.2.5") (bm "0"))
;; Keywords: tools

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Visual Bookmarks for Dashboard.
;;

;;; Code:

(require 'dashboard)
(require 'bm)

(defun dashboard-bm-show-bm-dash-action ()
  (interactive)
  (let ((file (get-text-property (point) 'file))
        (position (get-text-property (point) 'position)))
    (if (not file)
        (message "No bookmark at this line."))
    (let ((buffer (find-file-noselect file)))
      (with-current-buffer buffer
        (pop-to-buffer buffer)
        (goto-char position)))))

(defun dashboard-bm-insert-bm (list-size)
  (let ((bms '()))
    ;; Ensure we're fresh and synced by save and then load fresh, suppressing messages
    (cl-letf (((symbol-function 'message) #'ignore))
      (bm-buffer-save-all)
      (bm-repository-save)
      (bm-repository-load))
    (cl-loop for entry in bm-repository
             do
             (let* ((file (car entry))
                    (filename (f-filename file))
                    (buffer-data (assoc file bm-repository))
                    (bookmarks (cdr (assoc 'bookmarks buffer-data)))
                    (new-keymap (make-sparse-keymap "+my/dashboard-actions")))
               (keymap-set new-keymap "RET" #'dashboard-bm-show-bm-dash-action)
               (keymap-set new-keymap "<mouse-1>" #'dashboard-bm-show-bm-dash-action)
               (cl-loop for bookmark in bookmarks
                        do
                        (let* ((pos (cdr (assoc 'position bookmark)))
                               (time (cdr (assoc 'time bookmark)))
                               (annotation (cdr (assoc 'annotation bookmark)))
                               (context-string
                                (concat (cdr (assoc 'before-context-string bookmark))
                                        (cdr (assoc 'after-context-string bookmark))))
                               (line
                                (propertize
                                 (concat
                                  (dashboard-icon-for-file filename)
                                  (format " %-25s %-15s %s"
                                          (format "%s:%s"
                                                  (propertize filename 'face
                                                              'dashboard-items-face)
                                                  pos)
                                          (s-left 15 (or annotation ""))
                                          (replace-regexp-in-string
                                           "\n" " " context-string)))
                                 'mouse-face 'highlight
                                 'help-echo "Jump to bookmark"))
                               (start 0)
                               (end (length line)))
                          (add-text-properties start end `(keymap ,new-keymap
                                                                  file ,file
                                                                  position ,pos)
                                               line)
                          (push (cons time line) bms)))))
    (let ((section-name "BM's:")
          (shortcut "b")
          (icon (all-the-icons-faicon
                 "bookmark-o" :height 1.2 :v-adjust 0.0 :face 'dashboard-heading)))
      (if (= (length bms) 0)
          (progn
            ;; Empty section, so no shortcut needed
            (keymap-unset dashboard-mode-map "b")
            (dashboard-insert-shortcut 'bm "" nil)
            (dashboard-insert-heading section-name nil icon)
            (insert "\n    "
                    (propertize "--- No items ---" 'face 'dashboard-no-items-face)))
        (let* ((size (min list-size (length bms)))
               (sorted (sort bms (lambda (a b) (time-less-p (car b) (car a))))))
          (dashboard-insert-shortcut 'bm "b" "BM's")
          (dashboard-insert-heading section-name shortcut icon)
          (insert "\n    " (mapconcat 'cdr (cl-subseq sorted 0 size) "\n    ")))))))

(push '(bm . dashboard-bm-insert-bm) dashboard-item-generators)
(dashboard-modify-heading-icons '((bm . "bookmark-o")))

(provide 'dashboard-bm)
;;; dashboard-bm.el ends here
