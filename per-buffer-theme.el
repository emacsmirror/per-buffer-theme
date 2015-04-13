;;; per-buffer-theme.el --- Change theme according to buffer name or major mode.

;; Copyright (C) 2015 Free Software Foundation, Inc.

;; Author: Iñigo Serna <inigoserna@gmail.com>
;; URL: https://bitbucket.com/inigoserna/per-buffer-theme.el
;; Version: 1.1
;; Keywords: themes
;; Package-Requires: ((cl-lib "0.5"))

;; This file is not part of GNU Emacs.

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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; `per-buffer-theme.el' is an Emacs library that automatically changes
;; the global theme according to buffer name or major mode.
;;
;; It runs through `window-configuration-change-hook' so it is not perfect.
;;
;; If buffer name matches any of `per-buffer-theme/ignored-buffernames-regex'
;; no theme change occurs.
;;
;; Customizable variable `per-buffer-theme/themes-alist' contains the
;; association between themes and buffer name or major modes.
;;
;; Special `notheme' theme can be used to make unload all themes and use emacs
;; default theme.
;;
;; If no theme matches then it'll load the theme stored in
;; `per-buffer-theme/default-theme'.

;;; Updates:

;; 2015/04/12 Initial version.
;; 2015/04/13 Changed `per-buffer-theme/theme-list' data type from plist
;;            to alist to make customization easier.
;;            Make code comply with (some) Emacs Lisp Code Conventions:
;;            - added public function to unload hook


;;; Code:
(require 'cl-lib)

;;; Variables
(defvar pbt/current-theme nil
  "Theme in use.")

;;; Customization
(defgroup per-buffer-theme nil
  "Change theme according to buffer name or major mode."
  :link '(emacs-library-link :tag "Source Lisp File" "per-buffer-theme.el")
  :prefix "per-buffer-theme/"
  :group 'customize)

(defcustom per-buffer-theme/default-theme 'inigo
  "Default theme to be used if no matching is found."
  :type 'symbol
  :group 'per-buffer-theme)

(defcustom per-buffer-theme/ignored-buffernames-regex '("^*mini" "^*Mini" "^*helm" "^*Helm")
  "If current buffer name matches one of these it won't change the theme."
  :type '(repeat string)
  :group 'per-buffer-theme)

(defcustom per-buffer-theme/themes-alist
      '(((:theme . notheme)
         (:buffernames . ("^*eww" "^*w3m" "^*mu4e"))
         (:modes . (eww-mode w3m-mode cfw:calendar-mode mu4e-main-mode mu4e-headers-mode mu4e-view-mode mu4e-compose-mode mu4e-about-mode mu4e-update-mode)))
        ((:theme . adwaita)
         (:buffernames . ("*Help*"))
         (:modes . (nil))))
  "An alist with default associations (theme buffernames modes).
Special `notheme' theme can be used to unload all themes."
  :type '(repeat alist)
  :group 'per-buffer-theme)

;;; Internal functions
(defun pbt~match-theme ()
  "Return theme if buffer name or major-mode match in
`per-buffer-theme/themes-alist' or nil."
  (let ((alist per-buffer-theme/themes-alist)
        the-theme)
    ;; (message "THEMES: %s" (prin1-to-string alist))
    (while alist
      (let* ((props (pop alist))
             (theme (cdr (assoc :theme props)))
             (buffernames (cdr (assoc :buffernames props)))
             (modes (cdr (assoc :modes props))))
        ;; (message "Theme: %s" (prin1-to-string theme))
        ;; (message "  Properties: %s" (prin1-to-string props))
        ;; (message "    Buffer names: %s" (prin1-to-string buffernames))
        ;; (message "    Major modes: %s" (prin1-to-string modes))
        (when (cl-some (lambda (regex) (string-match regex (buffer-name))) buffernames)
          ;; (message "=> Matched buffer name with regex '%s' => Theme: %s " (match-string 0 (buffer-name)) theme)
          (setq the-theme theme
                alist nil))
        (when (member major-mode modes)
          ;; (message "=> Matched with major mode '%s' => Theme: %s " major-mode theme)
          (setq the-theme theme
                alist nil))))
    the-theme))

;;; Public interface
(defun per-buffer-theme/change-theme-if-buffer-matches ()
  "Change theme according to active buffer major mode or name.
Don't do anything if buffer name matches in
`per-buffer-theme/ignored-buffernames-regex'.
Search for theme matches in `per-buffer-theme/themes-alist'
customizable variable.
If none is found uses default theme stored in `per-buffer-theme/default-theme'.
Special `notheme' theme can be used to disable all loaded themes."
  (interactive)
  (unless (cl-some (lambda (regex) (string-match regex (buffer-name))) per-buffer-theme/ignored-buffernames-regex)
    (let ((theme (pbt~match-theme)))
      (unless (equal pbt/current-theme theme)
        (cond
         ((equal theme 'notheme)
          (mapc #'disable-theme custom-enabled-themes))
         ((equal theme nil)
          (load-theme per-buffer-theme/default-theme t))
         (t
          (load-theme theme t)))
        (setq pbt/current-theme theme)))))

(defun per-buffer-theme/unload-hook ()
  "Disable `per-buffer-theme' package."
  (interactive)
  (remove-hook 'window-configuration-change-hook 'per-buffer-theme/change-theme-if-buffer-matches))

;;; Hooks and key bindings
(add-hook 'window-configuration-change-hook 'per-buffer-theme/change-theme-if-buffer-matches)
;; (global-set-key '[C-f3] 'per-buffer-theme/change-theme-if-buffer-matches)

(provide 'per-buffer-theme)
;;; per-buffer-theme.el ends here
