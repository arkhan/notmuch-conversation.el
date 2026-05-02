;;; notmuch-conversation.el --- Sender colors and cite-region for notmuch-show  -*- lexical-binding: t -*-

;; Copyright (C) 2024  Contributors

;; Author: Contributors
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1") (notmuch "0.37"))
;; Keywords: mail, notmuch
;; URL: https://github.com/arkhan/notmuch-conversation.el

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
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

;; This package adds two features to notmuch-show:
;;
;;  1. Sender colors: each unique sender in a thread gets a distinct color,
;;     derived from outline-1..8 faces.  Your own messages use a special face.
;;
;;  2. cite-region: select text in notmuch-show, press the keybind, and a
;;     reply compose window opens with just that region cited (not the full
;;     message body).
;;
;; Installation (elpaca example):
;;
;;   (use-package notmuch-conversation
;;     :load-path "~/notmuch-conversation.el/"
;;     :after notmuch
;;     :config
;;     (notmuch-conversation-mode 1))
;;
;; The minor mode wires everything up.  `notmuch-conversation-cite-region' is
;; also bound to "R" in `notmuch-show-mode-map' when the mode is enabled.

;;; Code:

(require 'notmuch)
(require 'notmuch-show)
(require 'notmuch-mua)

;;; Options

(defgroup notmuch-conversation nil
  "Sender colors and cite-region for notmuch-show."
  :group 'notmuch)

(defcustom notmuch-conversation-max-colors 8
  "Maximum number of sender colors to cycle through.
Set to 0 to disable sender coloring."
  :type 'integer
  :group 'notmuch-conversation)

(defcustom notmuch-conversation-cite-prefix "> "
  "Prefix string used when citing a region."
  :type 'string
  :group 'notmuch-conversation)

;;; Faces

(defface notmuch-conversation-sender-me
  '((t :inherit font-lock-keyword-face))
  "Face for messages sent by the user themselves."
  :group 'notmuch-conversation)

;; Faces inherit from outline-N.  We use :inherit so the face definition works
;; even in batch/headless mode where outline faces may not yet be loaded.
(defface notmuch-conversation-sender-1
  '((t :inherit outline-1))
  "Face for the first external sender in a thread."
  :group 'notmuch-conversation)

(defface notmuch-conversation-sender-2
  '((t :inherit outline-2))
  "Face for the second external sender in a thread."
  :group 'notmuch-conversation)

(defface notmuch-conversation-sender-3
  '((t :inherit outline-3))
  "Face for the third external sender in a thread."
  :group 'notmuch-conversation)

(defface notmuch-conversation-sender-4
  '((t :inherit outline-4))
  "Face for the fourth external sender in a thread."
  :group 'notmuch-conversation)

(defface notmuch-conversation-sender-5
  '((t :inherit outline-5))
  "Face for the fifth external sender in a thread."
  :group 'notmuch-conversation)

(defface notmuch-conversation-sender-6
  '((t :inherit outline-6))
  "Face for the sixth external sender in a thread."
  :group 'notmuch-conversation)

(defface notmuch-conversation-sender-7
  '((t :inherit outline-7))
  "Face for the seventh external sender in a thread."
  :group 'notmuch-conversation)

(defface notmuch-conversation-sender-8
  '((t :inherit outline-8))
  "Face for the eighth external sender in a thread."
  :group 'notmuch-conversation)

;;; Internal helpers

(defvar notmuch-conversation--sender-faces
  [notmuch-conversation-sender-1
   notmuch-conversation-sender-2
   notmuch-conversation-sender-3
   notmuch-conversation-sender-4
   notmuch-conversation-sender-5
   notmuch-conversation-sender-6
   notmuch-conversation-sender-7
   notmuch-conversation-sender-8]
  "Vector of sender faces, indexed 0-7.")

(defun notmuch-conversation--user-emails ()
  "Return a list of the user's own email addresses.
Uses `notmuch-user-primary-email' and `notmuch-user-other-email'."
  (let ((primary (bound-and-true-p notmuch-user-primary-email))
        (others  (bound-and-true-p notmuch-user-other-email)))
    (delq nil (append (when primary (list primary))
                      (when (listp others) others)
                      (when (stringp others) (list others))))))

(defun notmuch-conversation--address-email (addr-string)
  "Extract the bare email address from ADDR-STRING.
ADDR-STRING may be \"Name <email>\" or just \"email\"."
  (if (string-match "<\\([^>]+\\)>" addr-string)
      (match-string 1 addr-string)
    (string-trim addr-string)))

(defun notmuch-conversation--from-email (msg-props)
  "Return the bare From email address from MSG-PROPS plist."
  (let ((from (plist-get (plist-get msg-props :headers) :From)))
    (when from
      (notmuch-conversation--address-email from))))

(defun notmuch-conversation--collect-senders ()
  "Return an alist mapping email -> face for every message in the buffer.

Iterates the buffer with `notmuch-show-mapc'.  Own addresses get
`notmuch-conversation-sender-me'; others are assigned faces from
`notmuch-conversation--sender-faces' in order of first appearance."
  (let ((user-emails (notmuch-conversation--user-emails))
        (sender-index 0)
        (seen-table (make-hash-table :test #'equal))
        result)
    (notmuch-show-mapc
     (lambda ()
       (let* ((props (notmuch-show-get-message-properties))
              (email (notmuch-conversation--from-email props)))
         (when (and email (not (gethash email seen-table)))
           (let ((face
                  (if (member email user-emails)
                      'notmuch-conversation-sender-me
                    (when (and (> notmuch-conversation-max-colors 0)
                               (< sender-index notmuch-conversation-max-colors))
                      (let ((f (aref notmuch-conversation--sender-faces
                                     (mod sender-index
                                          (length notmuch-conversation--sender-faces)))))
                        (setq sender-index (1+ sender-index))
                        f)))))
             (puthash email face seen-table)
             (push (cons email face) result))))))
    (nreverse result)))

;;; Overlays for sender colors

(defvar-local notmuch-conversation--overlays nil
  "List of overlays created by `notmuch-conversation--apply-colors'.")

(defun notmuch-conversation--clear-overlays ()
  "Remove all notmuch-conversation color overlays from the current buffer."
  (mapc #'delete-overlay notmuch-conversation--overlays)
  (setq notmuch-conversation--overlays nil))

(defun notmuch-conversation--apply-colors ()
  "Apply per-sender face overlays to every message in the current buffer.

Called via `notmuch-show-hook' after the buffer is fully populated."
  (when (eq major-mode 'notmuch-show-mode)
    (notmuch-conversation--clear-overlays)
    (let ((sender-faces (notmuch-conversation--collect-senders)))
      (notmuch-show-mapc
       (lambda ()
         (let* ((props  (notmuch-show-get-message-properties))
                (email  (notmuch-conversation--from-email props))
                (face   (cdr (assoc email sender-faces)))
                (extent (get-text-property (point) :notmuch-message-extent)))
           (when (and face extent)
             (let* ((start (car extent))
                    (end   (cdr extent))
                    (ov    (make-overlay start end)))
               (overlay-put ov 'face face)
               (overlay-put ov 'notmuch-conversation t)
               (push ov notmuch-conversation--overlays)))))))))

;;; cite-region

(defun notmuch-conversation--cite-string (text)
  "Return TEXT cited with `notmuch-conversation-cite-prefix'."
  (let ((lines (split-string text "\n")))
    (mapconcat (lambda (line)
                 (if (string-empty-p line)
                     ""
                   (concat notmuch-conversation-cite-prefix line)))
               lines
               "\n")))

;;;###autoload
(defun notmuch-conversation-cite-region (beg end)
  "Reply to the current message, citing only the selected region (BEG..END).

Select text in a notmuch-show buffer and call this command.  A compose
window opens pre-filled with the standard reply headers and the selected
text cited (not the full message).  Uses `notmuch-show-reply' to build
the reply skeleton, then replaces the auto-inserted citation with the
selected region."
  (interactive "r")
  (unless (region-active-p)
    (user-error "No active region; select text first"))
  (let* ((region-text (buffer-substring-no-properties beg end))
         (cited-text  (notmuch-conversation--cite-string region-text))
         (msg-id      (notmuch-show-get-message-id))
         ;; Grab From/Date for the citation header line.
         (props       (notmuch-show-get-message-properties))
         (headers     (plist-get props :headers))
         (from        (or (plist-get headers :From) ""))
         (date        (or (plist-get headers :Date) "")))
    ;; Open a standard reply (reply-to-all).
    (notmuch-mua-new-reply msg-id nil t)
    ;; Now we are in the compose buffer.  Replace whatever citation was
    ;; inserted with just our region, preserving the attribution line.
    (save-restriction
      (message-goto-body)
      ;; Delete everything from body start to signature (if present).
      (let ((body-start (point))
            (sig-pos    (save-excursion
                          (if (re-search-forward message-signature-separator nil t)
                              (match-beginning 0)
                            (point-max)))))
        (delete-region body-start sig-pos)
        ;; Insert attribution + cited region.
        (insert (format "On %s, %s wrote:\n" date from))
        (insert cited-text)
        (unless (string-suffix-p "\n" cited-text)
          (insert "\n"))
        (insert "\n")))
    ;; Leave point at body start for typing.
    (message-goto-body)))

;;; Minor mode

(defvar notmuch-conversation-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "R") #'notmuch-conversation-cite-region)
    map)
  "Keymap for `notmuch-conversation-mode' (active in notmuch-show buffers).")

;;;###autoload
(define-minor-mode notmuch-conversation-mode
  "Global minor mode adding sender colors and cite-region to notmuch-show.

When enabled:
- Each unique sender in a thread is highlighted with a distinct color.
- \"R\" in notmuch-show opens a reply citing only the selected region."
  :global t
  :group 'notmuch-conversation
  :lighter " NConv"
  (if notmuch-conversation-mode
      (progn
        (add-hook 'notmuch-show-hook #'notmuch-conversation--apply-colors)
        (define-key notmuch-show-mode-map (kbd "R")
                    #'notmuch-conversation-cite-region))
    (remove-hook 'notmuch-show-hook #'notmuch-conversation--apply-colors)
    (define-key notmuch-show-mode-map (kbd "R") nil)
    ;; Clean up overlays in all show buffers.
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (eq major-mode 'notmuch-show-mode)
          (notmuch-conversation--clear-overlays))))))

(provide 'notmuch-conversation)
;;; notmuch-conversation.el ends here
