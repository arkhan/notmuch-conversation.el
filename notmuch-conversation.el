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

;;; Inline compose area

(defface notmuch-conversation-compose-separator
  '((t :inherit font-lock-comment-face :extend t))
  "Face for the separator line between thread and compose area."
  :group 'notmuch-conversation)

(defcustom notmuch-conversation-separator
  (make-string 72 ?-)
  "Separator string shown between the thread and the inline compose area."
  :type 'string
  :group 'notmuch-conversation)

(defvar-local notmuch-conversation--compose-overlay nil
  "Overlay marking the compose area inserted at the bottom of the show buffer.")

(defvar-local notmuch-conversation--compose-msg-id nil
  "Message-id of the message being replied to inline.")

(defvar-local notmuch-conversation--compose-headers nil
  "Reply headers plist (To, Cc, Subject, From, In-Reply-To, References).")

(defun notmuch-conversation--compose-active-p ()
  "Return non-nil if an inline compose area is currently open."
  (and notmuch-conversation--compose-overlay
       (overlay-buffer notmuch-conversation--compose-overlay)))

(defun notmuch-conversation--compose-start ()
  "Return the start position of the compose area, or nil."
  (when (notmuch-conversation--compose-active-p)
    (overlay-start notmuch-conversation--compose-overlay)))

(defun notmuch-conversation--compose-body-start ()
  "Return point just after the header block in the compose area."
  (when (notmuch-conversation--compose-active-p)
    (save-excursion
      (goto-char (overlay-start notmuch-conversation--compose-overlay))
      ;; Skip separator line + blank line + header lines.
      (re-search-forward "^$" (overlay-end notmuch-conversation--compose-overlay) t)
      (forward-char 1)
      (point))))

(defun notmuch-conversation--remove-compose-area ()
  "Remove the inline compose area and restore buffer read-only state."
  (when (notmuch-conversation--compose-active-p)
    (let ((inhibit-read-only t)
          (start (overlay-start notmuch-conversation--compose-overlay))
          (end   (overlay-end   notmuch-conversation--compose-overlay)))
      (delete-overlay notmuch-conversation--compose-overlay)
      (delete-region start end)))
  (setq notmuch-conversation--compose-overlay nil
        notmuch-conversation--compose-msg-id  nil
        notmuch-conversation--compose-headers nil)
  (setq buffer-read-only t))

(defun notmuch-conversation--build-header-block (reply-plist)
  "Return a header string built from REPLY-PLIST (:reply-headers sub-plist)."
  (let* ((rh      (plist-get reply-plist :reply-headers))
         (to      (or (plist-get rh :To) ""))
         (cc      (or (plist-get rh :Cc) ""))
         (subject (or (plist-get rh :Subject) ""))
         (from    (or (plist-get rh :From) "")))
    (concat
     (format "From: %s\n" from)
     (format "To: %s\n" to)
     (when (and cc (not (string-empty-p cc)))
       (format "Cc: %s\n" cc))
     (format "Subject: %s\n" subject)
     "\n")))

(defun notmuch-conversation--insert-compose-area (reply-plist cited-body)
  "Insert an editable compose area at the bottom of the current show buffer.

REPLY-PLIST is the sexp returned by `notmuch reply --format=sexp'.
CITED-BODY is the pre-formatted string to pre-fill as the message body."
  (let* ((inhibit-read-only t)
         (sep      (propertize
                    (concat "\n" notmuch-conversation-separator "\n")
                    'face 'notmuch-conversation-compose-separator
                    'read-only t
                    'rear-nonsticky '(read-only face)))
         (header-block (notmuch-conversation--build-header-block reply-plist))
         (header-str   (propertize header-block
                                   'face 'message-header-other
                                   'read-only t
                                   'rear-nonsticky '(read-only face)))
         (start (progn (goto-char (point-max)) (point))))
    (insert sep header-str cited-body)
    (setq notmuch-conversation--compose-overlay
          (make-overlay start (point-max) nil nil t))
    (overlay-put notmuch-conversation--compose-overlay
                 'notmuch-conversation-compose t)
    ;; Make buffer writable from compose-start onwards.
    (setq buffer-read-only nil)
    ;; Re-apply read-only to everything before compose area.
    (put-text-property (point-min) start 'read-only t)))

;;;###autoload
(defun notmuch-conversation-reply-inline (&optional cite-region-p)
  "Open an inline compose area at the bottom of the notmuch-show buffer.

When CITE-REGION-P is non-nil (or called with prefix arg and an active
region), cite only the selected region instead of the full message body.

The compose area shows the reply headers (From/To/Cc/Subject) as
read-only and leaves the body editable.  Use:
  \\[notmuch-conversation-send]       to send
  \\[notmuch-conversation-abort-compose] to discard"
  (interactive (list (and current-prefix-arg (region-active-p))))
  (when (notmuch-conversation--compose-active-p)
    (user-error "Compose area already open; send or abort first"))
  (let* ((msg-id  (notmuch-show-get-message-id))
         (props   (notmuch-show-get-message-properties))
         (headers (plist-get props :headers))
         (from    (or (plist-get headers :From) ""))
         (date    (or (plist-get headers :Date) ""))
         ;; Fetch reply skeleton from notmuch CLI.
         (reply-plist (apply #'notmuch-call-notmuch-sexp
                             `("reply" "--format=sexp" "--format-version=5"
                               "--reply-to=all" ,msg-id)))
         (original    (plist-get reply-plist :original))
         ;; Build cited body.
         (cited-body
          (if (and cite-region-p (region-active-p))
              ;; Cite only region.
              (let ((rtxt (buffer-substring-no-properties
                           (region-beginning) (region-end))))
                (concat (format "On %s, %s wrote:\n" date from)
                        (notmuch-conversation--cite-string rtxt)
                        "\n\n"))
            ;; Cite full message body replicating notmuch-mua-reply logic.
            (with-temp-buffer
              (let ((notmuch-show-insert-text/plain-hook nil)
                    (notmuch-show-max-text-part-size 0)
                    (notmuch-show-insert-header-p-function
                     notmuch-mua-reply-insert-header-p-function)
                    (notmuch-show-indent-multipart nil)
                    (mm-inline-override-types (notmuch--inline-override-types)))
                ;; Insert From/Date headers so notmuch-mua-cite-function can read them.
                (insert "From: " from "\n")
                (insert "Date: " date "\n\n")
                (cl-letf (((symbol-function 'notmuch-crypto-insert-sigstatus-button) #'ignore)
                          ((symbol-function 'notmuch-crypto-insert-encstatus-button) #'ignore))
                  (notmuch-show-insert-body original (plist-get original :body) 0))
                (set-mark (point))
                (goto-char (point-min))
                (funcall notmuch-mua-cite-function)
                (buffer-substring-no-properties (point-min) (point-max))))))
    (setq notmuch-conversation--compose-msg-id  msg-id
          notmuch-conversation--compose-headers reply-plist)
    (save-excursion
      (notmuch-conversation--insert-compose-area reply-plist cited-body))
    ;; Move point to after the headers (body start).
    (goto-char (point-max))
    (when (re-search-backward "^$"
                              (notmuch-conversation--compose-start) t)
      (forward-line 1))))

;;;###autoload
(defun notmuch-conversation-abort-compose ()
  "Discard the inline compose area without sending."
  (interactive)
  (unless (notmuch-conversation--compose-active-p)
    (user-error "No inline compose area is open"))
  (when (yes-or-no-p "Discard inline reply? ")
    (notmuch-conversation--remove-compose-area)
    (goto-char (point-max))
    (message "Reply discarded.")))

;;;###autoload
(defun notmuch-conversation-send ()
  "Send the message composed in the inline compose area.

Collects the headers and body from the compose area, opens a
`message-mode' buffer, inserts everything, and calls `message-send-and-exit'."
  (interactive)
  (unless (notmuch-conversation--compose-active-p)
    (user-error "No inline compose area to send"))
  (let* ((compose-start (notmuch-conversation--compose-start))
         (compose-end   (point-max))
         ;; Extract body: everything after the blank line following headers.
         (body-start    (save-excursion
                          (goto-char compose-start)
                          (if (re-search-forward "^$" compose-end t)
                              (progn (forward-char 1) (point))
                            compose-end)))
         (body-text     (buffer-substring-no-properties body-start compose-end))
         (rh            (plist-get notmuch-conversation--compose-headers
                                   :reply-headers))
         (to            (or (plist-get rh :To) ""))
         (subject       (or (plist-get rh :Subject) ""))
         (extra-headers (notmuch-headers-plist-to-alist rh))
         (msg-id        notmuch-conversation--compose-msg-id))
    ;; Remove compose area before opening message buffer to avoid confusion.
    (notmuch-conversation--remove-compose-area)
    ;; Open message buffer using notmuch's mail function.
    (notmuch-mua-mail to subject extra-headers nil (notmuch-mua-get-switch-function))
    ;; Insert body.
    (message-goto-body)
    (insert body-text)
    ;; Queue replied tag change.
    (when notmuch-message-replied-tags
      (setq notmuch-message-queued-tag-changes
            (list (cons msg-id notmuch-message-replied-tags))))
    (set-buffer-modified-p nil)
    (message "Ready to send — use %s or M-x message-send-and-exit."
             (substitute-command-keys "\\[message-send-and-exit]"))))

;;; cite-region (standalone — opens normal compose window)

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

;;;###autoload
(define-minor-mode notmuch-conversation-mode
  "Global minor mode adding sender colors and inline compose to notmuch-show.

When enabled:
- Each unique sender in a thread is highlighted with a distinct color.
- \"r\" opens an inline compose area at the bottom of the show buffer.
- \"R\" (with active region) cites only the selected text inline.
- \"C-c C-c\" in the compose area sends the message.
- \"C-c C-k\" discards the compose area."
  :global t
  :group 'notmuch-conversation
  :lighter " NConv"
  (if notmuch-conversation-mode
      (progn
        (add-hook 'notmuch-show-hook #'notmuch-conversation--apply-colors)
        (define-key notmuch-show-mode-map (kbd "r")
                    #'notmuch-conversation-reply-inline)
        (define-key notmuch-show-mode-map (kbd "R")
                    (lambda ()
                      (interactive)
                      (notmuch-conversation-reply-inline t)))
        (define-key notmuch-show-mode-map (kbd "C-c C-c")
                    #'notmuch-conversation-send)
        (define-key notmuch-show-mode-map (kbd "C-c C-k")
                    #'notmuch-conversation-abort-compose))
    (remove-hook 'notmuch-show-hook #'notmuch-conversation--apply-colors)
    (define-key notmuch-show-mode-map (kbd "r") nil)
    (define-key notmuch-show-mode-map (kbd "R") nil)
    (define-key notmuch-show-mode-map (kbd "C-c C-c") nil)
    (define-key notmuch-show-mode-map (kbd "C-c C-k") nil)
    ;; Clean up overlays and compose areas in all show buffers.
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (eq major-mode 'notmuch-show-mode)
          (notmuch-conversation--clear-overlays)
          (when (notmuch-conversation--compose-active-p)
            (notmuch-conversation--remove-compose-area)))))))

(provide 'notmuch-conversation)
;;; notmuch-conversation.el ends here
