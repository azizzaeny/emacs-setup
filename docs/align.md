align

```elisp

;; align
(defun align-to-colon (begin end)
  "Align region to colon"
  (interactive "r")
  (align-regexp begin end
                (rx (group (zero-or-more (syntax whitespace))) ":"  ) 1 1 ))

(defun align-to-comma (begin end)
  "Align region to comma signs"
  (interactive "r")
  (align-regexp begin end
                (rx "," (group (zero-or-more (syntax whitespace))) ) 1 1 ))

(defun align-to-equals (begin end)
  "Align region to equal signs"
  (interactive "r")
  (align-regexp begin end
                (rx (group (zero-or-more (syntax whitespace))) "=") 1 1 ))

(defun align-to-hash (begin end)
  "Align region to hash ( => ) signs"
  (interactive "r")
  (align-regexp begin end
                (rx (group (zero-or-more (syntax whitespace))) "=>") 1 1 ))

;; work with this
(defun align-to-comma-before (begin end)
  "Align region to equal signs"
  (interactive "r")
  (align-regexp begin end
                (rx (group (zero-or-more (syntax whitespace))) ",") 1 1 ))

```

popper 

```elisp 

(defvar my-popup-window nil
  "The current popup window, if any.")

(defun my-popup-open-or-switch (buffer-name)
  "Open a temporary popup window and display the specified BUFFER-NAME.
If the popup is already open, switch to the specified buffer."
  (interactive "BBuffer name: ")
  (let* ((buffer (get-buffer-create buffer-name))
         (popup-window (or my-popup-window
                           (split-window (frame-root-window)
                                         (floor (* 0.75 (window-total-height)))
                                         'below))))
    (set-window-buffer popup-window buffer)
    (setq my-popup-window popup-window)
    (select-window popup-window)))

(defun my-popup-close ()
  "Close the current popup window, if it exists."
  (interactive)
  (when (window-live-p my-popup-window)
    (delete-window my-popup-window)
    (setq my-popup-window nil)))

(defun my-popup-toggle ()
  "Toggle the popup window.
If it exists, close it. Otherwise, prompt for a buffer to open."
  (interactive)
  (if (window-live-p my-popup-window)
      (my-popup-close)
    (let ((buffer-name (read-buffer "Buffer name to display: ")))
      (my-popup-open-or-switch buffer-name))))

```

delete word

```elisp
(require 'subword)

(defun delete-block-forward ()
  (interactive)
  (if (eobp)
      (message "End of buffer")
    (let* ((syntax-move-point
            (save-excursion
              (skip-syntax-forward (string (char-syntax (char-after))))
              (point)
              ))
           (subword-move-point
            (save-excursion
              (subword-forward)
              (point))))
      (kill-region (point) (min syntax-move-point subword-move-point)))))

(defun delete-block-backward ()
  (interactive)
  (if (bobp)
      (message "Beginning of buffer")
    (let* ((syntax-move-point
            (save-excursion
              (skip-syntax-backward (string (char-syntax (char-before))))
              (point)
              ))
           (subword-move-point
            (save-excursion
              (subword-backward)
              (point))))
      (kill-region (point) (max syntax-move-point subword-move-point)))))


(defun kill-whole-line ()
  "Kill the entire current line."
  (interactive)
  (beginning-of-line)
  (kill-line 1))

(defun join-line ()
  "Join the following line to this one."
  (interactive)
  (end-of-line)
  (delete-char 1)
  (delete-horizontal-space)
  (insert " "))

(defun move-line-down ()
  "Move the current line down by one line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1))

```
