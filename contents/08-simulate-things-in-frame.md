## Simulating Open File In Frame

the idea is to make open files in frame of the choices, we can make this to follow up the notes open by making tcp call to the emacs server


```elisp

;; (defun my-open-file-in-frame (file-path frame-name)
;;   "Open the file at FILE-PATH in the frame named FRAME-NAME."
;;   (interactive "fOpen file: \nsIn frame: ")
;;   (let ((frame (get-frame-by-name frame-name)))
;;     (when frame
;;       (select-frame-set-input-focus frame)
;;       (find-file file-path))))

;; (defun my-list-frames ()
;;   "List all frames by name."
;;   (interactive)
;;   (let ((frame-names (mapcar 'frame-parameter (frame-list) 'name)))
;;     (message "Frames: %s" (mapconcat 'identity frame-names ", "))))

;; (defun select-frame-by-name (name)
;;   "Select the frame with the given NAME."
;;   (interactive "sFrame name: ")
;;   (let* ((frames (frame-list))
;;          (frame (seq-find (lambda (f) (string-equal (frame-parameter f 'name) name)) frames)))
;;     (if frame
;;         (select-frame-set-input-focus frame)
;;       (error "No frame with name %s" name))))

(defun find-file-in-frame (file frame-name)
  "Find FILE in the frame named FRAME-NAME."
  (interactive "fFile to open: \nsFrame name: ")
  (let ((frame (seq-find (lambda (f) (string-equal (frame-parameter f 'name) frame-name)) (frame-list))))
    (if frame
        (progn
          (select-frame-set-input-focus frame)
          (find-file file))
      (error "No frame with name %s" frame-name))))


```
