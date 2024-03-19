(defvar md-block-header "^```elisp")
(defvar md-block-end "^```$")

(defun load-markdown (file-paths &optional evaluator)
  (interactive)
  (when (file-exists-p (expand-file-name file-paths))
    (with-temp-buffer
      (insert-file-contents file-paths)
      (goto-char (point-min))
      (while (not (eobp))
	(forward-line 1)
	(let ((starting-pos (progn (re-search-forward md-block-header (point-max) t)
				   (match-end 0)))
	      (end-pos (progn (re-search-forward md-block-end (point-max) t)
			      (match-beginning 0))))
	  (if evaluator
	      (funcall evaluator starting-pos end-pos)
	    (eval-region starting-pos end-pos)))))))

(load-markdown "contents/configuration.md")
(load-markdown "contents/snippet.md")
(load-markdown "contents/markdown.md")
(load-markdown "contents/repl.md")
(load-markdown "contents/align.md")
(load-markdown "contents/notes.md")


(global-set-key (kbd "C-s") 'swiper)

(global-set-key (kbd "C-g") 'minibuffer-keyboard-quit)
;; (global-set-key (kbd "C-s") 'counsel-grep-or-swiper)
(global-set-key (kbd "C-s") 'swiper)

(global-unset-key (kbd "C-r"))
(global-set-key (kbd "C-r") 'replace-string)

(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "C-x f") 'counsel-find-library)

(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <left>") 'windmove-left)
(global-set-key (kbd "C-x <right>") 'windmove-right)
(global-set-key (kbd "C-x C-<left>") 'windmove-left)
(global-set-key (kbd "C-x C-<right>") 'windmove-right)

(global-unset-key (kbd "C-x m"))
;;(global-set-key (kbd "C-x m") 'multi-term)

(global-set-key (kbd "C-x v v") 'ivy-push-view)
(global-set-key (kbd "C-x v p") 'ivy-pop-view)


(global-unset-key (kbd "C-x e"))
(global-set-key (kbd "C-x e") 'eval-region)
(global-set-key (kbd "C-x c v") 'visual-line-mode)
(global-set-key (kbd "C-x g") 'goto-line)

(global-unset-key (kbd "C-x l"))
(global-set-key (kbd "C-x l") 'global-display-line-numbers-mode)

(global-unset-key (kbd "C-x r"))
(global-unset-key (kbd "C-x a"))
(global-set-key (kbd "C-x a a") 'ansi-term)
(global-set-key (kbd "C-x r b") 'rename-buffer)

;;(global-set-key (kbd "C-x y r") 'yas-reload-all)
;;(global-set-key (kbd "C-x y n") 'yas-new-snippet)

(global-unset-key (kbd "C-t")) ;; tranpose
(global-unset-key (kbd "C-h")) ;; help
(global-unset-key (kbd "C-x C-t")) ;;transpse line

(global-set-key (kbd "C-c b m") 'hs-minor-mode)
(global-set-key (kbd "C-c b h") 'hs-hide-block)
(global-set-key (kbd "C-c b s") 'hs-show-block)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(yaml-mode yasnippet expand-region poly-markdown polymode aggressive-indent php-mode js2-mode python-mode auto-complete counsel clojure-mode cmake-mode)))


