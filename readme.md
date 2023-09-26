## What is this
Emacs development configuration. all configuration is described and loaded from markdown files.
 
### How to start 
clone the repository `git clone git@github.com:azizzaeny/emacs-setup.git` then `cd emacs-setup`  
`emacs -l init.el .` and then start by opening files or project directory with `emacsclient -t .`  

### Explanation 
emacs start with loading `init.el` files which is contain parser to parse markdown file, the emacs will start as server running in the background  

### Contents 
- [00 - Emacs markdown parser](./init.el) 
- [01 - Setup package](./contents/01-install-package.md)
- [02 - Make it home](./contents/02-make-it-home.md)
- [03 - Beyond editor](./contents/03-beyond-editor.md)
- [04 - Send string to buffer](./contents/04-repl-send-string.md)
- [05 - Literate programming in markdown](./contents/05-literate-tangling.md)
- [06 - Align to something](./contents/06-align-to-sign.md)
- [07 - Eval through TCP](./contents/07-making-tcp-communication.md)
- [08 - Simulate in frame](./contents/08-simulate-things-in-frame.md)

```elisp
(load-markdown "./contents/01-install-package.md")
(load-markdown "./contents/02-make-it-home.md")
(load-markdown "./contents/03-beyond-editor.md")
;;(load-markdown "./contents/04-repl-send-string.md")
(load-markdown "./contents/05-literate-tangling.md")
(load-markdown "./contents/06-align-to-sign.md")
;;(load-markdown "./contents/07-making-tcp-communication.md")
(load-markdown "./contents/08-simulate-things-in-frame.md")
```
