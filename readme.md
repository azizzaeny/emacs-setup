## What is this
Emacs development configuration. all configuration is described and loaded from markdown files.
 
### How to start 
clone the repository `git clone git@github.com:azizzaeny/emacs-setup.git` then `cd emacs-setup`  
`emacs -l init.el .` and then start by opening files or project directory with `emacsclient -t .`  

### Explanation 
emacs start with loading `init.el` files which is contain parser to parse markdown file, the emacs will start as server running in the background  

### Start by creating link into emacs.d folder

```sh
cd ~
pwd
ln -s /path/to/emacs-setup ~/.emacs.d
ln -s /home/user/config/emacs-folder ~/.emacs.d

cp .tmux.conf ~

# in .bashrc add this line
export TERM=xterm-256color

```

### setup tmux conf

```txt

#set -g mouse on
set -g mouse off
#set-option -g mode-keys emacs

#set -g terminal-overrides 'xterm*:smcup@:rmcup@'
#set -g alternate-screen off
    
#bind-key C-v run-shell 'echo "hellow world" > /tmp/output.txt'
#bind-key C-f command-prompt -p "Session name:" "run-shell '~/find-and-switch-session.sh %1'"
bind-key f command-prompt -p "Session name:" "run-shell 'tmux switch-client -t %1 || tmux display-message \"Session %1 not found.\"'"
bind-key C-r source-file ~/.tmux.conf
#bind-key f command-prompt -p "Session name:" "run-shell 'tmux switch-client -t %1 || echo Session %1 not found.'"

bind | split-window -h
bind - split-window -v

unbind ]
bind C-c copy-mode
bind C-v paste-buffer

```
