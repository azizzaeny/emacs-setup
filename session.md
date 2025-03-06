always start with env first to setup it 

**create console**
```sh 
tmux new-session -d -s console -n default
tmux new-window -t console: -n monitor
```

**create setup notes**
```sh 
tmux new-session -d -s note -n editor
tmux new-window -t note: -n git
```

**create gateway**
```sh 
tmux new-session -d -s gateway -n editor
tmux new-window -t gateway: -n shell
```

create workspaces
```sh 
```

**create research**
```sh 
tmux new-session -d -s research -n editor
tmux new-window -t research: -n git
```

**create remote**
```sh 
tmux new-session -d -s remote -n public
tmux new-window -t remote: -n nv15
tmux new-window -t remote: -n host-ca
```

create kbm-development, kbm-alpha, kbm-beta, kbm-productions
```sh 
tmux new-session -d -s kbm-development -n shell
tmux new-window -t kbm-development: -n local-api
tmux new-window -t kbm-development: -n kbm-alpha
tmux new-window -t kbm-development: -n kbm-beta
tmux new-window -t kbm-development: -n kbm-prod
```
