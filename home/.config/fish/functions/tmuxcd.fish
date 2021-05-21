function tmuxcd -d 'Cd to pwd of tmux last-pane'
  cd (tmux last-pane; tmux-path getpanepath; tmux last-pane)
end
