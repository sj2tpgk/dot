function tmuxcd -d 'Cd to pwd of tmux last-pane'
  cd (tmux last-pane; tmux display -pF "#{pane_current_path}"; tmux last-pane)
end
