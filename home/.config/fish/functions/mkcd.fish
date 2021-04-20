function mkcd -d 'mkdir plus cd'
  set dirname $argv[1]
  mkdir -p $dirname
  eval "cd" $dirname
end
