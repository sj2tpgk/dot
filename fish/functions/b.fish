function b -d 'Execute as bash command'
  if test -n "$argv"
    bash -c $argv
  else
    bash -c (read)
  end
end

