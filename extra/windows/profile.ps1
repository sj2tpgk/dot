Set-Alias n nvim
function ..() { cd ../ }
function scoi() { scoop install $args }
function sci()  { scoop install $args }
function scos() { scoop-search  $args }
function scs()  { scoop-search  $args }
function ec() { echo $args }
Set-PSReadLineOption -HistorySearchCursorMovesToEnd
Set-PSReadLineOption -HistoryNoDuplicates
Set-PSReadLineKeyHandler -Key UpArrow   -Function HistorySearchBackward
Set-PSReadLineKeyHandler -Key DownArrow -Function HistorySearchForward
