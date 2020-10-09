" Misc (options) {{{
set nocompatible      " required in project.vim
set showmatch         " Show matching brackets.
set autowrite         " Automatically save before commands like :next and :make
set mouse=a           " Enable mouse usage (see also: 'ttymouse')
set pumheight=12      " Height of popup menu in completion
set showcmd           " Show command being input
set hidden            " Allow changeing buffer without saving
set textwidth=0       " Prevent automatically insert '\n' and goto next line
set scrolloff=3       " Cursor > (bottom of screen + n)
set sidescrolloff=5
let mapleader=" "
set t_Co=16           " Number of colors = 16

if exists("&ttymouse")
    set ttymouse=xterm2   " Mouse drag to resize windows
endif

set clipboard=unnamedplus

" Japanese encodings
" don't use modeline(comment at the end of a file) to set enc (vim:enc=euc-jp etc.)
set fileencodings=ucs-bom,utf-8,iso-2022-jp,sjis,cp932,euc-jp,cp20932
" }}}
" Misc (mappings) {{{
nnore D dd
nnore Y yy

nore j gj
nore k gk
nore gj j
nore gk k

nore si s

nore gm %
nore gl $
onore m %
onore l $

" Yank path/directory of current file
nnore yd :let @+=expand('%:p:h')<CR>
nnore yp :let @+=expand('%:p')<CR>

nnore <c-e> <c-i>

nnore <leader>/ :noh<cr>

nnore yb ggVGy
nnore db ggVGd

nnore <home> ^
inore <home> <c-o>^
" }}}

" Highlighting {{{
syntax on
filetype on
colorscheme desert
hi Search ctermfg=0 ctermbg=12 guifg=wheat guibg=peru
" }}}
" Cursor {{{
aug vimrc_cursor
  au!
  au! InsertEnter * set cursorline!
  au! InsertLeave * set nocursorline!
aug END
" }}}
" Indent {{{
set expandtab               " Use spaces instead of tab
set tabstop=2               " A tab shows as N spaces (abbr. ts)
set softtabstop=2           " Insert N spaces as a tab (abbr. sts)
set shiftwidth=2            " Tab=N spaces in << >> etc. (abbr. sw)
set autoindent
filetype plugin indent on
set smartindent
" }}}
" Folding {{{
set fdm=marker
nnore <tab> za
nnore <leader>z :set fdm=marker<cr>zm

" close folding by 'h' if cursor is 0 of the line and in a opened folding
nnore <expr> h  (getcurpos()[2] == 1) && (foldlevel('.') != 0) ? 'zc' : 'h'
" }}}
" Modeline {{{
set statusline=%<%{FileStatus()}\ %f%*%=%-10.(%l,%c%V%)\ %y%6.(%P%)
hi StatusLine   cterm=reverse,bold ctermfg=2 ctermbg=0
hi StatusLineNC cterm=reverse,bold ctermbg=0
fu! FileStatus()
  let s = ""
  if &readonly | let s=s.'R' | else | let s=s.'' | endif
  if &modified | let s=s.'+' | else | let s=s.'-' | endif
  return '['.s.']'
endfu
set laststatus=2
" }}}

" Search {{{
set ignorecase
set smartcase
set incsearch
set hlsearch
" }}}

" Esc {{{
set ttimeoutlen=8
augroup vimrc_inoreesc
  autocmd!
  autocmd InsertLeave * normal! l
augroup END
" }}}

" Save/Quit {{{
nnore <leader>w :wa<cr>
nnore <leader>q :qa<cr>

" Save fold settings.
" autocmd BufWinLeave * if expand('%') != '' && &buftype !~ 'nofile' | mkview | endif
" autocmd BufWinEnter * if expand('%') != '' && &buftype !~ 'nofile' | silent loadview | endif
" Don't save options.
set viewoptions-=options

" }}}
" Buffer {{{
nnore s<leader> :ls<cr>:b<space>
nnore <leader><tab> :b#<cr>
nnore sb :bp\|bd #<cr>
" }}}
" Window {{{
nnore sd :q<cr>
nnore so :only<cr>
nnore sh <c-w>h
nnore sj <c-w>j
nnore sk <c-w>k
nnore sl <c-w>l
nnore ss :sp<space>
nnore sv :vsp<space>
nnore %  <c-w><c-w>
" }}}
" Tab {{{
nnore <leader>1 1gt
nnore <leader>2 2gt
nnore <leader>3 3gt
nnore <leader>4 4gt
nnore <leader>5 5gt
nnore sw :tabclose<cr>
nnore st :tabe<space>
" }}}

" Eval {{{
augroup vimrc_eval
    au!
    au FileType vim nnore ,b :source %<cr>
    au FileType vim nnore ,l  yy:@"<cr>
    au FileType vim vnore ,l  y:@"<cr>
augroup END
" }}}
" Vimrc {{{
nnore <leader>fr :e ~/.vimrc<cr>
nnore <f5> :wa<cr>:source ~/.vimrc<cr>
" }}}

" TODO complete from all buffers
" Completion {{{
" Fuzzy completion {{{
fu! FuzComp(findstart, base)
  if a:findstart
    " locate the start of the word
    let line = getline('.')
    let start = col('.') - 1
    while start > 0 && line[start - 1] =~ '\a'
      let start -= 1
    endwhile
    return start
  else
    return Collect(FuzPat(a:base))
  endif
endfu

fu! Collect(pat)
  " Collect words matching a:pat in buffer
  let sav = getcurpos()
  call cursor(1, 1)
  let res = {}
  let m = searchpos(a:pat, 'W')
  while m[0] != 0
    let res[expand('<cword>')] = 1
    let m = searchpos(a:pat, 'W')
  endw
  call setpos('.', sav)
  return keys(res)
endfu

fu! FuzPat(str)
  " Generate regexp for fuzzy-matching of string a:str
  " FuzPat("abc") matches abc, axbxc, abcxx etc.
  let s = ""
  for c in split(a:str, '\zs')
    let s = s . c . '\w*'
  endfor
  return '\<' . s . '\>'
endfu
" }}}

set completefunc=FuzComp

"inore <tab>   <c-r>=pumvisible() ? "\<lt>c-n>" : "\<lt>c-x>\<lt>c-u>"<cr>
"inore <s-tab> <c-r>=pumvisible() ? "\<lt>c-p>" : "\<lt>c-x>\<lt>c-u>"<cr>
inore <tab>   <c-n>
inore <s-tab> <c-p>
inore <c-f> <c-x><c-f>
" }}}

" Gvim {{{
if has('gui_running')

  " Space between lines
  set linespace=3

  " Base16 solarflare theme {{{

  " base16-vim (https://github.com/chriskempson/base16-vim)
  " by Chris Kempson (http://chriskempson.com)
  " Solar Flare scheme by Chuck Harmston (https://chuck.harmston.ch)

  " This enables the coresponding base16-shell script to run so that
  " :colorscheme works in terminals supported by base16-shell scripts
  " User must set this variable in .vimrc
  "   let g:base16_shell_path=base16-builder/output/shell/
  " if !has("gui_running")
  "   if exists("g:base16_shell_path")
  "     execute "silent !/bin/sh ".g:base16_shell_path."/base16-solarflare.sh"
  "   endif
  " endif

  " Colors {{{
  " GUI color definitions
  " let s:gui00        = "18262F"
  " let g:base16_gui00 = "18262F"
  let s:gui00        = "10191f"
  let g:base16_gui00 = "10191f"
  let s:gui01        = "222E38"
  let g:base16_gui01 = "222E38"
  let s:gui02        = "586875"
  let g:base16_gui02 = "586875"
  let s:gui03        = "667581"
  let g:base16_gui03 = "667581"
  let s:gui04        = "85939E"
  let g:base16_gui04 = "85939E"
  let s:gui05        = "A6AFB8"
  let g:base16_gui05 = "A6AFB8"
  let s:gui06        = "E8E9ED"
  let g:base16_gui06 = "E8E9ED"
  let s:gui07        = "F5F7FA"
  let g:base16_gui07 = "F5F7FA"
  let s:gui08        = "EF5253"
  let g:base16_gui08 = "EF5253"
  let s:gui09        = "E66B2B"
  let g:base16_gui09 = "E66B2B"
  let s:gui0A        = "E4B51C"
  let g:base16_gui0A = "E4B51C"
  let s:gui0B        = "7CC844"
  let g:base16_gui0B = "7CC844"
  let s:gui0C        = "52CBB0"
  let g:base16_gui0C = "52CBB0"
  let s:gui0D        = "33B5E1"
  let g:base16_gui0D = "33B5E1"
  let s:gui0E        = "A363D5"
  let g:base16_gui0E = "A363D5"
  let s:gui0F        = "D73C9A"
  let g:base16_gui0F = "D73C9A"

  " Terminal color definitions
  let s:cterm00        = "00"
  let g:base16_cterm00 = "00"
  let s:cterm03        = "08"
  let g:base16_cterm03 = "08"
  let s:cterm05        = "07"
  let g:base16_cterm05 = "07"
  let s:cterm07        = "15"
  let g:base16_cterm07 = "15"
  let s:cterm08        = "01"
  let g:base16_cterm08 = "01"
  let s:cterm0A        = "03"
  let g:base16_cterm0A = "03"
  let s:cterm0B        = "02"
  let g:base16_cterm0B = "02"
  let s:cterm0C        = "06"
  let g:base16_cterm0C = "06"
  let s:cterm0D        = "04"
  let g:base16_cterm0D = "04"
  let s:cterm0E        = "05"
  let g:base16_cterm0E = "05"
  if exists("base16colorspace") && base16colorspace == "256"
    let s:cterm01        = "18"
    let g:base16_cterm01 = "18"
    let s:cterm02        = "19"
    let g:base16_cterm02 = "19"
    let s:cterm04        = "20"
    let g:base16_cterm04 = "20"
    let s:cterm06        = "21"
    let g:base16_cterm06 = "21"
    let s:cterm09        = "16"
    let g:base16_cterm09 = "16"
    let s:cterm0F        = "17"
    let g:base16_cterm0F = "17"
  else
    let s:cterm01        = "10"
    let g:base16_cterm01 = "10"
    let s:cterm02        = "11"
    let g:base16_cterm02 = "11"
    let s:cterm04        = "12"
    let g:base16_cterm04 = "12"
    let s:cterm06        = "13"
    let g:base16_cterm06 = "13"
    let s:cterm09        = "09"
    let g:base16_cterm09 = "09"
    let s:cterm0F        = "14"
    let g:base16_cterm0F = "14"
  endif

  " Neovim terminal colours
  if has("nvim")
    let g:terminal_color_0 =  "#18262F"
    let g:terminal_color_1 =  "#EF5253"
    let g:terminal_color_2 =  "#7CC844"
    let g:terminal_color_3 =  "#E4B51C"
    let g:terminal_color_4 =  "#33B5E1"
    let g:terminal_color_5 =  "#A363D5"
    let g:terminal_color_6 =  "#52CBB0"
    let g:terminal_color_7 =  "#A6AFB8"
    let g:terminal_color_8 =  "#667581"
    let g:terminal_color_9 =  "#EF5253"
    let g:terminal_color_10 = "#7CC844"
    let g:terminal_color_11 = "#E4B51C"
    let g:terminal_color_12 = "#33B5E1"
    let g:terminal_color_13 = "#A363D5"
    let g:terminal_color_14 = "#52CBB0"
    let g:terminal_color_15 = "#F5F7FA"
    let g:terminal_color_background = g:terminal_color_0
    let g:terminal_color_foreground = g:terminal_color_5
    if &background == "light"
      let g:terminal_color_background = g:terminal_color_7
      let g:terminal_color_foreground = g:terminal_color_2
    endif
  elseif has("terminal")
    let g:terminal_ansi_colors = [
          \ "#18262F",
          \ "#EF5253",
          \ "#7CC844",
          \ "#E4B51C",
          \ "#33B5E1",
          \ "#A363D5",
          \ "#52CBB0",
          \ "#A6AFB8",
          \ "#667581",
          \ "#EF5253",
          \ "#7CC844",
          \ "#E4B51C",
          \ "#33B5E1",
          \ "#A363D5",
          \ "#52CBB0",
          \ "#F5F7FA",
          \ ]
  endif
  " }}}

  " Theme setup
  hi clear
  syntax reset
  let g:colors_name = "base16-solarflare"

  " Highlighting function {{{
  " Optional variables are attributes and guisp
  function! g:Base16hi(group, guifg, guibg, ctermfg, ctermbg, ...)
    let l:attr = get(a:, 1, "")
    let l:guisp = get(a:, 2, "")

    if a:guifg != ""
      exec "hi " . a:group . " guifg=#" . a:guifg
    endif
    if a:guibg != ""
      exec "hi " . a:group . " guibg=#" . a:guibg
    endif
    if a:ctermfg != ""
      exec "hi " . a:group . " ctermfg=" . a:ctermfg
    endif
    if a:ctermbg != ""
      exec "hi " . a:group . " ctermbg=" . a:ctermbg
    endif
    if l:attr != ""
      exec "hi " . a:group . " gui=" . l:attr . " cterm=" . l:attr
    endif
    if l:guisp != ""
      exec "hi " . a:group . " guisp=#" . l:guisp
    endif
  endfunction


  fun <sid>hi(group, guifg, guibg, ctermfg, ctermbg, attr, guisp)
    call g:Base16hi(a:group, a:guifg, a:guibg, a:ctermfg, a:ctermbg, a:attr, a:guisp)
  endfun
  " }}}

  " Highlighting {{{
  " Vim editor colors
  call <sid>hi("Normal",        s:gui06, s:gui00, s:cterm05, s:cterm00, "", "")
  call <sid>hi("Bold",          "", "", "", "", "bold", "")
  call <sid>hi("Debug",         s:gui08, "", s:cterm08, "", "", "")
  call <sid>hi("Directory",     s:gui0D, "", s:cterm0D, "", "", "")
  call <sid>hi("Error",         s:gui00, s:gui08, s:cterm00, s:cterm08, "", "")
  call <sid>hi("ErrorMsg",      s:gui08, s:gui00, s:cterm08, s:cterm00, "", "")
  call <sid>hi("Exception",     s:gui08, "", s:cterm08, "", "", "")
  call <sid>hi("FoldColumn",    s:gui0C, s:gui01, s:cterm0C, s:cterm01, "", "")
  call <sid>hi("Folded",        s:gui03, s:gui01, s:cterm03, s:cterm01, "", "")
  call <sid>hi("IncSearch",     s:gui01, s:gui09, s:cterm01, s:cterm09, "none", "")
  call <sid>hi("Italic",        "", "", "", "", "none", "")
  call <sid>hi("Macro",         s:gui08, "", s:cterm08, "", "", "")
  call <sid>hi("MatchParen",    "", s:gui03, "", s:cterm03,  "", "")
  call <sid>hi("ModeMsg",       s:gui0B, "", s:cterm0B, "", "", "")
  call <sid>hi("MoreMsg",       s:gui0B, "", s:cterm0B, "", "", "")
  call <sid>hi("Question",      s:gui0D, "", s:cterm0D, "", "", "")
  call <sid>hi("Search",        s:gui01, s:gui0A, s:cterm01, s:cterm0A,  "", "")
  call <sid>hi("Substitute",    s:gui01, s:gui0A, s:cterm01, s:cterm0A, "none", "")
  call <sid>hi("SpecialKey",    s:gui03, "", s:cterm03, "", "", "")
  call <sid>hi("TooLong",       s:gui08, "", s:cterm08, "", "", "")
  call <sid>hi("Underlined",    s:gui08, "", s:cterm08, "", "", "")
  call <sid>hi("Visual",        "", s:gui02, "", s:cterm02, "", "")
  call <sid>hi("VisualNOS",     s:gui08, "", s:cterm08, "", "", "")
  call <sid>hi("WarningMsg",    s:gui08, "", s:cterm08, "", "", "")
  call <sid>hi("WildMenu",      s:gui08, s:gui0A, s:cterm08, "", "", "")
  call <sid>hi("Title",         s:gui0D, "", s:cterm0D, "", "none", "")
  call <sid>hi("Conceal",       s:gui0D, s:gui00, s:cterm0D, s:cterm00, "", "")
  call <sid>hi("Cursor",        s:gui00, s:gui05, s:cterm00, s:cterm05, "", "")
  call <sid>hi("NonText",       s:gui03, "", s:cterm03, "", "", "")
  call <sid>hi("LineNr",        s:gui03, s:gui01, s:cterm03, s:cterm01, "", "")
  call <sid>hi("SignColumn",    s:gui03, s:gui01, s:cterm03, s:cterm01, "", "")
  call <sid>hi("StatusLine",    s:gui06, s:gui02, s:cterm04, s:cterm02, "none", "")
  call <sid>hi("StatusLineNC",  s:gui03, s:gui01, s:cterm03, s:cterm01, "none", "")
  call <sid>hi("VertSplit",     s:gui02, s:gui02, s:cterm02, s:cterm02, "none", "")
  call <sid>hi("ColorColumn",   "", s:gui01, "", s:cterm01, "none", "")
  call <sid>hi("CursorColumn",  "", s:gui01, "", s:cterm01, "none", "")
  call <sid>hi("CursorLine",    "", s:gui01, "", s:cterm01, "none", "")
  call <sid>hi("CursorLineNr",  s:gui04, s:gui01, s:cterm04, s:cterm01, "", "")
  call <sid>hi("QuickFixLine",  "", s:gui01, "", s:cterm01, "none", "")
  call <sid>hi("PMenu",         s:gui05, s:gui01, s:cterm05, s:cterm01, "none", "")
  call <sid>hi("PMenuSel",      s:gui01, s:gui05, s:cterm01, s:cterm05, "", "")
  call <sid>hi("TabLine",       s:gui03, s:gui01, s:cterm03, s:cterm01, "none", "")
  call <sid>hi("TabLineFill",   s:gui03, s:gui01, s:cterm03, s:cterm01, "none", "")
  call <sid>hi("TabLineSel",    s:gui0B, s:gui01, s:cterm0B, s:cterm01, "none", "")

  " Standard syntax highlighting
  call <sid>hi("Boolean",      s:gui09, "", s:cterm09, "", "", "")
  call <sid>hi("Character",    s:gui08, "", s:cterm08, "", "", "")
  call <sid>hi("Comment",      s:gui03, "", s:cterm03, "", "", "")
  call <sid>hi("Conditional",  s:gui0E, "", s:cterm0E, "", "", "")
  call <sid>hi("Constant",     s:gui09, "", s:cterm09, "", "", "")
  call <sid>hi("Define",       s:gui0E, "", s:cterm0E, "", "none", "")
  call <sid>hi("Delimiter",    s:gui0F, "", s:cterm0F, "", "", "")
  call <sid>hi("Float",        s:gui09, "", s:cterm09, "", "", "")
  call <sid>hi("Function",     s:gui0D, "", s:cterm0D, "", "", "")
  call <sid>hi("Identifier",   s:gui08, "", s:cterm08, "", "none", "")
  call <sid>hi("Include",      s:gui0D, "", s:cterm0D, "", "", "")
  call <sid>hi("Keyword",      s:gui0E, "", s:cterm0E, "", "", "")
  call <sid>hi("Label",        s:gui0A, "", s:cterm0A, "", "", "")
  call <sid>hi("Number",       s:gui09, "", s:cterm09, "", "", "")
  call <sid>hi("Operator",     s:gui05, "", s:cterm05, "", "none", "")
  call <sid>hi("PreProc",      s:gui0A, "", s:cterm0A, "", "", "")
  call <sid>hi("Repeat",       s:gui0A, "", s:cterm0A, "", "", "")
  call <sid>hi("Special",      s:gui0C, "", s:cterm0C, "", "", "")
  call <sid>hi("SpecialChar",  s:gui0F, "", s:cterm0F, "", "", "")
  call <sid>hi("Statement",    s:gui08, "", s:cterm08, "", "", "")
  call <sid>hi("StorageClass", s:gui0A, "", s:cterm0A, "", "", "")
  call <sid>hi("String",       s:gui0B, "", s:cterm0B, "", "", "")
  call <sid>hi("Structure",    s:gui0E, "", s:cterm0E, "", "", "")
  call <sid>hi("Tag",          s:gui0A, "", s:cterm0A, "", "", "")
  call <sid>hi("Todo",         s:gui0A, s:gui01, s:cterm0A, s:cterm01, "", "")
  call <sid>hi("Type",         s:gui0A, "", s:cterm0A, "", "none", "")
  call <sid>hi("Typedef",      s:gui0A, "", s:cterm0A, "", "", "")
  " }}}

  " Filetype-specific highlightings {{{
  " C highlighting
  call <sid>hi("cOperator",   s:gui0C, "", s:cterm0C, "", "", "")
  call <sid>hi("cPreCondit",  s:gui0E, "", s:cterm0E, "", "", "")

  " C# highlighting
  call <sid>hi("csClass",                 s:gui0A, "", s:cterm0A, "", "", "")
  call <sid>hi("csAttribute",             s:gui0A, "", s:cterm0A, "", "", "")
  call <sid>hi("csModifier",              s:gui0E, "", s:cterm0E, "", "", "")
  call <sid>hi("csType",                  s:gui08, "", s:cterm08, "", "", "")
  call <sid>hi("csUnspecifiedStatement",  s:gui0D, "", s:cterm0D, "", "", "")
  call <sid>hi("csContextualStatement",   s:gui0E, "", s:cterm0E, "", "", "")
  call <sid>hi("csNewDecleration",        s:gui08, "", s:cterm08, "", "", "")

  " CSS highlighting
  call <sid>hi("cssBraces",      s:gui05, "", s:cterm05, "", "", "")
  call <sid>hi("cssClassName",   s:gui0E, "", s:cterm0E, "", "", "")
  call <sid>hi("cssColor",       s:gui0C, "", s:cterm0C, "", "", "")

  " Diff highlighting
  call <sid>hi("DiffAdd",      s:gui0B, s:gui01,  s:cterm0B, s:cterm01, "", "")
  call <sid>hi("DiffChange",   s:gui03, s:gui01,  s:cterm03, s:cterm01, "", "")
  call <sid>hi("DiffDelete",   s:gui08, s:gui01,  s:cterm08, s:cterm01, "", "")
  call <sid>hi("DiffText",     s:gui0D, s:gui01,  s:cterm0D, s:cterm01, "", "")
  call <sid>hi("DiffAdded",    s:gui0B, s:gui00,  s:cterm0B, s:cterm00, "", "")
  call <sid>hi("DiffFile",     s:gui08, s:gui00,  s:cterm08, s:cterm00, "", "")
  call <sid>hi("DiffNewFile",  s:gui0B, s:gui00,  s:cterm0B, s:cterm00, "", "")
  call <sid>hi("DiffLine",     s:gui0D, s:gui00,  s:cterm0D, s:cterm00, "", "")
  call <sid>hi("DiffRemoved",  s:gui08, s:gui00,  s:cterm08, s:cterm00, "", "")

  " Git highlighting
  call <sid>hi("gitcommitOverflow",       s:gui08, "", s:cterm08, "", "", "")
  call <sid>hi("gitcommitSummary",        s:gui0B, "", s:cterm0B, "", "", "")
  call <sid>hi("gitcommitComment",        s:gui03, "", s:cterm03, "", "", "")
  call <sid>hi("gitcommitUntracked",      s:gui03, "", s:cterm03, "", "", "")
  call <sid>hi("gitcommitDiscarded",      s:gui03, "", s:cterm03, "", "", "")
  call <sid>hi("gitcommitSelected",       s:gui03, "", s:cterm03, "", "", "")
  call <sid>hi("gitcommitHeader",         s:gui0E, "", s:cterm0E, "", "", "")
  call <sid>hi("gitcommitSelectedType",   s:gui0D, "", s:cterm0D, "", "", "")
  call <sid>hi("gitcommitUnmergedType",   s:gui0D, "", s:cterm0D, "", "", "")
  call <sid>hi("gitcommitDiscardedType",  s:gui0D, "", s:cterm0D, "", "", "")
  call <sid>hi("gitcommitBranch",         s:gui09, "", s:cterm09, "", "bold", "")
  call <sid>hi("gitcommitUntrackedFile",  s:gui0A, "", s:cterm0A, "", "", "")
  call <sid>hi("gitcommitUnmergedFile",   s:gui08, "", s:cterm08, "", "bold", "")
  call <sid>hi("gitcommitDiscardedFile",  s:gui08, "", s:cterm08, "", "bold", "")
  call <sid>hi("gitcommitSelectedFile",   s:gui0B, "", s:cterm0B, "", "bold", "")

  " GitGutter highlighting
  call <sid>hi("GitGutterAdd",     s:gui0B, s:gui01, s:cterm0B, s:cterm01, "", "")
  call <sid>hi("GitGutterChange",  s:gui0D, s:gui01, s:cterm0D, s:cterm01, "", "")
  call <sid>hi("GitGutterDelete",  s:gui08, s:gui01, s:cterm08, s:cterm01, "", "")
  call <sid>hi("GitGutterChangeDelete",  s:gui0E, s:gui01, s:cterm0E, s:cterm01, "", "")

  " HTML highlighting
  call <sid>hi("htmlBold",    s:gui0A, "", s:cterm0A, "", "", "")
  call <sid>hi("htmlItalic",  s:gui0E, "", s:cterm0E, "", "", "")
  call <sid>hi("htmlEndTag",  s:gui05, "", s:cterm05, "", "", "")
  call <sid>hi("htmlTag",     s:gui05, "", s:cterm05, "", "", "")

  " JavaScript highlighting
  call <sid>hi("javaScript",        s:gui05, "", s:cterm05, "", "", "")
  call <sid>hi("javaScriptBraces",  s:gui05, "", s:cterm05, "", "", "")
  call <sid>hi("javaScriptNumber",  s:gui09, "", s:cterm09, "", "", "")
  " pangloss/vim-javascript highlighting
  call <sid>hi("jsOperator",          s:gui0D, "", s:cterm0D, "", "", "")
  call <sid>hi("jsStatement",         s:gui0E, "", s:cterm0E, "", "", "")
  call <sid>hi("jsReturn",            s:gui0E, "", s:cterm0E, "", "", "")
  call <sid>hi("jsThis",              s:gui08, "", s:cterm08, "", "", "")
  call <sid>hi("jsClassDefinition",   s:gui0A, "", s:cterm0A, "", "", "")
  call <sid>hi("jsFunction",          s:gui0E, "", s:cterm0E, "", "", "")
  call <sid>hi("jsFuncName",          s:gui0D, "", s:cterm0D, "", "", "")
  call <sid>hi("jsFuncCall",          s:gui0D, "", s:cterm0D, "", "", "")
  call <sid>hi("jsClassFuncName",     s:gui0D, "", s:cterm0D, "", "", "")
  call <sid>hi("jsClassMethodType",   s:gui0E, "", s:cterm0E, "", "", "")
  call <sid>hi("jsRegexpString",      s:gui0C, "", s:cterm0C, "", "", "")
  call <sid>hi("jsGlobalObjects",     s:gui0A, "", s:cterm0A, "", "", "")
  call <sid>hi("jsGlobalNodeObjects", s:gui0A, "", s:cterm0A, "", "", "")
  call <sid>hi("jsExceptions",        s:gui0A, "", s:cterm0A, "", "", "")
  call <sid>hi("jsBuiltins",          s:gui0A, "", s:cterm0A, "", "", "")

  " Mail highlighting
  call <sid>hi("mailQuoted1",  s:gui0A, "", s:cterm0A, "", "", "")
  call <sid>hi("mailQuoted2",  s:gui0B, "", s:cterm0B, "", "", "")
  call <sid>hi("mailQuoted3",  s:gui0E, "", s:cterm0E, "", "", "")
  call <sid>hi("mailQuoted4",  s:gui0C, "", s:cterm0C, "", "", "")
  call <sid>hi("mailQuoted5",  s:gui0D, "", s:cterm0D, "", "", "")
  call <sid>hi("mailQuoted6",  s:gui0A, "", s:cterm0A, "", "", "")
  call <sid>hi("mailURL",      s:gui0D, "", s:cterm0D, "", "", "")
  call <sid>hi("mailEmail",    s:gui0D, "", s:cterm0D, "", "", "")

  " Markdown highlighting
  call <sid>hi("markdownCode",              s:gui0B, "", s:cterm0B, "", "", "")
  call <sid>hi("markdownError",             s:gui05, s:gui00, s:cterm05, s:cterm00, "", "")
  call <sid>hi("markdownCodeBlock",         s:gui0B, "", s:cterm0B, "", "", "")
  call <sid>hi("markdownHeadingDelimiter",  s:gui0D, "", s:cterm0D, "", "", "")

  " NERDTree highlighting
  call <sid>hi("NERDTreeDirSlash",  s:gui0D, "", s:cterm0D, "", "", "")
  call <sid>hi("NERDTreeExecFile",  s:gui05, "", s:cterm05, "", "", "")

  " PHP highlighting
  call <sid>hi("phpMemberSelector",  s:gui05, "", s:cterm05, "", "", "")
  call <sid>hi("phpComparison",      s:gui05, "", s:cterm05, "", "", "")
  call <sid>hi("phpParent",          s:gui05, "", s:cterm05, "", "", "")
  call <sid>hi("phpMethodsVar",      s:gui0C, "", s:cterm0C, "", "", "")

  " Python highlighting
  call <sid>hi("pythonOperator",  s:gui0E, "", s:cterm0E, "", "", "")
  call <sid>hi("pythonRepeat",    s:gui0E, "", s:cterm0E, "", "", "")
  call <sid>hi("pythonInclude",   s:gui0E, "", s:cterm0E, "", "", "")
  call <sid>hi("pythonStatement", s:gui0E, "", s:cterm0E, "", "", "")

  " Ruby highlighting
  call <sid>hi("rubyAttribute",               s:gui0D, "", s:cterm0D, "", "", "")
  call <sid>hi("rubyConstant",                s:gui0A, "", s:cterm0A, "", "", "")
  call <sid>hi("rubyInterpolationDelimiter",  s:gui0F, "", s:cterm0F, "", "", "")
  call <sid>hi("rubyRegexp",                  s:gui0C, "", s:cterm0C, "", "", "")
  call <sid>hi("rubySymbol",                  s:gui0B, "", s:cterm0B, "", "", "")
  call <sid>hi("rubyStringDelimiter",         s:gui0B, "", s:cterm0B, "", "", "")

  " SASS highlighting
  call <sid>hi("sassidChar",     s:gui08, "", s:cterm08, "", "", "")
  call <sid>hi("sassClassChar",  s:gui09, "", s:cterm09, "", "", "")
  call <sid>hi("sassInclude",    s:gui0E, "", s:cterm0E, "", "", "")
  call <sid>hi("sassMixing",     s:gui0E, "", s:cterm0E, "", "", "")
  call <sid>hi("sassMixinName",  s:gui0D, "", s:cterm0D, "", "", "")

  " Signify highlighting
  call <sid>hi("SignifySignAdd",     s:gui0B, s:gui01, s:cterm0B, s:cterm01, "", "")
  call <sid>hi("SignifySignChange",  s:gui0D, s:gui01, s:cterm0D, s:cterm01, "", "")
  call <sid>hi("SignifySignDelete",  s:gui08, s:gui01, s:cterm08, s:cterm01, "", "")

  " Spelling highlighting
  call <sid>hi("SpellBad",     "", "", "", "", "undercurl", s:gui08)
  call <sid>hi("SpellLocal",   "", "", "", "", "undercurl", s:gui0C)
  call <sid>hi("SpellCap",     "", "", "", "", "undercurl", s:gui0D)
  call <sid>hi("SpellRare",    "", "", "", "", "undercurl", s:gui0E)

  " Startify highlighting
  call <sid>hi("StartifyBracket",  s:gui03, "", s:cterm03, "", "", "")
  call <sid>hi("StartifyFile",     s:gui07, "", s:cterm07, "", "", "")
  call <sid>hi("StartifyFooter",   s:gui03, "", s:cterm03, "", "", "")
  call <sid>hi("StartifyHeader",   s:gui0B, "", s:cterm0B, "", "", "")
  call <sid>hi("StartifyNumber",   s:gui09, "", s:cterm09, "", "", "")
  call <sid>hi("StartifyPath",     s:gui03, "", s:cterm03, "", "", "")
  call <sid>hi("StartifySection",  s:gui0E, "", s:cterm0E, "", "", "")
  call <sid>hi("StartifySelect",   s:gui0C, "", s:cterm0C, "", "", "")
  call <sid>hi("StartifySlash",    s:gui03, "", s:cterm03, "", "", "")
  call <sid>hi("StartifySpecial",  s:gui03, "", s:cterm03, "", "", "")

  " Java highlighting
  call <sid>hi("javaOperator",     s:gui0D, "", s:cterm0D, "", "", "")
  " }}}

  " Remove functions
  delf <sid>hi

  " Remove color variables
  unlet s:gui00 s:gui01 s:gui02 s:gui03  s:gui04  s:gui05  s:gui06  s:gui07  s:gui08  s:gui09 s:gui0A  s:gui0B  s:gui0C  s:gui0D  s:gui0E  s:gui0F
  unlet s:cterm00 s:cterm01 s:cterm02 s:cterm03 s:cterm04 s:cterm05 s:cterm06 s:cterm07 s:cterm08 s:cterm09 s:cterm0A s:cterm0B s:cterm0C s:cterm0D s:cterm0E s:cterm0F

endif
" }}}

  " Cursor (after theme setting) {{{
  highlight Cursor guifg=white guibg=green
  highlight iCursor guifg=white guibg=orange
  set guicursor=n-v-c:block-Cursor
  set guicursor+=i:ver25-iCursor
  set guicursor+=a:blinkon0
  " }}}

  " GUI elements {{{
  " see :h 'guioptions'
  set go =P " Auto copy on selection
  set go+=c " Use console dialog
  set go+=m " Menubar
  " }}}

" }}}

" vim-commentary {{{
" commentary.vim - Comment stuff out
" Maintainer:   Tim Pope <http://tpo.pe/>
" Version:      1.3
" GetLatestVimScripts: 3695 1 :AutoInstall: commentary.vim

if !(exists("g:loaded_commentary") || v:version < 700)
  let g:loaded_commentary = 1

  function! s:surroundings() abort
    return split(get(b:, 'commentary_format', substitute(substitute(substitute(
          \ &commentstring, '^$', '%s', ''), '\S\zs%s',' %s', '') ,'%s\ze\S', '%s ', '')), '%s', 1)
  endfunction

  function! s:strip_white_space(l,r,line) abort
    let [l, r] = [a:l, a:r]
    if l[-1:] ==# ' ' && stridx(a:line,l) == -1 && stridx(a:line,l[0:-2]) == 0
      let l = l[:-2]
    endif
    if r[0] ==# ' ' && a:line[-strlen(r):] != r && a:line[1-strlen(r):] == r[1:]
      let r = r[1:]
    endif
    return [l, r]
  endfunction

  function! s:go(...) abort
    if !a:0
      let &operatorfunc = matchstr(expand('<sfile>'), '[^. ]*$')
      return 'g@'
    elseif a:0 > 1
      let [lnum1, lnum2] = [a:1, a:2]
    else
      let [lnum1, lnum2] = [line("'["), line("']")]
    endif

    let [l, r] = s:surroundings()
    let uncomment = 2
    for lnum in range(lnum1,lnum2)
      let line = matchstr(getline(lnum),'\S.*\s\@<!')
      let [l, r] = s:strip_white_space(l,r,line)
      if len(line) && (stridx(line,l) || line[strlen(line)-strlen(r) : -1] != r)
        let uncomment = 0
      endif
    endfor

    for lnum in range(lnum1,lnum2)
      let line = getline(lnum)
      if strlen(r) > 2 && l.r !~# '\\'
        let line = substitute(line,
              \'\M'.r[0:-2].'\zs\d\*\ze'.r[-1:-1].'\|'.l[0].'\zs\d\*\ze'.l[1:-1],
              \'\=substitute(submatch(0)+1-uncomment,"^0$\\|^-\\d*$","","")','g')
      endif
      if uncomment
        let line = substitute(line,'\S.*\s\@<!','\=submatch(0)[strlen(l):-strlen(r)-1]','')
      else
        let line = substitute(line,'^\%('.matchstr(getline(lnum1),'^\s*').'\|\s*\)\zs.*\S\@<=','\=l.submatch(0).r','')
      endif
      call setline(lnum,line)
    endfor
    let modelines = &modelines
    try
      set modelines=0
      silent doautocmd User CommentaryPost
    finally
      let &modelines = modelines
    endtry
    return ''
  endfunction

  function! s:textobject(inner) abort
    let [l, r] = s:surroundings()
    let lnums = [line('.')+1, line('.')-2]
    for [index, dir, bound, line] in [[0, -1, 1, ''], [1, 1, line('$'), '']]
      while lnums[index] != bound && line ==# '' || !(stridx(line,l) || line[strlen(line)-strlen(r) : -1] != r)
        let lnums[index] += dir
        let line = matchstr(getline(lnums[index]+dir),'\S.*\s\@<!')
        let [l, r] = s:strip_white_space(l,r,line)
      endwhile
    endfor
    while (a:inner || lnums[1] != line('$')) && empty(getline(lnums[0]))
      let lnums[0] += 1
    endwhile
    while a:inner && empty(getline(lnums[1]))
      let lnums[1] -= 1
    endwhile
    if lnums[0] <= lnums[1]
      execute 'normal! 'lnums[0].'GV'.lnums[1].'G'
    endif
  endfunction

  command! -range -bar Commentary call s:go(<line1>,<line2>)
  xnoremap <expr>   <Plug>Commentary     <SID>go()
  nnoremap <expr>   <Plug>Commentary     <SID>go()
  nnoremap <expr>   <Plug>CommentaryLine <SID>go() . '_'
  onoremap <silent> <Plug>Commentary        :<C-U>call <SID>textobject(get(v:, 'operator', '') ==# 'c')<CR>
  nnoremap <silent> <Plug>ChangeCommentary c:<C-U>call <SID>textobject(1)<CR>
  nmap <silent> <Plug>CommentaryUndo :echoerr "Change your <Plug>CommentaryUndo map to <Plug>Commentary<Plug>Commentary"<CR>

  if !hasmapto('<Plug>Commentary') || maparg('gc','n') ==# ''
    xmap gc  <Plug>Commentary
    nmap gc  <Plug>Commentary
    omap gc  <Plug>Commentary
    nmap gcc <Plug>CommentaryLine
    if maparg('c','n') ==# '' && !exists('v:operator')
      nmap cgc <Plug>ChangeCommentary
    endif
    nmap gcu <Plug>Commentary<Plug>Commentary
  endif

endif
" }}}
nmap g<leader> gcc

" Misc functions {{{
" }}}

noh

" vim:fdm=marker
