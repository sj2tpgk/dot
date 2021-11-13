" vim: fdm=marker
" todo: smart-beg, count for togcmt

" Fast startup {{{
let g:python_host_skip_check=1
let g:loaded_python3_provider=1
set noloadplugins
" set shada="none"
" filetype off
" syntax off
" aug vimrc_speed
"   au!
"   au UIEnter * exe "filetype on | syntax on | filetype plugin indent on"
" aug END
" }}}

" Plugin {{{
" let data_dir = has('nvim') ? stdpath('data') . '/site' : '~/.vim'
" if empty(glob(data_dir . '/autoload/plug.vim'))
"   silent execute '!curl -fLo '.data_dir.'/autoload/plug.vim --create-dirs  https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'
"   autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
" endif
"
" call plug#begin('~/.vim/plugged')
" Plug 'neovim/nvim-lspconfig'
" call plug#end()
" }}}

" Misc (options) {{{
set nocompatible      " required in project.vim
set showmatch         " Show matching brackets.
set autowrite         " Automatically save before commands like :next and :make
set mouse=a           " Enable mouse usage (see also: 'ttymouse')
set pumheight=12      " Height of popup menu in completion
set showcmd           " Show command being input
set hidden            " Allow changing buffer without saving
set textwidth=0       " Prevent automatically insert '\n' and goto next line
set scrolloff=3       " Cursor > (bottom of screen + n)
set sidescrolloff=5
let mapleader=" "
set t_Co=16           " Number of colors = 16
set modeline          " Recognize modeline (e.g. vim: ft=lua ("set" doesn't work!))
set modelines=5       " Search modeline for N lines from top and bottom
set number            " Line number
set splitbelow
set splitright
set confirm           " Ask on :q :wq etc.

if exists("&ttymouse")
    set ttymouse=xterm2   " Mouse drag to resize windows
endif

set clipboard=unnamedplus

" Japanese encodings
" don't use modeline(comment at the end of a file) to set enc (vim:enc=euc-jp etc.)
set fileencodings=ucs-bom,utf-8,iso-2022-jp,sjis,cp932,euc-jp,cp20932
" }}}

" Indent {{{
set expandtab               " Use spaces instead of tab
set autoindent
filetype plugin indent on   " This triggers filetype.vim (slow)
set smartindent

set shiftwidth=4            " Tab = N spaces in << >> etc.
set softtabstop=4           " Insert N spaces as a tab
set tabstop=4               " A tab shows as N spaces
" }}}

" Folding {{{
set fdm=marker
nnore <tab> za
nnore <leader>z :set fdm=marker<cr>zm

" close folding by 'h' if cursor is 0 of the line and in a opened folding
nnore <expr> h  (getcurpos()[2] == 1) && (foldlevel('.') != 0) ? 'zc' : 'h'

fu! NonEmptyLine(lnum, dir) " find nonempty line (but not a:lnum) in given direction (1 or -1)
  let lnum = a:lnum + a:dir
  let max = line("$")
  while lnum >= 1 && lnum <= max && getline(lnum) =~ '^\s*$'
    let lnum += a:dir
  endwh
  return lnum
endfu

aug vimrc_folding
  au!
  au FileType lua setl fdm=expr fde=max([indent(v:lnum),indent(NonEmptyLine(v:lnum,1)),indent(NonEmptyLine(v:lnum,-1))])/&shiftwidth
aug END
" }}}

" View {{{
" remove "options" from 'viewoptions' -- otherwise modifying vimrc sometimes ineffective
set viewoptions=cursor,folds
aug vimrc_view
  " save folding status
  au BufWinLeave * if expand('%') != '' && &buftype !~ 'nofile' | mkview | endif
  au BufWinEnter * if expand('%') != '' && &buftype !~ 'nofile' | silent! loadview | endif
  " ! in sil! is needed (for new or unvisited-yet files)
aug END
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

" Keys (keyboard layout agnostic) {{{

" vimrc
nnore <f5> :wa<cr>:sil source $MYVIMRC<cr>
nnore s<f5> :lua smartSp("$MYVIMRC")<cr>

" motion
onore m %
nnore m %
nnore M m

" edit
nnore D dd
nnore Y yy
nnore U <c-r>
nnore si s
nnore yb :call SaveExcursion("ggVGy")<cr>
nnore yf :let @+=expand("%:t")<cr>
nnore yp :let @+=expand("%:p")<cr>
nnore db ggVGd
nnore s= :call SaveExcursion("ggVG=")<cr>
nnore ;  :lua toggleCmt(false)<cr>
vnore ;  :lua toggleCmt(true)<cr>

" window, buffer
nnore Q :q<cr>
nnore - <C-w>w
nnore so <c-w>o
nnore sd <c-w>c
nnore ss :sp<cr>
nnore sv :vsp<cr>
nnore sm :ls<cr>:b<space>
" nnore sb :sp\|b#<cr><c-w>p:bd<cr>
nnore sb :bd<cr>
nnore sp :lua smartSp()<cr>

" misc
nnore s/ :noh<cr>
nnore :<cr> :<up><cr>

fu! SaveExcursion(normcmd)
  let l:w = winsaveview()
  exe "norm! " . a:normcmd
  call winrestview(l:w)
endfu

" }}}

" Completion {{{
inore <tab> <c-n>
inore <s-tab> <c-p>

inore <expr> <del> pumvisible() ? "\<c-e>" : "\<del>"

set shortmess+=c                           " No message like "Pattern not found"
set completeopt+=menuone,noinsert,noselect " Needed for auto completion

let g:comp_minlen = 2  " At least N chars to start completion

aug vimrc_complete
  au!

  " Completion using dictionary files for rlwrap
  fu! AddCompSource(ft, name)
  " exe "au FileType " . a:ft . " setl cpt+=k~/.cpt/" . a:name
    exe "au FileType " . a:ft . " setl cpt+=k~/.rlwrap/" . a:name . "_completions"
  endfu
  for ft in ["lua", "python"] | call AddCompSource(ft, ft) | endfor
  " call AddCompSource("javascript", "node")

  au FileType lua setl iskeyword+=.

  " Auto complete
  fu! OpenCompletion()
    " check (menu visible && inserting alphabet && at least comp_minlen chars)
    if !pumvisible() && ((v:char >= 'a' && v:char <= 'z') || (v:char >= 'A' && v:char <= 'Z')) && (g:comp_minlen == 1 || (col(".") >= (g:comp_minlen-1) && matchstr(getline("."), '\%' . (col('.')-(g:comp_minlen-1)) . 'c[a-zA-Z_]\{' . (g:comp_minlen-1) . '\}') != ""))
      call feedkeys("\<C-n>", "n")
    endif
  endfu
  au InsertCharPre * call OpenCompletion()

aug END
" }}}

" Colorscheme, cursor {{{
"colorscheme slate
"colorscheme ron
colorscheme default

fu! MyColor()
  hi Constant     ctermfg=green cterm=bold
  hi NonText      ctermfg=magenta
  hi comment      ctermfg=blue
  "hi statement    ctermfg=red
  hi String       ctermfg=green
  hi Type         ctermfg=cyan cterm=bold
  hi Conditional  ctermfg=green cterm=bold
  hi preproc      ctermfg=cyan
  "hi Identifier   ctermfg=red cterm=bold
  hi Special      ctermfg=red
endfu
call MyColor()
aug vimrc_hi " :hi need to be autocmd on first run??
  au!
  au VimEnter * :call MyColor()
aug END

set cursorline
aug vimrc_cursor
  au!
  au! InsertEnter * set nocursorline!
  au! InsertLeave * set cursorline!
aug END

set list listchars=trail:.
" }}}

" Lua part

lua << EOF

do -- Keys (keyboard layout specific) {{{

    local mykbd = (vim.env.MYKBD == "colemakdh")

    local mappings = {
        "nv  j  n  gj",
        "nv  k  e  gk",
        "nv  J  N  <c-d>",
        "nv  K  E  <c-u>",
        "v   h  k  h",
        "v   l  i  l",
        "nv  gh gk ^",
        "nv  gl gi <end>",
        "nv  i  l  i",
        "nv  I  L  I",
        "nv  si sl s",

        "nv  n  j  n",
        "nv  N  J  N",

        "nvo e  h  e",
        "nvo E  H  E",

        "o   h  k  0",
        "o   l  i  $",
        "o   iw lw iw",
        "o   iW lW iW",
    }

    for _, entry in ipairs(mappings) do
        local a1,a2,a3,a4
        for i1,i2,i3,i4 in entry:gmatch("(%S+)%s+(%S+)%s+(%S+)%s+(.+)") do a1,a2,a3,a4=i1,i2,i3,i4 break end
        for mode in a1:gmatch("%S") do
            vim.cmd(mode .. "nore " .. (mykbd and a3 or a2) .. " " .. a4)
        end
    end

    vim.cmd("nnore <expr> " .. (mykbd and "k" or "h") .. " (getcurpos()[2] == 1) && (foldlevel('.') != 0) ? 'zc' : 'h'")
    vim.cmd("nnore <expr> " .. (mykbd and "i" or "l") .. " (foldclosed('.') != -1) ? 'zo' : 'l'")

end -- }}}

function smartSp(file) -- {{{
    local w = vim.fn.winwidth(0)
    local h = vim.fn.winheight(0)
    vim.cmd((w/h < 3) and "sp" or "vsp")
    if file then vim.cmd("e " .. file) end
end -- }}}

function toggleCmt(visual) -- {{{
    local function regEscape(s) return (s:gsub("([^%w])", "%%%1")) end

    -- Analyze commentstring
    local cms    = vim.bo.cms
    local x,y,z  = cms:match("(.*%S)(%s*)%%s(.*)")
    local p      = "^(%s*)" .. regEscape(x) .. "(.*)" .. regEscape(z)
    local p1     = "^(%s*)" .. regEscape(x) .. y:gsub(".", " ?") .. "(.*)" .. regEscape(z)

    -- let cms = "# %s"
    -- p  : must match line if middle space is absent
    -- p1 : middle space must be at most 1 (%s should eat rest spaces)

    -- Get range
    local lbeg   = visual and vim.fn.line("'<") or vim.fn.line(".")
    local lend   = visual and vim.fn.line("'>") or lbeg

    for i = lbeg, lend do

        local line = vim.fn.getline(i)
        if line:match(p) then
            -- print("commented")
            local new = line:gsub(p1, "%1%2")
            vim.fn.setline(i, new)
        else
            -- print("not commented")
            local new = cms:gsub("%%s", regEscape(line))
            vim.fn.setline(i, new)
        end

    end
end -- }}}

EOF

let aaaaa = "aaa%1"
let bb = "aaa%1"
    "let ccc = "aaa%1"
