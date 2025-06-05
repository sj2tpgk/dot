-- vim: fdm=marker fmr=<<<,>>>
-- default fold marker breaks indentation
-- if above modeline does not work, check ~/.local/state/nvim/view/ for cache

-- init.lua with treesitter indent has better stability over lua in init.vim

vim.cmd [[ " Early <<<

" Clear
mapclear | imapclear | cmapclear | tmapclear

" Easy lua debug. Use L! to print nested table. Example ":L 123,456"
command! -nargs=* -bang -complete=lua L lua pp(<q-args>, true, ("<bang>" == "") and 1 or 99, <args>)

" Fast startup
let g:python_host_skip_check=1
let g:loaded_python3_provider=1

" Environment info
let g:env = {
    \ "vscode": exists("g:vscode"),
    \ "git":    executable("git"),
    \ "curl":   executable("curl"),
    \ "dark":   !empty($MY_DARK) ? ($MY_DARK == "1") : ($MYKBD == "colemakdh"),
    \ "llama":  !empty($MY_LLAMA) ? $MY_LLAMA : v:false,
    \ }

]] -- >>>

function plug(url, lazy, branch) -- Plugin manager <<<
    -- Example: plug "neovim/nvim-lspconfig"
    -- Just git clone repo, windows compatible
    -- Using /site/pack/**/opt/ instead of /site/pack/**/start/ to not automatically load packages
    -- "packadd! nvim-treesitter" to actually load
    -- To remove package, remove the directory mentioned above
    -- Limitations: need manual update or remove, no hooks, no async etc.

    -- Use github as default source
    if url:find("http") ~= 1 then url = "https://github.com/" .. url end

    -- e.g. "nvim-lspconfig"
    local name = url:gsub("^.*/", ""):gsub("%.git$", "")

    -- e.g. "~/.local/share/nvim/site/pack/nvim-lspconfig/opt/"
    -- Using slash as path delimiter is ok on windows
    local dir = vim.fn.stdpath("data") .. "/site/pack/" .. name .. "/opt/"

    -- Download package if not already installed
    if 0 == vim.fn.isdirectory(dir .. "/" .. name) then

        -- Check if git is available
        if not vim.fn.executable("git") then print("plug: git is not available!") return end

        -- Create dir to clone git repo in
        vim.fn.mkdir(dir, "p")

        -- Run git clone
        cmd = { "git", "-C", dir, "clone", "--depth", 1 }
        if branch then table.insert(cmd, "-b") table.insert(cmd, branch) end
        table.insert(cmd, url)
        vim.fn.system(cmd)

        -- Regenerate help (after vim startup); needed for vim to recognize docs
        vim.cmd("aug plug \n au! \n au VimEnter * helpt ALL \n aug END")

    end

    -- Load package (not needed if you use /start instead of /opt)
    if not lazy then vim.cmd("packadd! " .. name) end

end
function can_require(x)
    if package.loaded[x] then
        return true
    end
    for _, searcher in ipairs(package.searchers or package.loaders) do
        local loader = searcher(x)
        if type(loader) == "function" then
            package.preload[x] = loader
            return true
        end
    end
end
function lazy(callback, delay) -- lazily run vimscript or lua function
    -- `callback` is either string (vimscript) or lua function (lua)
    delay = delay or 0 -- if 0 it's similar to asynchronous-ness but may still block UI
    local timer = vim.uv.new_timer()
    timer:start(0, 0, vim.schedule_wrap(function()
        if type(callback) == "string" then vim.api.nvim_command(callback) else callback() end
    end))
end
-- >>>

do -- Plugins <<<

    -- Better syntax highlighting and indent for langs
    plug "Vimjas/vim-python-pep8-indent"

    -- Treesitter
    -- plug ("nvim-treesitter/nvim-treesitter", 1) -- lazy loading (experimental)
    -- vim.cmd("aug vimrc_loadts \n au! \n au FileType sh,c,css,cpp,go,html,javascript,kotlin,lua,python,vim,help lua lazy('packadd nvim-treesitter | call v:lua.ts_config() | au! vimrc_loadts') \n aug END")

    -- LSP
    plug "neovim/nvim-lspconfig"

    -- AI
    -- plug ('ggml-org/llama.vim', 1)
    plug ("https://codeberg.org/sj2tpgk/llama.vim", 0, "new")


    -- Text editing
    -- plug "junegunn/vim-easy-align"
    -- plug "mg979/vim-visual-multi"
    plug "windwp/nvim-autopairs"

    -- Misc
    -- plug "stevearc/profile.nvim"

    -- My plugins
    plug "https://codeberg.org/sj2tpgk/vim-fast-syntax"

end -- >>>

vim.cmd [[ " Plugin options <<<
let g:javascript_plugin_jsdoc       = 1
let g:python_highlight_all          = 1
let g:python_highlight_space_errors = 0
let g:python_highlight_func_calls   = 0
let g:fastsyntax_enable_symbol      = 1
]] -- >>>

vim.cmd [[ " Vim config <<<

" Misc options <<<
set nocompatible      " required in project.vim
set showmatch         " Show matching brackets.
set autowrite         " Automatically save before commands like :next and :make
set mouse=a           " Enable mouse usage (see also: 'ttymouse')
set pumheight=12      " Height of popup menu in completion
set noshowcmd         " No show command being input
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
set autochdir         " Auto change working directory to current files' like in emacs
set wildignorecase    " Case-insensitive filename completion
"set paste             " No auto-indent on paste

if exists("&mousescroll") | set mousescroll=ver:5,hor:6 | endif
if exists("&ttymouse") | set ttymouse=xterm2 | endif " Mouse drag to resize windows

set clipboard=unnamedplus
" If my clipboard wrapper is available, use it.
if executable("xcopy") && executable("xpaste")
    let g:clipboard = { 'name': 'my', 'copy': { '+': 'xcopy', '*': 'xcopy', }, 'paste': { '+': 'xpaste', '*': 'xpaste', }, 'cache_enabled': 1, }
endif


" Japanese encodings
" don't use modeline(comment at the end of a file) to set enc (vim:enc=euc-jp etc.)
set fileencodings=ucs-bom,utf-8,iso-2022-jp,sjis,cp932,euc-jp,cp20932

" Completely disable automatic line break at certain column ON ALL FILES
aug vimrc_disable_auto_line_break
au!
au FileType * set tw=0 fo-=t
aug END
" >>>

" Status line <<<
" show directory name, filetype, encoding
fu! StlDirName() " e.g. ~/.c/nvim when editing ~/.config/nvim/init.lua
    let x = substitute(expand('%:p:h'), $HOME, '~', '')
    " unix
    let x = substitute(x, '\([-._]*[^/]\|[^/]\)[^/]*/', '\1/', 'g')
    " windows (backslash instead of slash; keep C: etc.)
    let x = substitute(x, '\(\a:\|[-._]*[^\\]\|[^\\]\)[^\\]*\\', '\1\\', 'g')
    return x
endfu
fu! StlFileTypeEnc() " e.g. (lua, utf-8)
    return join(filter([&ft, &fenc], 'len(v:val) > 0'), ', ')
endfu
fu! StlLspSigOrFtEnc()
    let lspsig = luaeval("lspsig")
    return len(lspsig) > 0 ? lspsig : ("(" . StlFileTypeEnc() . ")")
endfu
fu! StlClose(minwid, nclicks, btn, mod) " when called, close buffer
    if len(filter(range(1, bufnr('$')), '! empty(bufname(v:val)) && buflisted(v:val)')) == 1
        q
    else
        bd
    endif
endfu
set statusline=%h%w%m%r\ \ %<%t\ \ \ [%{StlDirName()}]\ \ \ (%{StlFileTypeEnc()})%=%-14.(%l,%c%V%)\ %P%3@StlClose@\ \ [X]%X

set laststatus=2
" set cmdheight=0
" >>>

" Tabline <<<
fu! Tabline() abort
    let l:line = ''
    let l:current = tabpagenr()

    for l:i in range(1, tabpagenr('$'))
        if l:i == l:current
            let l:line .= '%#TabLineSel#'
        else
            let l:line .= '%#TabLine#'
        endif

        let l:label = fnamemodify(
            \ bufname(tabpagebuflist(l:i)[tabpagewinnr(l:i) - 1]),
            \ ':t'
            \ )

        let l:line .= '%' . i . 'T' " Starts mouse click target region.
        let l:line .= '  ' . l:label . '  '
    endfor

    let l:line .= '%#TabLineFill#'
    let l:line .= '%T' " Ends mouse click target region(s).

    return l:line
endfu

set tabline=%!Tabline()
" >>>

" Indent <<<
set expandtab               " Use spaces instead of tab
set autoindent
filetype plugin indent on   " This triggers filetype.vim (slow)
" set smartindent

set shiftwidth=4            " Tab = N spaces in << >> etc.
set softtabstop=4           " Insert N spaces as a tab
set tabstop=4               " A tab shows as N spaces
set shiftround              " Indents to next multiple of 'shiftwidth'
" >>>

" Folding <<<
set fdm=marker
set fdn=2
nnore <tab> za

" close folding by 'h' if cursor is 0 of the line and in a opened folding
"nnore <expr> h  (getcurpos()[2] == 1) && (foldlevel('.') != 0) ? 'zc' : 'h'

"set nofoldenable

set foldminlines=3
set fillchars=fold:\  foldtext='\ '.substitute(getline(v:foldstart),'{{{','','g').'\ \ '.(v:foldend-v:foldstart).'\ '

" Better indent-based folding; alto take 'fdn' into account
fu! MyFold(l, python)
    let l = a:l
    let sw = &shiftwidth
    if IsFirstCharInString(l)
        let lev = 1 + (indent(MyFold_NextLine(l, -1)) / sw)
    elseif IsFirstCharInString(l+1)
        let lev = 1 + (indent(l) / sw)
    else
        if a:python
            " In python, only when inside multiline string, take into account NextLine(v:lnum,-1)
            let lev = max([indent(v:lnum),indent(MyFold_NextLine(v:lnum,1)),(IsFirstCharInString(v:lnum)?indent(MyFold_NextLine(v:lnum,-1)):0)])/&shiftwidth
        else
            let lev = max([indent(l),indent(MyFold_NextLine(l,1)),indent(MyFold_NextLine(l,-1))])/sw
        endif
    endif
    return min([lev, &fdn])
endfu

fu! MyFold_NextLine(lnum, dir)
    " Find next nonempty and non-multiline-string line (but not a:lnum) in given direction (1 or -1)
    let lnum = a:lnum + a:dir
    let max = line("$")
    while lnum >= 1 && lnum <= max && (getline(lnum) =~ '^\s*$' || IsFirstCharInString(lnum))
        let lnum += a:dir
    endwh
    return lnum
endfu

fu! IsFirstCharInString(lnum)
    if exists("*v:lua.ts_IsFirstCharInString") | return v:lua.ts_IsFirstCharInString(a:lnum) | endif
    " Maybe true  if and only if  inside a multiline string.
    let synNames = map(synstack(a:lnum, 1), "synIDattr(synIDtrans(v:val), 'name')")
    return len(synNames) > 0 && synNames[-1] == "String"
endfu

" Markdown folding (https://stackoverflow.com/a/4677454)
fu! MarkdownLevel()
    if getline(v:lnum) =~ '^# .*$'
        return ">1"
    endif
    if getline(v:lnum) =~ '^## .*$'
        return ">2"
    endif
    if getline(v:lnum) =~ '^### .*$'
        return ">3"
    endif
    if getline(v:lnum) =~ '^#### .*$'
        return ">4"
    endif
    if getline(v:lnum) =~ '^##### .*$'
        return ">5"
    endif
    if getline(v:lnum) =~ '^###### .*$'
        return ">6"
    endif
    return "="
endfu

aug vimrc_folding
au!
au FileType awk,c,cpp,go,html,javascript,json,json5,lua,perl,python,markdown call MyFolding()
aug END

fu! MyFolding()
    let ft = &ft
    if index(["awk", "c", "cpp", "go", "html", "javascript", "json", "json5", "lua", "perl"], ft) != -1
        setl fdm=expr fde=MyFold(v:lnum,0)
    elseif ft == "python"
        setl fdm=expr fde=MyFold(v:lnum,1)
    elseif ft == "Markdown"
        setl fdm=expr fde=MarkdownLevel()
    "elseif index(["lua", "javascript", "html", "perl", "c", "cpp", "awk"], ft) != -1
    else
        setl fdm=expr fde=MyFold(v:lnum,0)
    endif
endfu

nnore <f7> :call MyFolding()<cr>

" >>>

" View <<<
" remove "options" from 'viewoptions' -- otherwise modifying vimrc sometimes ineffective
set viewoptions=cursor,folds
aug vimrc_view
" save folding status
au BufWinLeave * if expand('%') != '' && &buftype !~ 'nofile' | mkview | endif
au BufWinEnter * if expand('%') != '' && &buftype !~ 'nofile' | silent! loadview | endif
" ! in sil! is needed (for new or unvisited-yet files)
aug END
" >>>

" Search <<<
set ignorecase
set smartcase
set incsearch
set hlsearch
" >>>

" Esc <<<
set ttimeoutlen=8

if !env.vscode
    " If popup window is open, close it
    fu! SmartEsc()
        let nr = 1 + index(map(range(1, winnr("$")), "win_gettype(v:val)"), 'popup')
        if nr >= 1
            " ':{nr}wincmd q' asks to save file when closing some lsp's popup windows. use ':{count}close' command
            " exe nr .. "wincmd q"
            exe nr .. "close"
        endif
        exe "norm! \<escape>"
    endfu
    nnore <silent> <escape> :call SmartEsc()<cr>
endif

" Do not move cursor position on leaving insert mode
" better version (https://stackoverflow.com/a/2296229)
inoremap <silent> <Esc> <Esc>`^
" old version (https://stackoverflow.com/a/2296229)
" aug vimrc_myesc
" au!
" " Two autocmds needed (for when cursor is at beginning or end of line)
" au InsertLeavePre * let myesc_col = col(".")
" au InsertLeave    * exe (myesc_col >= 2) ? "norm! l" : ""
" aug END
" >>>

" Keys (keyboard layout independent) <<<

" vimrc
nnore <f5> :wa<cr>:sil source $MYVIMRC<cr>
nnore s<f5> :lua smartSp("$MYVIMRC")<cr>
nnore sr    :lua smartSp("$MYVIMRC")<cr>

" motion
onore m %
nnore m %
vnore m %
nnore M m
nnore <silent> 0      :lua smartHome()<cr>
nnore <silent> <home> :lua smartHome()<cr>
inore <silent> <home> <c-o>:lua smartHome(true)<cr>
nnore <c-u> <c-i>
nnore <silent> <expr> f (reg_recording() . reg_executing()) != "" ? "f" : ":lua smartf(1)\<cr>"
nnore <silent> <expr> F (reg_recording() . reg_executing()) != "" ? "F" : ":lua smartf(-1)\<cr>"
nnore <space> <c-d>
nnore <backspace> <c-u>

" edit
nnore D dd
nnore Y yy
nnore U <c-r>
nnore si s
nnore yb :call SaveExcursion("ggVGy")<cr>
nnore yf :let @+=expand("%:t")<cr>
nnore yp :let @+=expand("%:p")<cr>
nnore yd :let @+=expand("%:h")<cr>
nnore yc :let @+=printf("%s:%s\n%s\n", expand("%:p"), line("."), getline(line(".")))<cr>
nnore db ggVGd
nnore s= :call SaveExcursion("ggVG=")<cr>
nnore <a-j> J
nnore <silent> ; :lua toggleCmt(false)<cr>
vnore <silent> ; :lua toggleCmt(true)<cr>
nnore <lt> <lt><lt>
nnore > >>

" window, buffer, tab
nnore Q :q<cr>
nnore <silent> q :lua smartq()<cr>
nnore <silent> - :call NextWindow()<cr>
" nnore <s--> <C-w>w
nnore so <c-w>o
nnore sd <c-w>c
nnore ss :sp<cr>
nnore sv :vsp<cr>
" nnore sb :sp\|b#<cr><c-w>p:bd<cr>
nnore sb :bd<cr>
"nnore sp :lua smartSp()<cr>
nnore s<space> :b#<cr>
"nnore sm :ls<cr>:b<space>
nnore <expr> sm FzfExists() ? ":FzfBuffers\<cr>" : ":ls\<cr>:b\<space>"
nnore <expr> sf FzfExists() ? ":FzfFiles\<cr>"   : ":e\<space>"
nnore <expr> sF FzfExists() ? ":FzfFiles!\<cr>"  : ":e\<space>"
nnore + :tabnext<cr>
nnore <silent> su :vsplit<bar>wincmd l<bar>exe "norm! Ljz<c-v><cr>"<cr>:set scb<cr>:wincmd h<cr>:set scb<cr>

" misc
nnore * :call SearchQF()<cr>
nnore sc :copen<cr>
nnore s/ :noh<cr>:let @/ = ""<cr>
nnore s* :call AddHighlight()<cr>
nnore :<cr> :<up><cr>
tnore <esc> <c-\><c-n>
nnore <c-h> :call DescribeFace()<cr>
nore  <cr> K
nnore <c-l> :call RecenterTopBottom()<cr>
"   commandline: <up> to prev candidate when completion popup menu in visible, otherwise go to previous history item with prefix matching
cnore <expr> <up>   pumvisible() ? "\<c-p>" : "\<up>"
cnore <expr> <down> pumvisible() ? "\<c-n>" : "\<down>"
vnore T :T<space>/
nnore { zm
nnore } zr
nnore ( zc
nnore ) zo
nnore zm zM
nnore zr zR

" Ignore function keys
for i in range(1, 16)
    for j in ["", "s-", "c-"]
        for k in ["", "i", "c"]
            if j == "" && i == 5 | continue | endif
            exe k . "nore <" . j . "f" . i . "> <nop>"
        endfor
    endfor
endfor

fu! SaveExcursion(normcmd)
    let l:w = winsaveview()
    exe "norm! " . a:normcmd
    call winrestview(l:w)
endfu

let g:RecenterTopBottom_time = 0 " last invoked time
fu! RecenterTopBottom()
    let h = winheight(".")
    let l = winline()
    let thresh = 2
    if abs(g:RecenterTopBottom_time - localtime()) >= 2
        " syn sync fromstart " slow for some reason?? (called even if this if clause is skipped??)
        exe "syn sync fromstart"
        exe "norm! \<c-l>"
        norm! zz
    elseif abs(l - (h/2)) <= thresh
        norm! zt
    elseif abs(l - &scrolloff) <= thresh
        norm! zb
    else
        norm! zz
    endif
    let g:RecenterTopBottom_time = localtime()
endfu

fu! GetVisualSelection()
    " Why is this not a built-in Vim script function?!
    let [line_start, column_start] = getpos("'<")[1:2]
    let [line_end, column_end] = getpos("'>")[1:2]
    let lines = getline(line_start, line_end)
    if len(lines) == 0
        return ''
    endif
    let lines[-1] = lines[-1][: column_end - (&selection == 'inclusive' ? 1 : 2)]
    let lines[0] = lines[0][column_start - 1:]
    return join(lines, "\n")
endfu

fu! AddHighlight()
    " Highlight word under cursor IN ADDITION TO currently highlighted ones.
    let word = expand("<cword>")
    let @/ = (@/ == "" ? "" : (@/ . "\\|")) . ("\\<" . word . "\\>")
    " Turn on highlighting (if no word was previously highlighted); need feedkeys for some reason
    call feedkeys(":set hlsearch\<cr>")
    " Goto next match
    norm! n
endfu

fu! NextWindow()
    let curWin = winnr()
    exec "norm! \<c-w>w"
    while (!buflisted(bufnr())) " skip windows with this condition
        if winnr() == curWin | exec "norm! \<c-w>w" | return | endif
        exec "norm! \<c-w>w"
    endwh
endfu

fu! GoUp(dir="up")
    if g:env.vscode " default command is too fast
        call VSCodeCall("cursorMove", { "by": "line", "value": 20, "to": a:dir == "down" ? "down" : "up" })
    else
        exe a:dir == "down" ? "norm! \<c-d>" : "norm! \<c-u>"
    endif
endfu

fu! SearchQF()
    " search <cword> with vimgrep (so you can :copen later)
    let word = expand("<cword>")
    let pos = getcurpos()
    exe "vimgrep /\\<" . word . "\\>/ %"
    call setpos('.', pos)
    " go to beginning of word (but don't move when already at beginning of word)
    exe "norm! viwo\<esc>"
    " set search word
    let @/ = "\\<" . word . "\\>"
endfu

fu! DescribeFace()
    " Try treesitter highlighting
    if exists("*v:lua.ts_DescribeFace()") && v:lua.ts_DescribeFace() | return | endif
    let first = 1
    for id in synstack(line("."), col("."))
        if first == 1 | let first = 0 | else | echon " > " | endif
        let name = synIDattr(id, "name")
        let nameTrans = synIDattr(synIDtrans(id), "name")
        exe "echohl " . name | echon name . (name != nameTrans ? "(" . nameTrans . ")" : "") | echohl None
    endfor
endfu

" >>>

" Highlight (colors) <<<

fu! MyHighlight_UI()
    " Popup menu
    hi Pmenu        ctermfg=blue ctermbg=black cterm=none
    hi PmenuSel     ctermfg=blue ctermbg=black cterm=bold,reverse
    if g:env.dark
        hi Pmenu        ctermfg=blue ctermbg=238 cterm=bold
    endif

    " Folding
    hi Folded       ctermfg=magenta ctermbg=black cterm=bold,underline
    "  hi Folded       ctermfg=magenta ctermbg=none cterm=bold
    if g:env.dark
        hi Folded       ctermfg=magenta ctermbg=236 cterm=bold
    endif

    " Visual
    hi Visual       ctermfg=black ctermbg=blue

    " Diagnostics
    hi DiagnosticError ctermfg=red
    hi DiagnosticWarn  ctermfg=yellow
    hi DiagnosticInfo  ctermfg=blue
    hi DiagnosticHint  ctermfg=blue

    " Quickfix
    hi qfLineNr ctermfg=blue

    " Error
    hi Error     ctermfg=white ctermbg=red
    hi ErrorMsg  ctermfg=white ctermbg=red

    " Status and tabs
    hi StatusLine   ctermbg=white ctermfg=black cterm=bold
    hi StatusLineNC ctermbg=white ctermfg=black cterm=NONE
    if !g:env.dark
        hi StatusLine   ctermfg=white ctermbg=249 cterm=bold
        hi StatusLineNC ctermfg=white ctermbg=251 cterm=NONE
        hi TabLine      ctermfg=white ctermbg=251 cterm=NONE
        hi TabLineFill  ctermfg=white ctermbg=251 cterm=NONE
        hi TabLineSel   ctermfg=white ctermbg=249 cterm=NONE
    endif

    " Misc
    hi Directory   ctermfg=cyan " "cterm=" in :hi command output etc.
    hi MoreMsg     ctermfg=cyan " "Save and exit?" question etc.
    hi Question    ctermfg=cyan " "Press ENTER or ..." etc.
    hi SpecialKey  ctermfg=red  " <Enter> etc.
    hi Underlined  ctermfg=cyan
    hi LineNr      ctermfg=248
    hi NormalFloat ctermfg=white ctermbg=black " Floating window
    hi Search      ctermfg=black ctermbg=blue
    hi Whitespace  ctermfg=magenta
    hi SignColumn  ctermfg=white ctermbg=none
    " hi MsgArea    ctermfg=blue cterm=reverse
    if g:env.dark
        hi LineNr       ctermfg=243
    endif

    " llama.vim
    hi llama_hl_hint ctermfg=magenta
    hi llama_hl_info ctermfg=blue
endfu

fu! MyHighlight_TS()
    hi      Normal                ctermfg=white   cterm=none
    hi      Bold                  ctermfg=green   cterm=none
    hi      Comment               ctermfg=blue    cterm=none
    hi      String                ctermfg=yellow  cterm=none
    hi      Number                ctermfg=white   cterm=none
    hi      FuncDef               ctermfg=cyan    cterm=none
    hi link VarDef                FuncDef
    hi      Type                  ctermfg=magenta cterm=none
    hi      Flow                  ctermfg=red     cterm=none
    hi      Special               ctermfg=red     cterm=none
    hi      PreProc               ctermfg=green   cterm=none
    hi      Title                 ctermfg=red     cterm=none

    if g:env.dark
        hi FuncDef ctermfg=white   cterm=bold
    endif

    hi link @constructor          FuncDef
    hi link @functiondef          FuncDef
    hi link @variabledef          VarDef
    hi link @parameter            VarDef
    hi link @string               String
    hi link @keyword.return       Flow
    hi link @keyword.break        Flow
    hi link @number               Number
    hi link @boolean              Number

    hi link @operator             Special
    hi link @punctuation          @operator
    hi link @punctuation.special  Flow " braces inside template string etc

    hi link @conditional          Bold
    hi link @constant             Bold
    hi link @exception            Bold
    hi link @function.builtin     Bold
    hi link @keyword              Bold
    hi link @repeat               Bold
    hi link @type.vim             Bold

    hi link @constant.builtin     Normal
    hi link @constructor          Normal
    hi link @field                Normal
    hi link @function             Normal
    hi link @method               Normal
    hi link @property             Normal
    hi link @variable             Normal

    hi link @type.builtin         Type

    " javascirpt
    hi link @constructor.javascript VarDef
    hi link @functiondef.javascript VarDef

    " kotlin
    hi link @type.qualifier.kotlin Normal

    " python
    hi link @type.python          Normal
    hi link @type.builtin.python  Normal

    " sh
    hi link @function.builtin.bash    Special
    hi      @keyword.break.bash       ctermfg=red cterm=bold
    hi link @keyword.return.bash      @keyword.break.bash
    hi link @parameter.bash           Normal
    hi link @operator.bash            Normal
    hi link @punctuation.bracket.bash @operator.bash
    hi link @punctuation.special.bash @variable.bash
    hi      @variable.bash            ctermfg=cyan
    hi link @variabledef.bash         Normal

endfu

fu! MyHighlight_RX()
    "syn clear yamlBlockMappingKey
    "syn match yamlBlockMappingKey /\_s*\zs\K\+/
    "hi link yamlBlockMappingKey Normal
    hi link dockerfileKeyword Special

    " vim-fast-syntax
    hi fastBuiltin ctermfg=cyan
    hi fastKeyword ctermfg=green
    hi fastReturn  ctermfg=red

    " markdown
    hi      markdownCode          ctermfg=blue
    hi link markdownCodeBlock     markdownCode
    hi link markdownCodeDelimiter markdownCode
endfu

fu! MyHighlight2()
    set notermguicolors
    call MyHighlight_UI()
    call MyHighlight_TS()
    call MyHighlight_RX()
endfu

aug vimrc_hi " :hi need to be in autocmd on first run??
au!
" when there is a 'colorscheme vim' in init.vim (note: it is possible that (2)(3) runs in reverse order):
" (1) apply colorscheme vim (2) vimrc's :hi commands run (3) opens file (4) apply colorscheme which clears your :hi
" we must use ColorScheme autocmd to run :hi commands after (4).
" note :au must be before :colorscheme
au ColorScheme vim :call MyHighlight2()
aug END
colorscheme vim
nnore <f6> :call MyHighlight2()<cr>
" >>>

" Appearance <<<
set list
set listchars=tab:\ \ ,trail:.

" >>>

" Surround <<<
nnore ds :call Surr_change()<cr>
nnore cs :let c=nr2char(getchar())\|call Surr_change(c)<cr>
vnore S  <esc>:let c=nr2char(getchar())\|call Surr_vsurr(c)<cr>

fu! Surr_pairChar(c)
    if     a:c == ")" | return ["(" , ")" ]
    elseif a:c == "(" | return ["( ", " )"]
    elseif a:c == "}" | return ["{" , "}" ]
    elseif a:c == "{" | return ["{ ", " }"]
    elseif a:c == "]" | return ["[" , "]" ]
    elseif a:c == "[" | return ["[ ", " ]"]
    elseif a:c == ">" | return ["<" , ">" ]
    elseif a:c == "<" | return ["< ", " >"]
    else              | return [a:c , a:c ]
    endif
endfu
" Change paren type (or delete parens if c=0)
fu! Surr_change(c = 0)
    let l:isc = type(a:c) == v:t_string
    if l:isc | let [l:c1, l:c2] = Surr_pairChar(a:c) | endif
    norm! %
    let p1 = getcurpos()
    norm! %
    let p2 = getcurpos()
    if p1[1] > p2[1] || (p1[1] == p2[1] && p1[2] > p2[2])
        let [p1, p2] = [p2, p1]
    endif
    call setpos('.', p2)
    exe "norm! " . (isc ? ("s" . c2) : "x")
    call setpos('.', p1)
    exe "norm! " . (isc ? ("s" . c1) : "x")
endfu
" Surround selection in visual mode
fu! Surr_vsurr(c = 0)
    let l:isc = type(a:c) == v:t_string
    if l:isc | let [l:c1, l:c2] = Surr_pairChar(a:c) | endif
    let p1 = getpos("'<")
    let p2 = getpos("'>")
    if p1[1] > p2[1] || (p1[1] == p2[1] && p1[2] > p2[2])
        let [p1, p2] = [p2, p1]
    endif
    call setpos('.', p2)
    exe "norm! a" . c2 . "\<esc>"
    call setpos('.', p1)
    exe "norm! i" . c1 . "\<esc>"
endfu
" >>>

" Fzf <<<
fu! FzfExists()
    return executable("fzf")
endfu
let s:fzf_callback = 0
let s:fzf_list = 0
fu! Fzf(list, callback)
    if !FzfExists() | echo "fzf not installed" | return 0 | endif
    " Fzf in new window (or else original win will be lost)
    split
    " Convenience
    au TermOpen  * ++once startinsert
    au TermOpen  * ++once tnore <buffer> <esc> <c-c>
    " On fzf exit, pass stdout to FzfOnExit (need to wait a bit to ensure "[Process exited N]" is printed)
    au TermClose * ++once call timer_start(10, { -> FzfOnExit(getline(1, line("$"))) })
    " Need to use callback (or some other mechanism), as :exe "term ..." doesn't wait until exit
    let s:fzf_callback = a:callback
    " Save a:list
    " I initially captured the output of fzf as the selected item, but due to line-wrapping of :term, it's not accurate when a very long string in a:list is selected.
    " So I now feed string like "0 aaa\n1 bbb\n..." to fzf, and extract the index number from fzf's output.
    let s:fzf_list = a:list
    let fzf_stdin = map(a:list, 'v:key . " " . v:val')
    " Print input for fzf to a tmpfile, then pipe to fzf
    " (Can I pipe directly to :term ? (it's possible if :! instead of :term))
    let tmpfile = tempname()
    call writefile(fzf_stdin, tmpfile)
    " Can't use termopen() here, as it *replaces* current buf with :term buf (thus lose the current buf)
    exe "term /bin/sh -c 'cat " . tmpfile . " | fzf'"
endfu
fu! FzfOnExit(stdout) " stdout = list of strings
    " Delete fzf buffer
    bd!
    " Do nothing if user cancelled fzf
    if -1 == index(a:stdout, "[Process exited 0]") | return | endif
    " Call callback with selected string
    " (get first line of stdout, extract index number, and get s:fzf_list[index])
    let firstline = a:stdout[0]
    let index = matchstr(firstline, '^[0-9]\+')
    let item = matchstr(s:fzf_list[index], '[^0-9 ].*')
    call s:fzf_callback(item)
endfu
com! -bar       FzfBuffers call Fzf(map(filter(range(1, bufnr("$")), "buflisted(v:val)"), "bufname(v:val)"), { sel -> execute("edit " . sel . "") })
com! -bar -bang FzfFiles   call Fzf(expand("<bang>" == "" ? "*" : "**", 0, 1),                               { sel -> execute("edit " . sel . "") })
" >>>

" Repl/Eval <<<
" set p (tmux lsp -f "#{==:sbcl,#{pane_current_command}}" -F '#{pane_id}')
" tmux send -t $p -l '(list 1 2 3)'
" tmux send -t $p 'Enter'
let g:ReplData = {
    \ "javascript": { "newl": "\r\n", "newlMore": 1, "cmd": "node" },
    \ "lisp":       { "newl": "\r",   "newlMore": 0, "cmd": "sbcl --noinform" },
    \ "python":     { "newl": "\r\n", "newlMore": 1, "cmd": "python" },
    \ "scheme":     { "newl": "\r",   "newlMore": 0, "cmd": "gosh" },
    \ } " cl, lua, node
fu! ReplGetOpenTermBuf(cmd) " (cmd)
    " Get (or open) repl term buffer for filetype (and return bufnr)
    " Also ensure there is a window for the repl

    let termBufPattern = "term://.*" . a:cmd
    let termBufURL     = "term://"   . a:cmd

    " Get repl bufnr
    for b in range(1, bufnr("$"))
        if buflisted(b) && bufname(b) =~ termBufPattern
            let running = jobwait([getbufvar(b, "&channel")], 0)[0] == -1
            if running
                " If there's no window for repl, open one
                if bufwinid(b) == -1 | call luaeval('smartSp("' . b . '", true)') | endif
                " Return bufnr
                return b
            endif
        endif
    endfor
    " Or open repl
    return luaeval('smartSp("' . termBufURL . '")')

endfu
fu! ReplSend(str, ft=&ft)
    let [bufSave, winSave] = [bufnr(), winnr()]
    let data = g:ReplData[a:ft]
    let bufRepl = ReplGetOpenTermBuf(data.cmd)

    if bufRepl
        " Get job id of repl
        exec "b" bufRepl
        let chan = b:terminal_job_id

        " Remove empty lines, and ensure newline characters are added appropriately so the repl executes.
        " (newline characters and its counts may differ for each repl)
        let lines = filter(split(a:str, "[\r\n]", 1), 'len(v:val) > 0')
        let strWithNewline = join(lines, data.newl) . data.newl . ((data.newlMore && len(lines) >= 2) ? data.newl : "")
        call jobsend(chan, strWithNewline)

        " Scroll repl to bottom
        for win in win_findbuf(bufRepl) | call win_execute(win, "norm! G", 1) | endfor

        " Restore window focus etc.
        exec "norm! \<c-w>" . winSave . "w"
        exec "b" bufSave

    endif
endfu
fu! ReplSend_evalIfVim(str)
    let str = a:str
    if &ft == "vim" | exec str | else | call ReplSend(str) | endif
endfu
" command ReplOpen " open or switch to win
" command ReplSend " noarg = curline or visual, arg = send it
" FIXME too early to send text when launching repl
" FIXME do not move cursor
nnore <silent> <bar> :call          ReplSend_evalIfVim(getline("."))<cr>
nnore <silent> ,b    ggVG:<c-u>call ReplSend_evalIfVim(GetVisualSelection())<cr>
nnore <silent> ,e    :call          ReplSend_evalIfVim(luaeval("getDefunByIndent()"))<cr>
vnore <silent> <bar> :<c-u>call     ReplSend_evalIfVim(GetVisualSelection())<cr>

aug vimrc_repl
au!
au TermOpen * syn match myTermPrompt "^gosh\S*[$>]" " gauche
au TermOpen * syn match myTermPrompt "^>"           " node
au TermOpen * hi myTermPrompt ctermfg=black ctermbg=green cterm=bold
aug END


fu! EvalLuaBuffer() " eval lua file in neovim context
    let code = GetVisualSelection()
    call execute(["lua <<EOFLUA"] + getline(1, "$") + ["EOFLUA"])
endfu
nnore ,l :call EvalLuaBuffer()<cr>

" >>>

" Filetype specific templates <<<
" Usage:
"   :Template tmpl_html5
command! -nargs=1 -complete=var Template :call append(".", <args>)
command! -nargs=1 -complete=var Tmpl     :call append(".", <args>)
command! -nargs=1 -complete=var Temp     :call append(".", <args>)
command! -nargs=1 -complete=var Tm       :call append(".", <args>)
let tmpl_html5 = [
    \ '<!DOCTYPE html>',
    \ '<html lang="en">',
    \ '<head>',
    \ '    <meta charset="UTF-8">',
    \ '    <meta name="viewport" content="width=device-width, initial-scale=1.0">',
    \ '    <title>Document</title>',
    \ '</head>',
    \ '<body>',
    \ '    <h1>Hello, world</h1>',
    \ '</body>',
    \ '</html>',
    \ ]
" >>>

" Filetype specific configs <<<

" === Applies to multiple langs ===
set keywordprg=:BrowseDoc
aug vimrc_ft_multi
au!
au FileType c,cpp,*sh set keywordprg=:Man
au FileType python    set keywordprg=:BrowseDoc
au FileType qf        nnore <buffer> <cr> <cr>
aug END

" === Ansible ===
aug vimrc_ft_ansible
au!
au BufRead,BufNewFile */playbooks/*.yml,*/playbooks/*.yaml set ft=yaml.ansible
aug END

" === Awk ===
fu! MyAwkFixIndent()
    set inde=GetAwkIndent2()
endfu
fu! GetAwkIndent2() " Same as GetAwkIndent(), except for the indent after '} else {'
    let prev_lineno = Get_prev_line2(v:lnum)
    if prev_lineno == 0 | return 0 | endif
    let prev_data = getline(prev_lineno)
    let ind = indent( prev_lineno )
    if match(trim(prev_data), "^}.*{$") != -1 | return ind + shiftwidth() | end
    return GetAwkIndent()
endfu
fu! Get_prev_line2( lineno ) " Same as Get_prev_line, just not script local
    let lnum = a:lineno - 1
    let data = getline( lnum )
    while lnum > 0 && (data =~ '^\s*#' || data =~ '^\s*$')
        let lnum = lnum - 1
        let data = getline( lnum )
    endwhile
    return lnum
endfu
aug vimrc_ft_awk
au!
au BufNewFile,BufRead *.awk call MyAwkFixIndent()
au FileType             awk call MyAwkFixIndent()
aug END

" === C, C++ ===
aug vimrc_ft_c
au!
au BufNewFile,BufRead *.c,*.h,*.cpp,*.hpp setl cms=//%s
au FileType           c,cpp               setl cms=//%s
au BufNewFile,BufRead *.c,*.h,*.cpp,*.hpp setl cino+=(0
au FileType           c,cpp               setl cino+=(0
au BufNewFile,BufRead *.c,*.h,*.cpp,*.hpp inore <buffer> $$ ->
au FileType           c,cpp               inore <buffer> $$ ->
aug END

" === D ===
aug vimrc_ft_d
au!
au BufNewFile,BufRead *.d setl cms=//%s
au FileType             d setl cms=//%s
aug END

" === HTML ===
let g:html_indent_autotags="html,head,body,style" " no indent for these tags
let g:html_indent_script1="zero"
let g:html_indent_style1="zero"
aug vimrc_ft_html
au!
au BufNewFile,BufRead *.html setl tabstop=4 shiftwidth=4
au BufNewFile,BufRead *.html call HtmlIndent_CheckUserSettings()
au FileType             html setl tabstop=4 shiftwidth=4
au FileType             html call HtmlIndent_CheckUserSettings()
aug END

" === Lua ===
aug vimrc_ft_lua
au!
au BufNewFile,BufRead *.lua iabbr <buffer> ll local
au FileType lua             iabbr <buffer> ll local
au BufNewFile,BufRead *.lua iabbr <buffer> fn function
au FileType lua             iabbr <buffer> fn function
aug END

" === JavaScript ===
aug vimrc_ft_javascript
au!
au BufNewFile,BufRead *.js,*.ts   iabbr <buffer> cs const
au FileType javascript,typescript iabbr <buffer> cs const
au BufNewFile,BufRead *.js,*.ts   iabbr <buffer> ts this
au FileType javascript,typescript iabbr <buffer> ts this
au BufNewFile,BufRead *.js,*.ts   inore <buffer> /// /**<space><space>*/<left><left><left>
au FileType javascript,typescript inore <buffer> /// /**<space><space>*/<left><left><left>
au BufNewFile,BufRead *.js,*.ts   inore <buffer> $$ <space>=><space>
au FileType javascript,typescript inore <buffer> $$ <space>=><space>
au BufNewFile,BufRead *.js,*.ts   inore <buffer> clog console.log()<left>
au FileType javascript,typescript inore <buffer> clog console.log()<left>
au BufNewFile,BufRead *.js,*.ts   setl iskeyword+=#
au FileType javascript,typescript setl iskeyword+=#
aug END

" === Org mode ===
fu! OrgLevel()
    let l  = getline(v:lnum)
    let lp = getline(v:lnum - 1)
    let ln = getline(v:lnum + 1)
    " if two empty lines before heading, one blank line between folds.
    if ln =~ '^\* .*$' && l =~ '^\s*$' && lp =~ '^\s*$'
        return "0"
    elseif ln =~ '^\*\* .*$' && l =~ '^\s*$' && lp =~ '^\s*$'
        return "1"
    elseif ln =~ '^\*\*\* .*$' && l =~ '^\s*$' && lp =~ '^\s*$'
        return "2"
    endif
    " foldings start from headings
    if l =~ '^\* .*$'
        return ">1"
    elseif l =~ '^\*\* .*$'
        return ">2"
    elseif l =~ '^\*\*\* .*$'
        return ">3"
    endif
    " otherwise use previous line
    return "="
endfu
fu! MyOrgIndent()
    return -1
endfu
fu! MyOrgSyntaxHighlight() " TODO reload syntax with bufdo fail
    set ft=org cms=#\ %s
    setl fdm=expr fde=OrgLevel()
    setl inde=MyOrgIndent()
    sil! syn clear orgProperty orgComment orgHeading1 orgHeading2 orgHeading3 orgMathInline orgBold orgTex
    syn keyword orgKeyword   begin_src,end_src,macro,title,options,noexport,begin_quote,end_quote
    syn region orgProperty   matchgroup=Special start=/^#+\S*/ end=/$/ oneline
    syn region orgComment    start=/^\s*#[^+]/ end=/$/   oneline contains=orgTODO
    syn region orgHeading1   start=/^\* /      end=/$/   oneline
    syn region orgHeading2   start=/^\*\{2} /  end=/$/   oneline
    syn region orgHeading3   start=/^\*\{3} /  end=/$/   oneline
    syn region orgHeading4   start=/^\*\{4} /  end=/$/   oneline
    syn region orgMathInline start=/\$/        end=/\$/  oneline

    syn region orgBold       start=/\*[^* ]/   end=/\*/  oneline
    syn region orgItalic     start=/\/[^/ ]/   end=/\//  oneline
    syn region orgUnder      start=/_[^_ ]/    end=/_/   oneline
    syn region orgStrike     start=/+[^+ ]/    end=/+/   oneline
    syn region orgMonospace  start=/\~[^~ ]/   end=/\~/  oneline
    syn region orgVerbatim   start=/=[^= ]/    end=/=/   oneline

    syn region orgMacro      start=/<<</       end=/>>>/ oneline contains=orgMacroComma,orgMacroName
    syn match  orgTex        /\\\S\+/
    syn match  orgMacroComma /\\\@<!,/         contained
    syn match  orgMacroName  /\(<<<\)\@<=\w\+/ contained
    " >>> <- dummy
    syn keyword orgTODO TODO
    let b:current_syntax = "org"
    hi link orgProperty   String
    hi link orgComment    Comment
    hi      orgHeading1   ctermfg=cyan cterm=bold,underline
    hi      orgHeading2   ctermfg=cyan
    hi      orgHeading3   ctermfg=green
    hi      orgHeading4   ctermfg=magenta
    hi      orgMathInline ctermfg=blue

    hi      orgBold       ctermfg=red
    hi      orgItalic     ctermfg=blue cterm=italic
    hi      orgUnder      ctermfg=cyan cterm=underline
    hi      orgStrike     ctermfg=green
    hi      orgMonospace  ctermfg=yellow
    hi      orgVerbatim   ctermfg=magenta

    hi link orgTex        Preproc
    hi link orgMacro      Special
    hi      orgMacroComma ctermfg=green ctermbg=black cterm=reverse,bold
    hi link orgMacroName  Identifier
    hi link orgTODO       Todo
endfu
aug vimrc_ft_org
au!
au BufNewFile,BufRead *.org call MyOrgSyntaxHighlight()
au FileType             org call MyOrgSyntaxHighlight()
aug END

" === Python ===
fu! MyPythonSyntax()
    "syn keyword myPythonFlow assert break continue pass raise return yield
    "hi def link myPythonFlow Flow
endfu
aug vimrc_ft_python
au!
au BufNewFile,BufRead *.py   call MyPythonSyntax()
au FileType           python call MyPythonSyntax()
aug END

" === Scheme ===
aug vimrc_ft_scheme
au!
au BufNewFile,BufRead *.scm setl formatoptions+=rol
au FileType             scm setl formatoptions+=rol
aug END

" === Wat (webassembly) ===
aug vimrc_ft_wat
au!
au BufNewFile,BufRead *.wat setl ft=lisp
au FileType             wat setl ft=lisp
aug END

" === Zig ===
aug vimrc_ft_zig
au!
au BufNewFile,BufRead *.zig iabbr <buffer> cs const
au FileType zig             iabbr <buffer> cs const
aug END

" >>>

]] -- >>>

vim.cmd [[
fu! I()
    return stridx(getline(line(".")-1),getline(".")[col(".")-1])
endfu
fu! AlignCalc(prevline, curline, col, mode)
    let col     = a:col - (a:col == len(a:curline) ? 1 : 0)
    let comma   = a:curline[col - 1]
    let prevpos = stridx(a:prevline, comma)
    let align   = a:mode ? prevpos : match(a:prevline, '\v\_S', prevpos + 1)
    let before  = a:curline[:a:col - (a:mode ? 2 : 1)]
    let pad     = repeat(" ", align - a:col + (a:mode ? 1 : 0))
    let after   = a:curline[a:col - (a:mode ? 1 : 0):]
    let newcol  = len(before . pad) + 1
    ec a:prevline . ";"
    ec a:curline  . ";"
    ec a:col      . ";"
    ec "---"
    ec comma      . ";"
    ec prevpos    . ";"
    ec align      . ";"
    ec "---"
    ec before     . ";"
    ec pad        . ";"
    ec after      . ";"
    ec newcol     . ";"
    return [before . pad . after, newcol]
endfu
fu! F(mode)
    let [newline, newcol] = AlignCalc(getline(line(".") - 1), getline("."), col("."), a:mode)
    cal setline(".", newline)
    cal cursor(line("."), newcol)
endfu
"inore <c-a> <c-o>:call F(0)<cr>
"inore <c-s> <c-o>:call F(1)<cr>
" hello,    world
]]

-- Keys (keyboard layout specific) <<<
do

    local mykbd = (vim.env and vim.env.MYKBD == "colemakdh")

    local mappings = {
        "nv  j  n  gj",
        "nv  k  e  gk",
        "nv  gj gn j",
        "nv  gk ge k",
        -- "nvS J  N  :call GoUp('down')<cr>",
        -- "nvS K  E  :call GoUp('up')<cr>",
        "nvS J  N  <c-d>",
        "nvS K  E  <c-u>",
        "v   h  k  h",
        "v   l  i  l",
        "nvS gh gk :lua smartHome()<cr>",
        "nv  gl gi <end>",
        "nv  i  l  i",
        "nv  I  L  I",
        "nv  si sl s",

        "n   sh sk <c-w>H",
        "n   sj sn <c-w>J",
        "n   sk se <c-w>K",
        "n   sl si <c-w>L",

        "nv  n  j  n",
        "nv  N  J  N",

        "nvo e  h  e",
        "nvo E  H  E",

        "o   h  k  0",
        "o   l  i  $",
        "o   iw lw iw",
        "o   iW lW iW",

        "n   H  K  <c-o>", -- back jump list (like browser hist)
        "n   L  I  <c-i>", -- forward
    }

    for _, entry in ipairs(mappings) do
        local a0,a1,a2,a3,a4
        for i0,i1,i2,i3,i4 in entry:gmatch("([nvo]+)([S]*)%s+(%S+)%s+(%S+)%s+(.+)") do a0,a1,a2,a3,a4=i0,i1,i2,i3,i4 break end
        local sil = string.len(a1) > 0
        for mode in a0:gmatch("%S") do
            vim.cmd(mode .. "nore " .. (sil and "<silent> " or "") .. (mykbd and a3 or a2) .. " " .. a4)
        end
    end

    vim.cmd("nnore <silent> " .. (mykbd and "k" or "h") .. " :lua smarth()<cr>")
    vim.cmd("nnore <expr> "   .. (mykbd and "i" or "l") .. " (foldclosed('.') != -1) ? 'zo' : 'l'")

end -- >>>

-- LSP <<<

function lsp_config() -- <<<
    if can_require"lspconfig" then
        lsp_config_1_misc()
        lsp_config_2_eldoc()
        if can_require"mason" and can_require"mason-lspconfig" then
            -- mason is an auto lang server installer. this is optional
            lsp_config_3_mason()
        end
        lsp_config_4_servers()
    end
end -- >>>

function lsp_config_1_misc() -- Lsp (1) misc config <<<

    vim.cmd [[ let g:diaglength = 30 ]]
    function rightAlignFormatFunction(diagnostic)
        local line = diagnostic.lnum -- 0-based
        local space = string.rep(" ", vim.o.columns - vim.fn.getline(1+line):len() - 11 - vim.g.diaglength)
        return ""
    end

    vim.cmd [[
    " Toggle diagnostic
    let g:myLspDiag = 1
    fu! MyLspDiagToggle()
        if g:myLspDiag
            lua vim.diagnostic.hide()
        else
            lua vim.diagnostic.enable()
            lua vim.diagnostic.show()
        endif
        let g:myLspDiag = !g:myLspDiag
    endfu

    " Toggle diagnostic level
    let g:myLspDiagLevels = [ "HINT", "INFO" ]
    let g:myLspDiagLevel  = 0
    fu! MyLspDiagLevel()
        lua vim.diagnostic.enable()
        lua vim.diagnostic.show()
        let g:myLspDiagLevel = (g:myLspDiagLevel + 1) % len(g:myLspDiagLevels)
        let g:myLspDiagLevelName = g:myLspDiagLevels[g:myLspDiagLevel]
        " lua (function(x) vim.diagnostic.config({ underline=x, virtual_text=x }) end)({severity={min=vim.diagnostic.severity[vim.g.myLspDiagLevelName]}})
        lua (function(x) vim.diagnostic.config({ virtual_text=x }) end)({severity={min=vim.diagnostic.severity[vim.g.myLspDiagLevelName]},prefix="",format=rightAlignFormatFunction})
    endfu
    call MyLspDiagLevel()

    " Custom commands
    com! LspToggleDiag      call MyLspDiagToggle()
    com! LspToggleDiagLevel call MyLspDiagLevel()
    com! LspLocList         lua vim.diagnostic.setloclist()
    com! LspQuickFix        lua vim.diagnostic.setqflist()
    ]]

    -- Disable in insert mode
    vim.lsp.handlers["textDocument/publishDiagnostics"] = vim.lsp.with(
        vim.lsp.diagnostic.on_publish_diagnostics, {
            -- delay update diagnostics
            update_in_insert = false,
        }
    )

    function my_lsp_setup(lsname, cmd, arg)
        -- call language server setup function if server program is present
        if vim.fn.executable(cmd or lsname) == 1 then
            require'lspconfig'[lsname]["setup"](arg)
        end
    end

    function my_lsp_on_attach(client, bufnr)
        vim.cmd [[
        setl omnifunc=v:lua.vim.lsp.omnifunc
        setl signcolumn=number
        nnore <buffer> <enter> :lua vim.lsp.buf.hover()<cr>
        nnore <buffer> sld :LspToggleDiag<cr>
        nnore <buffer> slD :LspToggleDiagLevel<cr>
        nnore <buffer> sll :LspLocList<cr>
        nnore <buffer> slq :LspQuickFix<cr>
        nnore <buffer> slr :lua vim.lsp.buf.rename()<cr>
        ]]
        local cap = client.server_capabilities
        cap.semanticTokensProvider = false -- prevent my highlighting gettting overridden
    end

end -- >>>

function lsp_config_2_eldoc() -- Lsp (2) eldoc <<<

    vim.cmd [[
    hi def link LspSig    Normal
    hi def link LspSigCur Identifier
    aug vimrc_lspsig
    au!
    au CursorHold,CursorHoldI,InsertEnter * lua lspsigUpdate()
    aug END
    set updatetime=700
    set noshowmode
    ]]

    lspsig = "" -- can be used in statusline or winbar (e.g. set winbar=%{%luaeval('lspsig')%})
    lspsig_cnt = 0
    lspsig_last = nil

    function lspsigUpdate()
        function signature_handler(err, result, ctx, config)
            -- save call count and params for debug
            lspsig_cnt = lspsig_cnt + 1
            lspsig_last = {
                err=err,
                result=result,
                ctx=ctx,
                config=config
            }

            -- not at function call
            if err ~= nil or result == nil or result.signatures == nil or result.signatures[1] == nil then
                if lspsig ~= "" then
                    -- only erase message when lspsig is not empty yet
                    -- if already empty, we should not erase msg, otherwise other messages gets overridden
                    vim.cmd("echo ''")
                end
                lspsig = ""
                return
            end

            -- highlight current param and show
            -- note: in some langs (kotlin) result.activeSignature might be -1
            local actSig = math.max(0, result.activeSignature or 0)
            local sig    = result.signatures[1+actSig]
            local sigLbl = sig.label
            local params = sig.parameters
            local actPar = sig.activeParameter or result.activeParameter
            lspsig = "ERROR!"

            --  gopls sets actPar to nil when a func has optional params but you haven't added one yet; we assume you are inputting the first param
            if actPar == nil then actPar = 0 end

            if (not params) or #params == 0 or #params <= actPar then -- no params (functions with no args etc.), or active param index is more than param count. None that in this case, value of params differ among server implementations
                local chunks = { { " ", "None" }, { sigLbl, "LspSig" } }
                nvim_echo_no_hitenter(chunks, true, {})
                return
            end

            local toks = {} -- list of { type ("prefix"|"param"|"paramAct"|"delim"|"suffix"), text }
            local tok
            local function addTok(tok) table.insert(toks, tok) end

            -- analyze signatures and construct toks
            if type(params[1+actPar].label) == 'table' then -- param label is not string (e.g. pyright)

                addTok({ "prefix", sigLbl:sub(1, params[1].label[1]) })
                for i, par in ipairs(params) do
                    if i >= 2 then
                        addTok({ "delim", sigLbl:sub(params[i-1].label[2]+1, par.label[1]) })
                    end
                    addTok({ i == 1+actPar and "paramAct" or "param", sigLbl:sub(par.label[1]+1, par.label[2]) })
                end
                addTok({ "suffix", sigLbl:sub(params[#params].label[2]+1) })

            else -- param label is string (e.g. tsserver)

                local s = 1
                local sePrev
                for i = 1, #params do
                    local parLbl = params[i].label
                    local ss, se = sigLbl:find(parLbl, s, true) -- find parLbl in sigLbl
                    if not ss then error("param " .. parLbl .. " not found") end
                    if i == 1 then
                        addTok({ "prefix", sigLbl:sub(1, ss-1) })
                    else
                        addTok({ "delim", sigLbl:sub(sePrev+1, ss-1) })
                    end
                    addTok({ i == 1+actPar and "paramAct" or "param", parLbl })
                    if i == #params then
                        addTok({ "suffix", sigLbl:sub(se+1) })
                    end
                    s = ss
                    sePrev = se
                end

            end

            toks1 = toks
            -- if sigLbl is too long, omit type signatures (from last params until sigLbl is sufficiently short)
            local maxw = (vim.fn.winwidth(0) - 12) + 5
            local sigLblLen = sigLbl:len()
            for i = #toks, 1, -1 do
                if sigLblLen < maxw then break end
                local tok = toks[i]
                local type = tok[1]
                local text = tok[2]
                if type == "param" or type == "paramAct" then
                    local colonIdx = text:find(":")
                    if colonIdx then -- note: colonIdx may be nil if param does not have type signature
                        tok[2] = text:sub(1, colonIdx - 1)
                        sigLblLen = sigLblLen - (text:len() - colonIdx + 1)
                    end
                end
            end

            -- convert toks to chunks and echo
            local chunks = { { " ", "None" } }
            for i, tok in ipairs(toks) do
                local type = tok[1]
                if type == "prefix" or type == "delim" or type == "suffix" then
                    table.insert(chunks, { tok[2], "None" })
                elseif type == "param" then
                    table.insert(chunks, { tok[2], "LspSig" })
                elseif type == "paramAct" then
                    table.insert(chunks, { tok[2], "LspSigCur" })
                else
                    error("invalid type", type)
                end
            end

            nvim_echo_no_hitenter(chunks, true, {})

            -- exmple value of "result"
            -- {
            --     ctx = ...,
            --     result = {
            --         activeParameter = 0,
            --         activeSignature = 0,
            --         signatures = { {
            --             label = "f(a: any, b: any, c: any): any",
            --             parameters = { {
            --                 label = "a: any"
            --             }, {
            --                     label = "b: any"
            --                 }, {
            --                     label = "c: any"
            --                 } }
            --         } }
            --     }
            -- }
        end

        -- check signatureHelp provider exists
        local hasProvider = false
        for _, client in pairs(vim.lsp.get_clients({bufnr=0})) do
            if client.server_capabilities.signatureHelpProvider then
                hasProvider = true
                break
            end
        end

        -- send lsp request
        if not hasProvider then return end
        local util = require('vim.lsp.util')
        vim.lsp.buf_request(
            0,
            'textDocument/signatureHelp',
            util.make_position_params(0, "utf-8"),
            vim.lsp.with(signature_handler, {})
        )
    end

end -- >>>

function lsp_config_3_mason() -- Lsp (3) install servers with meson <<<
    local mason = require"mason"
    local mason_lspconfig = require"mason-lspconfig"
    mason.setup()
    mason_lspconfig.setup {
        -- this installs servers in ~/.local/share/nvim/mason/
        -- this does NOT config neovim for servers. please use lspconfig.xxx.setup()
        -- ensure_installed = { "bashls", "lua_ls", "pyright", "ts_ls" },
    }
    -- Note: LspInstall and ensure_installed may fail on invalid certificate
    --       workaround: add "check_certificate = off" in ~/.wgetrc
    -- Note: Some external tools must be installed manually e.g. shellcheck
    -- Note: To install pylsp plugins:
    --       :PylspInstall pyls-flake8 pylsp-mypy pyls-isort
end -- >>>

function lsp_config_4_servers() -- Lsp (4) configure servers <<<

    local lspconfig = require"lspconfig"
    local function f(lang, lsname, cmds, ...)
        -- setup server `lsname` where the argument is all tables in {...} merged to a single table
        -- abort if at least one command in `cmds` is not executable
        local arg = {}
        for _, cmd in pairs(cmds) do if vim.fn.executable(cmd) == 0 then return end end
        for _, tbl in ipairs({...}) do for k, v in pairs(tbl) do arg[k] = v end end
        lspconfig[lsname].setup(arg)
    end

    local a = { on_attach = my_lsp_on_attach }
    local s = { single_file_support = true }

    f("ansible", "ansiblels", { "ansible-language-server" },            a, s)
    f("c",       "clangd",    { "clangd" },                             a, s)
    f("css",     "cssls",     { "vscode-css-language-server" },         a, s)
    f("go",      "gopls",     { "gopls" },                              a)
    f("html",    "html",      { "vscode-html-language-server" },        a)
    f("js/ts",   "ts_ls",     { "typescript-language-server" },         a, s)
    f("lua",     "lua_ls",    { "lua-language-server" },                a)
    -- f("python", "basedpyright", a)
    f("python",  "pyright",   { "pyright" },                            a)
    f("python",  "pylsp",     { "pylsp" },                              a)
    f("shell",   "bashls",    { "bash-language-server", "shellcheck" }, a)
    f("zig",     "zls",       { "zls" },                                a)

    -- setup("pylsp",                  nil,                      { on_attach = on_attach, settings = { pylsp = { plugins = { pycodestyle = { ignore = {'W391'}, maxLineLength = 100 } } } } })
    -- setup("ruff_lsp",               "ruff",                   { on_attach = on_attach, init_options = { settings = { args = { "--config", 'lint.ignore = ["E401", "E731"]' } } } })

    -- For JS/TS: put libaries in node_modules/ to let LSP recognize it.

end -- >>>

lsp_config()

-- >>>

-- TreeSitter <<<

function ts_config() -- TreeSitter <<<
    ts_config_1()
    ts_config_2()
    ts_config_3()
end -- >>>

function ts_config_1() -- TreeSitter (1) setup <<<

    local nvim_treesitter_configs = require"nvim-treesitter.configs"

    nvim_treesitter_configs.setup {
        -- note: please update autocmd pattern for lazy loading
        ensure_installed = { "bash", "c", "css", "cpp", "go", "html", "javascript", "kotlin", "lua", "python", "vim", "vimdoc", },
        highlight = {
            enable  = true,
        },
        indent = {
            enable  = false,
            disable = { "python", "html", "javascript", }
        },
        rainbow = {
            enable = true,
            extended_mode = false, -- Also highlight non-bracket delimiters like html tags, boolean or table: lang -> boolean
            max_file_lines = 2000, -- Do not enable for files with more than n lines, int
            -- colors = {}, -- table of hex strings
            -- termcolors = {} -- table of colour name strings
        }
    }

end -- >>>

function ts_config_2() -- TreeSitter (2) utilities <<<

    function ts_DescribeFace()
        local parsers     = require "nvim-treesitter.parsers"
        local highlighter = require "vim.treesitter.highlighter"
        local treesitter  = require "vim.treesitter"
        local bufnr  = vim.api.nvim_get_current_buf()
        local cursor = vim.api.nvim_win_get_cursor(0)
        local line   = cursor[1] - 1  -- cursor[1] is 1-based
        local col    = cursor[2]       -- cursor[2] is 0-based

        -- show parsed tree
        local root_lang_tree = parsers.get_parser(bufnr)
        if not root_lang_tree then return false end
        local lang_tree = root_lang_tree:language_for_range { line, col, line, col }
        print("Lang: root=" .. root_lang_tree:lang() .. " here=" .. lang_tree:lang())
        local function rec(node, ind)
            for child, name in node:iter_children() do
                local child_row0, child_col0 = child:start() -- both 0-based
                local s_line = string.format("%4d ", child_row0 + 1)
                local s_sign = treesitter.is_in_node_range(child, line, col) and "*" or " "
                local s_node = tostring(child)
                local s_name = name and (":" .. name) or ""
                local s_text = string.format("%-6s", vim.fn.getline(child_row0+1):sub(child_col0+1, child_col0+6))
                local s_err  = child:has_error() and "ERROR" or ""

                local function synNameAt(row1, col1) -- 1-based
                    local hlslist1 = queryHlsList(row1, col1)
                    return vim.fn.synIDattr(vim.fn.synIDtrans(hlslist1[1][#hlslist1[1]]), "name")
                end

                local sep = { " ", "None" }
                local chunks = {
                    { s_line, "None" }, sep,
                    { s_text:sub(1, 1), synNameAt(child_row0+1, child_col0+1) },
                    { s_text:sub(2, 2), synNameAt(child_row0+1, child_col0+2) },
                    { s_text:sub(3, 3), synNameAt(child_row0+1, child_col0+3) },
                    { s_text:sub(4, 4), synNameAt(child_row0+1, child_col0+4) },
                    { s_text:sub(5, 5), synNameAt(child_row0+1, child_col0+5) },
                    { s_text:sub(6, 6), synNameAt(child_row0+1, child_col0+6) },
                    sep,
                    { ind   , "None" }, sep,
                    { s_sign, "None" }, sep,
                    { s_node, "None" }, sep,
                    { s_name, "None" }, sep,
                    { s_err , "None" }, sep,
                }

                vim.api.nvim_echo(chunks, true, {})
                -- print(s_line, s_text, ind, s_sign, s_node, s_name, s_text, s_err)

                if treesitter.is_in_node_range(child, line, col) then
                    rec(child, ind .. "  ")
                end
            end
        end
        for i, tree in ipairs(lang_tree:trees()) do
            print("Tree #" .. i .. " (" .. lang_tree:lang() .. ")")
            local node = tree:root()
            rec(node, "")
        end

        -- show syntax highlighting
        local hlslist = queryHlsList(line + 1, col + 1)
        for _, hls in ipairs(hlslist) do
            showSyn(hls)
        end
    end

    function ts_IsFirstCharInString(lnum)
        local hlslist = queryHlsList(lnum, 1)
        for _, hls in ipairs(hlslist) do
            for _, hl in ipairs(hls) do
                if vim.fn.synIDattr(vim.fn.synIDtrans(hl), "name") == "String" then
                    return true
                end
            end
        end
        return false
    end

    function ts_GetLangAtCursor()
        local parsers     = require "nvim-treesitter.parsers"
        local highlighter = require "vim.treesitter.highlighter"
        local bufnr  = vim.api.nvim_get_current_buf()
        local cursor = vim.api.nvim_win_get_cursor(0)
        local line   = cursor[1] - 1  -- cursor[1] is 1-based
        local col    = cursor[2]       -- cursor[2] is 0-based

        -- show parsed tree
        local root_lang_tree = parsers.get_parser(bufnr)
        local lang_tree = root_lang_tree:language_for_range { line, col, line, col }
        return lang_tree:lang()
    end

    function queryHlsList(lnum, col) -- lnum and col are 1-based
        local lnum0 = lnum - 1 -- 0-based
        local col0  = col  - 1 -- 0-based
        local highlighter = require "vim.treesitter.highlighter"
        local treesitter  = require "vim.treesitter"
        local hlslist = {}
        local buf_highlighter = highlighter.active[vim.api.nvim_get_current_buf()]
        buf_highlighter.tree:for_each_tree(function(tstree, langtree)
            local query = buf_highlighter:get_query(langtree:lang())
            local iter = query:query():iter_captures(tstree:root(), buf_highlighter.bufnr, lnum0, lnum0 + 1)
            local hls = {}
            for capture, node, metadata in iter do
                local hl = query.hl_cache[capture]
                if hl and treesitter.is_in_node_range(node, lnum0, col0) then
                    table.insert(hls, hl)
                end
            end
            table.insert(hlslist, hls)
        end)
        return hlslist
    end

    function showSyn(syns)
        local chunks = {}
        for i, id in ipairs(syns) do
            local idt   = vim.fn.synIDtrans(id)
            local name  = vim.fn.synIDattr(id,  "name")
            local trans = vim.fn.synIDattr(idt, "name")
            if i >= 2 then table.insert(chunks, { "> ", "None" }) end
            table.insert(chunks, { name .. " " .. id .. " " .. (id ~= idt and ("(" .. trans .. " " .. idt .. ") ") or ""), trans })
        end
        if #chunks >= 1 then vim.api.nvim_echo(chunks, true, {}) end
    end

    -- Utilities to define/modify treesitter queries

    -- based on https://github.com/nvim-treesitter/nvim-treesitter/issues/3058
    function ts_safe_read(filename, read_quantifier)
        local file, err = io.open(filename, 'r')
        if not file then
            error(err)
        end
        local content = file:read(read_quantifier)
        io.close(file)
        return content
    end

    function ts_read_query_files(filenames)
        local contents = {}
        for _, filename in ipairs(filenames) do
            table.insert(contents, ts_safe_read(filename, '*a'))
        end
        return table.concat(contents, '')
    end

    function ts_set_query(lang, query_name, text)
        vim.treesitter.query.set(lang, query_name, text)
    end

    function ts_add_query(lang, query_name, text) -- will RESET query
        local query = ts_read_query_files(vim.treesitter.query.get_files(lang, query_name))
        ts_set_query(lang, query_name, query .. "\n" .. text)
    end

end -- >>>

function ts_config_3() -- TreeSitter (3) custom queries <<<

    -- Note: this will fail on first install, because treesitter parsers will be installed later asynchronously and when this config is loaded there is no parser yet.

    -- Customize highlights.scm

    ts_add_query("bash", "highlights", [[
(variable_assignment (variable_name) @variabledef.bash)
;(function_definition (word) @functiondef)
((command_name (word) @keyword.break)
 (#any-of? @keyword.break "break" "continue"))
;((command_name (word) @keyword.return) ; this mark all commands as break/return (why?)
; (#any-of? @keyword.break "exit" "return"))
;(command_name (word) @functiondef)
    ]])

    ts_add_query("c", "highlights", [[
(pointer_declarator (identifier) @variabledef) ; "arr" in "int (*arr)[]"
[ "break" "continue" ] @keyword.break
[ "goto" ] @keyword.return
((call_expression (identifier) @keyword.return)
 (#eq? @keyword.return "exit"))
    ]])

    ts_add_query("javascript", "highlights", [[
(function_declaration (identifier) @functiondef)
(method_definition (property_identifier) @functiondef)
(variable_declarator (identifier) @variabledef)
(variable_declarator (object_pattern (shorthand_property_identifier_pattern) @variabledef))
(class_declaration (identifier) @functiondef)
(class_heritage (identifier) @typestrong)
(class_body (field_definition (private_property_identifier) @variabledef))
(class_body (field_definition (property_identifier) @variabledef))
[ "break" "continue" "throw" ] @keyword.break
    ]])

    ts_add_query("lua", "highlights", [[
(function_declaration (identifier) @functiondef)
(variable_declaration (assignment_statement (variable_list (identifier) @variabledef)))
(variable_declaration (variable_list (identifier) @variabledef))
(break_statement) @keyword.break
    ]])

    ts_add_query("python", "highlights", [[
(function_definition (identifier) @functiondef)
(class_definition (identifier) @functiondef)
[ "break" "continue" "pass" "raise" "assert" ] @keyword.break
;(function_definition (parameters (identifier) @variabledef))
;(function_definition (parameters (default_parameter . (identifier) @variabledef)))
    ]])


    -- Customize indents.scm

    -- Available captures:
    -- https://github.com/nvim-treesitter/nvim-treesitter/blob/master/CONTRIBUTING.md#indents
    -- @indent         ; indent children when matching this node
    -- @indent_end     ; marks the end of indented block
    -- @aligned_indent ; behaves like python aligned/hanging indent
    -- @dedent         ; dedent children when matching this node
    -- @branch         ; dedent itself when matching this node
    -- @ignore         ; do not indent in this node
    -- @auto           ; behaves like 'autoindent' buffer option
    -- @zero_indent    ; sets this node at position 0 (no indent)

    ts_add_query("html", "indents", [[
(element (start_tag (tag_name) @_no_indent_tag (#any-of? @_no_indent_tag "html" "head" "body" "script" "style"))) @dedent
    ]])

    ts_add_query("javascript", "indents", [[
; queries here overrides default ones

; ===== brace-less blocks =====

[
 (for_in_statement)
 (for_statement)
 (while_statement)
] @indent ; do indent on brace-less blocks


; ===== if block =====

; Tree structure of "if(x)...else if(y)...else if(z)...else..." is like:
; (if_statement (else_clause (if_statement (else_clause (if_statement (else_clause))))))

(if_statement (_)) @indent   ; (1) indent children of a if_statement
(else_clause "else" @branch) ; (2) if we're indented N levels; dedent "else" to N-1 but children are at N

(else_clause (if_statement) @dedent) ; (3) prevent children of else-if from double-indented by (1)
(else_clause (if_statement (statement_block) @dedent)) ; (4) somehow needed

(else_clause (statement_block) @dedent) ; (5) do the same as (3) for last else clause

; (6) Add following if you don't use default indents.scm:
; "}" @branch
; (statement_block) @indent


; ===== manual indent for comments and multiline string =====

[
 (comment)
 (template_string)
] @auto ; manual indent

    ]])

    ts_set_query("vim", "indents", [[
[
 (if_statement)
 (function_definition)
 (for_loop)
 (while_loop)
 (call_expression)
 (dictionnary)
 (list)
] @indent

[
 "endif"
 "endfunction"
 "endfor"
 "endwhile"
] @indent_end

[
 "endif"
 "endfunction"
 "endfor"
 "endwhile"
 (else_statement)
 (elseif_statement)
] @branch
    ]])

end -- >>>

-- >>>

-- Plugin config <<<

vim.cmd [[ " llama.vim (experimental) <<<
    if g:env.llama && !exists("g:llama_config")
        let g:llama_config = {
            \ 'endpoint': g:env.llama . "/infill",
            \ 'show_info': 0,
            \ 't_max_prompt_ms': 1000,
            \ 't_max_predict_ms': 1000,
            \ 'n_predict': 256,
            \ 'keymap_accept_word': "<C-S>",
            \ }
        packadd llama.vim
    endif
]] -- >>>

if can_require"nvim-autopairs" then -- nvim-autopairs <<<
    local AutoPairs = require"nvim-autopairs"
    local Conds = require"nvim-autopairs.conds"
    AutoPairs.setup()
    AutoPairs.get_rule("'")[1].not_filetypes = { "scheme", "lisp" }
    AutoPairs.get_rule("'")[1]:with_pair(Conds.not_after_text("["))
end -- >>>

if vim.fn.match(vim.o.rtp, "vim-easy-align") ~= -1 then -- vim-easy-align <<<
    vim.cmd [[
    xmap ga <Plug>(EasyAlign)
    xmap T  <Plug>(EasyAlign)
    ]]
end -- >>>

if vim.fn.match(vim.o.rtp, "vim-visual-multi") ~= -1 then -- vim-visual-multi <<<
    vim.cmd [[
    nnore <c-d> <Plug>(VM-Add-Cursor-Down)
    ]]
end -- >>>

-- >>>

-- My utils <<<

local smarth_last_time = 0
function myTime() return vim.fn.str2float(vim.fn.reltimestr(vim.fn.reltime())) end -- Get time in seconds (with milliseconds precision)
function smarth() -- Go left and optionally close a fold <<<
    local time = myTime()
    vim.cmd("norm! " .. ((time - smarth_last_time > .18 and vim.fn.getcurpos()[3] == 1 and vim.fn.foldlevel(".") ~= 0) and "zc" or "h"))
    smarth_last_time = time
end -- >>>

function smartSp(file, isBufNr) -- Split or VSplit and return new bufnr <<<
    if vim.fn.bufname() == "" then
        vim.cmd("e " .. file)
        return file
    end
    local w = vim.fn.winwidth(0)
    local h = vim.fn.winheight(0)
    vim.cmd((w/h < 3) and "sp" or "vsp")
    if file then
        if isBufNr then
            vim.cmd("b" .. tostring(file))
            return file
        else
            vim.cmd("e " .. file)
            return vim.fn.bufnr()
        end
    end
end -- >>>

local smartf_last_time, smartf_last_char, smartf_rev = 0, nil, nil
function smartf(direction) -- <<<
    local function regEscape(s) return (s:gsub("([^%w])", "%%%1")) end
    local function revfind(str, char, init)
        local n = str:reverse():find(char, #str + 1 - init)
        return n and (#str + 1 - n)
    end
    local time = myTime()
    local char = (time - smartf_last_time <= 1) and smartf_last_char or vim.fn.nr2char(vim.fn.getchar())
    local reg  = regEscape(char:lower())
    local line = vim.fn.getline("."):lower()
    local col  = vim.fn.col(".")
    -- 'F' followed by 'f' should be a backward jump
    local rev
    if time - smartf_last_time <= 1 then
        rev = (smartf_rev and (direction == 1)) or ((not smartf_rev) and (direction == -1))
    else
        rev = (direction == -1)
        smartf_rev = rev
    end
    -- Do not use ternary here (if direction==1 and line:find is falsy, then revfind gets called!)
    -- local col2 = (direction == 1) and line:find(reg, col+1) or revfind(line, reg, col-1)
    -- Need 6 lines!?
    local col2
    if not rev then
        col2 = line:find(reg, col+1)
    else
        col2 = revfind(line, reg, col-1)
    end
    if col2 then vim.fn.setpos(".", { 0, vim.fn.line("."), col2 }) end
    smartf_last_time = time
    smartf_last_char = char
end -- >>>

function smartq() -- Close temporary window or act as q key (recording) <<<

    local function shouldBufBeClosed(wintype, name, ft)
        -- return ft == "help" or ft == "qf"
        return wintype == "popup" or ft == "help" or ft == "qf"
    end

    -- Try closing temporary window
    for i = 1, vim.fn.winnr("$") do
        local wintype = vim.fn.win_gettype(i)
        local bufnr   = vim.fn.winbufnr(i)
        local bufname = vim.fn.bufname(bufnr)
        local ft      = vim.fn.getbufvar(bufnr, "&ft")
        if shouldBufBeClosed(wintype, bufname, ft) then
            print("Close", bufname, ft)
            vim.fn.win_execute(vim.fn.win_getid(i), "close", 1)
            return
        end
    end

    -- Call vim's q
    vim.api.nvim_feedkeys("q", "n", false)

end -- >>>

function toggleCmt(visual) -- <<<
    -- TODO dont put "# " etc on empty line when commenting
    local function hasSyntax(synName)
        for _, v in pairs(vim.fn.synstack(vim.fn.line("."), vim.fn.col("."))) do
            if vim.fn.synIDattr(v, "name") == synName then
                return true
            end
        end
        return false
    end
    local function tslang(lang) -- check current treesitter tree has given lang
        return ts_GetLangAtCursor and (ts_GetLangAtCursor() == lang)
    end
    local function getCMSHere()
        if vim.bo.ft == "vim" and (hasSyntax("vimLuaRegion") or tslang("lua")) then
            return "--%s"
        elseif vim.bo.ft == "lua" and tslang("vim") then
            return "\"%s"
        elseif vim.bo.ft == "html" and (hasSyntax("javaScript") or tslang("javascript")) then
            return "//%s"
        elseif vim.bo.ft == "html" and (hasSyntax("cssStyle") or tslang("css")) then
            return "/*%s*/"
        elseif vim.bo.ft == "c" or vim.bo.ft == "cpp" then
            return "//%s"
        else
            local cms = vim.bo.cms
            return (cms and cms:len() >= 1) and cms or "#%s" -- fallback to #%s when no cms
        end
    end
    local function regEscape(s) return (s:gsub("([^%w])", "%%%1")) end

    -- Analyze commentstring
    local cms    = getCMSHere()
    local x,y,z  = cms:match("(.*%S)(%s*)%%s(.*)")

    -- Helper functions
    local p1     = "^(%s*)" .. regEscape(x) .. "(.*)" .. regEscape(z)
    local function isCommented(line)
        if vim.bo.ft == "org" then return line:match(p1) and (not line:match("^#%+")) end
        return line:match(p1) and true or false
    end
    local function comment(line, indTo)
        -- Optionally indent to indTo'th column
        local indCur,text = line:match("^(%s*)(.*)") -- note that %s* is greedy
        indTo = indTo or indCur:len()
        local ind = string.rep(" ", indTo)
        local sp = string.rep(" ", indCur:len() - indTo) -- nonempty if current indent is larger than indTo; so need extra space after "#", "//" etc.
        local y1 = (y == "") and " " or y -- at least one space after "#", "//" etc.
        -- If line is "      echo", shiftwidth=2 and indTo=2 then ...
        -- ind="  ", x="#", y1=" ", sp="    ", text="echo", z=""
        local zWithSpace = z:len() >= 1 and (" " .. z) or "" -- add space between text and "*/", "-->" etc.
        return ind .. x .. y1 .. sp .. text .. zWithSpace
    end
    local function unComment(line)
        -- If line is "  #     echo" and shiftwidth=2 then ...
        -- ind="  ", xmatch="#", sp1=" ", sp2="    ", text="echo", zmatch=""
        -- We need to strip xmatch, sp1, zmatch
        local ind,xmatch,sp1,sp2,text,sp3,zmatch = line:match("^(%s*)(" .. regEscape(x) .. ")(%s?)(%s*)(.*)(" .. regEscape(z) .. ")")
        text = text:match("^(.*%S)") or text -- strip trailing whitespaces (probably before "*/", "-->" etc.)
        return ind .. sp2 .. text
    end

    -- Debug
    -- local l = vim.fn.getline(vim.fn.line("."))
    -- pp1({ isCommented(l), "[" .. (isCommented(l) and unComment(l) or comment(l)) .. "]" })

    -- Get range
    local lbeg   = visual and vim.fn.line("'<") or vim.fn.line(".")
    local lend   = visual and vim.fn.line("'>") or (lbeg + math.max(0, vim.v.count - 1))

    -- Compute willComment (if at least one line in range is currently uncommented), and leastIndent in range
    local willComment = false
    local leastIndent = 10000
    for i = lbeg, lend do
        local line = vim.fn.getline(i)
        local ind = line:match("^(%s*)"):len()
        leastIndent = math.min(ind, leastIndent)
        if not isCommented(line) then willComment = true break end
    end

    -- Do comment or unComment
    for i = lbeg, lend do
        local line = vim.fn.getline(i)
        vim.fn.setline(i, willComment and comment(line, leastIndent) or unComment(line))
    end

end -- >>>

function smartHome(insert) -- <<<
    local c = vim.fn.col(".") - (insert and 1 or 0)
    vim.cmd("norm! ^")
    if c == vim.fn.col(".") then vim.cmd("norm! 0") end
end -- >>>

function getDefunByIndent() -- Get current block in lisps, python etc. <<<
    -- Assuming indent is correct
    local l = vim.fn.line(".")
    local l1 = l
    local lE = vim.fn.line("$")
    while l1 >= 2 and vim.fn.getline(l1):match("^%s*$") or vim.fn.indent(l1) > 0 do l1 = l1 - 1 end
    local l2 = l + 1
    while l2 < lE and vim.fn.getline(l2):match("^%s*$") or vim.fn.indent(l2) > 0 do l2 = l2 + 1 end
    if l1 == l2 - 1 then
        return vim.fn.getline(l1)
    elseif l2 == lE and vim.fn.indent(l2) > 0 then
        return vim.fn.join(vim.fn.getline(l1, l2), "\n")
    else
        return vim.fn.join(vim.fn.getline(l1, l2 - 1), "\n")
    end
end -- >>>

function browseDoc(range, text) -- <<<
    if range >= 1 then -- <range>
        text = vim.fn.GetVisualSelection()
    elseif not (type(text) == "string" and text:len() > 0) then
        text = vim.fn.expand("<cword>")
    end
    local prefixes = {
        javascript       = "mdn js",
        css              = "mdn css",
        html             = "mdn html",
        ["yaml.ansible"] = "ansible doc",
    }
    local prefix = prefixes[vim.bo.ft] or (vim.bo.ft .. " doc")
    local query = prefix .. " " .. text
    -- TODO: w3m?
    -- local cmd   = "sil! !firefox 'https://lite.duckduckgo.com/lite/?q=" .. query .. "'"
    -- vim.cmd(cmd)
end
vim.cmd [[
command! -range -nargs=* -range BrowseDoc lua browseDoc(<range>, <q-args>)
]]
 -- >>>

function nvim_echo_no_hitenter(chunks, history, opts) -- <<<
    -- Similar to vim.api.nvim_echo but do not show hit-enter message
    local len = 0
    local maxw = vim.fn.winwidth(0) - 12
    local chunks2 = {}
    for i, v in ipairs(chunks) do
        table.insert(chunks2, v)
        if len + v[1]:len() > maxw then
            v[1] = v[1]:sub(1, maxw - len)
            break
        end
        len = len + v[1]:len()
    end
    vim.api.nvim_echo(chunks2, history, opts)
end -- >>>

function pp(input, doprint, maxdepth, ...) -- <<<
    local strs = {}
    for _, x in pairs({...}) do
        table.insert(strs, vim.inspect(x))
    end
    local allstr = table.concat(strs, "\n")
    if doprint then
        print(allstr)
        -- Create *Messages* buffer if not present
        local b = "*Messages*"
        if vim.fn.bufexists(b) == 0 then
            vim.fn.bufadd(b)
            vim.fn.bufload(b)
            vim.fn.setbufvar(b, "&swapfile", 0)
            vim.fn.setbufvar(b, "&buflisted", true)
            vim.fn.setbufvar(b, "&buftype", "nofile")
        end
        -- Print to *Messages* buffer like repl
        vim.fn.appendbufline(b, vim.fn.getbufinfo(b)[1].linecount, "> " .. input)
        vim.fn.appendbufline(b, vim.fn.getbufinfo(b)[1].linecount, strs) -- \n not allowed; use list
    end
    return allstr
end -- >>>

-- >>>

-- Completion setup <<<
vim.cmd [[
inore <expr> <tab>       pumvisible() ? "\<c-n>" : "\<c-x>\<c-u>"
inore <expr> <plug>MyTab pumvisible() ? "\<c-n>" : "\<c-x>\<c-u>"
inore <expr> <s-tab>     pumvisible() ? "\<c-p>" : "\<c-x>\<c-u>"
inore <expr> <c-f>       pumvisible() ? "\<c-n>" : "\<c-x>\<c-f>"

set shortmess+=c                           " No message like "Pattern not found"
set completeopt+=menuone,noinsert,noselect " Needed for auto completion
set completeopt-=longest                   " -=longest is needed otherwise first candidate appears and then disappears before menu is created, looks weird
set completeopt-=menu,preview
set noinfercase

" My completion
set completefunc=MyComp
fu! MyComp(findstart, base)
    return v:lua.mycomp(a:findstart, a:base)
endfu
aug vimrc_complete_mycomp
au!
au CompleteDone * lua mycomp_done()
aug END

let g:comp_minlen = 2  " At least N chars to start completion

aug vimrc_complete
au!

au FileType lua setl iskeyword+=.
au FileType sh  setl iskeyword+=.,-

" Auto complete (https://stackoverflow.com/questions/35837990)
let g:comp_open_prev = [-1, -1]
let g:comp_enable = v:true
fu! OpenCompletion()

    if !g:comp_enable | return | endif

    " prefetch lsp completion asynchronously
    " let minlen = g:comp_minlen - 1
    " if !pumvisible() && (('a' <= v:char && v:char <= 'z') || ('A' <= v:char && v:char <= 'Z') || (v:char == '_')) && (minlen == 1 || (col(".") >= (minlen-1) && matchstr(getline("."), '\%' . (col('.')-(minlen-1)) . 'c[a-zA-Z_]\{' . (minlen-1) . '\}') != ""))
    "     lua mycomp_lsp_omnifunc_prefetch()
    " endif

    " check (menu invisible && inserting alphabet)
    let minlen = g:comp_minlen
    let [line1, col1] = [line("."), col(".")]
    if !pumvisible() && (('a' <= v:char && v:char <= 'z') || ('A' <= v:char && v:char <= 'Z') || (v:char == '_'))
        " && (minlen == 1 || (col(".") >= (minlen-1) && matchstr(getline("."), '\%' . (col('.')-(minlen-1)) . 'c[a-zA-Z_]\{' . (minlen-1) . '\}') != ""))
        " Check number of keywords chars before cursor (at most minlen+1)
        let prefix = matchstr(getline("."), '[[:keyword:]' . ']\{,' . (minlen+1) . '\}\%' . col1 . 'c')
        " If completing at same pos and prefix length is already >= minlen+1 yet not pumvisible ==> abort
        " (avoid calling MyTab when there is no possibility of completion)
        if len(prefix) >= (minlen+1) && g:comp_open_prev == [line1, col1-1]
            :
        elseif len(prefix) >= 1
            call feedkeys("\<plug>MyTab", "")
            " Note: call feedkeys("\<c-x>\<c-u>", "n") will mess up repeating (.)
        endif
    endif
    let g:comp_open_prev = [line1, col1]

    " if !pumvisible() && (('a' <= v:char && v:char <= 'z') || ('A' <= v:char && v:char <= 'Z') || (v:char == '_')) && (g:comp_minlen == 1 || (col(".") >= (g:comp_minlen-1) && matchstr(getline("."), '\%' . (col('.')-(g:comp_minlen-1)) . 'c[a-zA-Z_]\{' . (g:comp_minlen-1) . '\}') != ""))
    "     call feedkeys("\<plug>MyTab", "")
    "     " call feedkeys("\<c-x>\<c-u>", "n") " this will mess up repeating (.)
    " endif

endfu
au InsertCharPre * call OpenCompletion()
au InsertEnter   * let g:comp_open_prev = [-1, -1]

aug END
]] -- >>>

-- My completefunc <<<

-- TODO: abstract memoization, should use prefix for performance?, async syntaxcomplete, too slow on first call
-- TODO: show source buffer, tags

local mycomp_cache = nil -- mycomp_cache[i] = comps cached when base:len() == i
timeit_time = 0
timeit_name = 0
function timeit(name)
    if nil then
        local now = vim.fn.reltimefloat(vim.fn.reltime())
        print(string.format("%-10s %f", timeit_name, now - timeit_time))
        timeit_time = now
        timeit_name = name
    end
end
function mycomp(findstart, base) -- <<<
    if findstart == 1 then
        if nil then return -2 end
        -- Initialize completion                          Example
        local line  = vim.fn.getline(vim.fn.line("."))    --  "abc def_ gh" (_ means cursor)
        local col   = vim.fn.col(".")                     --          ^ cursor position
        local sub   = line:sub(1, col - 1)                --  "abc def"
        local match = sub:match(mycomp_word_reg() .. "$") --      "def"
        return match and (col - #match - 1) or -2 -- , { sub=sub, col=col, match=match, len=len }
    else
        if nil then
            return {
                words = {
                    { menu="B", word="function", },
                    { menu="B", word="if", },
                    { menu="B", word="then", },
                    { menu="B", word="local", },
                    { menu="B", word="return", },
                    { menu="B", word="and", },
                    { menu="B", word="or", },
                    { menu="B", word="else", },
                    { menu="B", word="true", },
                    { menu="B", word="false", },
                    { menu="B", word="break", },
                    { menu="B", word="for", },
                    { menu="B", word="while", },
                    { menu="B", word="table", },
                    { menu="B", word="string", },
                    { menu="B", word="math", },
                },
                refresh = "always"
            }
        end
        -- Simple version
        -- return { words = mycomp_filter(base, mycomp_collect()), refresh = "always" }

        -- Cached version
        -- Initialize cache if not present (don't take base into account)
        timeit("collect")
        if not mycomp_cache then
            mycomp_cache = {}
            mycomp_cache[0] = mycomp_collect()
        end
        -- Get most detailed cache
        timeit("get cache")
        local cached
        for i = (base:len() - 1), 0, -1 do
            cached = mycomp_cache[i]
            if cached then break end
        end
        -- Filter cache further, and cache the new one
        timeit("filter")
        local comps = mycomp_filter(base, cached)
        timeit("end")
        mycomp_cache[base:len()] = comps
        return { words = comps, refresh = "always" } -- refresh="always" is required for fuzzy matching
    end
end -- >>>

function mycomp_done() -- Reset cache, add history etc. <<<
    mycomp_cache = nil
    local comp = vim.v.completed_item
    if comp then mycomp_add_history(comp) end
end -- >>>

-- function mycomp_memoizef(func, shouldUpdate) -- memoization <<<
--     local cache = {}
--     return function(...)
--         if shouldUpdate() then
--             cache = func()
--         end
--         return cache
--     end
-- end -- >>>

function mycomp_word_reg() -- Make regexp that match chars in 'iskeyword' <<<
    -- See https://vi.stackexchange.com/questions/31478
    -- return something like "[abc]+"

    -- Note: We limit to "usual" ascii chars (0x21 '!' to 0x7e '~')
    --       to prevent garbage code like <80> in completion.
    --       (happens when buffer contains multibyte characters)
    --       Multibyte strings are not welcome in VimL/Lua5.1

    return "[" .. vim.fn.substitute(vim.fn.join(vim.fn.map(vim.fn.range(0x21, 0x7e), 'nr2char(v:val)'), ''), '[^[:keyword:]]', '', 'g') .. "]+"
end -- >>>

function mycomp_compword(comp) -- (1) { word=w } => w, (2) 'str' => 'str' <<<
    local wd = (type(comp) == "table" and comp.word) and comp.word or ((type(comp) == "string") and comp or nil)
    if wd and wd:sub(1, 1) == "." then -- temporary fix: prevent js props completed with beginning dot
      return wd:sub(2)
    else
      return wd
    end
    -- return (type(comp) == "table" and comp.word) and comp.word or ((type(comp) == "string") and comp or nil)
end -- >>>

function mycomp_filter(base, list) -- <<<
    -- TODO: Currently converting BASE and each item in LIST to lower case. Better solutions?
    base = base:lower()
    -- vim.cmd("sleep " .. math.min(1000, math.floor(1 + #list)) .. "m")
    -- e.g. max_common_length("abcd", "bcx") = 2
    -- TODO: ("abcxyz", "abxy") should have higher score than ("abcxyz", "ab")
    local maxscore = 5 + base:len()
    local function max_common_length(str, pat) -- assumes the answer >= 1 (for performance)
        -- length of longest *prefix* of pat, that is also a substring of str
        local n = math.min(str:len(), pat:len())
        local max = 1
        while max < n and str:find(pat:sub(1, max + 1)) do
            max = max + 1
        end
        return max
    end
    local function score(str, pat)
        local max = max_common_length(str, pat)
        local prefix = (str:find(pat:sub(1, max)) == 1)
        return max + (prefix and 1 or 0) -- Add 1 if max common is prefix
    end
    -- Setup regexp and more
    -- local fuzreg = "^.*" .. base:gsub(".", "%1.*"):gsub("%.%*$", ""):gsub("%%", "%%%%")
    local fuzreg = base:sub(2):gsub(".", "%1.*"):gsub("%.%*$", ""):gsub("%%", "%%%%")
    local base1 = base:sub(1, 1):lower()
    -- Setup score table
    local t = {} -- t[i] = list of comps with score i
    for i = 1, maxscore do
        t[i] = {}
    end
    -- Calc score for each fuzzy match
    for _, comp in ipairs(list) do
        -- local w = mycomp_compword(comp)
        -- if w:lower():match(fuzreg) then
        --     table.insert(t[score(w:lower(), base)], comp)
        -- end
        -- Search in two steps for performance
        local w = mycomp_compword(comp):lower()
        local pos = w:find(base1, 1, true) -- search first char of base in w without using regex
        if pos and w:find(fuzreg, pos+1) then -- then search the rest of base in w[pos+1:] with regex
            table.insert(t[score(w, base)], comp)
        end
    end
    -- Result
    local res = {}
    for i = maxscore, 2, -1 do
        for _, v in ipairs(t[i]) do
            table.insert(res, v)
        end
    end
    return res
end -- >>>

function mycomp_collect() -- Collect words <<<
    -- vim.cmd("sleep 1")
    local function copyTable(t)
        local t2 = {}
        for k, v in pairs(t) do t2[k] = v end
        return t2
    end

    local ABBR_THRESHOLD = 40

    timeit("collect h")
    local c1 = { "h", mycomp_collect_history() }
    timeit("collect o")
    local c2 = { "o", mycomp_collect_omni() }
    timeit("collect b")
    local c3 = { "b", mycomp_collect_bufferall() }
    timeit("collect k")
    local c4 = { "k", mycomp_collect_keywords() }
    timeit("collect t")
    local c5 = { "t", mycomp_collect_tmux() }
    timeit("collect -")
    local comps_list = { -- defines order of words
        c1, c2, c3, c4, c5,
        -- { "h", mycomp_collect_history() },
        -- { "o", mycomp_collect_omni() },
        -- { "b", mycomp_collect_bufferall() },
        -- { "k", mycomp_collect_keywords() },
        -- { "t", mycomp_collect_tmux() },
    }
    local priority = { o=1, k=2, b=3, h=4, t=5 } -- which source has priority for extra info of word

    local result, items = {}, {}

    for _, v in ipairs(comps_list) do
        local source, comps = v[1], v[2]
        for _, comp in pairs(comps) do
            local w = mycomp_compword(comp)

            if not items[w] then

                -- Add new word
                local item   = (type(comp) == "table") and copyTable(comp) or {}
                item.word    = w
                item.menu    = source .. " " .. (comp.menu or "")
                item._source = source
                table.insert(result, item)
                items[w] = item

                -- Add abbreviation for too long word
                if w:len() > ABBR_THRESHOLD then
                    item.abbr = w:sub(1, ABBR_THRESHOLD) .. ".."
                end

            elseif priority[items[w]._source] > priority[source] then

                -- Override existing word's extra info by that of another source
                local item = items[w]
                if type(comp) == "table" then
                    for k, v in pairs(comp) do item[k] = v end
                end
                item.word    = w -- needed because mycomp_compword may modify word???
                item.menu    = source .. " " .. (comp.menu or "")
                item._source = source

            end
        end
    end
    timeit("collect --")

    return result
end -- >>>

local mycomp_history, mycomp_history_max = {}, 300
function mycomp_collect_history() -- Collect from completion history <<<
    return mycomp_history
end
function mycomp_add_history(comp)
    if mycomp_compword(comp) then
        table.insert(mycomp_history, 1, mycomp_compword(comp))
    end
    if #mycomp_history > mycomp_history_max * 2 then
        while #mycomp_history > mycomp_history_max do
            table.remove(mycomp_history, #mycomp_history)
        end
    end
end -- >>>

function mycomp_collect_bufferall() -- Collect from all bufs <<<
    local comps = {}
    -- buffers.filter(buflisted).map(collect).join()
    for i = 1, vim.fn.bufnr("$") do
        if vim.fn.buflisted(i) == 1 then
            for _, comp in pairs(mycomp_collect_buffer(i)) do
                table.insert(comps, comp)
            end
        end
    end
    return comps
end -- >>>

local mycomp_collect_buffer_cache = {} -- each item is: { res, lines, lastused }
function mycomp_collect_buffer(buf) -- Collect from a buf <<<
    -- TODO: better cache: rescan relevant parts of file (already good? seems lastused only change on comp start)
    -- Helpers
    local function union(itvls) -- union of integer intervals
        -- note: destructive
        -- note: [a,b] and [b+1,c] will be merged
        local res = {}
        table.sort(itvls, function (it1, it2) return it1[1] < it2[1] end)
        local n = #itvls
        local it1 = itvls[1]
        for i = 2, n do
            local it2 = itvls[i]
            if it1[2] + 2 <= it2[1] then
                table.insert(res, it1)
                it1 = it2
            else
                it1 = { it1[1], math.max(it1[2], it2[2]) }
            end
        end
        table.insert(res, it1)
        return res
    end
    local function findall(str, regex) -- regex matches to list
        local matches = {}
        for s in str:gmatch(regex) do
            table.insert(matches, s)
        end
        return matches
    end
    local function lnums_to_scan(buf, C) -- Which lines to scan?
        local GRADSCAN_LINES = 20
        local CHANGE_CONTEXT = 5
        local lnums = {}
        local linecount = vim.fn.getbufinfo(buf)[1].linecount
        -- Lines for initial scan
        if #C.res == 0 then return { { "1", "$" } } end
        -- Lines from gradual scan (has one line overlap)
        table.insert(lnums, {
            math.max(1, C.gradscan),
            math.min(linecount, C.gradscan + GRADSCAN_LINES)
        })
        C.gradscan = C.gradscan + GRADSCAN_LINES
        -- Lines from changelist
        local chlist = vim.fn.getchangelist(buf)[1]
        local chlistlen = #chlist
        for i = math.max(1, chlistlen - 3), chlistlen do
            local lnum = chlist[i].lnum
            table.insert(lnums, {
                math.max(1, lnum - CHANGE_CONTEXT),
                math.min(linecount, lnum + CHANGE_CONTEXT)
            })
        end -- todo remove duplicates
        return union(lnums)
    end
    -- Get buffer name string
    buf = (type(buf) == "string") and buf or vim.fn.bufname(buf or "%")
    -- If cache exists and up-to-date (lastused did not change), return it.
    local C = mycomp_collect_buffer_cache[buf]
    local lastused = vim.fn.getbufinfo(buf)[1].lastused
    if C and C.lastused == lastused then
        return C.res
    end
    -- Initialize cache if not yet
    if not C then
        C = { res = {}, gradscan = 0, linewords = {}, lastused = lastused }
    end
    -- Extract words from buf
    local word_reg = mycomp_word_reg()
    for _, lnums in pairs(lnums_to_scan(buf, C)) do
        for i, line in pairs(vim.fn.getbufline(buf, lnums[1], lnums[2])) do
            C.linewords[lnums[1] + i - 1] = findall(line, word_reg)
        end
    end
    -- Merge results
    local seen = {} -- to prevent dupes
    C.res = {}
    for _, words in pairs(C.linewords) do
        for _, s in pairs(words) do
            if (not seen[s]) then
                seen[s] = true
                table.insert(C.res, { word=s, menu=buf })
            end
        end
    end
    -- vim.cmd("sleep 1") -- for cache test
    mycomp_collect_buffer_cache[buf] = C
    return C.res
end -- >>>

local mycomp_collect_tmux_cache = { res = {}, time = 0 }
function mycomp_collect_tmux() -- Collect from tmux session <<<
    if not os.getenv("TMUX") then
        return {}
    end
    local time = myTime()
    if math.abs(time - mycomp_collect_tmux_cache.time) > 10 then
        mycomp_collect_tmux_schedule_update(time)
    end
    return mycomp_collect_tmux_cache.res
end
function mycomp_collect_tmux_schedule_update(time)
    -- Collect words from tmux panes and store them in mycomp_collect_tmux_cache
    -- To avoid blocking user input, we use another thread via neovim's libuv.
    -- Alternatively, we might use another process with job-control.
    local function f(word_reg, time)
        -- Get all outputs of all panes in the session (-s flag)
        local cmd = [[
            sh -c "for p in \$(tmux list-panes -s -F '#{pane_id}'); do tmux capture-pane -p -J -t \$p; done"
        ]]
        local handle = io.popen(cmd)
        local out = handle:read("*a")
        handle:close()
        return out, word_reg, time -- cannot return tables here
        -- Synchronous version
        -- local cmd = "for p in $(tmux list-panes -s -F '#{pane_id}'); do tmux capture-pane -p -J -t $p; done"
        -- return out = vim.fn.system({ "sh", "-c", cmd })
    end
    local function g(out, word_reg, time)
        -- Extract all words by regex
        local res, seen = {}, {} -- use 2 tables to prevent dupes
        for s in out:gmatch(word_reg) do
            if (not seen[s]) then
                seen[s] = true
                table.insert(res, s)
            end
        end
        -- Update cache
        mycomp_collect_tmux_cache.time = time
        mycomp_collect_tmux_cache.res  = res
    end
    -- Get libuv and run function in background
    local luv = vim.loop
    local work = luv.new_work(f, g)
    -- Note:
    --   Callbacks for new_work can neither call vimscript functions nor capture values using closure.
    --   We must pass necessary values as function args.
    --   Also note that thread args cannot be tables.
    local word_reg = mycomp_word_reg()
    work:queue(word_reg, time) -- this calls f(word_reg, time) in another thread and when finished, call g with f's return values
end -- >>>

function mycomp_collect_omni() -- Collect from omnifunc <<<
    if (not vim.bo.omnifunc) or (vim.bo.omnifunc == "") then
        return {}
    end
    local function getfunc_under_v_lua(fname)
        -- Get function object from string
        local obj = _G
        for x in fname:gmatch("[^.]+") do
            -- ("a.b"):gmatch("[^.]*") yields 2 strings "a","b" in 5.3 but yields 4 strings "a","","b","" in 5.2. Need to use "+" instead of "*".
            obj = obj[x]
            if not obj then
               return error("Function " .. fname .. " does not exist")
            end
        end
        return obj
    end
    local function callOmnifunc(findstart, base)
        local enableLSP = false
        local ofu = vim.bo.omnifunc
        if ofu == "v:lua.vim.lsp.omnifunc" then
            -- how to programatically call ofu that starts with "v:lua" ?
            if enableLSP then
                -- return vim.lsp.omnifunc(findstart, base)
                return mycomp_lsp_omnifunc_sync(findstart, base)
            else
                return mycomp_lsp_dummy(findstart, base)
            end
        elseif ofu:match("^v:lua%.") then
            -- vim.call does not work with a function name starting with v:lua, so convert the function name to a lua object
            return getfunc_under_v_lua(ofu:sub(7))(findstart, base)
        else
            return vim.call(ofu, findstart, base)
        end
    end
    -- Emulate first call of omnifunc (a:findstart = 1)
    local col = callOmnifunc(1, nil)
    if col >= 0 then
        -- Emulate second call of omnifunc (a:findstart = 0, a:base = the word being typed)
        local ofu_base = vim.fn.getline("."):sub(col + 1)
        local omnicomps = callOmnifunc(0, ofu_base)
        mycomp_collect_omni_cache = omnicomps
        lastcomps = omnicomps
        return omnicomps
    else
        -- No omni completion at cursor
        mycomp_collect_omni_cache = {}
        return {}
    end
end -- >>>

mycomp_lsp_omnifunc_cache = nil
mycomp_lsp_omnifunc_prefetched = nil
function mycomp_lsp_omnifunc_sync(findstart, base) -- synchronous lsp omnifunc (https://github.com/neovim/neovim/issues/12390) <<<
    local pos = vim.api.nvim_win_get_cursor(0)
    local line = vim.api.nvim_get_current_line()

    if findstart == 1 then
        -- Cache state of cursor line and position due to the fact that it will
        -- change at the second call to this function (with `findstart = 0`). See:
        -- https://github.com/vim/vim/issues/8510.
        -- This is needed because request to LSP server is made on second call.
        -- If not done, server's completion mechanics will operate on different
        -- document and position.
        mycomp_lsp_omnifunc_cache = {pos = pos, line = line}

        -- On first call return column of completion start
        local line_to_cursor = line:sub(1, pos[2])
        return vim.fn.match(line_to_cursor, '\\k*$')
    end

    -- Restore cursor line and position to the state of first call
    -- vim.api.nvim_set_current_line(mycomp_lsp_omnifunc_cache.line) -- temporarily disabled (neovim 0.8, now textlock is set, E565)
    vim.api.nvim_win_set_cursor(0, mycomp_lsp_omnifunc_cache.pos)

    -- Make request
    local bufnr = vim.api.nvim_get_current_buf()
    local params = vim.lsp.util.make_position_params(0, "utf-8")
    -- local result = vim.lsp.buf_request_sync(bufnr, 'textDocument/completion', params, 2000)
    local result = mycomp_lsp_omnifunc_prefetched or vim.lsp.buf_request_sync(bufnr, 'textDocument/completion', params, 2000)
    mycomp_lsp_omnifunc_prefetched = nil
    if not result then return {} end

    -- Transform request answer to list of completion matches
    local items = {}
    for _, item in pairs(result) do
        if not item.err then
            local matches = vim.lsp.util.text_document_completion_list_to_complete_items(item.result, base)
            vim.list_extend(items, matches)
        end
    end

    -- Restore back cursor line and position to the state of this call's start
    -- (avoids outcomes of Vim's internal line postprocessing)
    -- vim.api.nvim_set_current_line(line) -- temporarily disabled (neovim 0.8, now textlock is set, E565)
    vim.api.nvim_win_set_cursor(0, pos)

    return items
end
function mycomp_lsp_omnifunc_prefetch()
    -- Suppose autocompletion starts at 3rd character.
    -- Then this function fetches lsp completion result when 2nd char is typed,
    -- and (hopefully) result is ready when 3rd char is typed.
    -- Prefetched result is stored in mycomp_lsp_omnifunc_prefetched
    local bufnr = vim.api.nvim_get_current_buf()
    local params = vim.lsp.util.make_position_params(0, "utf-8")
    function callback(result) mycomp_lsp_omnifunc_prefetched = result end
    vim.lsp.buf_request_all(bufnr, 'textDocument/completion', params, callback)
end
-- >>>

function mycomp_lsp_dummy(findstart, base) -- <<<
    if findstart == 1 then
        return -1
    else
        return {}
    end
end -- >>>

local mycomp_collect_keywords_cache = {}
local mycomp_collect_keywords_extra = { -- extra keywords for mycomp_collect_keywords <<<
    python = {
        -- Generated by:
        --   1. Visit this URL:
        --        https://docs.python.org/3/genindex-_.html
        --   2. Run this JS in the devtool
        --        a={};[...document.body.innerText.matchAll(/__.*?__/g)].forEach(x=>a[x[0]]=true);b=Object.getOwnPropertyNames(a);b.sort();b;
        --   3. Pipe result to this command:
        --        awk 'BEGIN{c="-"}{cc=substr($0,6,1);if(c!=cc){c=cc;print""}printf"%s",$0}'
        "__abs__",  "__add__",  "__aenter__",  "__aexit__",  "__aiter__",  "__all__",  "__and__",  "__anext__",  "__annotations__",  "__args__",  "__await__",
        "__bases__",  "__bool__",  "__bound__",  "__breakpointhook__",  "__bytes__",
        "__cached__",  "__call__",  "__callback__",  "__cause__",  "__ceil__",  "__class__",  "__class_getitem__",  "__classcell__",  "__closure__",  "__code__",  "__complex__",  "__concat__",  "__constraints__",  "__contains__",  "__context__",  "__contravariant__",  "__copy__",  "__covariant__",
        "__debug__",  "__deepcopy__",  "__defaults__",  "__del__",  "__delattr__",  "__delete__",  "__delitem__",  "__dict__",  "__dir__",  "__displayhook__",  "__divmod__",  "__doc__",
        "__enter__",  "__eq__",  "__excepthook__",  "__exit__",
        "__file__",  "__float__",  "__floor__",  "__floordiv__",  "__format__",  "__fspath__",  "__func__",  "__future__",
        "__ge__",  "__get__",  "__getattr__",  "__getattribute__",  "__getitem__",  "__getnewargs__",  "__getnewargs_ex__",  "__getstate__",  "__globals__",  "__gt__",
        "__hash__",
        "__iadd__",  "__iand__",  "__iconcat__",  "__ifloordiv__",  "__ilshift__",  "__imatmul__",  "__imod__",  "__import__",  "__imul__",  "__index__",  "__init__",  "__init_subclass__",  "__instancecheck__",  "__int__",  "__interactivehook__",  "__inv__",  "__invert__",  "__ior__",  "__ipow__",  "__irshift__",  "__isub__",  "__iter__",  "__itruediv__",  "__ixor__",
        "__kwdefaults__",
        "__le__",  "__len__",  "__length_hint__",  "__loader__",  "__lshift__",  "__lt__",
        "__main__",  "__matmul__",  "__missing__",  "__mod__",  "__module__",  "__mro__",  "__mro_entries__",  "__mul__",
        "__name__",  "__ne__",  "__neg__",  "__new__",  "__next__",  "__not__",  "__notes__",
        "__optional_keys__",  "__or__",  "__origin__",
        "__package__",  "__parameters__",  "__path__",  "__pos__",  "__pow__",  "__prepare__",
        "__PYVENV_LAUNCHER__",
        "__qualname__",
        "__radd__",  "__rand__",  "__rdivmod__",  "__reduce__",  "__reduce_ex__",  "__repr__",  "__required_keys__",  "__reversed__",  "__rfloordiv__",  "__rlshift__",  "__rmatmul__",  "__rmod__",  "__rmul__",  "__ror__",  "__round__",  "__rpow__",  "__rrshift__",  "__rshift__",  "__rsub__",  "__rtruediv__",  "__rxor__",
        "__self__",  "__set__",  "__set_name__",  "__setattr__",  "__setitem__",  "__setstate__",  "__slots__",  "__spec__",  "__stderr__",  "__stdin__",  "__stdout__",  "__str__",  "__sub__",  "__subclasscheck__",  "__subclasses__",  "__subclasshook__",  "__supertype__",  "__suppress_context__",
        "__total__",  "__traceback__",  "__truediv__",  "__trunc__",
        "__unpacked__",  "__unraisablehook__",
        "__version__",
        "__xor__"
    },
    javascript = {
        "console.log", "console.error",
        "clearTimeout", "clearInterval", "setTimeout", "setInterval",
        "querySelector", "querySelectorAll",
        "getContext",
        "addEventListener", "createElement",
        "constructor",
        "process.argv",
        "import", "require",
        -- Array
        "map", "forEach", "filter", "reduce", "reduceRight", "every", "some", "indexOf", "lastIndexOf", "slice",
    },
    ps1 = {
        "cd", "cd\\",
        -- The following list is generated by:
        --   powershell.exe Get-Command | awk '(!/cd/) && ($2 != "Name") && ($2 != "----") {print $2}' | sort | xargs -I{} printf '"%s", ' '{}'
        "A:", "Add-AppPackage", "Add-AppPackageVolume", "Add-AppProvisionedPackage", "Add-AppvClientConnectionGroup", "Add-AppvClientPackage", "Add-AppvPublishingServer", "Add-AppxPackage", "Add-AppxProvisionedPackage", "Add-AppxVolume", "Add-BCDataCacheExtension", "Add-BitLockerKeyProtector", "Add-BitsFile", "Add-CertificateEnrollmentPolicyServer", "Add-Computer", "Add-Content", "Add-DnsClientNrptRule", "Add-DtcClusterTMMapping", "Add-EtwTraceProvider", "Add-History", "Add-InitiatorIdToMaskingSet", "Add-JobTrigger", "Add-KdsRootKey", "Add-LocalGroupMember", "Add-Member", "Add-MpPreference", "Add-NetEventNetworkAdapter", "Add-NetEventPacketCaptureProvider", "Add-NetEventProvider", "Add-NetEventVFPProvider", "Add-NetEventVmNetworkAdapter", "Add-NetEventVmSwitch", "Add-NetEventVmSwitchProvider", "Add-NetEventWFPCaptureProvider", "Add-NetIPHttpsCertBinding", "Add-NetLbfoTeamMember", "Add-NetLbfoTeamNic", "Add-NetNatExternalAddress", "Add-NetNatStaticMapping", "Add-NetSwitchTeamMember", "Add-OdbcDsn", "Add-PartitionAccessPath", "Add-PhysicalDisk", "Add-Printer", "Add-PrinterDriver", "Add-PrinterPort", "Add-ProvisionedAppPackage", "Add-ProvisionedAppxPackage", "Add-ProvisioningPackage", "Add-PSSnapin", "Add-SignerRule", "Add-StorageFaultDomain", "Add-TargetPortToMaskingSet", "Add-TrustedProvisioningCertificate", "Add-Type", "Add-VirtualDiskToMaskingSet", "Add-VpnConnection", "Add-VpnConnectionRoute", "Add-VpnConnectionTriggerApplication", "Add-VpnConnectionTriggerDnsConfiguration", "Add-VpnConnectionTriggerTrustedNetwork", "Add-WindowsCapability", "Add-WindowsDriver", "Add-WindowsImage", "Add-WindowsPackage", "AfterAll", "AfterEach", "Apply-WindowsUnattend", "Assert-MockCalled", "Assert-VerifiableMocks", "B:", "Backup-BitLockerKeyProtector", "BackupToAAD-BitLockerKeyProtector", "BeforeAll", "BeforeEach", "Block-FileShareAccess", "Block-SmbShareAccess", "C:", "Checkpoint-Computer", "Clear-AssignedAccess", "Clear-BCCache", "Clear-BitLockerAutoUnlock", "Clear-Content", "Clear-Disk", "Clear-DnsClientCache", "Clear-EventLog", "Clear-FileStorageTier", "Clear-History", "Clear-Host", "Clear-Item", "Clear-ItemProperty", "Clear-KdsCache", "Clear-PcsvDeviceLog", "Clear-RecycleBin", "Clear-StorageBusDisk", "Clear-StorageDiagnosticInfo", "Clear-Tpm", "Clear-UevAppxPackage", "Clear-UevConfiguration", "Clear-Variable", "Clear-WindowsCorruptMountPoint", "Close-SmbOpenFile", "Close-SmbSession", "Compare-Object", "Complete-BitsTransfer", "Complete-DtcDiagnosticTransaction", "Complete-Transaction", "Compress-Archive", "Configuration", "Confirm-SecureBootUEFI", "Connect-IscsiTarget", "Connect-PSSession", "Connect-VirtualDisk", "Connect-WSMan", "Context", "ConvertFrom-CIPolicy", "ConvertFrom-Csv", "ConvertFrom-Json", "ConvertFrom-SddlString", "ConvertFrom-SecureString", "ConvertFrom-String", "ConvertFrom-StringData", "Convert-Path", "Convert-String", "ConvertTo-Csv", "ConvertTo-Html", "ConvertTo-Json", "ConvertTo-ProcessMitigationPolicy", "ConvertTo-SecureString", "ConvertTo-TpmOwnerAuth", "ConvertTo-Xml", "Copy-Item", "Copy-ItemProperty", "Copy-NetFirewallRule", "Copy-NetIPsecMainModeCryptoSet", "Copy-NetIPsecMainModeRule", "Copy-NetIPsecPhase1AuthSet", "Copy-NetIPsecPhase2AuthSet", "Copy-NetIPsecQuickModeCryptoSet", "Copy-NetIPsecRule", "D:", "Debug-FileShare", "Debug-Job", "Debug-MMAppPrelaunch", "Debug-Process", "Debug-Runspace", "Debug-StorageSubSystem", "Debug-Volume", "Delete-DeliveryOptimizationCache", "Describe", "Disable-AppBackgroundTaskDiagnosticLog", "Disable-Appv", "Disable-AppvClientConnectionGroup", "Disable-BC", "Disable-BCDowngrading", "Disable-BCServeOnBattery", "Disable-BitLocker", "Disable-BitLockerAutoUnlock", "Disable-ComputerRestore", "Disable-DAManualEntryPointSelection", "Disable-DeliveryOptimizationVerboseLogs", "Disable-DscDebug", "Disable-JobTrigger", "Disable-LocalUser", "Disable-MMAgent", "Disable-NetAdapter", "Disable-NetAdapterBinding", "Disable-NetAdapterChecksumOffload", "Disable-NetAdapterEncapsulatedPacketTaskOffload", "Disable-NetAdapterIPsecOffload", "Disable-NetAdapterLso", "Disable-NetAdapterPacketDirect", "Disable-NetAdapterPowerManagement", "Disable-NetAdapterQos", "Disable-NetAdapterRdma", "Disable-NetAdapterRsc", "Disable-NetAdapterRss", "Disable-NetAdapterSriov", "Disable-NetAdapterUso", "Disable-NetAdapterVmq", "Disable-NetDnsTransitionConfiguration", "Disable-NetFirewallRule", "Disable-NetIPHttpsProfile", "Disable-NetIPsecMainModeRule", "Disable-NetIPsecRule", "Disable-NetNatTransitionConfiguration", "Disable-NetworkSwitchEthernetPort", "Disable-NetworkSwitchFeature", "Disable-NetworkSwitchVlan", "Disable-OdbcPerfCounter", "Disable-PhysicalDiskIdentification", "Disable-PhysicalDiskIndication", "Disable-PnpDevice", "Disable-PSBreakpoint", "Disable-PSRemoting", "Disable-PSSessionConfiguration", "Disable-PSTrace", "Disable-PSWSManCombinedTrace", "Disable-RunspaceDebug", "Disable-ScheduledJob", "Disable-ScheduledTask", "Disable-SmbDelegation", "Disable-StorageBusCache", "Disable-StorageBusDisk", "Disable-StorageDiagnosticLog", "Disable-StorageEnclosureIdentification", "Disable-StorageEnclosurePower", "Disable-StorageHighAvailability", "Disable-StorageMaintenanceMode", "Disable-TlsCipherSuite", "Disable-TlsEccCurve", "Disable-TlsSessionTicketKey", "Disable-TpmAutoProvisioning", "Disable-Uev", "Disable-UevAppxPackage", "Disable-UevTemplate", "Disable-WdacBidTrace", "Disable-WindowsErrorReporting", "Disable-WindowsOptionalFeature", "Disable-WSManCredSSP", "Disable-WSManTrace", "Disconnect-IscsiTarget", "Disconnect-PSSession", "Disconnect-VirtualDisk", "Disconnect-WSMan", "Dismount-AppPackageVolume", "Dismount-AppxVolume", "Dismount-DiskImage", "Dismount-WindowsImage", "E:", "Edit-CIPolicyRule", "Enable-AppBackgroundTaskDiagnosticLog", "Enable-Appv", "Enable-AppvClientConnectionGroup", "Enable-BCDistributed", "Enable-BCDowngrading", "Enable-BCHostedClient", "Enable-BCHostedServer", "Enable-BCLocal", "Enable-BCServeOnBattery", "Enable-BitLocker", "Enable-BitLockerAutoUnlock", "Enable-ComputerRestore", "Enable-DAManualEntryPointSelection", "Enable-DeliveryOptimizationVerboseLogs", "Enable-DscDebug", "Enable-JobTrigger", "Enable-LocalUser", "Enable-MMAgent", "Enable-NetAdapter", "Enable-NetAdapterBinding", "Enable-NetAdapterChecksumOffload", "Enable-NetAdapterEncapsulatedPacketTaskOffload", "Enable-NetAdapterIPsecOffload", "Enable-NetAdapterLso", "Enable-NetAdapterPacketDirect", "Enable-NetAdapterPowerManagement", "Enable-NetAdapterQos", "Enable-NetAdapterRdma", "Enable-NetAdapterRsc", "Enable-NetAdapterRss", "Enable-NetAdapterSriov", "Enable-NetAdapterUso", "Enable-NetAdapterVmq", "Enable-NetDnsTransitionConfiguration", "Enable-NetFirewallRule", "Enable-NetIPHttpsProfile", "Enable-NetIPsecMainModeRule", "Enable-NetIPsecRule", "Enable-NetNatTransitionConfiguration", "Enable-NetworkSwitchEthernetPort", "Enable-NetworkSwitchFeature", "Enable-NetworkSwitchVlan", "Enable-OdbcPerfCounter", "Enable-PhysicalDiskIdentification", "Enable-PhysicalDiskIndication", "Enable-PnpDevice", "Enable-PSBreakpoint", "Enable-PSRemoting", "Enable-PSSessionConfiguration", "Enable-PSTrace", "Enable-PSWSManCombinedTrace", "Enable-RunspaceDebug", "Enable-ScheduledJob", "Enable-ScheduledTask", "Enable-SmbDelegation", "Enable-StorageBusCache", "Enable-StorageBusDisk", "Enable-StorageDiagnosticLog", "Enable-StorageEnclosureIdentification", "Enable-StorageEnclosurePower", "Enable-StorageHighAvailability", "Enable-StorageMaintenanceMode", "Enable-TlsCipherSuite", "Enable-TlsEccCurve", "Enable-TlsSessionTicketKey", "Enable-TpmAutoProvisioning", "Enable-Uev", "Enable-UevAppxPackage", "Enable-UevTemplate", "Enable-WdacBidTrace", "Enable-WindowsErrorReporting", "Enable-WindowsOptionalFeature", "Enable-WSManCredSSP", "Enable-WSManTrace", "Enter-PSHostProcess", "Enter-PSSession", "Exit-PSHostProcess", "Exit-PSSession", "Expand-Archive", "Expand-WindowsCustomDataImage", "Expand-WindowsImage", "Export-Alias", "Export-BCCachePackage", "Export-BCSecretKey", "Export-BinaryMiLog", "Export-Certificate", "Export-Clixml", "Export-Console", "Export-Counter", "Export-Csv", "Export-FormatData", "Export-ModuleMember", "Export-ODataEndpointProxy", "Export-PfxCertificate", "Export-ProvisioningPackage", "Export-PSSession", "Export-ScheduledTask", "Export-StartLayout", "Export-StartLayoutEdgeAssets", "Export-TlsSessionTicketKey", "Export-Trace", "Export-UevConfiguration", "Export-UevPackage", "Export-WindowsCapabilitySource", "Export-WindowsDriver", "Export-WindowsImage", "F:", "Find-Command", "Find-DscResource", "Find-Module", "Find-NetIPsecRule", "Find-NetRoute", "Find-Package", "Find-PackageProvider", "Find-RoleCapability", "Find-Script", "Flush-EtwTraceSession", "Flush-Volume", "ForEach-Object", "Format-Custom", "Format-Hex", "Format-List", "Format-SecureBootUEFI", "Format-Table", "Format-Volume", "Format-Wide", "G:", "Get-Acl", "Get-Alias", "Get-AppBackgroundTask", "Get-AppLockerFileInformation", "Get-AppLockerPolicy", "Get-AppPackage", "Get-AppPackageDefaultVolume", "Get-AppPackageLastError", "Get-AppPackageLog", "Get-AppPackageManifest", "Get-AppPackageVolume", "Get-AppProvisionedPackage", "Get-AppvClientApplication", "Get-AppvClientConfiguration", "Get-AppvClientConnectionGroup", "Get-AppvClientMode", "Get-AppvClientPackage", "Get-AppvPublishingServer", "Get-AppvStatus", "Get-AppvVirtualProcess", "Get-AppxDefaultVolume", "Get-AppxLastError", "Get-AppxLog", "Get-AppxPackage", "Get-AppxPackageManifest", "Get-AppxProvisionedPackage", "Get-AppxVolume", "Get-AssignedAccess", "Get-AuthenticodeSignature", "Get-AutologgerConfig", "Get-BCClientConfiguration", "Get-BCContentServerConfiguration", "Get-BCDataCache", "Get-BCDataCacheExtension", "Get-BCHashCache", "Get-BCHostedCacheServerConfiguration", "Get-BCNetworkConfiguration", "Get-BCStatus", "Get-BitLockerVolume", "Get-BitsTransfer", "Get-Certificate", "Get-CertificateAutoEnrollmentPolicy", "Get-CertificateEnrollmentPolicyServer", "Get-CertificateNotificationTask", "Get-ChildItem", "Get-CimAssociatedInstance", "Get-CimClass", "Get-CimInstance", "Get-CimSession", "Get-CIPolicy", "Get-CIPolicyIdInfo", "Get-CIPolicyInfo", "Get-Clipboard", "Get-ClusteredScheduledTask", "Get-CmsMessage", "Get-Command", "Get-ComputerInfo", "Get-ComputerRestorePoint", "Get-Content", "Get-ControlPanelItem", "Get-Counter", "Get-Credential", "Get-Culture", "Get-DAClientExperienceConfiguration", "Get-DAConnectionStatus", "Get-DAEntryPointTableItem", "Get-DAPolicyChange", "Get-Date", "Get-DedupProperties", "Get-DeliveryOptimizationLog", "Get-DeliveryOptimizationLogAnalysis", "Get-DeliveryOptimizationPerfSnap", "Get-DeliveryOptimizationPerfSnapThisMonth", "Get-DeliveryOptimizationStatus", "Get-Disk", "Get-DiskImage", "Get-DiskSNV", "Get-DiskStorageNodeView", "Get-DnsClient", "Get-DnsClientCache", "Get-DnsClientGlobalSetting", "Get-DnsClientNrptGlobal", "Get-DnsClientNrptPolicy", "Get-DnsClientNrptRule", "Get-DnsClientServerAddress", "Get-DOConfig", "Get-DODownloadMode", "Get-DOPercentageMaxBackgroundBandwidth", "Get-DOPercentageMaxForegroundBandwidth", "Get-DscConfiguration", "Get-DscConfigurationStatus", "Get-DscLocalConfigurationManager", "Get-DscResource", "Get-Dtc", "Get-DtcAdvancedHostSetting", "Get-DtcAdvancedSetting", "Get-DtcClusterDefault", "Get-DtcClusterTMMapping", "Get-DtcDefault", "Get-DtcLog", "Get-DtcNetworkSetting", "Get-DtcTransaction", "Get-DtcTransactionsStatistics", "Get-DtcTransactionsTraceSession", "Get-DtcTransactionsTraceSetting", "Get-EtwTraceProvider", "Get-EtwTraceSession", "Get-Event", "Get-EventLog", "Get-EventSubscriber", "Get-ExecutionPolicy", "Get-FileHash", "Get-FileIntegrity", "Get-FileShare", "Get-FileShareAccessControlEntry", "Get-FileStorageTier", "Get-FormatData", "Get-Help", "Get-History", "Get-HnsEndpoint", "Get-HnsNamespace", "Get-HnsNetwork", "Get-HnsPolicyList", "Get-Host", "Get-HotFix", "Get-InitiatorId", "Get-InitiatorPort", "Get-InstalledModule", "Get-InstalledScript", "Get-IscsiConnection", "Get-IscsiSession", "Get-IscsiTarget", "Get-IscsiTargetPortal", "Get-IseSnippet", "Get-Item", "Get-ItemProperty", "Get-ItemPropertyValue", "Get-Job", "Get-JobTrigger", "Get-KdsConfiguration", "Get-KdsRootKey", "Get-LocalGroup", "Get-LocalGroupMember", "Get-LocalUser", "Get-Location", "Get-LogProperties", "Get-MaskingSet", "Get-Member", "Get-MMAgent", "Get-MockDynamicParameters", "Get-Module", "Get-MpComputerStatus", "Get-MpPreference", "Get-MpThreat", "Get-MpThreatCatalog", "Get-MpThreatDetection", "Get-NCSIPolicyConfiguration", "Get-Net6to4Configuration", "Get-NetAdapter", "Get-NetAdapterAdvancedProperty", "Get-NetAdapterBinding", "Get-NetAdapterChecksumOffload", "Get-NetAdapterEncapsulatedPacketTaskOffload", "Get-NetAdapterHardwareInfo", "Get-NetAdapterIPsecOffload", "Get-NetAdapterLso", "Get-NetAdapterPacketDirect", "Get-NetAdapterPowerManagement", "Get-NetAdapterQos", "Get-NetAdapterRdma", "Get-NetAdapterRsc", "Get-NetAdapterRss", "Get-NetAdapterSriov", "Get-NetAdapterSriovVf", "Get-NetAdapterStatistics", "Get-NetAdapterUso", "Get-NetAdapterVmq", "Get-NetAdapterVMQQueue", "Get-NetAdapterVPort", "Get-NetCompartment", "Get-NetConnectionProfile", "Get-NetDnsTransitionConfiguration", "Get-NetDnsTransitionMonitoring", "Get-NetEventNetworkAdapter", "Get-NetEventPacketCaptureProvider", "Get-NetEventProvider", "Get-NetEventSession", "Get-NetEventVFPProvider", "Get-NetEventVmNetworkAdapter", "Get-NetEventVmSwitch", "Get-NetEventVmSwitchProvider", "Get-NetEventWFPCaptureProvider", "Get-NetFirewallAddressFilter", "Get-NetFirewallApplicationFilter", "Get-NetFirewallDynamicKeywordAddress", "Get-NetFirewallInterfaceFilter", "Get-NetFirewallInterfaceTypeFilter", "Get-NetFirewallPortFilter", "Get-NetFirewallProfile", "Get-NetFirewallRule", "Get-NetFirewallSecurityFilter", "Get-NetFirewallServiceFilter", "Get-NetFirewallSetting", "Get-NetIPAddress", "Get-NetIPConfiguration", "Get-NetIPHttpsConfiguration", "Get-NetIPHttpsState", "Get-NetIPInterface", "Get-NetIPsecDospSetting", "Get-NetIPsecMainModeCryptoSet", "Get-NetIPsecMainModeRule", "Get-NetIPsecMainModeSA", "Get-NetIPsecPhase1AuthSet", "Get-NetIPsecPhase2AuthSet", "Get-NetIPsecQuickModeCryptoSet", "Get-NetIPsecQuickModeSA", "Get-NetIPsecRule", "Get-NetIPv4Protocol", "Get-NetIPv6Protocol", "Get-NetIsatapConfiguration", "Get-NetLbfoTeam", "Get-NetLbfoTeamMember", "Get-NetLbfoTeamNic", "Get-NetNat", "Get-NetNatExternalAddress", "Get-NetNatGlobal", "Get-NetNatSession", "Get-NetNatStaticMapping", "Get-NetNatTransitionConfiguration", "Get-NetNatTransitionMonitoring", "Get-NetNeighbor", "Get-NetOffloadGlobalSetting", "Get-NetPrefixPolicy", "Get-NetQosPolicy", "Get-NetRoute", "Get-NetSwitchTeam", "Get-NetSwitchTeamMember", "Get-NetTCPConnection", "Get-NetTCPSetting", "Get-NetTeredoConfiguration", "Get-NetTeredoState", "Get-NetTransportFilter", "Get-NetUDPEndpoint", "Get-NetUDPSetting", "Get-NetworkSwitchEthernetPort", "Get-NetworkSwitchFeature", "Get-NetworkSwitchGlobalData", "Get-NetworkSwitchVlan", "Get-NonRemovableAppsPolicy", "Get-OdbcDriver", "Get-OdbcDsn", "Get-OdbcPerfCounter", "Get-OffloadDataTransferSetting", "Get-OperationValidation", "Get-Package", "Get-PackageProvider", "Get-PackageSource", "Get-Partition", "Get-PartitionSupportedSize", "Get-PcsvDevice", "Get-PcsvDeviceLog", "Get-PfxCertificate", "Get-PfxData", "Get-PhysicalDisk", "Get-PhysicalDiskSNV", "Get-PhysicalDiskStorageNodeView", "Get-PhysicalExtent", "Get-PhysicalExtentAssociation", "Get-PmemDisk", "Get-PmemPhysicalDevice", "Get-PmemUnusedRegion", "Get-PnpDevice", "Get-PnpDeviceProperty", "Get-PrintConfiguration", "Get-Printer", "Get-PrinterDriver", "Get-PrinterPort", "Get-PrinterProperty", "Get-PrintJob", "Get-Process", "Get-ProcessMitigation", "Get-ProvisionedAppPackage", "Get-ProvisionedAppxPackage", "Get-ProvisioningPackage", "Get-PSBreakpoint", "Get-PSCallStack", "Get-PSDrive", "Get-PSHostProcessInfo", "Get-PSProvider", "Get-PSReadLineKeyHandler", "Get-PSReadLineOption", "Get-PSRepository", "Get-PSSession", "Get-PSSessionCapability", "Get-PSSessionConfiguration", "Get-PSSnapin", "Get-Random", "Get-ResiliencySetting", "Get-Runspace", "Get-RunspaceDebug", "Get-ScheduledJob", "Get-ScheduledJobOption", "Get-ScheduledTask", "Get-ScheduledTaskInfo", "Get-SecureBootPolicy", "Get-SecureBootUEFI", "Get-Service", "Get-SmbBandWidthLimit", "Get-SmbClientConfiguration", "Get-SmbClientNetworkInterface", "Get-SmbConnection", "Get-SmbDelegation", "Get-SmbGlobalMapping", "Get-SmbMapping", "Get-SmbMultichannelConnection", "Get-SmbMultichannelConstraint", "Get-SmbOpenFile", "Get-SmbServerCertificateMapping", "Get-SmbServerConfiguration", "Get-SmbServerNetworkInterface", "Get-SmbSession", "Get-SmbShare", "Get-SmbShareAccess", "Get-SmbWitnessClient", "Get-StartApps", "Get-StorageAdvancedProperty", "Get-StorageBusBinding", "Get-StorageBusDisk", "Get-StorageChassis", "Get-StorageDiagnosticInfo", "Get-StorageEnclosure", "Get-StorageEnclosureSNV", "Get-StorageEnclosureStorageNodeView", "Get-StorageEnclosureVendorData", "Get-StorageExtendedStatus", "Get-StorageFaultDomain", "Get-StorageFileServer", "Get-StorageFirmwareInformation", "Get-StorageHealthAction", "Get-StorageHealthReport", "Get-StorageHealthSetting", "Get-StorageHistory", "Get-StorageJob", "Get-StorageNode", "Get-StoragePool", "Get-StorageProvider", "Get-StorageRack", "Get-StorageReliabilityCounter", "Get-StorageScaleUnit", "Get-StorageSetting", "Get-StorageSite", "Get-StorageSubSystem", "Get-StorageTier", "Get-StorageTierSupportedSize", "Get-SupportedClusterSizes", "Get-SupportedFileSystems", "Get-SystemDriver", "Get-TargetPort", "Get-TargetPortal", "Get-TestDriveItem", "Get-TimeZone", "Get-TlsCipherSuite", "Get-TlsEccCurve", "Get-Tpm", "Get-TpmEndorsementKeyInfo", "Get-TpmSupportedFeature", "Get-TraceSource", "Get-Transaction", "Get-TroubleshootingPack", "Get-TrustedProvisioningCertificate", "Get-TypeData", "Get-UevAppxPackage", "Get-UevConfiguration", "Get-UevStatus", "Get-UevTemplate", "Get-UevTemplateProgram", "Get-UICulture", "Get-Unique", "Get-Variable", "Get-Verb", "Get-VirtualDisk", "Get-VirtualDiskSupportedSize", "Get-Volume", "Get-VolumeCorruptionCount", "Get-VolumeScrubPolicy", "Get-VpnConnection", "Get-VpnConnectionTrigger", "Get-WdacBidTrace", "Get-WheaMemoryPolicy", "Get-WIMBootEntry", "Get-WinAcceptLanguageFromLanguageListOptOut", "Get-WinCultureFromLanguageListOptOut", "Get-WinDefaultInputMethodOverride", "Get-WindowsCapability", "Get-WindowsDeveloperLicense", "Get-WindowsDriver", "Get-WindowsEdition", "Get-WindowsErrorReporting", "Get-WindowsImage", "Get-WindowsImageContent", "Get-WindowsOptionalFeature", "Get-WindowsPackage", "Get-WindowsReservedStorageState", "Get-WindowsSearchSetting", "Get-WindowsUpdateLog", "Get-WinEvent", "Get-WinHomeLocation", "Get-WinLanguageBarOption", "Get-WinSystemLocale", "Get-WinUILanguageOverride", "Get-WinUserLanguageList", "Get-WmiObject", "Get-WSManCredSSP", "Get-WSManInstance", "Grant-FileShareAccess", "Grant-SmbShareAccess", "Group-Object", "H:", "help", "Hide-VirtualDisk", "I:", "Import-Alias", "Import-BCCachePackage", "Import-BCSecretKey", "Import-BinaryMiLog", "Import-Certificate", "Import-Clixml", "Import-Counter", "Import-Csv", "Import-IseSnippet", "Import-LocalizedData", "Import-Module", "Import-PackageProvider", "Import-PfxCertificate", "Import-PowerShellDataFile", "Import-PSSession", "Import-StartLayout", "ImportSystemModules", "Import-TpmOwnerAuth", "Import-UevConfiguration", "In", "Initialize-Disk", "Initialize-PmemPhysicalDevice", "Initialize-Tpm", "Initialize-Volume", "InModuleScope", "Install-Dtc", "Install-Module", "Install-Package", "Install-PackageProvider", "Install-ProvisioningPackage", "Install-Script", "Install-TrustedProvisioningCertificate", "Invoke-AsWorkflow", "Invoke-CimMethod", "Invoke-Command", "Invoke-CommandInDesktopPackage", "Invoke-DscResource", "Invoke-Expression", "Invoke-History", "Invoke-Item", "Invoke-Mock", "Invoke-OperationValidation", "Invoke-Pester", "Invoke-RestMethod", "Invoke-TroubleshootingPack", "Invoke-WebRequest", "Invoke-WmiMethod", "Invoke-WSManAction", "It", "J:", "Join-DtcDiagnosticResourceManager", "Join-Path", "K:", "L:", "Limit-EventLog", "Lock-BitLocker", "M:", "Measure-Command", "Measure-Object", "Merge-CIPolicy", "mkdir", "Mock", "more", "Mount-AppPackageVolume", "Mount-AppvClientConnectionGroup", "Mount-AppvClientPackage", "Mount-AppxVolume", "Mount-DiskImage", "Mount-WindowsImage", "Move-AppPackage", "Move-AppxPackage", "Move-Item", "Move-ItemProperty", "Move-SmbClient", "Move-SmbWitnessClient", "N:", "New-Alias", "New-AppLockerPolicy", "New-AutologgerConfig", "New-CertificateNotificationTask", "New-CimInstance", "New-CimSession", "New-CimSessionOption", "New-CIPolicy", "New-CIPolicyRule", "New-DAEntryPointTableItem", "New-DscChecksum", "New-DtcDiagnosticTransaction", "New-EapConfiguration", "New-EtwTraceSession", "New-Event", "New-EventLog", "New-FileCatalog", "New-FileShare", "New-Fixture", "New-Guid", "New-IscsiTargetPortal", "New-IseSnippet", "New-Item", "New-ItemProperty", "New-JobTrigger", "New-LocalGroup", "New-LocalUser", "New-MaskingSet", "New-Module", "New-ModuleManifest", "New-NetAdapterAdvancedProperty", "New-NetEventSession", "New-NetFirewallDynamicKeywordAddress", "New-NetFirewallRule", "New-NetIPAddress", "New-NetIPHttpsConfiguration", "New-NetIPsecAuthProposal", "New-NetIPsecDospSetting", "New-NetIPsecMainModeCryptoProposal", "New-NetIPsecMainModeCryptoSet", "New-NetIPsecMainModeRule", "New-NetIPsecPhase1AuthSet", "New-NetIPsecPhase2AuthSet", "New-NetIPsecQuickModeCryptoProposal", "New-NetIPsecQuickModeCryptoSet", "New-NetIPsecRule", "New-NetLbfoTeam", "New-NetNat", "New-NetNatTransitionConfiguration", "New-NetNeighbor", "New-NetQosPolicy", "New-NetRoute", "New-NetSwitchTeam", "New-NetTransportFilter", "New-NetworkSwitchVlan", "New-Object", "New-Partition", "New-PesterOption", "New-PmemDisk", "New-ProvisioningRepro", "New-PSDrive", "New-PSRoleCapabilityFile", "New-PSSession", "New-PSSessionConfigurationFile", "New-PSSessionOption", "New-PSTransportOption", "New-PSWorkflowExecutionOption", "New-PSWorkflowSession", "New-ScheduledJobOption", "New-ScheduledTask", "New-ScheduledTaskAction", "New-ScheduledTaskPrincipal", "New-ScheduledTaskSettingsSet", "New-ScheduledTaskTrigger", "New-ScriptFileInfo", "New-SelfSignedCertificate", "New-Service", "New-SmbGlobalMapping", "New-SmbMapping", "New-SmbMultichannelConstraint", "New-SmbServerCertificateMapping", "New-SmbShare", "New-StorageBusBinding", "New-StorageBusCacheStore", "New-StorageFileServer", "New-StoragePool", "New-StorageSubsystemVirtualDisk", "New-StorageTier", "New-TemporaryFile", "New-TimeSpan", "New-TlsSessionTicketKey", "New-Variable", "New-VirtualDisk", "New-VirtualDiskClone", "New-VirtualDiskSnapshot", "New-Volume", "New-VpnServerAddress", "New-WebServiceProxy", "New-WindowsCustomImage", "New-WindowsImage", "New-WinEvent", "New-WinUserLanguageList", "New-WSManInstance", "New-WSManSessionOption", "O:", "Open-NetGPO", "Optimize-AppProvisionedPackages", "Optimize-AppxProvisionedPackages", "Optimize-ProvisionedAppPackages", "Optimize-ProvisionedAppxPackages", "Optimize-StoragePool", "Optimize-Volume", "Optimize-WindowsImage", "oss", "Out-Default", "Out-File", "Out-GridView", "Out-Host", "Out-Null", "Out-Printer", "Out-String", "P:", "Pause", "Pop-Location", "prompt", "Protect-CmsMessage", "PSConsoleHostReadLine", "Publish-AppvClientPackage", "Publish-BCFileContent", "Publish-BCWebContent", "Publish-DscConfiguration", "Publish-Module", "Publish-Script", "Push-Location", "Q:", "R:", "Read-Host", "Read-PrinterNfcTag", "Receive-DtcDiagnosticTransaction", "Receive-Job", "Receive-PSSession", "Register-ArgumentCompleter", "Register-CimIndicationEvent", "Register-ClusteredScheduledTask", "Register-DnsClient", "Register-EngineEvent", "Register-IscsiSession", "Register-ObjectEvent", "Register-PackageSource", "Register-PSRepository", "Register-PSSessionConfiguration", "Register-ScheduledJob", "Register-ScheduledTask", "Register-StorageSubsystem", "Register-UevTemplate", "Register-WmiEvent", "Remove-AppPackage", "Remove-AppPackageVolume", "Remove-AppProvisionedPackage", "Remove-AppvClientConnectionGroup", "Remove-AppvClientPackage", "Remove-AppvPublishingServer", "Remove-AppxPackage", "Remove-AppxProvisionedPackage", "Remove-AppxVolume", "Remove-AutologgerConfig", "Remove-BCDataCacheExtension", "Remove-BitLockerKeyProtector", "Remove-BitsTransfer", "Remove-CertificateEnrollmentPolicyServer", "Remove-CertificateNotificationTask", "Remove-CimInstance", "Remove-CimSession", "Remove-CIPolicyRule", "Remove-Computer", "Remove-DAEntryPointTableItem", "Remove-DnsClientNrptRule", "Remove-DscConfigurationDocument", "Remove-DtcClusterTMMapping", "Remove-EtwTraceProvider", "Remove-EtwTraceSession", "Remove-Event", "Remove-EventLog", "Remove-FileShare", "Remove-HnsEndpoint", "Remove-HnsNamespace", "Remove-HnsNetwork", "Remove-HnsPolicyList", "Remove-InitiatorId", "Remove-InitiatorIdFromMaskingSet", "Remove-IscsiTargetPortal", "Remove-Item", "Remove-ItemProperty", "Remove-Job", "Remove-JobTrigger", "Remove-LocalGroup", "Remove-LocalGroupMember", "Remove-LocalUser", "Remove-MaskingSet", "Remove-Module", "Remove-MpPreference", "Remove-MpThreat", "Remove-NetAdapterAdvancedProperty", "Remove-NetEventNetworkAdapter", "Remove-NetEventPacketCaptureProvider", "Remove-NetEventProvider", "Remove-NetEventSession", "Remove-NetEventVFPProvider", "Remove-NetEventVmNetworkAdapter", "Remove-NetEventVmSwitch", "Remove-NetEventVmSwitchProvider", "Remove-NetEventWFPCaptureProvider", "Remove-NetFirewallDynamicKeywordAddress", "Remove-NetFirewallRule", "Remove-NetIPAddress", "Remove-NetIPHttpsCertBinding", "Remove-NetIPHttpsConfiguration", "Remove-NetIPsecDospSetting", "Remove-NetIPsecMainModeCryptoSet", "Remove-NetIPsecMainModeRule", "Remove-NetIPsecMainModeSA", "Remove-NetIPsecPhase1AuthSet", "Remove-NetIPsecPhase2AuthSet", "Remove-NetIPsecQuickModeCryptoSet", "Remove-NetIPsecQuickModeSA", "Remove-NetIPsecRule", "Remove-NetLbfoTeam", "Remove-NetLbfoTeamMember", "Remove-NetLbfoTeamNic", "Remove-NetNat", "Remove-NetNatExternalAddress", "Remove-NetNatStaticMapping", "Remove-NetNatTransitionConfiguration", "Remove-NetNeighbor", "Remove-NetQosPolicy", "Remove-NetRoute", "Remove-NetSwitchTeam", "Remove-NetSwitchTeamMember", "Remove-NetTransportFilter", "Remove-NetworkSwitchEthernetPortIPAddress", "Remove-NetworkSwitchVlan", "Remove-OdbcDsn", "Remove-Partition", "Remove-PartitionAccessPath", "Remove-PhysicalDisk", "Remove-PmemDisk", "Remove-Printer", "Remove-PrinterDriver", "Remove-PrinterPort", "Remove-PrintJob", "Remove-ProvisionedAppPackage", "Remove-ProvisionedAppxPackage", "Remove-ProvisioningPackage", "Remove-PSBreakpoint", "Remove-PSDrive", "Remove-PSReadLineKeyHandler", "Remove-PSSession", "Remove-PSSnapin", "Remove-SmbBandwidthLimit", "Remove-SMBComponent", "Remove-SmbGlobalMapping", "Remove-SmbMapping", "Remove-SmbMultichannelConstraint", "Remove-SmbServerCertificateMapping", "Remove-SmbShare", "Remove-StorageBusBinding", "Remove-StorageFaultDomain", "Remove-StorageFileServer", "Remove-StorageHealthIntent", "Remove-StorageHealthSetting", "Remove-StoragePool", "Remove-StorageTier", "Remove-TargetPortFromMaskingSet", "Remove-TrustedProvisioningCertificate", "Remove-TypeData", "Remove-Variable", "Remove-VirtualDisk", "Remove-VirtualDiskFromMaskingSet", "Remove-VpnConnection", "Remove-VpnConnectionRoute", "Remove-VpnConnectionTriggerApplication", "Remove-VpnConnectionTriggerDnsConfiguration", "Remove-VpnConnectionTriggerTrustedNetwork", "Remove-WindowsCapability", "Remove-WindowsDriver", "Remove-WindowsImage", "Remove-WindowsPackage", "Remove-WmiObject", "Remove-WSManInstance", "Rename-Computer", "Rename-DAEntryPointTableItem", "Rename-Item", "Rename-ItemProperty", "Rename-LocalGroup", "Rename-LocalUser", "Rename-MaskingSet", "Rename-NetAdapter", "Rename-NetFirewallRule", "Rename-NetIPHttpsConfiguration", "Rename-NetIPsecMainModeCryptoSet", "Rename-NetIPsecMainModeRule", "Rename-NetIPsecPhase1AuthSet", "Rename-NetIPsecPhase2AuthSet", "Rename-NetIPsecQuickModeCryptoSet", "Rename-NetIPsecRule", "Rename-NetLbfoTeam", "Rename-NetSwitchTeam", "Rename-Printer", "Repair-AppvClientConnectionGroup", "Repair-AppvClientPackage", "Repair-FileIntegrity", "Repair-UevTemplateIndex", "Repair-VirtualDisk", "Repair-Volume", "Repair-WindowsImage", "Reset-BC", "Reset-ComputerMachinePassword", "Reset-DAClientExperienceConfiguration", "Reset-DAEntryPointTableItem", "Reset-DtcLog", "Reset-NCSIPolicyConfiguration", "Reset-Net6to4Configuration", "Reset-NetAdapterAdvancedProperty", "Reset-NetDnsTransitionConfiguration", "Reset-NetIPHttpsConfiguration", "Reset-NetIsatapConfiguration", "Reset-NetTeredoConfiguration", "Reset-PhysicalDisk", "Reset-StorageReliabilityCounter", "Resize-Partition", "Resize-StorageTier", "Resize-VirtualDisk", "Resolve-DnsName", "Resolve-Path", "Restart-Computer", "Restart-NetAdapter", "Restart-PcsvDevice", "Restart-PrintJob", "Restart-Service", "Restore-Computer", "Restore-DscConfiguration", "Restore-NetworkSwitchConfiguration", "Restore-UevBackup", "Restore-UevUserSetting", "Resume-BitLocker", "Resume-BitsTransfer", "Resume-Job", "Resume-PrintJob", "Resume-ProvisioningSession", "Resume-Service", "Resume-StorageBusDisk", "Revoke-FileShareAccess", "Revoke-SmbShareAccess", "S:", "SafeGetCommand", "Save-EtwTraceSession", "Save-Help", "Save-Module", "Save-NetGPO", "Save-NetworkSwitchConfiguration", "Save-Package", "Save-Script", "Save-WindowsImage", "Select-Object", "Select-String", "Select-Xml", "Send-AppvClientReport", "Send-DtcDiagnosticTransaction", "Send-EtwTraceSession", "Send-MailMessage", "Set-Acl", "Set-Alias", "Set-AppBackgroundTaskResourcePolicy", "Set-AppLockerPolicy", "Set-AppPackageDefaultVolume", "Set-AppPackageProvisionedDataFile", "Set-AppvClientConfiguration", "Set-AppvClientMode", "Set-AppvClientPackage", "Set-AppvPublishingServer", "Set-AppxDefaultVolume", "Set-AppXProvisionedDataFile", "Set-AssignedAccess", "Set-AuthenticodeSignature", "Set-AutologgerConfig", "Set-BCAuthentication", "Set-BCCache", "Set-BCDataCacheEntryMaxAge", "Set-BCMinSMBLatency", "Set-BCSecretKey", "Set-BitsTransfer", "Set-CertificateAutoEnrollmentPolicy", "Set-CimInstance", "Set-CIPolicyIdInfo", "Set-CIPolicySetting", "Set-CIPolicyVersion", "Set-Clipboard", "Set-ClusteredScheduledTask", "Set-Content", "Set-Culture", "Set-DAClientExperienceConfiguration", "Set-DAEntryPointTableItem", "Set-Date", "Set-DeliveryOptimizationStatus", "Set-Disk", "Set-DnsClient", "Set-DnsClientGlobalSetting", "Set-DnsClientNrptGlobal", "Set-DnsClientNrptRule", "Set-DnsClientServerAddress", "Set-DODownloadMode", "Set-DOPercentageMaxBackgroundBandwidth", "Set-DOPercentageMaxForegroundBandwidth", "Set-DscLocalConfigurationManager", "Set-DtcAdvancedHostSetting", "Set-DtcAdvancedSetting", "Set-DtcClusterDefault", "Set-DtcClusterTMMapping", "Set-DtcDefault", "Set-DtcLog", "Set-DtcNetworkSetting", "Set-DtcTransaction", "Set-DtcTransactionsTraceSession", "Set-DtcTransactionsTraceSetting", "Set-DynamicParameterVariables", "Set-EtwTraceProvider", "Set-EtwTraceSession", "Set-ExecutionPolicy", "Set-FileIntegrity", "Set-FileShare", "Set-FileStorageTier", "Set-HVCIOptions", "Set-InitiatorPort", "Set-IscsiChapSecret", "Set-Item", "Set-ItemProperty", "Set-JobTrigger", "Set-KdsConfiguration", "Set-LocalGroup", "Set-LocalUser", "Set-Location", "Set-LogProperties", "Set-MMAgent", "Set-MpPreference", "Set-NCSIPolicyConfiguration", "Set-Net6to4Configuration", "Set-NetAdapter", "Set-NetAdapterAdvancedProperty", "Set-NetAdapterBinding", "Set-NetAdapterChecksumOffload", "Set-NetAdapterEncapsulatedPacketTaskOffload", "Set-NetAdapterIPsecOffload", "Set-NetAdapterLso", "Set-NetAdapterPacketDirect", "Set-NetAdapterPowerManagement", "Set-NetAdapterQos", "Set-NetAdapterRdma", "Set-NetAdapterRsc", "Set-NetAdapterRss", "Set-NetAdapterSriov", "Set-NetAdapterUso", "Set-NetAdapterVmq", "Set-NetConnectionProfile", "Set-NetDnsTransitionConfiguration", "Set-NetEventPacketCaptureProvider", "Set-NetEventProvider", "Set-NetEventSession", "Set-NetEventVFPProvider", "Set-NetEventVmSwitchProvider", "Set-NetEventWFPCaptureProvider", "Set-NetFirewallAddressFilter", "Set-NetFirewallApplicationFilter", "Set-NetFirewallInterfaceFilter", "Set-NetFirewallInterfaceTypeFilter", "Set-NetFirewallPortFilter", "Set-NetFirewallProfile", "Set-NetFirewallRule", "Set-NetFirewallSecurityFilter", "Set-NetFirewallServiceFilter", "Set-NetFirewallSetting", "Set-NetIPAddress", "Set-NetIPHttpsConfiguration", "Set-NetIPInterface", "Set-NetIPsecDospSetting", "Set-NetIPsecMainModeCryptoSet", "Set-NetIPsecMainModeRule", "Set-NetIPsecPhase1AuthSet", "Set-NetIPsecPhase2AuthSet", "Set-NetIPsecQuickModeCryptoSet", "Set-NetIPsecRule", "Set-NetIPv4Protocol", "Set-NetIPv6Protocol", "Set-NetIsatapConfiguration", "Set-NetLbfoTeam", "Set-NetLbfoTeamMember", "Set-NetLbfoTeamNic", "Set-NetNat", "Set-NetNatGlobal", "Set-NetNatTransitionConfiguration", "Set-NetNeighbor", "Set-NetOffloadGlobalSetting", "Set-NetQosPolicy", "Set-NetRoute", "Set-NetTCPSetting", "Set-NetTeredoConfiguration", "Set-NetUDPSetting", "Set-NetworkSwitchEthernetPortIPAddress", "Set-NetworkSwitchPortMode", "Set-NetworkSwitchPortProperty", "Set-NetworkSwitchVlanProperty", "Set-NonRemovableAppsPolicy", "Set-OdbcDriver", "Set-OdbcDsn", "Set-PackageSource", "Set-Partition", "Set-PcsvDeviceBootConfiguration", "Set-PcsvDeviceNetworkConfiguration", "Set-PcsvDeviceUserPassword", "Set-PhysicalDisk", "Set-PrintConfiguration", "Set-Printer", "Set-PrinterProperty", "Set-ProcessMitigation", "Set-ProvisionedAppPackageDataFile", "Set-ProvisionedAppXDataFile", "Set-PSBreakpoint", "Set-PSDebug", "Set-PSReadLineKeyHandler", "Set-PSReadLineOption", "Set-PSRepository", "Set-PSSessionConfiguration", "Set-ResiliencySetting", "Set-RuleOption", "Set-ScheduledJob", "Set-ScheduledJobOption", "Set-ScheduledTask", "Set-SecureBootUEFI", "Set-Service", "Set-SmbBandwidthLimit", "Set-SmbClientConfiguration", "Set-SmbPathAcl", "Set-SmbServerConfiguration", "Set-SmbShare", "Set-StorageBusProfile", "Set-StorageFileServer", "Set-StorageHealthSetting", "Set-StoragePool", "Set-StorageProvider", "Set-StorageSetting", "Set-StorageSubSystem", "Set-StorageTier", "Set-StrictMode", "Set-TestInconclusive", "Set-TimeZone", "Set-TpmOwnerAuth", "Set-TraceSource", "Set-UevConfiguration", "Set-UevTemplateProfile", "Setup", "Set-Variable", "Set-VirtualDisk", "Set-Volume", "Set-VolumeScrubPolicy", "Set-VpnConnection", "Set-VpnConnectionIPsecConfiguration", "Set-VpnConnectionProxy", "Set-VpnConnectionTriggerDnsConfiguration", "Set-VpnConnectionTriggerTrustedNetwork", "Set-WheaMemoryPolicy", "Set-WinAcceptLanguageFromLanguageListOptOut", "Set-WinCultureFromLanguageListOptOut", "Set-WinDefaultInputMethodOverride", "Set-WindowsEdition", "Set-WindowsProductKey", "Set-WindowsReservedStorageState", "Set-WindowsSearchSetting", "Set-WinHomeLocation", "Set-WinLanguageBarOption", "Set-WinSystemLocale", "Set-WinUILanguageOverride", "Set-WinUserLanguageList", "Set-WmiInstance", "Set-WSManInstance", "Set-WSManQuickConfig", "Should", "Show-Command", "Show-ControlPanelItem", "Show-EventLog", "Show-NetFirewallRule", "Show-NetIPsecRule", "Show-StorageHistory", "Show-VirtualDisk", "Show-WindowsDeveloperLicenseRegistration", "Sort-Object", "Split-Path", "Split-WindowsImage", "Start-AppBackgroundTask", "Start-AppvVirtualProcess", "Start-AutologgerConfig", "Start-BitsTransfer", "Start-DscConfiguration", "Start-Dtc", "Start-DtcDiagnosticResourceManager", "Start-DtcTransactionsTraceSession", "Start-EtwTraceSession", "Start-Job", "Start-MpScan", "Start-MpWDOScan", "Start-NetEventSession", "Start-OSUninstall", "Start-PcsvDevice", "Start-Process", "Start-ScheduledTask", "Start-Service", "Start-Sleep", "Start-StorageDiagnosticLog", "Start-Trace", "Start-Transaction", "Start-Transcript", "Stop-AppvClientConnectionGroup", "Stop-AppvClientPackage", "Stop-Computer", "Stop-DscConfiguration", "Stop-Dtc", "Stop-DtcDiagnosticResourceManager", "Stop-DtcTransactionsTraceSession", "Stop-EtwTraceSession", "Stop-Job", "Stop-NetEventSession", "Stop-PcsvDevice", "Stop-Process", "Stop-ScheduledTask", "Stop-Service", "Stop-StorageDiagnosticLog", "Stop-StorageJob", "Stop-Trace", "Stop-Transcript", "Suspend-BitLocker", "Suspend-BitsTransfer", "Suspend-Job", "Suspend-PrintJob", "Suspend-Service", "Suspend-StorageBusDisk", "Switch-Certificate", "Sync-AppvPublishingServer", "Sync-NetIPsecRule", "T:", "TabExpansion2", "Tee-Object", "Test-AppLockerPolicy", "Test-Certificate", "Test-ComputerSecureChannel", "Test-Connection", "Test-DscConfiguration", "Test-Dtc", "Test-FileCatalog", "Test-KdsRootKey", "Test-ModuleManifest", "Test-NetConnection", "Test-Path", "Test-PSSessionConfigurationFile", "Test-ScriptFileInfo", "Test-UevTemplate", "Test-WSMan", "Trace-Command", "U:", "Unblock-File", "Unblock-FileShareAccess", "Unblock-SmbShareAccess", "Unblock-Tpm", "Undo-DtcDiagnosticTransaction", "Undo-Transaction", "Uninstall-Dtc", "Uninstall-Module", "Uninstall-Package", "Uninstall-ProvisioningPackage", "Uninstall-Script", "Uninstall-TrustedProvisioningCertificate", "Unlock-BitLocker", "Unprotect-CmsMessage", "Unpublish-AppvClientPackage", "Unregister-AppBackgroundTask", "Unregister-ClusteredScheduledTask", "Unregister-Event", "Unregister-IscsiSession", "Unregister-PackageSource", "Unregister-PSRepository", "Unregister-PSSessionConfiguration", "Unregister-ScheduledJob", "Unregister-ScheduledTask", "Unregister-StorageSubsystem", "Unregister-UevTemplate", "Unregister-WindowsDeveloperLicense", "Update-AutologgerConfig", "Update-Disk", "Update-DscConfiguration", "Update-DscConfiguration", "Update-EtwTraceSession", "Update-FormatData", "Update-Help", "Update-HostStorageCache", "Update-IscsiTarget", "Update-IscsiTargetPortal", "Update-List", "Update-Module", "Update-ModuleManifest", "Update-MpSignature", "Update-NetFirewallDynamicKeywordAddress", "Update-NetIPsecRule", "Update-Script", "Update-ScriptFileInfo", "Update-SmbMultichannelConnection", "Update-StorageFirmware", "Update-StoragePool", "Update-StorageProviderCache", "Update-TypeData", "Update-UevTemplate", "Update-WIMBootEntry", "Use-Transaction", "Use-WindowsUnattend", "V:", "W:", "Wait-Debugger", "Wait-Event", "Wait-Job", "Wait-Process", "Where-Object", "Write-Debug", "Write-DtcTransactionsTraceSession", "Write-Error", "Write-EventLog", "Write-FileSystemCache", "Write-Host", "Write-Information", "Write-Output", "Write-PrinterNfcTag", "Write-Progress", "Write-Verbose", "Write-VolumeCache", "Write-Warning", "X:", "Y:", "Z:",
    },
    nginx = {
        -- generated by
        -- cat /usr/share/nvim/runtime/syntax/nginx.vim | awk '(!/^"/)&&($2=="keyword"){for(i=4;i<=NF;i++){print$i}}' | sort -fu | awk '{cc=tolower(substr($0,1,1));if(c!=cc){print""}printf" \""$0"\",";c=cc}'
        "absolute_redirect", "acceptex_read", "accept_filter", "accept_mutex", "accept_mutex_delay", "access_by_lua", "access_by_lua_block", "access_by_lua_file", "access_by_lua_no_postpone", "access_handler_code", "access_handler_name", "access_handler_property", "access_handler_type", "accesskey", "accesskey_arg", "accesskey_hashmethod", "accesskey_signature", "access_log", "access_log_bypass_if", "ack_window", "add_after_body", "add_before_body", "add_header", "addition_types", "aio", "aio_write", "alias", "allow", "always_read_body", "ancient_browser", "ancient_browser_value", "application", "array_join", "array_map", "array_map_op", "array_split", "auth_basic", "auth_basic_user_file", "auth_digest", "auth_digest_expires", "auth_digest_replays", "auth_digest_shm_size", "auth_digest_timeout", "auth_digest_user_file", "auth_gss", "auth_gss_allow_basic_fallback", "auth_gss_authorized_principal", "auth_gss_keytab", "auth_gss_realm", "auth_gss_service_name", "auth_http", "auth_http_header", "auth_http_pass_client_cert", "auth_http_timeout", "auth_jwt", "auth_jwt_key_file", "auth_pam", "auth_pam_service_name", "auth_request", "auth_request_set", "autoindex", "autoindex_exact_size", "autoindex_format", "autoindex_localtime", "auto_upgrade_ws", "aws_access_key", "aws_endpoint", "aws_key_scope", "aws_s3_bucket", "aws_sign", "aws_signing_key",
        "backlog", "backtrace_log", "backtrace_max_stack_size", "backup", "balancer_by_lua_block", "balancer_by_lua_file", "basic_rule", "BasicRule", "bind", "body_filter_by_lua", "body_filter_by_lua_block", "body_filter_by_lua_file", "break", "brotli", "brotli_buffers", "brotli_comp_level", "brotli_min_length", "brotli_static", "brotli_types", "brotli_window",
        "charset", "charset_map", "charset_types", "check", "check_http_expect_alive", "check_http_send", "check_rule", "CheckRule", "check_shm_size", "check_smtp_expect_alive", "check_smtp_send", "check_status", "chunked_transfer_encoding", "chunkin", "chunkin_keepalive", "chunkin_max_chunks_per_buf", "chunkin_resume", "chunk_size", "circle_gif", "circle_gif_max_radius", "circle_gif_min_radius", "circle_gif_step_radius", "client_body_buffer_size", "client_body_in_file_only", "client_body_in_single_buffer", "client_body_temp_path", "client_body_timeout", "client_header_buffer_size", "client_header_timeout", "client_max_body_size", "concat", "concat_delimiter", "concat_ignore_file_error", "concat_max_files", "concat_types", "concat_unique", "connection_pool_size", "connections", "consistent_hash", "contained", "content_by_lua", "content_by_lua_block", "content_by_lua_file", "content_handler_code", "content_handler_name", "content_handler_property", "content_handler_type", "cookie", "create_full_put_path",
        "daemon", "dash", "dash_cleanup", "dash_fragment", "dash_nested", "dash_path", "dash_playlist_length", "dav_access", "dav_methods", "debug_connection", "debug_points", "default_server", "default_type", "deferred", "degradation", "degrade", "denied_url", "DeniedUrl", "deny", "devpoll_changes", "devpoll_events", "directio", "directio_alignment", "disable_symlinks", "domain", "down", "drizzle_buffer_size", "drizzle_connect_timeout", "drizzle_keepalive", "drizzle_module_header", "drizzle_pass", "drizzle_query", "drizzle_recv_cols_timeout", "drizzle_recv_rows_timeout", "drizzle_send_query_timeout", "drizzle_server", "drizzle_status", "drop_idle_publisher", "dynamic_etags", "dyups_interface", "dyups_read_msg_timeout", "dyups_shm_zone_size", "dyups_trylock", "dyups_upstream_conf",
        "echo", "echo_after_body", "echo_before_body", "echo_blocking_sleep", "echo_client_error_log_level", "echo_discard_request", "echo_duplicate", "echo_end", "echo_exec", "echo_flush", "echo_flush_wait", "echo_foreach_split", "echo_lingering_close", "echo_lingering_time", "echo_lingering_timeout", "echo_location", "echo_location_async", "echo_read_buffer_size", "echo_read_bytes", "echo_read_line", "echo_read_request_body", "echo_read_timeout", "echo_request_body", "echo_request_data", "echo_reset_timer", "echo_send_timeout", "echo_sleep", "echo_status", "echo_subrequest", "echo_subrequest_async", "empty_gif", "encrypted_session_expires", "encrypted_session_iv", "encrypted_session_key", "enhanced_memcached_allow_delete", "enhanced_memcached_allow_put", "enhanced_memcached_bind", "enhanced_memcached_buffer_size", "enhanced_memcached_connect_timeout", "enhanced_memcached_flush", "enhanced_memcached_flush_namespace", "enhanced_memcached_hash_keys_with_md5", "enhanced_memcached_pass", "enhanced_memcached_read_timeout", "enhanced_memcached_send_timeout", "enhanced_memcached_stats", "env", "epoll_events", "error", "error_log", "error_page", "etag", "eval", "eval_buffer_size", "eval_escalate", "eval_override_content_type", "eval_subrequest_in_memory", "eventport_events", "events", "exec", "exec_kill_signal", "exec_options", "exec_play", "exec_play_done", "exec_publish", "exec_publish_done", "exec_pull", "exec_push", "exec_record_done", "exec_static", "experemental", "expires",
        "f4f", "f4f_buffer_size", "fail_timeout", "fair", "fancyindex", "fancyindex_css_href", "fancyindex_default_sort", "fancyindex_directories_first", "fancyindex_exact_size", "fancyindex_footer", "fancyindex_header", "fancyindex_hide_symlinks", "fancyindex_ignore", "fancyindex_localtime", "fancyindex_name_length", "fancyindex_show_path", "fancyindex_time_format", "fastcgi_bind", "fastcgi_buffering", "fastcgi_buffers", "fastcgi_buffer_size", "fastcgi_busy_buffers_size", "fastcgi_cache", "fastcgi_cache_bypass", "fastcgi_cache_key", "fastcgi_cache_lock", "fastcgi_cache_lock_age", "fastcgi_cache_lock_timeout", "fastcgi_cache_max_range_offset", "fastcgi_cache_methods", "fastcgi_cache_min_uses", "fastcgi_cache_path", "fastcgi_cache_purge", "fastcgi_cache_revalidate", "fastcgi_cache_use_stale", "fastcgi_cache_valid", "fastcgi_catch_stderr", "fastcgi_connect_timeout", "fastcgi_force_ranges", "fastcgi_hide_header", "fastcgi_ignore_client_abort", "fastcgi_ignore_headers", "fastcgi_index", "fastcgi_intercept_errors", "fastcgi_keep_conn", "fastcgi_limit_rate", "fastcgi_max_temp_file_size", "fastcgi_next_upstream", "fastcgi_next_upstream_timeout", "fastcgi_next_upstream_tries", "fastcgi_no_cache", "fastcgi_param", "fastcgi_pass", "fastcgi_pass_header", "fastcgi_pass_request_body", "fastcgi_pass_request_headers", "fastcgi_read_timeout", "fastcgi_request_buffering", "fastcgi_send_lowat", "fastcgi_send_timeout", "fastcgi_split_path_info", "fastcgi_store", "fastcgi_store_access", "fastcgi_temp_file_write_size", "fastcgi_temp_path", "fastcgi_upstream_fail_timeout", "fastcgi_upstream_max_fails", "fastopen", "FileETag", "file_in_unzip", "file_in_unzip_archivefile", "file_in_unzip_extract", "flv", "footer", "footer_if", "footer_types", "form_auth", "form_auth_login", "form_auth_pam_service", "form_auth_password", "form_auth_remote_user",
        "g2o", "g2o_key", "g2o_nonce", "geo", "geoip2", "geoip_city", "geoip_country", "geoip_country_file", "geoip_org", "geoip_proxy", "geoip_proxy_recursive", "google_perftools_profiles", "gridfs", "gunzip", "gunzip_buffers", "gzip", "gzip_buffers", "gzip_comp_level", "gzip_disable", "gzip_hash", "gzip_http_version", "gzip_min_length", "gzip_no_buffer", "gzip_proxied", "gzip_static", "gzip_types", "gzip_vary", "gzip_window",
        "handlers_lazy_init", "hash", "hash_again", "hash_key", "header_filter_by_lua", "header_filter_by_lua_block", "header_filter_by_lua_file", "header_filter_code", "header_filter_name", "header_filter_property", "header_filter_type", "health_check", "healthcheck_buffer", "healthcheck_delay", "healthcheck_enabled", "healthcheck_expected", "healthcheck_failcount", "healthcheck_send", "healthcheck_status", "health_check_timeout", "healthcheck_timeout", "hls", "hls_base_url", "hls_buffers", "hls_cleanup", "hls_continuous", "hls_forward_args", "hls_fragment", "hls_fragment_naming", "hls_fragment_slicing", "hls_fragments_per_key", "hls_key_path", "hls_keys", "hls_key_url", "hls_mp4_buffer_size", "hls_mp4_max_buffer_size", "hls_nested", "hls_path", "hls_playlist_length", "hls_sync", "hls_type", "hls_variant", "http", "http2", "http2_body_preread_size", "http2_chunk_size", "http2_idle_timeout", "http2_max_concurrent_streams", "http2_max_field_size", "http2_max_header_size", "http2_max_requests", "http2_push", "http2_push_preload", "http2_recv_buffer_size", "http2_recv_timeout", "http3", "http3_hq", "http3_max_concurrent_pushes", "http3_max_concurrent_streams", "http3_push", "http3_push_preload", "http3_stream_buffer_size", "http_403", "http_404", "http_429", "http_500", "http_502", "http_503", "http_504", "http_accounting", "http_accounting_id", "http_accounting_interval", "http_accounting_log", "http_accounting_perturb", "httponly",
        "iconv_buffer_size", "iconv_filter", "idle_streams", "if", "if_modified_since", "ignore_invalid_headers", "image_filter", "image_filter_buffer", "image_filter_interlace", "image_filter_jpeg_quality", "image_filter_sharpen", "image_filter_transparency", "image_filter_webp_quality", "imap", "imap_auth", "imap_capabilities", "imap_client_buffer", "include", "index", "init_by_lua", "init_by_lua_block", "init_by_lua_file", "init_worker_by_lua", "init_worker_by_lua_block", "init_worker_by_lua_file", "interleave", "internal", "internal_redirect_if", "internal_redirect_if_no_postponed", "invalid_header", "iocp_threads", "ip2location_database", "ip_blocker", "ip_hash", "ipv6only",
        "jdomain", "js", "js_access", "js_content", "js_filter", "js_filter_types", "js_include", "js_load", "js_maxmem", "js_preread", "js_require", "js_set", "js_utf8", "jvm_classpath", "jvm_classpath_check", "jvm_exit_handler_code", "jvm_exit_handler_name", "jvm_handler_type", "jvm_init_handler_code", "jvm_init_handler_name", "jvm_options", "jvm_path", "jvm_var", "jvm_workers",
        "keepalive", "keepalive_disable", "keepalive_requests", "keepalive_timeout", "keepidle", "ketama_chash", "kqueue_changes", "kqueue_events",
        "lang_cookie", "lang_get_var", "lang_host", "lang_list", "lang_post_var", "lang_referer", "large_client_header_buffers", "learn", "learning_mode", "LearningMode", "least_conn", "least_time", "libinjection_sql", "LibInjectionSql", "libinjection_xss", "LibInjectionXss", "limit_conn", "limit_conn_dry_run", "limit_conn_log_level", "limit_conn_status", "limit_conn_zone", "limit_except", "limit_rate", "limit_rate_after", "limit_req", "limit_req_dry_run", "limit_req_log_level", "limit_req_status", "limit_req_zone", "limit_upload_rate", "limit_upload_rate_after", "limit_upstream_conn", "limit_upstream_log_level", "limit_upstream_zone", "limit_zone", "lingering_close", "lingering_time", "lingering_timeout", "listen", "live", "load_module", "location", "lock_file", "log_by_lua", "log_by_lua_block", "log_by_lua_file", "log_format", "log_not_found", "log_request_speed_filter", "log_request_speed_filter_timeout", "log_subrequest", "log_zmq_endpoint", "log_zmq_format", "log_zmq_off", "log_zmq_server", "lower", "lua_check_client_abort", "lua_code_cache", "lua_file", "lua_http10_buffering", "lua_lingering_close", "lua_lingering_time", "lua_lingering_timeout", "lua_malloc_trim", "lua_max_pending_timers", "lua_max_running_timers", "lua_need_request_body", "lua_package_cpath", "lua_package_path", "lua_regex_cache_max_entries", "lua_regex_match_limit", "lua_resolver", "lua_resolver_timeout", "lua_shared_dict", "lua_socket_buffer_size", "lua_socket_connect_timeout", "lua_socket_keepalive_timeout", "lua_socket_log_errors", "lua_socket_pool_size", "lua_socket_read_timeout", "lua_socket_send_lowat", "lua_socket_send_timeout", "lua_ssl_ciphers", "lua_ssl_crl", "lua_ssl_protocols", "lua_ssl_trusted_certificate", "lua_ssl_verify_depth", "lua_transform_underscores_in_response_headers", "lua_use_default_type",
        "mail", "main_rule", "MainRule", "map", "map_hash_bucket_size", "map_hash_max_size", "master_process", "match", "max_connections", "max_connections_max_queue_length", "max_connections_queue_timeout", "max_conns", "max_fails", "max_message", "max_queue", "max_ranges", "max_streams", "md5_filter", "memcached_bind", "memcached_buffer_size", "memcached_connect_timeout", "memcached_force_ranges", "memcached_gzip_flag", "memcached_next_upstream", "memcached_next_upstream_timeout", "memcached_next_upstream_tries", "memcached_pass", "memcached_read_timeout", "memcached_send_timeout", "memc_buffer_size", "memc_cmds_allowed", "memc_connect_timeout", "memc_flags_to_last_modified", "memc_next_upstream", "memc_pass", "memc_read_timeout", "memc_send_timeout", "memc_upstream_fail_timeout", "memc_upstream_max_fails", "merge_slashes", "meta", "min_delete_depth", "modern_browser", "modern_browser_value", "ModSecurityConfig", "ModSecurityEnabled", "mogilefs_class", "mogilefs_connect_timeout", "mogilefs_domain", "mogilefs_methods", "mogilefs_noverify", "mogilefs_pass", "mogilefs_read_timeout", "mogilefs_send_timeout", "mogilefs_tracker", "mongo_auth", "mongo_bind", "mongo_buffering", "mongo_buffers", "mongo_buffer_size", "mongo_busy_buffers_size", "mongo_connect_timeout", "mongo_json", "mongo_next_upstream", "mongo_pass", "mongo_query", "mongo_read_timeout", "mongo_send_timeout", "more_clear_headers", "more_clear_input_headers", "more_set_headers", "more_set_input_headers", "mp4", "mp4_buffer_size", "mp4_limit_rate", "mp4_limit_rate_after", "mp4_max_buffer_size", "msie_padding", "msie_refresh", "multi_accept", "mysql_test",
        "nchan_access_control_allow_origin", "nchan_authorize_request", "nchan_channel_events_channel_id", "nchan_channel_event_string", "nchan_channel_group", "nchan_channel_group_accounting", "nchan_channel_id", "nchan_channel_id_split_delimiter", "nchan_channel_timeout", "nchan_eventsource_event", "nchan_group_location", "nchan_group_max_channels", "nchan_group_max_messages", "nchan_group_max_messages_disk", "nchan_group_max_messages_memory", "nchan_group_max_subscribers", "nchan_longpoll_multipart_response", "nchan_max_channel_id_length", "nchan_max_channel_subscribers", "nchan_max_reserved_memory", "nchan_message_buffer_length", "nchan_message_timeout", "nchan_publisher", "nchan_publisher_channel_id", "nchan_publisher_upstream_request", "nchan_pubsub", "nchan_redis_idle_channel_cache_timeout", "nchan_redis_namespace", "nchan_redis_pass", "nchan_redis_ping_interval", "nchan_redis_server", "nchan_redis_storage_mode", "nchan_redis_url", "nchan_storage_engine", "nchan_store_messages", "nchan_stub_status", "nchan_subscribe_existing_channels_only", "nchan_subscriber", "nchan_subscriber_channel_id", "nchan_subscriber_compound_etag_message_id", "nchan_subscribe_request", "nchan_subscriber_first_message", "nchan_subscriber_http_raw_stream_separator", "nchan_subscriber_last_message_id", "nchan_subscriber_message_id_custom_etag_header", "nchan_subscriber_timeout", "nchan_unsubscribe_request", "nchan_use_redis", "nchan_websocket_ping_interval", "nextgroup=ngxGzipOn,ngxGzipOff", "nextgroup=ngxMailProtocol", "nextgroup=ngxSSLPreferServerCiphersOff,ngxSSLPreferServerCiphersOn", "nextgroup=ngxSSLProtocol", "nextgroup=ngxSSLProtocol,ngxSSLProtocolDeprecated", "nextgroup=ngxSSLSessionTicketsOn,ngxSSLSessionTicketsOff", "ngx_hls_audio_track", "ngx_hls_audio_track_output_format", "ngx_hls_audio_track_output_header", "ngx_hls_audio_track_rootpath", "non_idempotent", "notice", "notice_type", "notify_method", "notify_relay_redirect", "notify_update_strict", "notify_update_timeout", "ntlm",
        "ocsp_cache_timeout", "ocsp_proxy", "off", "omallow", "omdeny", "on", "on_connect", "on_done", "on_play", "on_play_done", "on_publish", "on_publish_done", "on_record_done", "on_start", "on_stop", "on_update", "open_file_cache", "open_file_cache_errors", "open_file_cache_events", "open_file_cache_min_uses", "open_file_cache_retest", "open_file_cache_valid", "open_log_file_cache", "openssl_builddate_minimum", "openssl_version_minimum", "optimize_server_names", "out_cork", "output_buffers", "out_queue", "override_charset",
        "pagespeed", "passenger_abort_websockets_on_process_shutdown", "passenger_app_env", "passenger_app_file_descriptor_ulimit", "passenger_app_group_name", "passenger_app_root", "passenger_app_type", "passenger_base_uri", "passenger_buffer_response", "passenger_buffers,", "passenger_buffer_size,", "passenger_busy_buffers_size", "passenger_concurrency_model", "passenger_core_file_descriptor_ulimit", "passenger_data_buffer_dir", "passenger_debugger", "passenger_debug_log_file", "passenger_default_group", "passenger_default_user", "passenger_disable_security_update_check", "passenger_document_root", "passenger_enabled", "passenger_env_var", "passenger_file_descriptor_log_file", "passenger_fly_with", "passenger_force_max_concurrent_requests_per_process", "passenger_friendly_error_pages", "passenger_group", "passenger_headers_hash_bucket_size", "passenger_headers_hash_max_size", "passenger_ignore_client_abort", "passenger_ignore_headers", "passenger_instance_registry_dir", "passenger_intercept_errors", "passenger_load_shell_envvars", "passenger_log_file", "passenger_log_level", "passenger_max_instances", "passenger_max_instances_per_app", "passenger_max_pool_size", "passenger_max_preloader_idle_time", "passenger_max_request_queue_size", "passenger_max_requests", "passenger_max_request_time", "passenger_memory_limit", "passenger_meteor_app_settings", "passenger_min_instances", "passenger_nodejs", "passenger_pass_header", "passenger_pool_idle_time", "passenger_pre_start", "passenger_python", "passenger_request_queue_overflow_status_code", "passenger_resist_deployment_errors", "passenger_response_buffer_high_watermark", "passenger_restart_dir", "passenger_rolling_restarts", "passenger_root", "passenger_ruby", "passenger_security_update_check_proxy", "passenger_set_header", "passenger_show_version_in_header", "passenger_socket_backlog", "passenger_spawn_method", "passenger_start_timeout", "passenger_startup_file", "passenger_stat_throttle_rate", "passenger_sticky_sessions", "passenger_sticky_sessions_cookie_name", "passenger_thread_count", "passenger_user", "passenger_user_switching", "path", "pcre_jit", "perl", "perl_modules", "perl_require", "perl_set", "php_session_parse", "php_session_strip_formatting", "pid", "ping", "ping_timeout", "play", "play_local_path", "play_restart", "play_temp_path", "pool_context", "pool_context_hash_size", "pop3", "pop3_auth", "pop3_capabilities", "port_in_redirect", "post_acceptex", "post_action", "postgres_connect_timeout", "postgres_escape", "postgres_keepalive", "postgres_output", "postgres_pass", "postgres_query", "postgres_result_timeout", "postgres_rewrite", "postgres_server", "postgres_set", "postpone_gzipping", "postpone_output", "preread_buffer_size", "preread_timeout", "protocol", "proxy", "proxy_bind", "proxy_buffer", "proxy_buffering", "proxy_buffers", "proxy_buffer_size", "proxy_busy_buffers_size", "proxy_cache", "proxy_cache_bypass", "proxy_cache_convert_head", "proxy_cache_key", "proxy_cache_lock", "proxy_cache_lock_age", "proxy_cache_lock_timeout", "proxy_cache_max_range_offset", "proxy_cache_methods", "proxy_cache_min_uses", "proxy_cache_path", "proxy_cache_purge", "proxy_cache_revalidate", "proxy_cache_use_stale", "proxy_cache_valid", "proxy_connect_timeout", "proxy_cookie_domain", "proxy_cookie_path", "proxy_download_rate", "proxy_force_ranges", "proxy_headers_hash_bucket_size", "proxy_headers_hash_max_size", "proxy_hide_header", "proxy_http_version", "proxy_ignore_client_abort", "proxy_ignore_headers", "proxy_intercept_errors", "proxy_limit_rate", "proxy_max_temp_file_size", "proxy_method", "proxy_next_upstream", "proxy_next_upstream_timeout", "proxy_next_upstream_tries", "proxy_no_cache", "proxy_pass", "proxy_pass_error_message", "proxy_pass_header", "proxy_pass_request_body", "proxy_pass_request_headers", "proxy_protocol", "proxy_protocol_timeout", "proxy_read_timeout", "proxy_redirect", "proxy_request_buffering", "proxy_responses", "proxy_send_lowat", "proxy_send_timeout", "proxy_set_body", "proxy_set_header", "proxy_ssl_certificate", "proxy_ssl_certificate_key", "proxy_ssl_ciphers", "proxy_ssl_crl", "proxy_ssl_name", "proxy_ssl_password_file", "proxy_ssl_protocols", "proxy_ssl_server_name", "proxy_ssl_session_reuse", "proxy_ssl_trusted_certificate", "proxy_ssl_verify", "proxy_ssl_verify_depth", "proxy_store", "proxy_store_access", "proxy_temp_file_write_size", "proxy_temp_path", "proxy_timeout", "proxy_upload_rate", "proxy_write_timeout", "pubcookie_addl_request", "pubcookie_app_id", "pubcookie_app_srv_id", "pubcookie_auth_type_names", "pubcookie_catenate_app_ids", "pubcookie_crypt_key_file", "pubcookie_dir_depth", "pubcookie_domain", "pubcookie_egd_device", "pubcookie_encryption", "pubcookie_end_session", "pubcookie_granting_cert_file", "pubcookie_hard_expire", "pubcookie_inactive_expire", "pubcookie_login", "pubcookie_login_method", "pubcookie_no_blank", "pubcookie_no_clean_creds", "pubcookie_no_obscure_cookies", "pubcookie_no_prompt", "pubcookie_on_demand", "pubcookie_post", "pubcookie_session_cert_file", "pubcookie_session_key_file", "pubcookie_session_reauth", "pubcookie_set_remote_user", "pubcookie_super_debug", "publish_notify", "pull", "push", "push_buffer_size", "push_listener", "push_message_timeout", "push_queue_messages", "push_reconnect", "push_sender", "push_stream_allow_connections_to_events_channel", "push_stream_allowed_origins", "push_stream_authorized_channels_only", "push_stream_channel_deleted_message_text", "push_stream_channel_inactivity_time", "push_stream_channel_info_on_publish", "push_stream_channels_path", "push_stream_channels_statistics", "push_stream_events_channel_id", "push_stream_footer_template", "push_stream_header_template", "push_stream_header_template_file", "push_stream_last_event_id", "push_stream_last_received_message_tag", "push_stream_last_received_message_time", "push_stream_longpolling_connection_ttl", "push_stream_max_channel_id_length", "push_stream_max_messages_stored_per_channel", "push_stream_max_number_of_channels", "push_stream_max_number_of_wildcard_channels", "push_stream_max_subscribers_per_channel", "push_stream_message_template", "push_stream_message_ttl", "push_stream_padding_by_user_agent", "push_stream_ping_message_interval", "push_stream_ping_message_text", "push_stream_publisher", "push_stream_shared_memory_size", "push_stream_store_messages", "push_stream_subscriber", "push_stream_subscriber_connection_ttl", "push_stream_timeout_with_body", "push_stream_user_agent", "push_stream_websocket_allow_publish", "push_stream_wildcard_channel_max_qtd", "push_stream_wildcard_channel_prefix",
        "queue", "quic", "quic_gso", "quic_host_key", "quic_mtu", "quic_retry",
        "rails_spawn_method", "random_index", "rcvbuf", "rdns", "rdns_allow", "rdns_deny", "rds_csv", "rds_csv_buffer_size", "rds_csv_content_type", "rds_csv_field_name_header", "rds_csv_field_separator", "rds_csv_row_terminator", "rds_json", "rds_json_buffer_size", "rds_json_content_type", "rds_json_errcode_key", "rds_json_errstr_key", "rds_json_format", "rds_json_ret", "rds_json_root", "rds_json_success_property", "rds_json_user_property", "read_ahead", "real_ip_header", "real_ip_recursive", "record", "record_append", "recorder", "record_interval", "record_lock", "record_max_frames", "record_max_size", "record_notify", "record_path", "record_suffix", "record_unique", "recursive_error_pages", "redis2_buffer_size", "redis2_connect_timeout", "redis2_literal_raw_query", "redis2_next_upstream", "redis2_pass", "redis2_query", "redis2_raw_queries", "redis2_raw_query", "redis2_read_timeout", "redis2_send_timeout", "redis_bind", "redis_buffer_size", "redis_connect_timeout", "redis_gzip_flag", "redis_next_upstream", "redis_pass", "redis_read_timeout", "redis_send_timeout", "referer_hash_bucket_size", "referer_hash_max_size", "replace_filter", "replace_filter_last_modified", "replace_filter_max_buffered_size", "replace_filter_skip", "replace_filter_types", "report_uploads", "request_pool_size", "reset_timedout_connection", "resolve", "resolver", "resolver_timeout", "respawn", "respawn_timeout", "return", "reuseport", "rewrite", "rewrite_by_lua", "rewrite_by_lua_block", "rewrite_by_lua_file", "rewrite_by_lua_no_postpone", "rewrite_handler_code", "rewrite_handler_name", "rewrite_handler_property", "rewrite_handler_type", "rewrite_log", "root", "route", "rrd_graph", "rrd_graph_root", "rtmp", "rtmp_auto_push", "rtmp_auto_push_reconnect", "rtmp_control", "rtmp_socket_dir", "rtmp_stat", "rtmp_stat_stylesheet", "rtmpt_proxy", "rtmpt_proxy_http_timeout", "rtmpt_proxy_rtmp_timeout", "rtmpt_proxy_stat", "rtmpt_proxy_stylesheet", "rtmpt_proxy_target", "rtsig_overflow_events", "rtsig_overflow_test", "rtsig_overflow_threshold", "rtsig_signo", "rules_disabled", "rules_enabled",
        "sass_compile", "sass_error_log", "sass_include_path", "sass_indent", "sass_is_indented_syntax", "sass_linefeed", "sass_output_style", "sass_precision", "sass_source_comments", "sass_source_map_embed", "satisfy", "satisfy_any", "scgi_bind", "scgi_buffering", "scgi_buffers", "scgi_buffer_size", "scgi_busy_buffers_size", "scgi_cache", "scgi_cache_bypass", "scgi_cache_key", "scgi_cache_lock", "scgi_cache_lock_age", "scgi_cache_lock_timeout", "scgi_cache_max_range_offset", "scgi_cache_methods", "scgi_cache_min_uses", "scgi_cache_path", "scgi_cache_purge", "scgi_cache_revalidate", "scgi_cache_use_stale", "scgi_cache_valid", "scgi_connect_timeout", "scgi_force_ranges", "scgi_hide_header", "scgi_ignore_client_abort", "scgi_ignore_headers", "scgi_intercept_errors", "scgi_limit_rate", "scgi_max_temp_file_size", "scgi_next_upstream", "scgi_next_upstream_timeout", "scgi_next_upstream_tries", "scgi_no_cache", "scgi_param", "scgi_pass", "scgi_pass_header", "scgi_pass_request_body", "scgi_pass_request_headers", "scgi_read_timeout", "scgi_request_buffering", "scgi_send_timeout", "scgi_store", "scgi_store_access", "scgi_temp_file_write_size", "scgi_temp_path", "SecRulesDisabled", "SecRulesEnabled", "secure", "secure_download", "secure_download_path_mode", "secure_download_secret", "secure_link", "secure_link_md5", "secure_link_secret", "selective_cache_purge_query", "selective_cache_purge_redis_database", "selective_cache_purge_redis_host", "selective_cache_purge_redis_port", "selective_cache_purge_redis_unix_socket", "sendfile", "sendfile_max_chunk", "send_lowat", "send_timeout", "server", "server_name", "server_name_in_redirect", "server_names_hash_bucket_size", "server_names_hash_max_size", "server_tokens", "service", "session_log", "session_log_format", "session_log_zone", "session_relay", "set", "set_base32_alphabet", "set_base32_padding", "set_by_lua", "set_by_lua_block", "set_by_lua_file", "set_cconv_to_simp", "set_cconv_to_trad", "set_decode_base32", "set_decode_base64", "set_decode_hex", "set_decrypt_session", "set_encode_base32", "set_encode_base64", "set_encode_hex", "set_encrypt_session", "set_escape_uri", "setfib", "set_formatted_gmt_time", "set_formatted_local_time", "set_form_input", "set_form_input_multi", "set_from_accept_language", "set_hashed_upstream", "set_hmac_sha1", "set_iconv", "set_if_empty", "set_lang", "set_lang_method", "set_local_today", "set_md5", "set_md5_upper", "set_misc_base32_padding", "set_murmur2", "set_murmur2_upper", "set_pinyin_to_normal", "set_quote_json_str", "set_quote_pgsql_str", "set_quote_sql_str", "set_random", "set_real_ip_from", "set_rotate", "set_secure_random_alphanum", "set_secure_random_lcalpha", "set_sha1", "set_sha1_upper", "set_unescape_uri", "sflow", "shared_map", "shib_request", "shib_request_set", "shib_request_use_headers", "skipwhite", "slice", "slice_arg_begin", "slice_arg_end", "slice_footer", "slice_footer_last", "slice_header", "slice_header_first", "slowfs_big_file_size", "slowfs_cache", "slowfs_cache_key", "slowfs_cache_min_uses", "slowfs_cache_path", "slowfs_cache_purge", "slowfs_cache_valid", "slowfs_temp_path", "slow_start", "small_light", "small_light_buffer", "small_light_getparam_mode", "small_light_imlib2_temp_dir", "small_light_material_dir", "small_light_pattern_define", "small_light_radius_max", "small_light_sigma_max", "smrzr_filename", "smrzr_ratio", "smtp", "smtp_auth", "smtp_capabilities", "smtp_client_buffer", "smtp_greeting_delay", "sndbuf", "so_keepalive", "sorted_querystring_filter_parameter", "source_charset", "spdy", "spdy_chunk_size", "spdy_headers_comp", "spdy_keepalive_timeout", "spdy_max_concurrent_streams", "spdy_pool_size", "spdy_recv_buffer_size", "spdy_recv_timeout", "spdy_streams_index_size", "sphinx2_bind", "sphinx2_buffer_size", "sphinx2_connect_timeout", "sphinx2_next_upstream", "sphinx2_pass", "sphinx2_read_timeout", "sphinx2_send_timeout", "split_clients", "srcache_default_expire", "srcache_fetch", "srcache_fetch_skip", "srcache_header_buffer_size", "srcache_ignore_content_encoding", "srcache_max_expire", "srcache_methods", "srcache_request_cache_control", "srcache_response_cache_control", "srcache_store", "srcache_store_hide_header", "srcache_store_max_size", "srcache_store_no_cache", "srcache_store_no_store", "srcache_store_pass_header", "srcache_store_private", "srcache_store_ranges", "srcache_store_skip", "srcache_store_statuses", "ssi", "ssi_ignore_recycled_buffers", "ssi_last_modified", "ssi_min_file_chunk", "ssi_silent_errors", "ssi_types", "ssi_value_length", "ssl", "ssl_buffer_size", "ssl_certificate", "ssl_certificate_by_lua_block", "ssl_certificate_by_lua_file", "ssl_certificate_key", "ssl_ciphers", "ssl_client_certificate", "ssl_conf_command", "ssl_crl", "ssl_ct", "ssl_ct_static_scts", "ssl_dhparam", "ssl_dyn_rec_enable", "ssl_dyn_rec_size_hi", "ssl_dyn_rec_size_lo", "ssl_dyn_rec_threshold", "ssl_dyn_rec_timeout", "ssl_early_data", "ssl_ecdh_curve", "ssl_engine", "ssl_handshake_timeout", "ssl_password_file", "ssl_prefer_server_ciphers", "ssl_preread", "ssl_protocols", "ssl_reject_handshake", "ssl_session_cache", "ssl_session_fetch_by_lua_block", "ssl_session_fetch_by_lua_file", "ssl_session_store_by_lua_block", "ssl_session_store_by_lua_file", "ssl_session_ticket_key", "ssl_session_tickets", "ssl_session_timeout", "ssl_stapling", "ssl_stapling_file", "ssl_stapling_responder", "ssl_stapling_verify", "ssl_trusted_certificate", "SSLv2", "SSLv3", "ssl_verify_client", "ssl_verify_depth", "sssd_info", "sssd_info_attribute", "sssd_info_attributes", "sssd_info_attribute_separator", "sssd_info_group", "sssd_info_groups", "sssd_info_group_separator", "sssd_info_output_to", "starttls", "state", "statsd_count", "statsd_sample_rate", "statsd_server", "statsd_timing", "status", "status_format", "status_zone", "sticky", "sticky_cookie_insert", "strip", "stub_status", "sub_filter", "sub_filter_last_modified", "sub_filter_once", "sub_filter_types", "subrange", "subs_filter", "subs_filter_types", "supervisord", "supervisord_inherit_backend_status", "supervisord_name", "supervisord_start", "supervisord_stop", "sync",
        "tcp", "tcp_nodelay", "tcp_nopush", "testcookie", "testcookie_arg", "testcookie_deny_keepalive", "testcookie_domain", "testcookie_expires", "testcookie_fallback", "testcookie_get_only", "testcookie_httponly_flag", "testcookie_https_location", "testcookie_internal", "testcookie_max_attempts", "testcookie_name", "testcookie_p3p", "testcookie_pass", "testcookie_path", "testcookie_redirect_via_refresh", "testcookie_refresh_encrypt_cookie", "testcookie_refresh_encrypt_cookie_key", "testcookie_refresh_encrypt_iv", "testcookie_refresh_status", "testcookie_refresh_template", "testcookie_secret", "testcookie_secure_flag", "testcookie_session", "testcookie_whitelist", "thread_pool", "thread_stack_size", "timeout", "timer_resolution", "tnt_buffer_size", "tnt_connect_timeout", "tnt_http_allowed_methods", "tnt_http_methods", "tnt_http_rest_methods", "tnt_method", "tnt_next_upstream", "tnt_next_upstream_timeout", "tnt_next_upstream_tries", "tnt_pass", "tnt_pass_http_request", "tnt_pass_http_request_buffer_size", "tnt_read_timeout", "tnt_send_timeout", "track_uploads", "try_files", "types", "types_filter", "types_filter_use_default", "types_hash_bucket_size", "types_hash_max_size",
        "underscores_in_headers", "uninitialized_variable_warn", "union_station_filter", "union_station_gateway_address", "union_station_gateway_cert", "union_station_gateway_port", "union_station_key", "union_station_proxy_address", "union_station_support", "upload_aggregate_form_field", "upload_buffer_size", "upload_cleanup", "upload_limit_rate", "upload_max_file_size", "upload_max_output_body_len", "upload_max_part_header_len", "upload_pass", "upload_pass_args", "upload_pass_form_field", "upload_progress", "upload_progress_content_type", "upload_progress_header", "upload_progress_json_output", "upload_progress_jsonp_output", "upload_progress_jsonp_parameter", "upload_progress_template", "upload_resumable", "upload_set_form_field", "upload_state_store", "upload_store", "upload_store_access", "upload_tame_arrays", "upper", "upstream", "upstream_conf", "upstream_fair_shm_size", "upstream_list", "upstream_show", "upsync", "upsync_dump_path", "upsync_lb", "upsync_show", "url_encoding_convert", "url_encoding_convert_from", "url_encoding_convert_to", "use", "user", "user_agent", "userid", "userid_domain", "userid_expires", "userid_mark", "userid_name", "userid_p3p", "userid_path", "userid_service", "uwsgi_bind", "uwsgi_buffering", "uwsgi_buffers", "uwsgi_buffer_size", "uwsgi_busy_buffers_size", "uwsgi_cache", "uwsgi_cache_background_update", "uwsgi_cache_bypass", "uwsgi_cache_key", "uwsgi_cache_lock", "uwsgi_cache_lock_age", "uwsgi_cache_lock_timeout", "uwsgi_cache_methods", "uwsgi_cache_min_uses", "uwsgi_cache_path", "uwsgi_cache_purge", "uwsgi_cache_revalidate", "uwsgi_cache_use_stale", "uwsgi_cache_valid", "uwsgi_connect_timeout", "uwsgi_force_ranges", "uwsgi_hide_header", "uwsgi_ignore_client_abort", "uwsgi_ignore_headers", "uwsgi_intercept_errors", "uwsgi_limit_rate", "uwsgi_max_temp_file_size", "uwsgi_modifier1", "uwsgi_modifier2", "uwsgi_next_upstream", "uwsgi_next_upstream_timeout", "uwsgi_next_upstream_tries", "uwsgi_no_cache", "uwsgi_param", "uwsgi_pass", "uwsgi_pass_header", "uwsgi_pass_request_body", "uwsgi_pass_request_headers", "uwsgi_read_timeout", "uwsgi_request_buffering", "uwsgi_send_timeout", "uwsgi_ssl_certificate", "uwsgi_ssl_certificate_key", "uwsgi_ssl_ciphers", "uwsgi_ssl_crl", "uwsgi_ssl_name", "uwsgi_ssl_password_file", "uwsgi_ssl_protocols", "uwsgi_ssl_server_name", "uwsgi_ssl_session_reuse", "uwsgi_ssl_trusted_certificate", "uwsgi_ssl_verify", "uwsgi_ssl_verify_depth", "uwsgi_store", "uwsgi_store_access", "uwsgi_string", "uwsgi_temp_file_write_size", "uwsgi_temp_path",
        "valid_referers", "variables_hash_bucket_size", "variables_hash_max_size", "vhost_traffic_status", "vhost_traffic_status_display", "vhost_traffic_status_display_format", "vhost_traffic_status_display_jsonp", "vhost_traffic_status_filter", "vhost_traffic_status_filter_by_host", "vhost_traffic_status_filter_by_set_key", "vhost_traffic_status_filter_check_duplicate", "vhost_traffic_status_limit", "vhost_traffic_status_limit_check_duplicate", "vhost_traffic_status_limit_traffic", "vhost_traffic_status_limit_traffic_by_set_key", "vhost_traffic_status_zone", "video_thumbextractor", "video_thumbextractor_image_height", "video_thumbextractor_image_width", "video_thumbextractor_next_time", "video_thumbextractor_only_keyframe", "video_thumbextractor_processes_per_worker", "video_thumbextractor_threads", "video_thumbextractor_tile_color", "video_thumbextractor_tile_cols", "video_thumbextractor_tile_margin", "video_thumbextractor_tile_max_cols", "video_thumbextractor_tile_max_rows", "video_thumbextractor_tile_padding", "video_thumbextractor_tile_rows", "video_thumbextractor_tile_sample_interval", "video_thumbextractor_video_filename", "video_thumbextractor_video_second",
        "wait_key", "wait_video", "weight", "worker_aio_requests", "worker_connections", "worker_cpu_affinity", "worker_priority", "worker_processes", "worker_rlimit_core", "worker_rlimit_nofile", "worker_rlimit_sigpending", "worker_threads", "working_directory", "write_page_size",
        "xclient", "xml_entities", "xslt_last_modified", "xslt_param", "xslt_string_param", "xslt_stylesheet", "xslt_types", "xss_callback_arg", "xss_check_status", "xss_get", "xss_input_types", "xss_override_status",
        "zone",
    },
} -- >>>
function mycomp_collect_keywords() -- Collect from (1) keyword file and (2) syntaxcomplete TODO: split 1 and 2 <<<
    local ft = vim.bo.ft
    if mycomp_collect_keywords_cache[ft] then
        return mycomp_collect_keywords_cache[ft]
    end
    -- Helpers
    local function mycomp_append(...) -- merge arrays <<<
        local res = {}
        for _, tbl in ipairs({...}) do
            for _, x in ipairs(tbl) do
                table.insert(res, x)
            end
        end
        return res
    end -- >>>
    local function load_comp_file(filetype) -- (1) try read ~/.rlwrap/XXX_completions <<<
        local file = io.open(vim.fn.expand("~/.rlwrap/" .. filetype .. "_completions"), "r")
        if file then
            local t = {}
            for line in file:lines() do
                table.insert(t, line)
            end
            io.close(file)
            return t
        else
            return nil
        end
    end -- >>>
    -- Combine results from (1) and (2)
    local res = mycomp_append(load_comp_file(ft) or {}, vim.call("syntaxcomplete#OmniSyntaxList"), (ft == "vim" and load_comp_file("lua")) or {}, mycomp_collect_keywords_extra[ft] or (ft == "html" and mycomp_collect_keywords_extra["javascript"]) or {})
    mycomp_collect_keywords_cache[ft] = res
    -- if html, include css, js
    -- if vim,  include lua
    -- how to call syntaxcomplete#OmniSyntaxList for ft different from current buf?
    return res
end -- >>>

-- >>>

