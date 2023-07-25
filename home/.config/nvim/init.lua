-- vim: fdm=marker fmr=<<<,>>>
-- default fold marker breaks indentation

-- TODO tmux source

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
    \ }

]] -- >>>

function plug(url) -- Plugin manager <<<
    -- just git clone repo, windows compatible
    -- using /site/pack/**/opt/ instead of /site/pack/**/start/ means packages are not automatically loaded
    -- "packadd! nvim-treesitter" to actually load
    -- need "helpt ALL" to recognize doc

    -- limitation cannot update or remove, no hooks, no async etc.

    -- use github as default source
    if url:find("http") ~= 1 then url = "https://github.com/" .. url end

    -- e.g. "nvim-lspconfig"
    local name = url:gsub("^.*/", ""):gsub("%.git$", "")

    -- e.g. "~/.local/share/nvim/site/pack/nvim-lspconfig/opt/"
    -- using slash as path delimiter is ok on windows
    local dir = vim.fn.stdpath("data") .. "/site/pack/" .. name .. "/opt/"

    -- download package if not already installed
    if 0 == vim.fn.isdirectory(dir .. "/" .. name) then

        -- create containing directory and git clone
        -- os.execute("[ -d '" .. dir .. "' ] || mkdir -p '" .. dir .. "'; git -C '" .. dir .. "' clone '" .. url .. "'")

        -- check if git is available
        if not vim.fn.executable("git") then print("plug: git is not available!") return end

        -- create dir to clone git repo in
        vim.fn.mkdir(dir, "p")

        -- run git clone
        vim.fn.system({ "git", "-C", dir, "clone", "--depth", 1, url })

        -- regenerate help (after vim startup)
        vim.cmd("aug plug \n au! \n au VimEnter * helpt ALL \n aug END")

    end

    -- load package (not needed if you use /start instead of /opt)
    vim.cmd("packadd! " .. name)

end
function can_require(x)
    if package.loaded[x] then
        return true
    else
        for _, searcher in ipairs(package.searchers or package.loaders) do
            local loader = searcher(x)
            if type(loader) == "function" then
                package.preload[x] = loader
                return true
            end
        end
        return false
    end
end
-- >>>

do -- Plugins <<<

    plug "pangloss/vim-javascript"
    plug "Vimjas/vim-python-pep8-indent"
    plug "bfrg/vim-cpp-modern"
    plug "gutenye/json5.vim"
    plug "dmix/elvish.vim"
    plug "udalov/kotlin-vim"

    -- plug "nvim-treesitter/nvim-treesitter"
    -- plug "https://github.com/nvim-treesitter/playground"
    -- plug "https://github.com/p00f/nvim-ts-rainbow"

    plug "neovim/nvim-lspconfig"

    -- -- plug 'lukas-reineke/cmp-rg'
    -- -- plug "hrsh7th/cmp-path"
    -- -- plug "hrsh7th/cmp-cmdline"
    -- -- plug "hrsh7th/cmp-nvim-lua"
    -- plug "hrsh7th/cmp-nvim-lsp"
    -- plug "hrsh7th/cmp-buffer"
    -- plug "hrsh7th/cmp-omni"
    -- plug "andersevenrud/cmp-tmux"
    -- plug "hrsh7th/nvim-cmp"

    plug "godlygeek/tabular"
    plug "smjonas/live-command.nvim"

    plug "windwp/nvim-autopairs"
    -- plug "steelsojka/pears.nvim"

end -- >>>

if can_require"nvim-autopairs" then require"nvim-autopairs".setup() end
if can_require"pears" then require"pears".setup() end
vim.cmd [[ let g:javascript_plugin_jsdoc = 1 ]]

vim.cmd [[

" Misc options <<<
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
set autochdir         " Auto change working directory to current files' like in emacs
set wildignorecase    " Case-insensitive filename completion

if exists("&mousescroll") | set mousescroll=ver:5,hor:6 | endif
if exists("&ttymouse") | set ttymouse=xterm2 | endif " Mouse drag to resize windows

set clipboard=unnamedplus
" If my clipboard wrapper is available, use it.
if executable("xcopy") && executable("xpaste")
    let g:clipboard = { 'name': 'my', 'copy': { '+': 'xcopy', '*': 'xcopy', }, 'paste': { '+': 'xpaste', '*': 'xpaste', }, 'cache_enabled': 0, }
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
fu! StlDirName()
    let x = substitute(expand('%:p:h'), $HOME, '~', '')
    " unix
    let x = substitute(x, '\([-._]*[^/]\|[^/]\)[^/]*/', '\1/', 'g')
    " windows (backslash instead of slash; keep C: etc.)
    let x = substitute(x, '\(\a:\|[-._]*[^\\]\|[^\\]\)[^\\]*\\', '\1\\', 'g')
    return x
endfu
fu! StlFileTypeEnc()
    return join(filter([&ft, &fenc], 'len(v:val) > 0'), ', ')
endfu
fu! StlLspSigOrFtEnc()
    let lspsig = luaeval("lspsig")
    return len(lspsig) > 0 ? lspsig : ("(" . StlFileTypeEnc() . ")")
endfu
fu! StlClose(minwid, nclicks, btn, mod)
    if len(filter(range(1, bufnr('$')), '! empty(bufname(v:val)) && buflisted(v:val)')) == 1
        q
    else
        bd
    endif
endfu
set statusline=%h%w%m%r\ \ %t\ \ \ [%{StlDirName()}]\ \ \ (%{StlFileTypeEnc()})%=%-14.(%l,%c%V%)\ %P%3@StlClose@\ \ [X]%X

set laststatus=2
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
nnore <leader>z :set fdm=marker<cr>zm

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
    if exists("*TSIsFirstCharInString") | return TSIsFirstCharInString(a:lnum) | endif
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
au FileType lua,javascript,python,markdown call MyFolding()
aug END

fu! MyFolding()
    let ft = &ft
    if index(["lua", "javascript", "html", "perl", "c", "cpp", "awk"], ft) != -1
        setl fdm=expr fde=MyFold(v:lnum,0)
    elseif ft == "python"
        setl fdm=expr fde=MyFold(v:lnum,1)
    elseif ft == "Markdown"
        setl fdm=expr fde=MarkdownLevel()
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
aug vimrc_myesc
au!
" Two autocmds needed (for when cursor is at beginning or end of line)
au InsertLeavePre * let myesc_col = col(".")
au InsertLeave    * exe (myesc_col >= 2) ? "norm! l" : ""
aug END
" >>>

" Keys (keyboard layout agnostic) <<<

" vimrc
nnore <f5> :wa<cr>:sil source $MYVIMRC<cr>
nnore s<f5> :lua smartSp("$MYVIMRC")<cr>

" motion
onore m %
nnore m %
vnore m %
nnore M m
nnore <silent> 0      :lua smartHome()<cr>
nnore <silent> <home> :lua smartHome()<cr>
inore <silent> <home> <c-o>:lua smartHome(true)<cr>
nnore ( <c-o>
nnore ) <c-i>
nnore <silent> <expr> f (reg_recording() . reg_executing()) != "" ? "f" : ":lua smartf(1)\<cr>"
nnore <silent> <expr> F (reg_recording() . reg_executing()) != "" ? "F" : ":lua smartf(-1)\<cr>"

" edit
nnore D dd
nnore Y yy
nnore U <c-r>
nnore si s
nnore yb :call SaveExcursion("ggVGy")<cr>
nnore yf :let @+=expand("%:t")<cr>
nnore yp :let @+=expand("%:p")<cr>
nnore yd :let @+=expand("%:h")<cr>
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
nnore sp :lua smartSp()<cr>
nnore s<space> :b#<cr>
"nnore sm :ls<cr>:b<space>
nnore <expr> sm FzfExists() ? ":FzfBuffers\<cr>" : ":ls\<cr>:b\<space>"
nnore <expr> sf FzfExists() ? ":FzfFiles\<cr>"   : ":e\<space>"
nnore <expr> sF FzfExists() ? ":FzfFiles!\<cr>"  : ":e\<space>"
nnore + :tabnext<cr>
nnore <silent> su :vsplit<bar>wincmd l<bar>exe "norm! Ljz<c-v><cr>"<cr>:set scb<cr>:wincmd h<cr>:set scb<cr>

" do not insert function key names
inore <F12>   <nop>
inore <S-F13> <nop>
inore <C-F14> <nop>

" misc
nnore * :call SearchQF()<cr>
nnore sc :copen<cr>
nnore s/ :noh<cr>:let @/ = ""<cr>
nnore s* :call AddHighlight()<cr>
nnore :<cr> :<up><cr>
tnore <esc> <c-\><c-n>
nnore <c-h> :call DescribeFace()<cr>
nnore <c-k> :lua browseDoc(false)<cr>
vnore <c-k> :lua browseDoc(true)<cr>
command! -nargs=* BD lua browseDoc(false, "<args>")
nnore <c-l> :call RecenterTopBottom()<cr>
"   commandline: <up> to prev candidate when completion popup menu in visible, otherwise go to previous history item with prefix matching
cnore <expr> <up>   pumvisible() ? "\<c-p>" : "\<up>"
cnore <expr> <down> pumvisible() ? "\<c-n>" : "\<down>"
vnore T :T<space>/
nnore zr zR
nnore zm zM

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
    if exists("*TSDescribeFace") && TSDescribeFace() | return | endif
    let first = 1
    for id in synstack(line("."), col("."))
        if first == 1 | let first = 0 | else | echon " > " | endif
        let name = synIDattr(id, "name")
        let nameTrans = synIDattr(synIDtrans(id), "name")
        exe "echohl " . name | echon name . (name != nameTrans ? "(" . nameTrans . ")" : "") | echohl None
    endfor
endfu

" >>>

" Custom Syntax (not used when using treesitter) <<<

fu! MySyntax()
    let ft = &ft
    if ft == "html"
        " TODO command to replace &forall; etc. with unicode chars?
        for i in [ "> gt", "< lt", "∀ forall", "∂ part", "∃ exist", "∅ empty", "∇ nabla", "∈ isin", "∉ notin", "∋ ni", "∏ prod", "∑ sum", "− minus", "∗ lowast", "√ radic", "∝ prop", "∞ infin", "∠ ang", "∧ and", "∨ or", "∩ cap", "∪ cup", "∫ int", "∴ there4", "∼ sim", "≅ cong", "≈ asymp", "≠ ne", "≡ equiv", "≤ le", "≥ ge", "⊂ sub", "⊃ sup", "⊄ nsub", "⊆ sube", "⊇ supe", "⊕ oplus", "⊗ otimes", "⊥ perp", "⋅ sdot", "∈ in" ]
            let from = matchstr(i, "\\S*$")
            let to   = matchstr(i, "^\\S*")
            exe "syntax match MyHtml_" . from . " \"&" . from . ";\" conceal cchar=" . to
        endfor
        syntax match MyHtml_br "<br>" conceal cchar=⏎
        sil! syntax clear MyHtml_span
        syntax region MyHtml_span matchgroup=MyHtml_hidden start='<span\( class="\?[^>"]*"\?\)\?>' end='<\/span>' oneline concealends
        hi MyHtml_span cterm=underline
        hi Conceal ctermfg=7 ctermbg=8 cterm=bold
        setlocal conceallevel=2 concealcursor=nc
    endif
    if ft == "javascript" || ft == "html"
        for i in ["jsVarDef", "jsVarDefName", "jsFuncDefName", "jsFuncDefArgs", "jsVarDefWrap"] | exe "sil! syn clear " . i | endfor

        " bigint
        sil! syn clear javaScriptBigInt
        syn match javaScriptBigInt /-\=\<\d\+\%(_\d\+\)*n\>/

        " TODO this fails for multiline string e.g. "const str = `aa\nbb\ncc`;" (\n means really newline)
        " workaround: put "=" and the string literal in the next line.

        " Match variable definition statement e.g. "const x = 1, y = 2;"
        " remove const/let/var from keywords
        sil! syn clear javaScriptIdentifier
        sil! syn clear javaScriptReserved
        syn keyword javaScriptIdentifier this arguments
        syn keyword javaScriptReserved long transient float int async synchronized protected static interface private final implements import goto export volatile class double short boolean char throws native enum public byte debugger package abstract extends super
        syn keyword javascriptStatement yield yield*
        syn keyword javascriptRepeat of

        " now define region
        syn region jsVarDef matchgroup=jsVarDefType start=/\<const\>/ start=/\<let\>/ start=/\<var\>/ matchgroup=NONE end=/;/ keepend end=/[^,;]$/ transparent containedin=javaScript contains=javaScript[a-zA-Z].*,jsVarDefName,jsVarDefWrap
        " symbols inside function calls, arrays and objects are not varname (except destructuring assignment)
        " syn region jsVarDefWrap start=/\(\(const\|let\|var\)\_s*\)\@<![\[({]/ end=/[])}]/ keepend transparent contained contains=javaScript[a-zA-Z].*
        syn region jsVarDefWrap start=/\(\(const\|let\|var\)\_s*\)\@<![\[({]/ end=/[])}]/ keepend transparent contained contains=javaScript[a-zA-Z].*

        " Match variable name in definitions e.g. "x" and "y" in "const x = 1, y = 2;" or "function f(x, y=2)"
        "                                    (varname)
        "        syn match jsVarDefName /\([-+=.&|<>*/]\_s*\)\@<!\<\h\w*\>/ contained containedin=jsFuncDefArgs,jsVarDef
        syn match jsVarDefName /\([-+=.&|<>*/]\_s*\)\@<!\zs\<\h\w*\>\ze\_s*[])=,;]/ contained containedin=jsFuncDefArgs,jsVarDef
        "        syn match jsVarDefName /[^ =]\_s*\<\h\w*\>/ contained containedin=jsFuncDefArgs,jsVarDef
        "                                      ^^ varname don't start with 0-9
        "                       ^^^^^^^^^ bad prefix (i.e. "=" (to exclude assignment rhs, default arg value etc.))
        "                            "AAA\@<!BBB" matches BBB not after AAA

        " Match function definition "function f(x, y=2)" (jsFuncDefName matches "f", jsFuncDefArgs matches "(x, y=2)")
        " TODO arrow function, "var f = function()..."
        syn iskeyword @,48-57,_,192-255,*
        sil! syn clear javaScriptFunction
        syn keyword javaScriptFunction function function* skipwhite nextgroup=jsFuncDefName
        syn match jsFuncDefName /\<\w\+\>/ skipwhite contained nextgroup=jsFuncDefArgs
        syn region jsFuncDefArgs start="(" end=")" keepend contained contains=javaScript[a-zA-Z].*,jsVarDefName

        " Note: Cannot modify nextgroup of existing groups; we must use regexp lookback OR wrap everything in a region
        "       Lookback example:                         ( "AAA\@<=BBB" matches BBB after AAA )
        "         syn match jsFuncDefName /\(\(function\)\_s\+\)\@<=\<\w\+\>/ skipwhite
        " Note: Keyword has precedence over match and region. If "const" is a keyword, a region with start=/const/ cannot start there.

    elseif ft == "org"
        call MyOrgSyntaxHighlight()

    elseif ft == "kotlin"
        " syn match myKtFuncName /\<\w\+\>\_s*\ze(/
        syn match myKtFuncUse  /\<\w\+\>\_s*\ze(/
        syn match myKtFuncName /\(fun\s_*\)\@<=\<\w\+\>\_s*\ze(/
        syn match myKtType     /\(:\s_*\)\@<=\<\w\+\>/

    elseif ft == "go"
        syn match  myGoType         /\v(\[\])+<\w+>/ " Array type
        syn match  myGoFuncName     /\v(func\s*(\(([] \[]|\w)+\))?\s*)@<=<\w+>/ skipwhite nextgroup=myGoFuncArgs

        syn region myGoFuncArgs     start=/(/ end=/)/ contained contains=myGoFuncArg,myGoFuncArgType
        syn match  myGoFuncArg      /\v([(,]\s*)@<=<\w+>/       contained " var name
        syn match  myGoFuncArgType  /\v(\w\s*)@<=[]\[]*<\w+>/  contained " var type
        syn match  myGoVar1         /\v(<\w+>(\,\s*)?)+\ze\s*\:\=/        " aaa := 123
        syn match  myGoVar2         /\v(<var>\s*)@<=<\w+>/                " var aaa

    elseif ft == "elvish"
        syn match  myElvVar        /\v((\s*^\=|<set>|<var>)\s*)@<=<\w+>/

    endif
endfu
nnore <f6> :call MySyntax()<cr>
"let firstbuf = bufnr("%")
"bufdo call MySyntax()
"exec "b" firstbuf

" Disable when using treesitter
" aug vimrc_syn
" au!
" au Syntax,FileType javascript,html call MySyntax()
" au BufNewFile,BufRead *.js,*.html  call MySyntax()
" au Syntax,FileType kotlin          call MySyntax()
" au Syntax,FileType go              call MySyntax()
" aug END

" >>>

" Highlight (colors) <<<

fu! MyHighlight()
    "  hi Constant     ctermfg=green cterm=bold
    hi Constant     ctermfg=blue
    hi NonText      ctermfg=magenta
    hi comment      ctermfg=blue
    "  hi String       ctermfg=green
    hi String       ctermfg=yellow
    "  hi Type         ctermfg=cyan
    hi Type         ctermfg=green
    hi Conditional  ctermfg=green cterm=bold
    hi Preproc      ctermfg=cyan
    "  hi Identifier   ctermfg=red cterm=none
    hi Identifier   ctermfg=green cterm=bold
    "  hi Identifier   ctermfg=cyan cterm=none
    hi Special      ctermfg=red
    hi Folded       ctermfg=magenta ctermbg=black cterm=bold,underline
    "  hi Folded       ctermfg=magenta ctermbg=none cterm=bold
    if g:env.dark
        hi Folded       ctermfg=magenta ctermbg=236 cterm=bold
    endif
    hi Visual       ctermfg=black ctermbg=blue
    "  hi Statement    ctermfg=green cterm=bold
    "  hi Statement    ctermfg=green cterm=none
    "  hi Statement    ctermfg=cyan cterm=none
    hi Statement    ctermfg=red cterm=none
    "  hi Statement    ctermfg=red cterm=bold
    "  hi Statement    ctermfg=magenta cterm=bold
    "  hi Statement    ctermfg=yellow cterm=none
    "  hi Identifier    ctermfg=yellow cterm=none
    "  hi Identifier   ctermfg=green cterm=none
    "  hi Identifier   ctermfg=green cterm=bold

    hi Question     ctermfg=green   " not applied when opening in-use file (since vimrc is not loaded yet)
    hi MoreMsg      ctermfg=cyan
    hi WarningMsg   ctermfg=red
    hi ErrorMsg     ctermfg=white ctermbg=red cterm=bold
    hi Directory    ctermfg=magenta " "ctermfg=" etc in :hi

    hi MatchParen   ctermbg=blue

    hi TypeSubtle   ctermfg=magenta

    " Pmenu (completion popup menu)
    " hi Pmenu        ctermfg=magenta ctermbg=237 cterm=bold
    " hi PmenuSel     ctermfg=magenta ctermbg=black cterm=bold,reverse
    " hi Pmenu        ctermfg=236 ctermbg=blue cterm=bold
    " hi PmenuSel     ctermfg=236 ctermbg=blue cterm=bold,reverse
    hi Pmenu        ctermfg=blue ctermbg=black
    hi PmenuSel     ctermfg=blue ctermbg=black cterm=bold,reverse
    if g:env.dark
        hi Pmenu        ctermfg=blue ctermbg=238 cterm=bold
    endif


    " Filetype specific
    " === HTML ===
    hi link javaScriptBigInt   Special
    hi link javaScript         Normal
    hi      javaScriptParens   ctermfg=magenta
    hi link javaScriptBraces   javaScriptParens
    hi link javaScriptBrackets javaScriptParens
    hi link javaScriptNumber   Number
    hi link htmlEvent          Special
    hi link htmlTag            Normal
    hi link htmlEndTag         htmlTag
    hi      htmlTagName        ctermfg=magenta
    hi link htmlSpecialTagName htmlTagName
    hi      Title              ctermfg=none cterm=bold
    hi link htmlTitle          Title

    " === Kotlin ===
    hi link myKtFuncName       myFuncName
    hi link myKtFuncUse        Statement
    hi link myKtType           Type

    " === XML ===
    hi link xmlTag             Special
    hi link xmlTagName         Type
    hi link xmlEndTag          xmlTag

    " === Go ===
    hi link myGoFuncName       myFuncName
    hi link myGoType           TypeSubtle
    hi link goType             myGoType
    hi link goSignedInts       myGoType
    hi link goUnsignedInts     myGoType
    hi link myGoFuncArg        myVarName
    hi link myGoFuncArgType    myGoType
    hi link myGoVar1           myVarName
    hi link myGoVar2           myVarName

    " === C ===
    hi link cFormat            Type

    " === Elvish ===
    hi link myElvVar           myVarName


    " Treesitter
    hi link @variable             None
    hi link @field                None
    hi link @property             None
    hi      @variabledef          ctermfg=cyan    cterm=bold
    hi      @functiondef          ctermfg=yellow  cterm=bold
    hi      @typestrong           cterm=bold
    hi      @keyword              ctermfg=green
    hi      @keyword.return       ctermfg=red     cterm=bold
    hi      @keyword.break        ctermfg=red     cterm=bold
    hi link @variable.builtin     @keyword
    hi link @parameter            @variabledef
    hi      @function.call        ctermfg=none    cterm=NONE
    hi link @constructor          @function.call
    hi link @method.call          @function.call
    hi      @keyword.vim          ctermfg=red
    hi      @keyword.function.vim ctermfg=red
    "hi      @function.call.python ctermfg=red cterm=NONE

    hi myVarName ctermfg=cyan cterm=bold
    hi link vimMapLhs    myVarName
    hi link jsVarDefName myVarName
    hi link jsVarDefType Type

    hi myFuncName ctermfg=yellow cterm=bold
    hi link vimFunction   myFuncName
    hi link jsFuncDefName myFuncName

endfu
fu! MyHighlight2()
    hi      Normal                ctermfg=white   cterm=none
    hi      Bold                  ctermfg=green   cterm=none
    hi      Comment               ctermfg=blue    cterm=none
    hi      String                ctermfg=yellow  cterm=none
    hi      Number                ctermfg=white   cterm=none
    hi      FuncDef               ctermfg=cyan    cterm=none
    hi link VarDef                FuncDef
    hi      Type                  ctermfg=white   cterm=none
    hi      Flow                  ctermfg=red     cterm=none
    hi      Special               ctermfg=red     cterm=none
    hi      PreProc               ctermfg=green   cterm=none

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

    " python
    hi link @type.python          Normal
    hi link @type.builtin.python  Normal

    " javascirpt
    hi link @constructor.javascript VarDef
    hi link @functiondef.javascript VarDef

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

    " Popup menu
    hi Pmenu        ctermfg=blue ctermbg=black
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

    if &ft == "javascript"
        hi link jsFuncCall       Normal
        hi link jsFunction       Bold
        hi link jsRepeat         Bold
        hi link jsConditional    Bold
        hi link jsTry            Bold
        hi link jsFinally        Bold
        hi link jsCatch          Bold
        hi link jsOf             Bold
        hi link jsStorageClass   Bold
        hi link jsThis           Bold
        hi link jsSuper          Bold
        hi link jsBuiltins       Bold
        hi link jsGlobalObjects  Bold
        hi link jsFuncName       FuncDef
        hi link jsClassProperty  FuncDef
        hi link jsFuncArgs       VarDef
        hi link jsReturn         Flow
        hi link jsException      Flow
        hi link jsStatement      Flow
        hi link jsArrowFunction  Special
        hi link Noise            Special
        hi link jsOperator       Special
    endif

    if &ft == "python"
        hi link pythonBuiltin     Normal
        hi link pythonStatement   Bold
        hi link pythonRepeat      Bold
        hi link pythonOperator    Bold
        hi link pythonConditional Bold
        hi link pythonAsync       Bold
        hi link pythonException   Bold
        hi link Identifier        FuncDef
        hi link pythonFunction    FuncDef

        syn keyword MyPythonFlow return pass raise break continue
        hi link MyPythonFlow     Flow

        syn match MyPythonPunctations /[^[:keyword:]#"']/
        hi link MyPythonPunctations Special

        syn keyword MyPythonDef        def skipwhite nextgroup=MyPythonDefName
        syn match   MyPythonDefName    /\<\w\+\>/ skipwhite contained nextgroup=MyPythonDefArgs
        syn region  MyPythonDefArgs    start="(" end=")" keepend contained contains=python[a-zA-Z].*,MyPythonDefArgName,MyPythonPunctations
        syn match   MyPythonDefArgName /\([(,*]\_s*\)\@5<=\<\w\+\>/ skipwhite contained
        hi link MyPythonDef        Bold
        hi link MyPythonDefName    FuncDef
        hi link MyPythonDefArgName VarDef
    endif

    if &ft == "html"
        hi link htmlTag            Special
        hi link htmlEndTag         Special
        hi link htmlSpecialTagName Bold
        hi link htmlTagName        Bold
    endif

    if &ft == "c"
        hi link cUserFunction Normal
        syn match MyCPunc /[][(){}+*=,;-]/
        hi link MyCPunc Special
    endif

    if &ft == "perl"
        hi link perlStatement        Normal
        hi link perlStatementFlow    Flow
        hi link perlStatementControl Flow
        syn match MyPerlPunc /[][(){}<>:?!+*=,;-\/]/
        hi link MyPerlPunc Special
        hi link perlVarPlain Bold
    endif

endfu
call MyHighlight2()
aug vimrc_hi " :hi need to be in autocmd on first run??
au!
au VimEnter * :call MyHighlight2()
aug END
nnore <f8> :call MyHighlight2()<cr>
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
    exe "term sh -c 'cat " . tmpfile . " | fzf'"
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
" b:terminal_job_id
" jobsend(34, join(getline(1,"$"), "\n") . "\n")
" \ "scheme": { "cmd": "gosh -fno-read-edit", "newl": "\r", "newlMore": 0 },
" \ "scheme": { "cmd": "rlwrap -pgreen gosh -fno-read-edit", "cmdBase": "gosh", "newl": "\r", "newlMore": 0 },
let g:ReplData = {
    \ "scheme":     { "cmd": "gosh", "newl": "\r", "newlMore": 0 },
    \ "python":     { "cmd": "python", "newl": "\r\n", "newlMore": 1 },
    \ "javascript": { "cmd": "node", "newl": "\r\n", "newlMore": 1 },
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

" Filetype specific configs <<<

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
aug vimrc_ft_javascript
au!
au BufNewFile,BufRead *.lua iabbr <buffer> ll local
au FileType lua             iabbr <buffer> ll local
au BufNewFile,BufRead *.lua iabbr <buffer> fn function
au FileType lua             iabbr <buffer> fn function
aug END

" === JavaScript ===
aug vimrc_ft_javascript
au!
au BufNewFile,BufRead *.js iabbr <buffer> cs const
au FileType javascript     iabbr <buffer> cs const
au BufNewFile,BufRead *.js iabbr <buffer> ts this
au FileType javascript     iabbr <buffer> ts this
au BufNewFile,BufRead *.js inore <buffer> /// /**<space><space>*/<left><left><left>
au FileType javascript     inore <buffer> /// /**<space><space>*/<left><left><left>
au BufNewFile,BufRead *.js inore <buffer> $$ <space>=><space>
au FileType javascript     inore <buffer> $$ <space>=><space>
au BufNewFile,BufRead *.js inore <buffer> clog console.log()<left>
au FileType javascript     inore <buffer> clog console.log()<left>
au BufNewFile,BufRead *.js setl iskeyword+=#
au FileType javascript     setl iskeyword+=#
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
    syn region orgComment    start=/^\s*#[^+]/ end=/$/   oneline
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

" === Scheme ===
aug vimrc_ft_scheme
au!
au BufNewFile,BufRead *.scm setl formatoptions+=rol
au FileType             scm setl formatoptions+=rol
aug END

" >>>

]]

do -- Keys (keyboard layout specific) <<<

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

if can_require"cmp" then -- Completion (nvim-cmp) <<<
    local cmp = require"cmp"

    cmp.setup({
        completion = {
            keyword_pattern = [[\k\+]],
        },
        mapping = cmp.mapping.preset.insert({
            ["<C-b>"] = cmp.mapping.scroll_docs(-4),
            ["<C-f>"] = cmp.mapping.scroll_docs(4),
            ["<C-Space>"] = cmp.mapping.complete(),
            ["<C-e>"] = cmp.mapping.abort(),
            ["<CR>"] = cmp.mapping.confirm({ select = false }), -- Accept currently selected item. Set `select` to `false` to only confirm explicitly selected items.
            ["<tab>"] = cmp.mapping.select_next_item(),
            ["<s-tab>"] = cmp.mapping.select_prev_item(),
        }),
        sources = cmp.config.sources({
            { name = "omni" },
            -- { name = "rg", keyword_length = 3 },
            { name = "nvim_lsp" },
            -- { name = "path", trailing_slash = true },
            {
                name = "buffer",
                option = { get_bufnrs = function() return vim.api.nvim_list_bufs() end },
            },
            {
                name = 'tmux',
                option = { all_panes = true, label = '[tmux]', },
            }
        }, {}),
        formatting = {
            fields = { "abbr", "kind", },

            format = function(_, vim_item)
                vim_item.kind = (vim_item.kind or '') .. vim.inspect(vim_item.kind)
                return vim_item
            end,
        },
        -- formatting = { fields = { "kind", "abbr", }, },
    })

end -- >>>

if can_require"lspconfig" then -- Lsp <<<
    local lspconfig = require"lspconfig"

    vim.cmd [[ let g:diaglength = 30 ]]
    function rightAlignFormatFunction(diagnostic)
        local line = diagnostic.lnum -- 0-based
        local space = string.rep(" ", vim.o.columns - vim.fn.getline(1+line):len() - 11 - vim.g.diaglength)
        return ""
        -- return string.format("%s* %s", space, diagnostic.message)
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

    local function on_attach(client, bufnr)
        --vim.diagnostic[vim.g.myLspDiag == 1 and "enable" or "disable"]()
        vim.cmd [[
        setl omnifunc=v:lua.vim.lsp.omnifunc
        setl signcolumn=number
        nnore <buffer> <enter> :lua vim.lsp.buf.hover()<cr>
        nnore <buffer> <leader>dd :LspToggleDiag<cr>
        nnore <buffer> <leader>dl :LspToggleDiagLevel<cr>
        nnore <buffer> <leader>ll :LspLocList<cr>
        nnore <buffer> <leader>lq :LspQuickFix<cr>
        nnore <buffer> <leader>lr :lua vim.lsp.buf.rename()<cr>
        ]]
        --vim.api.nvim_buf_set_option(bufnr, 'omnifunc', 'v:lua.vim.lsp.omnifunc')
        local cap = client.server_capabilities
        lastcap = cap
        cap.semanticTokensProvider = false -- prevent my highlighting gettting overridden
    end

    local function setup(lsname, cmd, arg)
        -- call language server setup function if server program is present
        if vim.fn.executable(cmd or lsname) == 1 then
            require'lspconfig'[lsname]["setup"](arg)
        end
    end
    setup("bashls",                 "bash-language-server",   { on_attach = on_attach })
    --setup("ccls",                   nil,                      { on_attach = on_attach, single_file_support = true })
    setup("clangd",                 nil,                      { on_attach = on_attach, single_file_support = true })
    setup("kotlin_language_server", "kotlin-language-server", { on_attach = on_attach })
    setup("pyright",                nil,                      { on_attach = on_attach })
    setup("tsserver",               nil,                      { on_attach = on_attach, single_file_support = true })
    setup("tsserver",               "typescript-language-server.cmd", { on_attach = on_attach, single_file_support = true })
    setup("serve_d",                "serve-d",                { on_attach = on_attach, single_file_support = true })
    setup("gopls",                  nil,                      { on_attach = on_attach, single_file_support = true })
    -- Place libaries in node_modules/ to let LSP recognize it.

end -- >>>

if can_require"lspconfig" then -- ElDoc (lsp signatureHelp) <<<

    vim.cmd [[
    hi link LspSig    Normal
    hi      LspSigCur ctermfg=cyan  ctermbg=none cterm=bold
    aug vimrc_lspsig
    au!
    au CursorHold,CursorHoldI,InsertEnter * lua lspsigUpdate()
    aug END
    set updatetime=800
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

            if #params == 0 or #params <= actPar then -- no params (functions with no args etc.), or active param index is more than param count
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

                -- local lbl = params[1+actPar].label -- { start, end }
                -- mySigPrefix =
                -- local chunks = {
                --     { " ", "None" },
                --     { sigLbl:sub(1, lbl[1]),        "LspSig"    },
                --     { sigLbl:sub(lbl[1]+1, lbl[2]), "LspSigCur" },
                --     { sigLbl:sub(lbl[2]+1),         "LspSig"    },
                -- }
                -- nvim_echo_no_hitenter(chunks, true, {})
                -- return
                --

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

                -- local s = 1
                -- for i = 1, #params do
                --     local parLbl = params[i].label
                --     local ss, se = sigLbl:find(parLbl, s, true) -- find parLbl in sigLbl
                --     if not ss then error("param " .. parLbl .. " not found") end
                --     if i == 1 + actPar then
                --         -- update lspsig
                --         lspsig = "%#LspSig# " .. sigLbl:sub(1, ss-1) .. "%#LspSigCur#" .. parLbl .. "%#LspSig#" .. sigLbl:sub(se+1) .. " %#None#"
                --         -- show
                --         local chunks = {
                --             { " ",                 "None"      },
                --             { sigLbl:sub(1, ss-1), "LspSig"    },
                --             { parLbl,              "LspSigCur" },
                --             { sigLbl:sub(se+1),    "LspSig"    },
                --         }
                --         nvim_echo_no_hitenter(chunks, true, {})
                --         return
                --     end
                --     s = ss
                -- end

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
        for _, client in pairs(vim.lsp.buf_get_clients()) do
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
            util.make_position_params(),
            vim.lsp.with(signature_handler, {})
        )
    end
end -- >>>

if can_require"nvim-treesitter.configs" then -- TreeSitter <<<
    local nvim_treesitter_configs = require"nvim-treesitter.configs"

    nvim_treesitter_configs.setup {
        ensure_installed = { "bash", "c", "css", "cpp", "help", "html", "javascript", "lua", "python", "vim", },
        highlight = {
            enable  = true,
            -- disable = true,
            -- disable = { "javascript", },
            -- additional_vim_regex_highlighting = { "lua" },
        },
        indent = {
            enable  = true,
            disable = { "python", }
        },
        rainbow = {
            enable = false,
            -- disable = { "jsx", "cpp" }, list of languages you want to disable the plugin for
            extended_mode = true, -- Also highlight non-bracket delimiters like html tags, boolean or table: lang -> boolean
            max_file_lines = nil, -- Do not enable for files with more than n lines, int
            -- colors = {}, -- table of hex strings
            -- termcolors = {} -- table of colour name strings
        }
    }

    vim.cmd [[
    fu! TSDescribeFace()
        return luaeval("tsdescribe()")
    endfu
    ]]

    function tsdescribe()
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
                local s_line = string.format("%4d ", (child:start()))
                local s_sign = treesitter.is_in_node_range(child, line, col) and "*" or " "
                local s_node = child
                local s_name = name and (":" .. name) or ""
                local s_err  = child:has_error() and "ERROR" or ""
                print(s_line, ind, s_sign, s_node, s_name, s_err)
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

    function ts_isFirstCharInString(lnum)
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

    vim.cmd [[
    fu! TSIsFirstCharInString(lnum)
        return luaeval("ts_isFirstCharInString(" . a:lnum . ")")
    endfu
    ]]

    function TSGetLangAtCursor()
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

    -- based on https://github.com/nvim-treesitter/nvim-treesitter/issues/3058
    local function safe_read(filename, read_quantifier)
        local file, err = io.open(filename, 'r')
        if not file then
            error(err)
        end
        local content = file:read(read_quantifier)
        io.close(file)
        return content
    end

    local function read_query_files(filenames)
        local contents = {}
        for _, filename in ipairs(filenames) do
            table.insert(contents, safe_read(filename, '*a'))
        end
        return table.concat(contents, '')
    end

    local function set_query(lang, query_name, text)
        vim.treesitter.query.set_query(lang, query_name, text)
    end

    local function add_query(lang, query_name, text) -- will RESET query
        local query = read_query_files(vim.treesitter.query.get_query_files(lang, query_name))
        set_query(lang, query_name, query .. "\n" .. text)
    end


    -- Customize highlights.scm

    add_query("bash", "highlights", [[
(variable_assignment (variable_name) @variabledef.bash)
;(function_definition (word) @functiondef)
((command_name (word) @keyword.break)
 (#any-of? @keyword.break "break" "continue"))
((command_name (word) @keyword.return)
 (#any-of? @keyword.break "exit" "return"))
;(command_name (word) @functiondef)
    ]])

    add_query("c", "highlights", [[
(pointer_declarator (identifier) @variabledef) ; "arr" in "int (*arr)[]"
[ "break" "continue" ] @keyword.break
[ "goto" ] @keyword.return
((call_expression (identifier) @keyword.return)
 (#eq? @keyword.return "exit"))
    ]])

    add_query("javascript", "highlights", [[
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

    add_query("lua", "highlights", [[
(function_declaration (identifier) @functiondef)
(variable_declaration (assignment_statement (variable_list (identifier) @variabledef)))
(variable_declaration (variable_list (identifier) @variabledef))
(break_statement) @keyword.break
    ]])

    add_query("python", "highlights", [[
(function_definition (identifier) @functiondef)
(class_definition (identifier) @functiondef)
[ "continue" "raise" ] @keyword.break
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

    add_query("html", "indents", [[
(element (start_tag (tag_name) @_no_indent_tag (#any-of? @_no_indent_tag "html" "head" "body" "script" "style"))) @dedent
    ]])

    add_query("javascript", "indents", [[
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

    set_query("vim", "indents", [[
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

if can_require"live-command" then -- Live Command <<<
    require"live-command".setup {
        commands = {
            T = { cmd = "Tabular" },
        },
        defaults = {
            enable_highlighting = false,
            inline_highlighting = false,
            hl_groups = {
                insertion = "DiffAdd",
                deletion = "DiffDelete",
                change = "DiffChange",
            },
        },
    }
end -- >>>


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
        return TSGetLangAtCursor and (TSGetLangAtCursor() == lang)
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

function browseDoc(visual, text) -- <<<
    text = visual and vim.fn.GetVisualSelection() or (text or vim.fn.expand("<cword>"))
    local ft    = vim.bo.ft
    local extra = (ft == "javascript" or ft == "html") and " mdn" or ""
    local query = text .. " " .. ft .. extra
    -- TODO: w3m?
    local cmd   = "sil! !firefox 'https://lite.duckduckgo.com/lite/?q=" .. query .. "'"
    vim.cmd(cmd)
end -- >>>

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


vim.cmd [[ " Completion setup <<<
inore <expr> <tab>       pumvisible() ? "\<c-n>" : "\<c-x>\<c-u>"
inore <expr> <plug>MyTab pumvisible() ? "\<c-n>" : "\<c-x>\<c-u>"
inore <expr> <s-tab>     pumvisible() ? "\<c-p>" : "\<c-x>\<c-u>"
inore <expr> <c-f>       pumvisible() ? "\<c-n>" : "\<c-x>\<c-f>"

set shortmess+=c                           " No message like "Pattern not found"
set completeopt+=menuone,noinsert,noselect " Needed for auto completion
set completeopt-=longest                   " -=longest is needed otherwise first candidate appears and then disappears before menu is created, looks weird
set completeopt-=menu,preview
set infercase

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
fu! OpenCompletion()

    " prefetch lsp completion asynchronously
    " let minlen = g:comp_minlen - 1
    " if !pumvisible() && (('a' <= v:char && v:char <= 'z') || ('A' <= v:char && v:char <= 'Z') || (v:char == '_')) && (minlen == 1 || (col(".") >= (minlen-1) && matchstr(getline("."), '\%' . (col('.')-(minlen-1)) . 'c[a-zA-Z_]\{' . (minlen-1) . '\}') != ""))
    "     lua mycomp_lsp_omnifunc_prefetch()
    " endif

    " check (menu visible && inserting alphabet && at least comp_minlen chars)
    let minlen = g:comp_minlen
    if !pumvisible() && (('a' <= v:char && v:char <= 'z') || ('A' <= v:char && v:char <= 'Z') || (v:char == '_')) && (minlen == 1 || (col(".") >= (minlen-1) && matchstr(getline("."), '\%' . (col('.')-(minlen-1)) . 'c[a-zA-Z_]\{' . (minlen-1) . '\}') != ""))
        call feedkeys("\<plug>MyTab", "")
        " call feedkeys("\<c-x>\<c-u>", "n") " this will mess up repeating (.)
    endif

    " if !pumvisible() && (('a' <= v:char && v:char <= 'z') || ('A' <= v:char && v:char <= 'Z') || (v:char == '_')) && (g:comp_minlen == 1 || (col(".") >= (g:comp_minlen-1) && matchstr(getline("."), '\%' . (col('.')-(g:comp_minlen-1)) . 'c[a-zA-Z_]\{' . (g:comp_minlen-1) . '\}') != ""))
    "     call feedkeys("\<plug>MyTab", "")
    "     " call feedkeys("\<c-x>\<c-u>", "n") " this will mess up repeating (.)
    " endif

endfu
au InsertCharPre * call OpenCompletion()

aug END
]] -- >>>

-- My completefunc <<<

-- TODO: abstract memoization, should use prefix for performance?, async syntaxcomplete, too slow on first call
-- TODO: show source buffer, tags

local mycomp_cache = nil -- mycomp_cache[i] = comps cached when base:len() == i
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
        if not mycomp_cache then
            mycomp_cache = {}
            mycomp_cache[0] = mycomp_collect()
        end
        -- Get most detailed cache
        local cached
        for i = (base:len() - 1), 0, -1 do
            cached = mycomp_cache[i]
            if cached then break end
        end
        -- Filter cache further, and cache the new one
        local comps = mycomp_filter(base, cached)
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
    -- Setup regexp and score table
    local fuzreg = "^.*" .. base:gsub(".", "%1.*"):gsub("%.%*$", ""):gsub("%%", "%%%%")
    local t = {} -- t[i] = list of comps with score i
    for i = 1, maxscore do
        t[i] = {}
    end
    -- Calc score for each fuzzy match
    for _, comp in ipairs(list) do
        local w = mycomp_compword(comp)
        if w:lower():match(fuzreg) then
            table.insert(t[score(w:lower(), base)], comp)
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

    local comps_list = { -- defines order of words
        { "h", mycomp_collect_history() },
        { "o", mycomp_collect_omni() },
        { "b", mycomp_collect_bufferall() },
        { "k", mycomp_collect_keywords() },
        }
    local priority = { o=1, k=2, b=3, h=4 } -- which source has priority for extra info of word

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

    return result
end -- >>>

local mycomp_history, mycomp_history_max = {}, 300
function mycomp_collect_history() -- Collect from completion history <<<
    return mycomp_history
end -- >>>
function mycomp_add_history(comp) -- <<<
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

local mycomp_collect_buffer_cache = {}
function mycomp_collect_buffer(buf) -- Collect from a buf <<<
    -- TODO: better cache: rescan relevant parts of file (already good? seems lastused only change on comp start)
    buf = (type(buf) == "string") and buf or vim.fn.bufname(buf or "%")
    -- If cache exists and up-to-date (lastused did not change), return it.
    local cache = mycomp_collect_buffer_cache[buf]
    if cache and cache.lastused == vim.fn.getbufinfo(buf)[1].lastused then
        return cache.res
    end
    -- Extract words from buf
    local word_reg = mycomp_word_reg()
    local res, seen = {}, {} -- use 2 tables to prevent dupes
    for _, line in pairs(vim.fn.getbufline(buf, "1", "$")) do
        for s in line:gmatch(word_reg) do
            if (not seen[s]) then
                seen[s] = true
                table.insert(res, { word=s, menu=buf })
            end
        end
    end
--    vim.cmd("sleep 1") -- for cache test
    mycomp_collect_buffer_cache[buf] = { res = res, lastused = vim.fn.getbufinfo(buf)[1].lastused }
    return res
end -- >>>

function mycomp_collect_omni() -- Collect from omnifunc <<<
    if (not vim.bo.omnifunc) or (vim.bo.omnifunc == "") then
        return {}
    end
    local function callOmnifunc(findstart, base)
        local ofu = vim.bo.omnifunc
        if ofu == "v:lua.vim.lsp.omnifunc" then
            -- how to programatically call ofu that starts with "v:lua" ?
            -- return vim.lsp.omnifunc(findstart, base)
            return mycomp_lsp_omnifunc_sync(findstart, base)
            -- return mycomp_lsp_dummy(findstart, base)
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
    local params = vim.lsp.util.make_position_params()
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
    local params = vim.lsp.util.make_position_params()
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
    javascript = {
        "console.log", "console.error",
        "clearTimeout", "clearInterval", "setTimeout", "setInterval",
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

