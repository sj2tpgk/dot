" vim: fdm=marker
"
" Standalone neovim config with lsp support via nvim-lspconfig (optional)
"
" TODO update encoding (let format not precompiled)
" TODO auto 16color
" TODO surround ys( ys) etc.
" TODO common english words
" TODO enter key behavior on line-end??
" TODO snippet (for statement)
" TODO scrach buffer of type
" TODO Lisp folding

" Charcode at cursor
" :echo char2nr(matchstr(getline('.'), '\%'.col('.').'c.'))

" Auto pair {{{
"inore " ""<left>
"inore ' ''<left>
"inore ( ()<left>
"inore [ []<left>
"inore { {}<left>
"inore {<CR> {<CR>}<ESC>O
"inore {;<CR> {<CR>};<ESC>O
" }}}

lua if not vim.cmd then vim.cmd = vim.api.nvim_command end
mapclear | imapclear

" Easy lua debug. Use L! to print nested table. Example ":L 123,456"
command! -nargs=* -bang -complete=lua L lua pp(<q-args>, true, ("<bang>" == "") and 1 or 99, <args>)

" Fast startup {{{
let g:python_host_skip_check=1
let g:loaded_python3_provider=1
"set noloadplugins
" set shada="none"
" filetype off
" syntax off
" aug vimrc_speed
"   au!
"   au UIEnter * exe "filetype on | syntax on | filetype plugin indent on"
" aug END
" }}}

" Plugin {{{
" TODO: nv0 = no plugin, nv = yes plugin etc.
if 1

    " Install vim-plug
    let autoload_plug_path = stdpath('data') . '/site/autoload/plug.vim'
    if !filereadable(autoload_plug_path)
        silent execute '!curl -fLo ' . autoload_plug_path . '  --create-dirs 
                    \ "https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim"'
        autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
    endif
    unlet autoload_plug_path

    " Plugin declaration
    call plug#begin(stdpath('data') . '/plugged')
    let g:rainbow_active = 1
    let g:rainbow_ctermfgs = [
                \ 'red',
                \ 'yellow',
                \ 'magenta',
                \ 'white',
                \ 'cyan',
                \ ]
    " Plug 'frazrepo/vim-rainbow'
    Plug 'neovim/nvim-lspconfig'
    " set cmdheight=2
    " let g:echodoc_enable_at_startup = 1
    " let g:echodoc#type = 'floating'
    " let g:echodoc#events = ['CompleteDone', 'TextChangedP', 'CursorMoved', 'CursorMovedI']
    " Plug 'Shougo/echodoc'
    " Plug 'nvim-treesitter/nvim-treesitter', { 'do': ':TSUpdate' }
    " Plug 'nvim-treesitter/playground'
    " Plug 'jelera/vim-javascript-syntax'
    call plug#end()

    " Install missing plugins
    autocmd VimEnter * if len(filter(values(g:plugs), '!isdirectory(v:val.dir)'))
                \| PlugInstall --sync | source $MYVIMRC
                \| endif

    if has_key(g:plugs, "nvim-lspconfig")

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
            lua (function(x) vim.diagnostic.config({ virtual_text=x }) end)({severity={min=vim.diagnostic.severity[vim.g.myLspDiagLevelName]}})
        endfu
        call MyLspDiagLevel()

        " Custom commands
        com! LspToggleDiag      call MyLspDiagToggle()
        com! LspToggleDiagLevel call MyLspDiagLevel()
        com! LspLocList         lua vim.diagnostic.setloclist()
        com! LspQuickFix        lua vim.diagnostic.setqflist()

        " Config
        lua <<EOFLUA
        function on_attach(client, bufnr)
            vim.diagnostic[vim.g.myLspDiag == 1 and "enable" or "disable"]()
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
            vim.api.nvim_buf_set_option(bufnr, 'omnifunc', 'v:lua.vim.lsp.omnifunc')
        end
        require'lspconfig'.pyright.setup  { on_attach = on_attach }
        require'lspconfig'.bashls.setup   { on_attach = on_attach }
        require'lspconfig'.tsserver.setup { on_attach = on_attach, single_file_support = true }
        -- Place libaries in node_modules/ to let LSP recognize it.
EOFLUA
    endif

    " Treesitter
    if has_key(g:plugs, "nvim-treesitter")
        lua <<EOFLUA
        require'nvim-treesitter.configs'.setup {
            highlight = {
                enable = true,
                custom_captures = {
                    -- Highlight the @foo.bar capture group with the "Identifier" highlight group.
                    ["foo.bar"]  = "Identifier",
                    ["keyword"]  = "Identifier",
                    ["function"] = "myFuncName",
                    ["variable"] = "myVarName",
                },
                -- Setting this to true will run `:h syntax` and tree-sitter at the same time.
                -- Set this to `true` if you depend on 'syntax' being enabled (like for indentation).
                -- Using this option may slow down your editor, and you may see some duplicate highlights.
                -- Instead of true it can also be a list of languages
                additional_vim_regex_highlighting = false,
            },
            incremental_selection = {
                enable = true,
                keymaps = {
                    init_selection = "gnn",
                    node_incremental = "grn",
                    scope_incremental = "grc",
                    node_decremental = "grm",
                },
            },
            -- indent = { enable = true },
        }
EOFLUA
    endif

    " Treesitter playground
    if has_key(g:plugs, "playground")
        fu! TSDescribeFace()
            " let matches = [['@variable', 'javascriptTSVariable', 'TSVariable'], ['@function', 'javascriptTSFunction', 'TSFunction']]
            let matches = luaeval("(function () local ms = {} for _, x in ipairs(require'nvim-treesitter-playground.hl-info'.get_treesitter_hl()) do local a2 = {} for y in x:gmatch('@?%a+') do table.insert(a2, y) end table.insert(ms, a2) end return ms end)()")
            if len(matches) > 0
                let first1 = 1
                for lis in matches
                    if first1 == 1 | let first1 = 0 | else | echo "" | endif
                    let first = 1
                    for name in lis
                        if first == 1 | let first = 0 | else | echon " > " | endif
                        let nameTrans = synIDattr(synIDtrans(hlID(name)), "name")
                        exe "echohl " . name | echon name . ((name != nameTrans && name == lis[-1]) ? "(" . nameTrans . ")" : "") | echohl None
                    endfor
                endfor
                return 1
            endif
            return 0
        endfu
    endif

endif

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

" Status line: show directory name, filetype, encoding
fu! StlDirName()
    return substitute(substitute(expand('%:p:h'), $HOME, '~', ''), '\(\.[^/]\|[^/]\)[^/]*/', '\1/', 'g')
endfu
fu! StlFileTypeEnc()
    return join(filter([&ft, &fenc], 'len(v:val) > 0'), ', ')
endfu
set statusline=%h%w%m%r\ \ %t\ \ \ [%{StlDirName()}]\ \ \ (%{StlFileTypeEnc()})\ %=%-14.(%l,%c%V%)\ %P
set laststatus=2
" }}}

" Indent {{{
set expandtab               " Use spaces instead of tab
set autoindent
filetype plugin indent on   " This triggers filetype.vim (slow)
set smartindent

set shiftwidth=4            " Tab = N spaces in << >> etc.
set softtabstop=4           " Insert N spaces as a tab
set tabstop=4               " A tab shows as N spaces
set shiftround              " Indents to next multiple of 'shiftwidth'
" }}}

" Folding {{{
set fdm=marker
set fdn=2
nnore <tab> za
nnore <leader>z :set fdm=marker<cr>zm

" close folding by 'h' if cursor is 0 of the line and in a opened folding
"nnore <expr> h  (getcurpos()[2] == 1) && (foldlevel('.') != 0) ? 'zc' : 'h'

"set nofoldenable

set foldminlines=3
set fillchars=fold:\  foldtext='==\ '.substitute(getline(v:foldstart),'{{{','','g').'\ \ '.(v:foldend-v:foldstart).'\ '
" }}} <- dummy

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
    " Maybe true  if and only if  inside a multiline string.
    let synNames = map(synstack(a:lnum, 1), "synIDattr(synIDtrans(v:val), 'name')")
    return len(synNames) > 0 && synNames[-1] == "String"
endfu

" Markdown folding (https://stackoverflow.com/a/4677454)
function! MarkdownLevel()
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
endfunction

aug vimrc_folding
    au!
    au FileType lua,javascript,python,markdown call MyFolding()
aug END

fu! MyFolding()
    let ft = &ft
    if index(["lua", "javascript", "html", "perl"], ft) != -1
        setl fdm=expr fde=MyFold(v:lnum,0)
    elseif ft == "python"
        setl fdm=expr fde=MyFold(v:lnum,1)
    elseif ft == "Markdown"
        setl fdm=expr fde=MarkdownLevel()
    endif
endfu

nnore <f7> :call MyFolding()<cr>
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

" If popup window is open, close it
fu! SmartEsc()
    let nr = 1 + index(map(range(1, winnr("$")), "win_gettype(v:val)"), 'popup')
    if nr >= 1 | exe nr .. "wincmd q" | endif
    exe "norm! \<escape>"
endfu
nnore <silent> <escape> :call SmartEsc()<cr>

" Do not move cursor position on leaving insert mode
augroup vimrc_myesc
  autocmd!
  " Two autocmds needed (for when cursor is at beginning or end of line)
  autocmd InsertLeavePre * let myesc_col = col(".")
  autocmd InsertLeave    * exe (myesc_col >= 2) ? "norm! l" : ""
augroup END
" }}}

" Keys (keyboard layout agnostic) {{{

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

" misc
nnore s/ :noh<cr>:let @/ = ""<cr>
nnore s* :call AddHighlight()<cr>
nnore :<cr> :<up><cr>
tnore <esc> <c-\><c-n>
nnore <c-h> :call DescribeFace()<cr>
nnore <c-k> :lua browseDoc(false)<cr>
vnore <c-k> :lua browseDoc(true)<cr>
command! -nargs=* BD lua browseDoc(false, "<args>")
nnore <c-l> :call RecenterTopBottom()<cr>

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

" }}}

" Completion {{{
" TODO fish like key (right to complete)
inore <expr> <tab>       pumvisible() ? "\<c-n>" : "\<c-x>\<c-u>"
inore <expr> <plug>MyTab pumvisible() ? "\<c-n>" : "\<c-x>\<c-u>"
inore <expr> <s-tab>     pumvisible() ? "\<c-p>" : "\<c-x>\<c-u>"
"inore <expr> <del>       pumvisible() ? "\<c-e>" : "\<del>"

inore <expr> <c-f> pumvisible() ? "\<c-n>" : "\<c-x>\<c-f>"

set shortmess+=c                           " No message like "Pattern not found"
set completeopt+=menuone,noinsert,noselect " Needed for auto completion
set completeopt+=longest
set completeopt-=menu,preview
set infercase
"set omnifunc=syntaxcomplete#Complete

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

  " " Completion using dictionary files for rlwrap
  " fu! AddCompSource(ft, name)
  " " exe "au FileType " . a:ft . " setl cpt+=k~/.cpt/" . a:name
  "   exe "au FileType " . a:ft . " setl cpt+=k~/.rlwrap/" . a:name . "_completions"
  " endfu
  " for ft in ["lua", "python"] | call AddCompSource(ft, ft) | endfor
  " " call AddCompSource("javascript", "node")

  au FileType lua setl iskeyword+=.

  " Auto complete (https://stackoverflow.com/questions/35837990)
  fu! OpenCompletion()
    " check (menu visible && inserting alphabet && at least comp_minlen chars)
    if !pumvisible() && ((v:char >= 'a' && v:char <= 'z') || (v:char >= 'A' && v:char <= 'Z')) && (g:comp_minlen == 1 || (col(".") >= (g:comp_minlen-1) && matchstr(getline("."), '\%' . (col('.')-(g:comp_minlen-1)) . 'c[a-zA-Z_]\{' . (g:comp_minlen-1) . '\}') != ""))
      call feedkeys("\<plug>MyTab", "")
"      call feedkeys("\<c-x>\<c-u>", "n") " this will mess up repeating (.)
    endif
  endfu
  au InsertCharPre * call OpenCompletion()

aug END
" }}}

" Colorscheme, syntax, cursor {{{
"colorscheme slate
"colorscheme ron
colorscheme default

" --- Custom Syntax ---
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

        " now define region
        syn region jsVarDef matchgroup=jsVarDefType start=/\<const\>/ start=/\<let\>/ start=/\<var\>/ matchgroup=NONE end=/;/ keepend end=/[^,;]$/ transparent containedin=javaScript contains=javaScript[a-zA-Z].*,jsVarDefName,jsVarDefWrap
        " symbols inside function calls, arrays and objects are not varname (except destructuring assignment)
        syn region jsVarDefWrap start=/\(\(const\|let\|var\)\_s*\)\@<![[({]/ end=/[])}]/ keepend transparent contained contains=javaScript[a-zA-Z].*

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
    endif
endfu
nnore <f6> :call MySyntax()<cr>
"let firstbuf = bufnr("%")
"bufdo call MySyntax()
"exec "b" firstbuf
aug vimrc_syn
  au!
  au Syntax,FileType javascript,html call MySyntax()
  au BufNewFile,BufRead *.js,*.html call MySyntax()
aug END

"hi myVarName ctermfg=blue cterm=bold
hi myVarName ctermfg=cyan cterm=bold
hi link vimMapLhs    myVarName
hi link jsVarDefName myVarName
hi link jsVarDefType Type

hi myFuncName ctermfg=yellow cterm=bold
hi link vimFunction   myFuncName
hi link jsFuncDefName myFuncName


" --- Custom Highlight ---
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
  " hi Folded       ctermfg=magenta ctermbg=black cterm=bold
  hi Folded       ctermfg=magenta ctermbg=236 cterm=bold
"  hi Folded       ctermfg=magenta ctermbg=none cterm=bold
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

  " Pmenu (completion popup menu)
  " hi Pmenu        ctermfg=magenta ctermbg=237 cterm=bold
  " hi PmenuSel     ctermfg=magenta ctermbg=black cterm=bold,reverse
  " hi Pmenu        ctermfg=236 ctermbg=blue cterm=bold
  " hi PmenuSel     ctermfg=236 ctermbg=blue cterm=bold,reverse
  hi Pmenu        ctermfg=blue ctermbg=238 cterm=bold
  hi PmenuSel     ctermfg=blue ctermbg=black cterm=bold,reverse

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
endfu
call MyHighlight()
aug vimrc_hi " :hi need to be in autocmd on first run??
  au!
  au VimEnter * :call MyHighlight()
aug END


" --- Cursor ---
"set cursorline
"aug vimrc_cursor
"  au!
"  au! InsertEnter * set nocursorline!
"  au! InsertLeave * set cursorline!
"aug END


" --- Misc ---
set list
set listchars=tab:\ \ ,trail:.

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
" }}}

" Surround {{{
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
" }}}

" Fzf {{{
fu! FzfExists()
    return executable("fzf")
endfu
let s:fzf_callback = 0
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
    " Print a:list to tmpfile then pipe to fzf
    " (Can I pipe a:list directly to :term ? (it's possible if :! instead of :term))
    let tmpfile = tempname()
    call writefile(a:list, tmpfile)
    " Can't use termopen() here, as it *replaces* current buf with :term buf (thus lose the current buf)
    exe "term sh -c 'cat " . tmpfile . " | fzf'"
endfu
fu! FzfOnExit(stdout) " stdout = list of strings
    " Delete fzf buffer
    bd!
    " Do nothing if user cancelled fzf
    if -1 == index(a:stdout, "[Process exited 0]") | return | endif
    " Call callback with selected string (first line of stdout)
    call s:fzf_callback(a:stdout[0])
endfu
com! -bar       FzfBuffers call Fzf(map(filter(range(1, bufnr("$")), "buflisted(v:val)"), "bufname(v:val)"), { sel -> execute("edit " . sel . "") })
com! -bar -bang FzfFiles   call Fzf(expand("<bang>" == "" ? "*" : "**", 0, 1),                               { sel -> execute("edit " . sel . "") })
" }}}

" Repl {{{
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
" }}}

" Eldoc {{{
set updatetime=1200

" TODO use info from lsp hover
" TODO function argument info
fu! MyEldoc()

    echohl ModeMsg
    echon ">>  "

    " Find nearest line to show in eldoc (start of function, org-mode heading etc.)

    if &ft == "org"
        " TODO show heading hierarchy (e.g. "* Diary > ** 06 > *** dinner")
        let lnum = search('^\*\+ ', "bWn")
    else
        let lnum = search("^[^ \t#/]\\{2}.*[^:]\s*$", "bWn")
    endif

    " Scan each character and show it in its syntax-highlighted color.

    let text     = getline(lnum)[:winwidth(".")-20] " TODO escape sequences and hiragana ==> width 2
    let namePrev = ""
    let start    = 0

    for i in range(len(text))
        let stack = synstack(lnum, i+1)
        let name  = len(stack) == 0 ? "None" : synIDattr(stack[-1], "name")
        if name != namePrev
            if i > 0 | echon text[start:i-1] | endif
            exe "echohl " . name
            let namePrev = name
            let start    = i
        endif
    endfor

    echon text[start:]
    echohl None

endfu

aug vimrc_eldoc
    au!
    " au CursorHold,CursorHoldI * call MyEldoc()
aug END
" }}}

" Filetype specific {{{

" === HTML ===
let g:html_indent_autotags="html,head,body,style" " no indent for these tags
let g:html_indent_script1="zero"
let g:html_indent_style1="zero"
aug vimrc_ft_html
    au!
    au BufNewFile,BufRead *.html setl tabstop=4 shiftwidth=4
    au BufNewFile,BufRead *.html call HtmlIndent_CheckUserSettings()
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
    syn region orgMonospace  start=/\~[^ ]/    end=/\~/  oneline
    syn region orgMacro      start=/{{{/       end=/}}}/ oneline contains=orgMacroComma,orgMacroName
    syn match  orgTex        /\\\S\+/
    syn match  orgMacroComma /\\\@<!,/         contained
    syn match  orgMacroName  /\({{{\)\@<=\w\+/ contained
    " }}} <- dummy
    syn keyword orgTODO TODO
    let b:current_syntax = "org"
    hi link orgProperty   String
    hi link orgComment    Comment
    hi      orgHeading1   ctermfg=cyan cterm=bold,underline
    hi      orgHeading2   ctermfg=cyan
    hi      orgHeading3   ctermfg=green
    hi      orgHeading4   ctermfg=magenta
    hi      orgMathInline ctermfg=blue
    hi link orgBold       Statement
    hi link orgMonospace  String
    hi link orgTex        Preproc
    hi link orgMacro      Special
    hi      orgMacroComma ctermfg=green ctermbg=black cterm=reverse,bold
    hi link orgMacroName  Identifier
    hi link orgTODO       Todo
endfu
aug vimrc_ft_org
    au!
    au BufNewFile,BufRead *.org call MyOrgSyntaxHighlight()
aug END

" === Scheme ===
aug vimrc_ft_scheme
    au!
    au BufNewFile,BufRead *.scm setl formatoptions+=rol
aug END

" }}}


" Lua part

lua << EOFLUA

do -- Keys (keyboard layout specific) {{{

    local mykbd = (vim.env and vim.env.MYKBD == "colemakdh")

    local mappings = {
        "nv  j  n  gj",
        "nv  k  e  gk",
        "nv  gj gn j",
        "nv  gk ge k",
        "nv  J  N  <c-d>",
        "nv  K  E  <c-u>",
        "v   h  k  h",
        "v   l  i  l",
        "nv  gh gk :lua smartHome()<cr>",
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
        local a1,a2,a3,a4
        for i1,i2,i3,i4 in entry:gmatch("(%S+)%s+(%S+)%s+(%S+)%s+(.+)") do a1,a2,a3,a4=i1,i2,i3,i4 break end
        for mode in a1:gmatch("%S") do
            vim.cmd(mode .. "nore " .. (mykbd and a3 or a2) .. " " .. a4)
        end
    end

    vim.cmd("nnore <expr> " .. (mykbd and "k" or "h") .. " (getcurpos()[2] == 1) && (foldlevel('.') != 0) ? 'zc' : 'h'")
    vim.cmd("nnore <expr> " .. (mykbd and "i" or "l") .. " (foldclosed('.') != -1) ? 'zo' : 'l'")

end -- }}}

function smartSp(file, isBufNr) -- Split or VSplit and return new bufnr {{{
    if vim.fn.bufname(".") == "" then
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
end -- }}}

local smartf_last_time, smartf_last_char, smartf_rev = 0, nil, nil
function smartf(direction) -- {{{
    local function regEscape(s) return (s:gsub("([^%w])", "%%%1")) end
    local function revfind(str, char, init)
        local n = str:reverse():find(char, #str + 1 - init)
        return n and (#str + 1 - n)
    end
    local time = os.time()
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
end -- }}}

function smartq() -- Close temporary window or act as q key (recording) {{{

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

end -- }}}

function toggleCmt(visual) -- {{{
    -- TODO dont put "# " etc on empty line when commenting
    local function hasSyntax(synName)
        for _, v in pairs(vim.fn.synstack(vim.fn.line("."), vim.fn.col("."))) do
            if vim.fn.synIDattr(v, "name") == synName then
                return true
            end
        end
        return false
    end
    local function getCMSHere()
        if vim.bo.ft == "vim" and hasSyntax("vimLuaRegion") then
            return "--%s"
        elseif vim.bo.ft == "html" and hasSyntax("javaScript") then
            return "//%s"
        elseif vim.bo.ft == "html" and hasSyntax("cssStyle") then
            return "/*%s*/"
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

end -- }}}

function smartHome(insert) -- {{{
    local c = vim.fn.col(".") - (insert and 1 or 0)
    vim.cmd("norm! ^")
    if c == vim.fn.col(".") then vim.cmd("norm! 0") end
end -- }}}

function getDefunByIndent() -- Get current block in lisps, python etc. {{{
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
end -- }}}

function browseDoc(visual, text) -- {{{
    text = visual and vim.fn.GetVisualSelection() or (text or vim.fn.expand("<cword>"))
    local ft    = vim.bo.ft
    local extra = (ft == "javascript" or ft == "html") and " mdn" or ""
    local query = text .. " " .. ft .. extra
    -- TODO: w3m?
    local cmd   = "sil! !firefox 'https://lite.duckduckgo.com/lite/?q=" .. query .. "'"
    vim.cmd(cmd)
end -- }}}

-- My completefunc {{{

-- TODO: abstract memoization, should use prefix for performance?, async syntaxcomplete, too slow on first call
-- TODO: show source buffer, tags

local mycomp_cache = nil -- mycomp_cache[i] = comps cached when base:len() == i
function mycomp(findstart, base) -- {{{
    if findstart == 1 then
        -- Initialize completion                          Example
        local line  = vim.fn.getline(vim.fn.line("."))    --  "abc def_ gh" (_ means cursor)
        local col   = vim.fn.col(".")                     --          ^ cursor position
        local sub   = line:sub(1, col - 1)                --  "abc def"
        local match = sub:match(mycomp_word_reg() .. "$") --      "def"
        return match and (col - #match - 1) or -2 -- , { sub=sub, col=col, match=match, len=len }
    else
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
end -- }}}

function mycomp_done() -- Reset cache, add history etc. {{{
    mycomp_cache = nil
    local comp = vim.v.completed_item
    if comp then mycomp_add_history(comp) end
end -- }}}

-- function mycomp_memoizef(func, shouldUpdate) -- memoization {{{
--     local cache = {}
--     return function(...)
--         if shouldUpdate() then
--             cache = func()
--         end
--         return cache
--     end
-- end -- }}}

function mycomp_word_reg() -- Make regexp that match chars in 'iskeyword' {{{
    -- See https://vi.stackexchange.com/questions/31478
    -- return something like "[abc]+"

    -- Note: We limit to "usual" ascii chars (0x21 '!' to 0x7e '~')
    --       to prevent garbage code like <80> in completion.
    --       (happens when buffer contains multibyte characters)
    --       Multibyte strings are not welcome in VimL/Lua5.1

    return "[" .. vim.fn.substitute(vim.fn.join(vim.fn.map(vim.fn.range(0x21, 0x7e), 'nr2char(v:val)'), ''), '[^[:keyword:]]', '', 'g') .. "]+"
end -- }}}

function mycomp_compword(comp) -- (1) { word=w } => w, (2) 'str' => 'str' {{{
    return (type(comp) == "table" and comp.word) and comp.word or ((type(comp) == "string") and comp or nil)
end -- }}}

function mycomp_filter(base, list) -- {{{
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
end -- }}}

function mycomp_collect() -- Collect words {{{
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
                item.menu    = source .. " " .. (comp.menu or "")
                item._source = source

            end
        end
    end

    return result
end -- }}}

local mycomp_history, mycomp_history_max = {}, 300
function mycomp_collect_history() -- Collect from completion history {{{
    return mycomp_history
end -- }}}
function mycomp_add_history(comp) -- {{{
    if mycomp_compword(comp) then
        table.insert(mycomp_history, 1, mycomp_compword(comp))
    end
    if #mycomp_history > mycomp_history_max * 2 then
        while #mycomp_history > mycomp_history_max do
            table.remove(mycomp_history, #mycomp_history)
        end
    end
end -- }}}

function mycomp_collect_bufferall() -- Collect from all bufs {{{
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
end -- }}}

local mycomp_collect_buffer_cache = {}
function mycomp_collect_buffer(buf) -- Collect from a buf {{{
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
end -- }}}

function mycomp_collect_omni() -- Collect from omnifunc {{{
    if (not vim.bo.omnifunc) or (vim.bo.omnifunc == "") then
        return {}
    end
    local function callOmnifunc(findstart, base)
        local ofu = vim.bo.omnifunc
        if ofu == "v:lua.vim.lsp.omnifunc" then
            -- how to programatically call ofu that starts with "v:lua" ?
            -- return vim.lsp.omnifunc(findstart, base)
            return mycomp_lsp_omnifunc_sync(findstart, base)
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
        return omnicomps
    else
        -- No omni completion at cursor
        mycomp_collect_omni_cache = {}
        return {}
    end
end -- }}}

mycomp_lsp_omnifunc_cache = nil
function mycomp_lsp_omnifunc_sync(findstart, base) -- synchronous lsp omnifunc (https://github.com/neovim/neovim/issues/12390) {{{
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
    vim.api.nvim_set_current_line(mycomp_lsp_omnifunc_cache.line)
    vim.api.nvim_win_set_cursor(0, mycomp_lsp_omnifunc_cache.pos)

    -- Make request
    local bufnr = vim.api.nvim_get_current_buf()
    local params = vim.lsp.util.make_position_params()
    local result = vim.lsp.buf_request_sync(bufnr, 'textDocument/completion', params, 2000)
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
    vim.api.nvim_set_current_line(line)
    vim.api.nvim_win_set_cursor(0, pos)

    return items
end
-- }}}

local mycomp_collect_keywords_cache = {}
local mycomp_collect_keywords_extra = { -- extra keywords for mycomp_collect_keywords {{{
    javascript = {
        "console.log", "console.error",
        "clearTimeout", "clearInterval", "setTimeout", "setInterval",
        "getContext",
        "addEventListener", "createElement",
        "constructor",
        -- Array
        "map", "forEach", "filter", "reduce", "reduceRight", "every", "some", "indexOf", "lastIndexOf", "slice",
    },
} -- }}}
function mycomp_collect_keywords() -- Collect from (1) keyword file and (2) syntaxcomplete TODO: split 1 and 2 {{{
    local ft = vim.bo.ft
    if mycomp_collect_keywords_cache[ft] then
        return mycomp_collect_keywords_cache[ft]
    end
    -- Helpers
    local function mycomp_append(...) -- merge arrays {{{
        local res = {}
        for _, tbl in ipairs({...}) do
            for _, x in ipairs(tbl) do
                table.insert(res, x)
            end
        end
        return res
    end -- }}}
    local function load_comp_file(filetype) -- (1) try read ~/.rlwrap/XXX_completions {{{
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
    end -- }}}
    -- Combine results from (1) and (2)
    local res = mycomp_append(load_comp_file(ft) or {}, vim.call("syntaxcomplete#OmniSyntaxList"), (ft == "vim" and load_comp_file("lua")) or {}, mycomp_collect_keywords_extra[ft] or (ft == "html" and mycomp_collect_keywords_extra["javascript"]) or {})
    mycomp_collect_keywords_cache[ft] = res
    -- if html, include css, js
    -- if vim,  include lua
    -- how to call syntaxcomplete#OmniSyntaxList for ft different from current buf?
    return res
end -- }}}

-- }}}

function pp(input, doprint, maxdepth, ...) -- Pretty print lua {{{
    -- TODO avoid circular recursion
    -- If 'doprint' is true and 'input' is given, print like in repl into *Messages* buffer
    local function compare(x, y) -- compare any two values
        local tx, ty = type(x), type(y)
        if (tx == "number" and ty == "number") or (tx == "string" and ty == "string") then
            return x < y
        else
            return tostring(x) < tostring(y)
        end
    end
    local strs = {}
    for _, x in pairs({...}) do
        if type(x) == "table" then
            -- TODO Maybe corrupt when x = { 20, nil, 30 }
            -- Check if keys are exactly 1, 2, ..., n
            local npairs, nipairs = 0, 0
            for k, v in pairs(x) do
                npairs = npairs + 1
            end
            for k, v in ipairs(x) do
                nipairs = nipairs + 1
            end
            local isarray = (npairs == nipairs)
            -- Sort keys
            local ks, i = {}, 0
            for k, y in pairs(x) do
                i = i + 1
                ks[i] = k
            end
            table.sort(ks, compare)
            -- Table to string, buf omit keys if keys are exactly 1, 2, ..., n
            local s = "{ "
            for j = 1, #ks do
                s = s .. (j == 1 and "" or ", ") .. (isarray and "" or ks[j] .. "=") .. ((maxdepth <= 1) and tostring(x[ks[j]]) or pp(nil, nil, maxdepth - 1, x[ks[j]]))
            end
            table.insert(strs, s .. " }")
        else
            table.insert(strs, tostring(x))
        end
    end
    local allstr = table.concat(strs, "\n")
    -- Optionally print to *Messages* buffer
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
end
function pp1(...)
    pp(false, false, 99, {...})
end -- }}}

EOFLUA


" Wiimote
finish
lua <<EOFLUA
--dirs = { "<left>", "<up>", "<right>", "<down>" }
-- 1 2 11 22
-- l r u d ll rr uu dd
-- 1 2 11 22
strs = { "arx?", "nmf!", "sdg'", "ybpq", "tcz.", "ihj,", "elk@", "ouvw" }
--b1 = { "d", "r", "u", "l", "dd", "rr", "uu", "ll" }
dirs = { "d", "r", "u", "l" }
arr = {}
for i, s in ipairs(strs) do
    local one = (i%2 == 1) and "1" or "11"
    local two = (i%2 == 1) and "2" or "22"
    local d = dirs[math.floor((i-1) / 2) + 1]
    arr[#arr+1] = { one ..d          , s:sub(1, 1) }
    arr[#arr+1] = { one ..d..d       , s:sub(2, 2) }
    arr[#arr+1] = { one ..d..d..d    , s:sub(3, 3) }
    arr[#arr+1] = { one ..d..d..d..d , s:sub(4, 4) }
    arr[#arr+1] = { two ..d          , s:sub(1, 1):upper() }
    arr[#arr+1] = { two ..d..d       , s:sub(2, 2):upper() }
    arr[#arr+1] = { two ..d..d..d    , s:sub(3, 3):upper() }
    arr[#arr+1] = { two ..d..d..d..d , s:sub(4, 4):upper() }
end

function wiidef(from, to)
--    local from2 = from:gsub("r", "<right>"):gsub("u", "<up>"):gsub("l", "<left>"):gsub("d", "<down>")
    local from2 = from:gsub("d", "<down>"):gsub("r", "<right>"):gsub("u", "<up>"):gsub("l", "<left>")
    vim.cmd("inore " .. from2 .. " " .. to)
end

for _, v in ipairs(arr) do
    wiidef(v[1], v[2])
end

EOFLUA

inore b<down>   <space>
inore b<right> <esc>
inore b<up>       <backspace>
imap  b<left>   <tab>

