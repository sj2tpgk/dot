" vim: fdm=marker
" TODO comment take indenting into account
" TODO update encoding (let format not precompiled)
" TODO describe-face
" TODO auto 16color

lua if not vim.cmd then vim.cmd = vim.api.nvim_command end

" Easy lua debug. Use L! to print nested table. Example ":L 123,456"
command! -nargs=* -bang -complete=lua L lua pp(<q-args>, true, ("<bang>" == "") and 1 or 99, <args>)

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

" Status line: show encoding
set statusline=%f\ %h%w%m%r\ \ \ (%{&fileencoding})\ %=%-14.(%l,%c%V%)\ %P
set laststatus=2

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
"nnore <expr> h  (getcurpos()[2] == 1) && (foldlevel('.') != 0) ? 'zc' : 'h'

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
  " Two autocmds needed (for when cursor is at beginning or end of line)
  autocmd InsertLeavePre * let vimrc_inoreesc_col = col(".")
  autocmd InsertLeave    * exe (vimrc_inoreesc_col >= 2) ? "norm! l" : ""
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
nnore <silent> 0      :lua smartHome()<cr>
nnore <silent> <home> :lua smartHome()<cr>
inore <silent> <home> <c-o>:lua smartHome(true)<cr>

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
nnore <a-j> J
nnore <silent> ; :lua toggleCmt(false)<cr>
vnore <silent> ; :lua toggleCmt(true)<cr>

" window, buffer
nnore Q :q<cr>
nnore - <C-w>w
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

" misc
nnore s/ :noh<cr>:let @/ = ""<cr>
nnore :<cr> :<up><cr>
tnore <esc> <c-\><c-n>
nnore <c-h>F :call DescribeFace()<cr>

fu! SaveExcursion(normcmd)
  let l:w = winsaveview()
  exe "norm! " . a:normcmd
  call winrestview(l:w)
endfu

" }}}

" Completion {{{
" TODO fish like key (right to complete)
inore <expr> <tab>       pumvisible() ? "\<c-n>" : "\<c-x>\<c-u>"
inore <expr> <plug>MyTab pumvisible() ? "\<c-n>" : "\<c-x>\<c-u>"
inore <expr> <s-tab>     pumvisible() ? "\<c-p>" : "\<c-x>\<c-u>"
"inore <expr> <del>       pumvisible() ? "\<c-e>" : "\<del>"

set shortmess+=c                           " No message like "Pattern not found"
set completeopt+=menuone,noinsert,noselect " Needed for auto completion
set completeopt+=longest
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

let g:comp_minlen = 3  " At least N chars to start completion

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
  hi Type         ctermfg=cyan
  hi Conditional  ctermfg=green cterm=bold
  hi preproc      ctermfg=cyan
  "hi Identifier   ctermfg=red cterm=bold
  hi Special      ctermfg=red
  hi Folded       ctermfg=magenta ctermbg=black cterm=bold
  hi Visual       ctermfg=black ctermbg=blue

  " Pmenu (completion popup menu)
  hi Pmenu        ctermfg=magenta ctermbg=black cterm=bold
  hi PmenuSel     ctermfg=magenta ctermbg=black cterm=bold,reverse

  " Filetype specific
  " === HTML ===
  hi link javaScript Normal
  hi link htmlEvent  Special
endfu
call MyColor()
aug vimrc_hi " :hi need to be in autocmd on first run??
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

fu! DescribeFace()
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

" Repl
" b:terminal_job_id
" jobsend(3, join(getline(1,"$"), "\n") . "\n")

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
" }}}

" Lua part

lua << EOFLUA

do -- Keys (keyboard layout specific) {{{

    local mykbd = (vim.env and vim.env.MYKBD == "colemakdh")

    local mappings = {
        "nv  j  n  gj",
        "nv  k  e  gk",
        "nv  J  N  <c-d>",
        "nv  K  E  <c-u>",
        "v   h  k  h",
        "v   l  i  l",
        "nv  gh gk :lua smartHome()<cr>",
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
    -- TODO multiple lines with commented & not commented mixed
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
            return vim.bo.cms
        end
    end
    local function regEscape(s) return (s:gsub("([^%w])", "%%%1")) end

    -- Analyze commentstring
    local cms    = getCMSHere()
    local x,y,z  = cms:match("(.*%S)(%s*)%%s(.*)")
    local p      = "^(%s*)" .. regEscape(x) .. "(.*)" .. regEscape(z)
    local p1     = "^(%s*)" .. regEscape(x) .. y:gsub(".", " ?") .. "(.*)" .. regEscape(z)

    -- Suppose cms == "# %s"
    -- p  : must match line if middle space is absent
    -- p1 : middle space must be at most 1 (%s should eat rest spaces)

    -- Get range
    local lbeg   = visual and vim.fn.line("'<") or vim.fn.line(".")
    local lend   = visual and vim.fn.line("'>") or (lbeg + math.max(0, vim.v.count - 1))

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

function smartHome(insert) -- {{{
    local c = vim.fn.col(".") - (insert and 1 or 0)
    vim.cmd("norm! ^")
    if c == vim.fn.col(".") then vim.cmd("norm! 0") end
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
    return "[" .. vim.fn.substitute(vim.fn.join(vim.fn.map(vim.fn.range(256), 'nr2char(v:val)'), ''), '[^[:keyword:]]', '', 'g') .. "]+"
end -- }}}

function mycomp_compword(comp) -- (1) { word=w } => w, (2) 'str' => 'str' {{{
    return (type(comp) == "table" and comp.word) and comp.word or ((type(comp) == "string") and comp or nil)
end -- }}}

function mycomp_filter(base, list) -- {{{
--    vim.cmd("sleep " .. math.min(1000, math.floor(1 + #list)) .. "m")
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
        if w:match(fuzreg) then
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
end -- }}}

function mycomp_collect() -- Collect words {{{
--    vim.cmd("sleep 1")
    local res, seen = {}, {}
    local comps_list = { { "h", mycomp_collect_history() }, { "b", mycomp_collect_bufferall() }, { "k", mycomp_collect_keywords() }, { "o", mycomp_collect_omni() } }
    for _, v in ipairs(comps_list) do
        local source, comps = v[1], v[2]
        for _, comp in pairs(comps) do
            -- when dupe occurs, first source is kept
            local w = mycomp_compword(comp)
            if (not seen[w]) then
                table.insert(res, { word = w, menu = source .. " " .. (comp.menu or ""), kind = comp.kind })
                seen[w] = true
            end
        end
    end
    return res
end -- }}}

local mycomp_history, mycomp_history_max = {}, 50
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
    -- Emulate first call of omnifunc (a:findstart = 1)
    local col = vim.call(vim.bo.omnifunc, 1, nil)
    if col >= 0 then
        -- Emulate second call of omnifunc (a:findstart = 0, a:base = the word being typed)
        local ofu_base = vim.fn.getline("."):sub(col + 1)
        local omnicomps = vim.call(vim.bo.omnifunc, 0, ofu_base)
        mycomp_collect_omni_cache = omnicomps
        return omnicomps
    else
        -- No omni completion at cursor
        mycomp_collect_omni_cache = {}
        return {}
    end
end -- }}}

local mycomp_collect_keywords_cache = {}
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
    local res = mycomp_append(load_comp_file(ft) or {}, vim.call("syntaxcomplete#OmniSyntaxList"), (ft == "vim" and load_comp_file("lua")) or {})
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
end -- }}}

EOFLUA
