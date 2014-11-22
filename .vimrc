set nocompatible              " be iMproved, required
filetype off                  " required

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
"set rtp+=~/.vim/bundle/powerline/powerline/bindings/vim
call vundle#begin()
" alternatively, pass a path where Vundle should install plugins
"call vundle#begin('~/some/path/here')

" let Vundle manage Vundle, required
Plugin 'gmarik/Vundle.vim'
Plugin 'tomasr/molokai'
" Plugin 'scrooloose/nerdtree'
Plugin 'tpope/vim-commentary'
Plugin 'tpope/vim-surround'
Plugin 'kien/ctrlp.vim'
Plugin 'mattn/emmet-vim'
Plugin 'pangloss/vim-javascript'
Plugin 'editorconfig/editorconfig-vim'
" Plugin 'L9'
" Plugin 'FuzzyFinder'
Plugin 'tpope/vim-fugitive'
Plugin 'tpope/vim-repeat'
Plugin 'tpope/vim-dispatch'
Plugin 'Markdown-syntax'
Plugin 'SirVer/ultisnips'
Plugin 'honza/vim-snippets'
" Plugin 'mbadran/headlights'
Plugin 'vim-airline'
"Plugin 'mklabs/grunt.vim'
"Plugin 'Lokaltog/powerline' ,{'rtp': '~/.vim/bundle/powerline/powerline/bindings/vim'}
Plugin 'airblade/vim-gitgutter'
"Plugin 'felixge/vim-nodejs-errorformat'
Plugin 'rking/ag.vim' " Faster ack, which is faster grep :)
Plugin 'justinmk/vim-gtfo'
Plugin 'godlygeek/tabular'
Plugin 'terryma/vim-expand-region'
Plugin 'jaxbot/selective-undo.vim'
Plugin 'Valloric/YouCompleteMe'
Plugin 'tacahiroy/ctrlp-funky'
Plugin 'elzr/vim-json'
Plugin 'scrooloose/syntastic'
" Plugin 'wookiehangover/jshint.vim'
Plugin 'tpope/vim-unimpaired'
Plugin 'lfilho/cosco.vim'
" Plugin 'summerfruit256.vim'
Plugin 'marijnh/tern_for_vim'
" Plugin 'altercation/vim-colors-solarized'
Plugin 'chriskempson/base16-vim'

" All of your Plugins must be added before the following line
call vundle#end()            " required
filetype plugin indent on    " required
" To ignore plugin indent changes, instead use:
"filetype plugin on
"
" Brief help
" :PluginList       - lists configured plugins
" :PluginInstall    - installs plugins; append `!` to update or just :PluginUpdate
" :PluginSearch foo - searches for foo; append `!` to refresh local cache
" :PluginClean      - confirms removal of unused plugins; append `!` to auto-approve removal
"
" see :h vundle for more details or wiki for FAQ
" Put your non-Plugin stuff after this line

" Josh starts here...

" Leader key
let mapleader = "\<Space>"

" Ctrl-P
"See: https://github.com/kien/ctrlp.vim
let g:ctrlp_map = '<c-p>'
let g:ctrlp_cmd = 'CtrlP'
let g:ctrlp_working_path_mode = 'ra'
let g:ctrlp_custom_ignore = 'node_modules\|DS_Store\|git'
let g:ctrlp_show_hidden = 1

" map ctrl-p
nnoremap <Leader>t :CtrlP<CR>
nnoremap <Leader>y :CtrlPLine<CR>
nnoremap si :CtrlPBookmarkDir<CR>
nnoremap s<c-i> :CtrlPBookmarkDirAdd<CR>


" Syntastic styntax checker
let g:syntastic_javascript_checkers = ["jshint"]
let g:syntastic_mode_map = { "mode": "passive"}

" CtrlP extension 'ctrlp-funky' allows lookup of function definition, without
" ...ctags
let g:ctrlp_extensions = ['funky']
nnoremap <Leader>fu :CtrlPFunky<Cr> 
" narrow the list down with a word under cursor 
nnoremap <Leader>fU :execute 'CtrlPFunky ' . expand('<cword>')<Cr> 

"  Expand region vim-expand-region
let g:expand_region_text_objects = {
      \ 'iw'  :0,
      \ 'iW'  :0,
      \ 'i"'  :1,
      \ 'i''' :1,
      \ 'i]'  :1, 
      \ 'ib'  :1, 
      \ 'iB'  :1, 
      \ 'ip'  :1,
      \ }

" Aswesome 80-character limiter
" execute "set colorcolumn=" . join(range(81,335), ',')
" hi ColorColumn guibg=#262626 ctermbg=235

" Ag - The Silver Searcher
if executable('ag')
  " Use ag over grep
  set grepprg=ag\ --nogroup\ --nocolor

  " Use ag in CtrlP for listing files. Lightning fast and respects .gitignore
  let g:ctrlp_user_command = 'ag %s -l --hidden --ignore ".git" --nocolor -g ""'

  " ag is fast enough that CtrlP doesn't need to cache
  let g:ctrlp_use_caching = 0

endif

" UltiSnips, a wonderfull snippet manager with python interpolation
" UltiSnip Trigger configuration. Do not use <tab> if you use https://github.com/Valloric/YouCompleteMe.
let g:UltiSnipsExpandTrigger="<c-l>"
let g:UltiSnipsJumpForwardTrigger="<c-j>"
let g:UltiSnipsJumpBackwardTrigger="<c-k>"
" If you want :UltiSnipsEdit to split your window.
let g:UltiSnipsEditSplit="vertical"

let g:tern_show_argument_hints = "on_hold"

" Unmappings
" Make sure nothing conflicts with tmux
map <c-a> <nop>

" ------------------------------ Key maps, Mappings...
" Remap _,_ to _'_  .......... used jumping to marks
" nnoremap , '

" expand region mappings
vmap v <Plug>(expand_region_expand)
vmap <C-v> <Plug>(expand_region_shrink)

" insert-mode mappings
inoremap jk <esc>

" insert-mode mappings
inoremap )) (

" Shortcut to toggle `set number`
nmap <Leader>n :set number!<CR>

" Shortcut to toggle `:SyntasticToggleMode`
nmap <Leader>sn :SyntasticToggleMode<CR>

" Shortcut to rapidly toggle `set list`
nmap <Leader>l :set list!<CR>

" Some useful tips...
" http://sheerun.net/2014/03/21/how-to-boost-your-vim-productivity/

" Map <leader>y and p for clipboard copy/paste
vmap <Leader>y "+y
vmap <Leader>d "+d
nmap <Leader>p "+p
nmap <Leader>P "+P
vmap <Leader>p "+p
vmap <Leader>P "+P

" Change mappings
nnoremap <Leader>0 ct)
nnoremap <Leader>- ct,
nnoremap <Leader>' ct'
nnoremap <Leader>" ct"

" Commentary map
nnoremap <Leader>c :Commentary<CR>
vnoremap <Leader>c :Commentary<CR>

" Map 'K' to search for word under cursor, ie: `find in files`
nnoremap K :Ag "\b<C-R><C-W>\b"<CR>:cw<CR>
vnoremap K :Ag "\b<C-R><C-W>\b"<CR>:cw<CR>

" Map visual indent > to keep selection
" vnoremap < <gv
" vnoremap > >gv

" "Map NERDTree
" nmap <leader>ne :NERDTreeToggle<cr>

" Mapping Tabs
" Map ctrl-w
nnoremap <c-w><c-w> :tabnext<cr>
nnoremap <Leader>T :tabnew<cr>
" nnoremap <leader>wl <C-W>v

"Map ctrl-q to close window...
" ...doesn't seem to work in console??
" nnoremap <c-q> <c-w>q

"Map leader Q to quit without saving
nnoremap <leader>Q :q!<CR>

"Map leader q to quit with saving
nnoremap <leader>q :q<CR>

"Map leader s to save file...
nnoremap <leader>w :w<CR>

" " Map wq to write and quit
" nnoremap <leader>wq :wq<CR>

"Map ctrl-shift-s to source current file...
nnoremap <leader>so :source %<CR>

" <Ctrl-l> redraws the screen and removes any search highlighting.
nnoremap <silent><C-l> :nohl<CR><C-l>

" Josh. Add newlines without leaving normal mode
nnoremap <silent><C-CR> o<ESC>
nnoremap <silent><S-CR> O<ESC>

" Map semi-colon to colon....
" ... no more shift :)
" nnoremap ; :

" Highlight word under cursor (uses search highlighting)
nmap <leader>* *N

" Vundle PluginInstall
nmap <leader>pi :PluginInstall<CR>

" Source local .vimrc file, shortcut...
nnoremap <leader>sv :source .vimrc<CR>

" UltiSnipEdit, shortcut...
nnoremap <silent><F10> :UltiSnipsEdit<CR>

" Edit this file, shortcut...
"nnoremap <silent><F11> :vsp ~/.vimrc<CR>
nnoremap <silent><leader>vrc :vsp ~/.vimrc<CR>

" Edit bashrc
nnoremap <silent><leader>brc :vsp ~/.bashrc<CR>

" Edit gvimrc, shortcut...
"nnoremap <silent><F12> :vsp ~/.gvimrc<CR>
nnoremap <silent><leader>grc :vsp ~/.gvimrc<CR>

" Replace inner word with 0 register...
nnoremap <leader>r diw"0Pb

" Git status mapping
nnoremap <silent><leader>gs :Gstatus<CR>

" Dispatch: npm start mapping
nnoremap <silent><leader>dns :Dispatch npm start<CR>

" Dispatch: npm install mapping
nnoremap <silent><leader>dni :Dispatch npm install<CR>

" Dispatch: npm test mapping
nnoremap <silent><leader>dnt :Dispatch npm test<CR>

" Dispatch: in ex mode
nnoremap <leader>dd :Dispatch<SPACE>

" Spelling on/off
nnoremap <leader>sp :set spell!<CR>

" Local .vimrc files...
"set exrc            " enable per-directory .vimrc files
"set secure          " disable unsafe commands in local .vimrc files

" Git Gutter
let g:gitgutter_map_keys = 0
nmap [h <Plug>GitGutterPrevHunk
nmap ]h <Plug>GitGutterNextHunk
nmap <Leader>hs <Plug>GitGutterStageHunk
nmap <Leader>hr <Plug>GitGutterRevertHunk

" line numbers - advanced
" Relative/Absolute lines numbers...
" set relativenumber
" autocmd InsertEnter * :set number
" autocmd InsertLeave * :set relativenumber
" autocmd FocusLost * :set number
" autocmd FocusGained * :set relativenumber

" my filetype syntax definitions
augroup filetypedetect 
  au! BufRead,BufNewFile *.hbs		set filetype=html
  au! BufRead,BufNewFile *.md  		set filetype=markdown
  au BufRead,BufNewFile *.json          set filetype=json
  " au! BufRead,BufNewFile *.xyz		setfiletype drawing
augroup END

" vim-surround, with a function!  Ed: not really used

autocmd FileType js let b:surround_45 = "function(){ \r }" " 45 = '-'. You can use `:echo char2nr('-')
autocmd FileType javascript,css noremap <silent>;; :call cosco#commaOrSemiColon()<CR>
" autocmd FileType javascript,css inoremap <silent>;; <c-o>:call cosco#commaOrSemiColon()<CR>

" Regex breaks syntax highlighting, workaround...
" ...mostly for the haroogon (can't remember spelling) Windows compiled bin
  "set regexpengine=1 

" Colours, colors
syntax on
" colorscheme molokai
" colorscheme summerfruit256
set background=light
" let g:solarized_termcolors=256
let base16colorspace=256  " Access colors present in 256 colorspace
colorscheme base16-solarized
" hi MatchParen ctermbg=3 cterm=underline term=NONE

" --------------------------------------- Oneliners

" Use the same symbols as TextMate for tabstops and EOLs
set listchars=tab:▸\ ,eol:¬,nbsp:_,trail:#

" fixed line numbers
" set number

" Autocomplete in command line...
set wildmode=longest,list,full
set wildmenu

" Backspace, goes over everything
set backspace=indent,eol,start
"set backspace=2

" Backup files, who needs 'em?
set nobackup
set nowritebackup
set noswapfile

" Indents       
set shiftwidth=2
set expandtab
set nowrap
set autoindent

" Colours
set visualbell
" set t_Co=256
set t_Co=16

" Search
set hlsearch
set incsearch
set ignorecase    " ignore case when searching
set smartcase     " ignore case if search pattern is all lowercase,
                    "    case-sensitive otherwise
"set guifont=Consolas\ for\ Powerline\ FixedD:h10

set encoding=utf-8
set laststatus=2

" The column where your cursor is, is highlighted. It's warming to me
set cursorline

" Status line with vim-airline
" Dfine if doesn't exist...
let g:airline#extensions#hunks#enabled = 1
let g:airline#extensions#branch#enabled = 1

if !exists('g:airline_symbols')
	let g:airline_symbols = {}
endif

" let g:airline_theme='tomorrow' " looks better than my normal molokai, but only for status bar
let g:airline_powerline_fonts=1

" Should be plugins, but have included them here...

" My Search strings, yank them into register then search...
" "\(.\{-}\)"/'\1'/g 

" ------------------------------ Another function
" .vim/plugin/qfdo.vim
" Run a command on each line in the Quickfix buffer.
" Qfdo! uses the location list instead.
" Author: Christian Brabandt
" Author: Douglas
" See: http://vim.1045645.n5.nabble.com/execute-command-in-vim-grep-results-td3236900.html
" See: http://efiquest.org/2009-02-19/32/
" Usage:
"     :Qfdo s#this#that#
"     :Qfdo! s#this#that#
"     :Qfdofile %s#this#that#
"     :Qfdofile! %s#this#that#

" Christian Brabandt runs the command on each *file*
" I have mapped Qfdo to line-by-line below
function! QFDo(bang, command)
   let qflist={}
   if a:bang
      let tlist=map(getloclist(0), 'get(v:val, ''bufnr'')')
   else
      let tlist=map(getqflist(), 'get(v:val, ''bufnr'')')
   endif
   if empty(tlist)
      echomsg "Empty Quickfixlist. Aborting"
      return
   endif
   for nr in tlist
      let item=fnameescape(bufname(nr))
      if !get(qflist, item,0)
            let qflist[item]=1
      endif
   endfor
   execute 'argl ' .join(keys(qflist))
   execute 'argdo ' . a:command
endfunction

" Run the command on each *line* in the Quickfix buffer (or location list)
" My own crack at it, based on Pavel Shevaev on efiquest
function! QFDo_each_line(bang, command)
   try
      if a:bang
         silent lrewind
      else
         silent crewind
      endif
      while 1
         echo bufname("%") line(".")
         execute a:command
         if a:bang
            silent lnext
         else
            silent cnext
         endif
      endwhile
   catch /^Vim\%((\a\+)\)\=:E\%(553\|42\):/
   endtry
endfunction

command! -nargs=1 -bang Qfdo :call QFDo_each_line(<bang>0,<q-args>)
command! -nargs=1 -bang Qfdofile :call QFDo(<bang>0,<q-args>)


" ------------------------------ Another function
"See diff of unsaved changes...
function! s:DiffWithSaved()
  let filetype=&ft
  diffthis
  vnew | r # | normal! 1Gdd
  diffthis
  exe "setlocal bt=nofile bh=wipe nobl noswf ro ft=" . filetype
endfunction
com! DiffSaved call s:DiffWithSaved()
nmap <leader>ds :DiffSaved<CR>

