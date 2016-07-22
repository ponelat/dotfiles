set nocompatible              " be iMproved, required
filetype off                  " required

" My own runtime
set rtp+=~/dotfiles/.vim

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

" let Vundle manage Vundle, required
Plugin 'gmarik/Vundle.vim'

Plugin 'tpope/vim-commentary'
Plugin 'tpope/vim-surround'
Plugin 'tpope/vim-fugitive'
Plugin 'tpope/vim-repeat'
Plugin 'tpope/vim-dispatch'
" Plugin 'tpope/vim-speeddating'
Plugin 'tpope/vim-unimpaired'
Plugin 'tpope/vim-jdaddy'
Plugin 'tpope/vim-eunuch'

Plugin 'scrooloose/syntastic'
Plugin 'jelera/vim-javascript-syntax'
Plugin 'pangloss/vim-javascript'
Plugin 'othree/javascript-libraries-syntax.vim'
Plugin 'marijnh/tern_for_vim'
Plugin 'mxw/vim-jsx'

Plugin 'editorconfig/editorconfig-vim'
Plugin 'vim-airline'
Plugin 'airblade/vim-gitgutter'
Plugin 'gabesoft/vim-ags' " Faster ack, which is faster grep :)
Plugin 'chriskempson/base16-vim'
Plugin 'christoomey/vim-tmux-navigator'
Plugin 'tmhedberg/matchit'
Plugin 'kien/ctrlp.vim'
Plugin 'mattn/emmet-vim'
Plugin 'kana/vim-textobj-user'
Plugin 'kana/vim-textobj-function'
Plugin 'kana/vim-textobj-line'
Plugin 'kana/vim-textobj-indent'
Plugin 'glts/vim-textobj-comment'
Plugin 'vim-scripts/ReplaceWithRegister'


Plugin 'SirVer/ultisnips'
Plugin 'Valloric/YouCompleteMe'
Plugin 'honza/vim-snippets'

Plugin 'Markdown-syntax'
Plugin 'jtratner/vim-flavored-markdown.git'
Plugin 'elzr/vim-json'
Plugin 'groenewege/vim-less'
Plugin 'powerman/vim-plugin-AnsiEsc'

Plugin 'neovim/node-host'
Plugin 'snoe/nvim-parinfer.js'

" All of your Plugins must be added before the following line
call vundle#end()            " required
filetype plugin indent on    " required

" Leader key
let mapleader = "\<Space>"
let maplocalleader = ","

"=========================================== Ctrl-P
"See: https://github.com/kien/ctrlp.vim
let g:ctrlp_map = '<c-p>'
let g:ctrlp_cmd = 'CtrlP'
let g:ctrlp_working_path_mode = 'raw'
" Selecting a file will default to a new tab...
let g:ctrlp_prompt_mappings = {
    \ 'AcceptSelection("e")': ['<c-t>'],
    \ 'AcceptSelection("t")': ['<cr>', '<2-LeftMouse>'],
    \ }
let g:ctrlp_follow_symlinks = 2
" Ag - The Silver Searcher
if executable('ag')
  " Use ag over grep
  set grepprg=ag\ --nogroup\ --nocolor

  " Use ag in CtrlP for listing files. Lightning fast and respects .gitignore
  let g:ctrlp_user_command = 'ag %s -l --hidden --ignore ".git" --nocolor -g ""'

  " ag is fast enough that CtrlP doesn't need to cache
  let g:ctrlp_use_caching = 0

endif
" Map ctrl-p
nnoremap <Leader>j :CtrlP<CR>
nnoremap <Leader><c-j> :cd %:p:h<CR>
nnoremap <Leader>b :CtrlPBuffer<CR>
" nnoremap <Leader><leader> :CtrlP<CR>
nnoremap <Leader>l :CtrlPTag<CR>
nnoremap <Leader>; :CtrlPBufTag<CR>

"=========================================== Syntastic styntax checker
let g:syntastic_javascript_checkers = ["eslint"]
let g:syntastic_javascript_eslint_exec = 'eslint_d'
let g:syntastic_mode_map = { "mode": "passive"}
" let g:syntastic_mode_map = { "mode": "active"}

" Now we're using ctags-exuberant
" see: http://stackoverflow.com/questions/1790623/how-can-i-make-vims-taglist-plugin-show-useful-information-for-javascript
let g:tlist_javascript_settings = 'javascript;s:string;a:array;o:object;f:function'

" Disable tab key in YCM
" Should resolve.. UltiSnip Trigger configuration. Do not use <tab> if you use https://github.com/Valloric/YouCompleteMe.
" let g:ycm_key_list_select_completion=[]
" let g:ycm_key_list_previous_completion=[]

" Status line with vim-airline
" Define if doesn't exist...
let g:airline#extensions#hunks#enabled = 0
let g:airline#extensions#branch#enabled = 1
let g:airline#extensions#tagbar#enabled = 1
if !exists('g:airline_symbols')
	let g:airline_symbols = {}
endif
let g:airline_left_sep = ' '
let g:airline_left_alt_sep = ' '
let g:airline_right_sep = ' '
let g:airline_right_alt_sep = ' '
let g:airline_symbols.branch = 'br:'
let g:airline_symbols.readonly = 'R'
let g:airline_symbols.linenr = 'ln'
let g:airline_section_y = ''
let g:airline_theme='tomorrow' " looks better than my normal molokai, but only for status bar
let g:airline_powerline_fonts=1

" see: http://stackoverflow.com/a/22253548/3933724
" Bind to c-p, which is then handled by supertab
" make YCM compatible with UltiSnips (using supertab)
let g:ycm_key_list_select_completion = ['<C-n>', '<Down>']
let g:ycm_key_list_previous_completion = ['<C-p>', '<Up>']
" let g:ycm_filetype_blacklist={'unite': 1}

" better key bindings for UltiSnipsExpandTrigger
let g:UltiSnipsExpandTrigger = "<tab>"
let g:UltiSnipsJumpForwardTrigger = "<tab>"
let g:UltiSnipsJumpBackwardTrigger = "<s-tab>"

" 'snips' is in my dotfiles runtimepath dir
let g:UltiSnipsSnippetsDir = "~/dotfiles/.vim/snips"
let g:UltiSnipsSnippetDirectories=["UltiSnips", "snips"]
let g:UltiSnipsEditSplit= "context"

" SuperTab
" let g:SuperTabDefaultCompletionType = "context"

" Tern
let g:tern_show_argument_hints = "no"

" ------------------------------ Key maps, Mappings...
" Remap _,_ to _'_  .......... used jumping to marks

" Remap c_a to leader_a, for compatibility with tmux
noremap <leader>a <c-a>
" Make sure nothing conflicts with tmux
map <c-a> <nop>


nmap <leader>z :set foldmethod=syntax<cr>
nmap <leader>Z :set foldmethod=manual<cr>
nmap <leader>d :Dispatch<space>

" Select inside function
nmap <leader>v <esc>/{<cr>%v%<s-v>

nmap <leader>x :cclose <bar> :lclose<cr>

" Shortcut to toggle `set number`
nmap <Leader>n :set number!<CR>

" Shortcut to toggle `:SyntasticToggleMode`
nmap <Leader>sn :SyntasticToggleMode<CR>

" Shortcut to rapidly toggle `set list`
nmap <Leader><tab>  :set list!<CR>

" Shortcut to close the location window
nmap <Leader>cl :lclose<cr>

" Convert quotes to " WARNING converts ALL quotes
nmap <leader>'' :%s/'/"/g<cr>
nmap <leader>'" :%s/"/'/g<cr>

" Some useful tips...
" http://sheerun.net/2014/03/21/how-to-boost-your-vim-productivity/

" Map <leader>y and p for clipboard copy/paste (yank/put)

" For consistant behaviour with s-D and s-C
nnoremap Y y$

set clipboard=unnamedplus

nnoremap <Leader>[ :Errors<cr>

" Map 'K' to search for word under cursor, ie: `find in files`
nnoremap K :Ag "\b<C-R><C-W>\b"<CR>:cw<CR>
vnoremap K :Ag "\b<C-R><C-W>\b"<CR>:cw<CR>

" Search with selection
vnoremap * y/<c-r>0<cr>

" Count search instances
nnoremap <leader>8 :%s///gn<cr>

"Map leader q to quit with saving
nnoremap <leader>q :q<CR>

"Map leader s to save file...
nnoremap <leader>w :w<CR>

" " Map wq to write and quit
" nnoremap <leader>wq :wq<CR>

"Map ctrl-shift-s to source current file...
nnoremap <leader>so :source %<CR>
nnoremap <leader>s. :! . %<CR>

" <Ctrl-l> redraws the screen and removes any search highlighting.
nnoremap <leader>/ :nohl \| :redraw!<cr>

" Map semi-colon to colon....
" ... no more shift :)
" nnoremap ; :
" I now use semi-colon, so this isn't useful any more

" Highlight word under cursor (uses search highlighting)
nmap <leader>* *N

" javascript
" nmap <leader><leader> /proto.*
nmap <leader>1 /proto.*

" Vundle PluginInstall
nmap <leader>pi :PluginInstall<CR>

" Source local .vimrc file, shortcut...
nnoremap <leader>sv :source .vimrc<CR>

" Show errors, 'syntastic'
nnoremap <leader>er :Errors<CR>
" Show errors and move into window
nnoremap <leader>err :w<CR>:Errors<CR><C-W>j<CR>

" Edit this file, shortcut...
nnoremap <silent><leader>ev :tabnew ~/.vimrc<CR>

" Edit bashrc
nnoremap <silent><leader>eb :tabnew ~/.bashrc<CR>
nnoremap <leader>eb :tabnew ~/.bashrc<CR>
nnoremap <leader>eu :UltiSnipsEdit<cr>

" Edit gvimrc, shortcut...
"nnoremap <silent><F12> :vsp ~/.gvimrc<CR>
nnoremap <silent><leader>grc :vsp ~/.gvimrc<CR>

" Git status mapping
nnoremap <silent><leader>gs :Gstatus<CR>
nnoremap <silent><leader>gd :Gvdiff<CR>

" Dispatch: npm start mapping
nnoremap <silent><leader>dns :Dispatch npm start<CR>

" Dispatch: npm install mapping
nnoremap <silent><leader>dni :Dispatch npm install<CR>

" Tests
" nnoremap <leader>t :TestNearest<CR>
" nnoremap <leader>T :TestSuite<CR>

" open with xdg-open
nnoremap <leader>o :!xdg-open %<CR>

" Dispatch: in ex mode
nnoremap <leader>dd :Dispatch<SPACE>

" Spelling on/off
nnoremap <leader>sp :set spell!<CR>

" function s:Mkdir()
"   let dir = expand('%:p:h')

"   if !isdirectory(dir)
"     call mkdir(dir, 'p')
"     echo 'Created non-existing directory: '.dir
"   endif
" endfunction

" Local .vimrc files...
"set exrc            " enable per-directory .vimrc files
"set secure          " disable unsafe commands in local .vimrc files

" Git Gutter
let g:gitgutter_map_keys = 0
nmap gbk <Plug>GitGutterPrevHunk
nmap gbj <Plug>GitGutterNextHunk
nmap gbs <Plug>GitGutterStageHunk
nmap gbr <Plug>GitGutterRevertHunk

" line numbers - advanced
" Relative/Absolute lines numbers...
set relativenumber
set number
autocmd InsertEnter * :set number
autocmd InsertLeave * :set relativenumber
autocmd FocusLost * :set number
autocmd FocusGained * :set relativenumber

" AutoCommands
" my filetype syntax definitions
augroup filetypedetect
  au BufRead,BufNewFile *.hbs,*.volt	set filetype=html
  au BufRead,BufNewFile *.md  		set filetype=ghmarkdown
  au BufRead,BufNewFile *.json          set filetype=json
  au BufRead,BufNewFile *.json          set formatprg=python\ -m\ json.tool
  au BufRead,BufNewFile *.snippets   set noexpandtab
  " au! BufRead,BufNewFile *.xyz		setfiletype drawing
augroup END

augroup markdown
  au!
  au BufNewFile, BufRead *.md, *.markdown setlocal filtetype=ghmarkdown
augroup END

" autocmd BufWritePre * call s:Mkdir()

" augroup file_matching
"   autocmd FileType html,css,sass,scss let g:SuperTabContextDefaultCompletionType = "<c-y>,"
" augroup END

" The row where your cursor is, is highlighted. It's warming to me
" ...Only show when in buffer
augroup enter_leave
  autocmd  BufLeave * set nocursorline
  autocmd  BufEnter * set cursorline
augroup END

" Colours, colors
set t_Co=256
let base16colorspace=256
syntax on
colorscheme base16-default
set background=dark
highlight Normal ctermbg=NONE
highlight nonText ctermbg=NONE
" colorscheme molokai
" colorscheme summerfruit256
" colorscheme badwolf
" colorscheme solarized
" let g:solarized_termcolors=256
" hi MatchParen ctermbg=3 cterm=underline term=NONE


" Emmet mapping
" let g:user_emmet_leader_key = '<leader>,'
" autocmd FileType html imap <tab> <plug>(emmet-expand-abbr)
" autocmd FileType mustache imap <tab> <plug>(emmet-expand-abbr)

" --------------------------------------- Neovim hacks
"NeoVim + tmux handles ESC keys as alt+key somtimes
" Be sure to have this line in Tmux
" set -sg escape-time 10
" --------------------------------------- Oneliners

set cursorline

" Remove the scratch/preview window that pops up when I use Tern/Omnicomplete
set completeopt-=preview

" Use the same symbols as TextMate for tabstops and EOLs
set listchars=tab:▸\ ,eol:¬,nbsp:_,trail:#

" For saving sane sessions...
set ssop-=options    " do not store global and local values in a session
set ssop-=folds      " do not store folds
map <leader>ss :source Session.vim<cr>

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
set softtabstop=2 
set tabstop=2 
set expandtab
set nowrap
set cindent
set autoindent

" Colours
set novisualbell
" set t_ut=

" Search
set hlsearch
set incsearch
set ignorecase    " ignore case when searching
set smartcase     " ignore case if search pattern is all lowercase,
                    "    case-sensitive otherwise
"set guifont=Consolas\ for\ Powerline\ FixedD:h10

" set encoding=utf-8
set laststatus=2



" Custom syntax highlighting
" hlsearch
highlight Search term=underline cterm=underline ctermfg=18 gui=underline guifg=#ab4642

