set nocompatible              " be iMproved, required
filetype off                  " required

" My own runtime
set rtp+=~/dotfiles/.vim

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

" let Vundle manage Vundle, required
Plugin 'gmarik/Vundle.vim'

" Motions, Core
Plugin 'tpope/vim-commentary'
Plugin 'tpope/vim-surround'
Plugin 'tpope/vim-repeat'
Plugin 'tpope/vim-dispatch'
Plugin 'tpope/vim-unimpaired'

" Git
Plugin 'tpope/vim-fugitive'

" Linters, javascript
Plugin 'tpope/vim-jdaddy'
Plugin 'scrooloose/syntastic'
Plugin 'jelera/vim-javascript-syntax'
Plugin 'pangloss/vim-javascript'
Plugin 'othree/javascript-libraries-syntax.vim'
Plugin 'marijnh/tern_for_vim'
Plugin 'mxw/vim-jsx'

" Basic utils
Plugin 'tpope/vim-eunuch'
Plugin 'editorconfig/editorconfig-vim'
" Plugin 'vim-airline'
Plugin 'airblade/vim-gitgutter'
Plugin 'gabesoft/vim-ags' " Faster ack, which is faster grep :)
Plugin 'chriskempson/base16-vim'
Plugin 'christoomey/vim-tmux-navigator'
Plugin 'tmhedberg/matchit'
" Plugin 'kien/ctrlp.vim'
Plugin 'mattn/emmet-vim'
Plugin 'kana/vim-textobj-user'
Plugin 'kana/vim-textobj-function'
Plugin 'kana/vim-textobj-line'
Plugin 'kana/vim-textobj-indent'
Plugin 'glts/vim-textobj-comment'
Plugin 'vim-scripts/ReplaceWithRegister'

" Completers
Plugin 'SirVer/ultisnips'
Plugin 'Valloric/YouCompleteMe'
Plugin 'honza/vim-snippets'

" Family syntaxes
Plugin 'Markdown-syntax'
Plugin 'jtratner/vim-flavored-markdown.git'
Plugin 'elzr/vim-json'
Plugin 'groenewege/vim-less'
Plugin 'powerman/vim-plugin-AnsiEsc'

" Nvim nodejs
Plugin 'neovim/node-host'

" Parenthese inflector
Plugin 'snoe/nvim-parinfer.js'

" Org mode
Plugin 'tpope/vim-speeddating' " Required by org-mode
Plugin 'jceb/vim-orgmode'

" All of your Plugins must be added before the following line
call vundle#end()            " required
filetype plugin indent on    " required

" Leader key
let mapleader = "\<Space>"
let maplocalleader = ","



"""" FZF
Plugin 'junegunn/fzf'
Plugin 'junegunn/fzf.vim'
nnoremap <leader>f :FzfGFiles<cr>
let g:fzf_command_prefix = 'Fzf'
nnoremap <a-x> :FzfCommands<cr>

function! s:echoline(line)
  execute append(line("."), a:line)
endfunction

command! -nargs=1 -complete=file Lines call fzf#run(
      \ {'source': 'cat <q-args>', 'sink': function('s:echoline'), 'options': '-m'})

let g:fzf_colors =
  \ { 'fg':    ['fg', 'Normal'],
  \ 'bg':      ['bg', 'ColorColumn'],
  \ 'hl':      ['fg', 'Comment'],
  \ 'fg+':     ['fg', 'CursorLine', 'CursorColumn', 'Normal'],
  \ 'bg+':     ['bg', 'CursorLine', 'CursorColumn'],
  \ 'hl+':     ['fg', 'Statement'],
  \ 'info':    ['fg', 'PreProc'],
  \ 'prompt':  ['fg', 'Conditional'],
  \ 'pointer': ['fg', 'Exception'],
  \ 'marker':  ['fg', 'Keyword'],
  \ 'spinner': ['fg', 'Label'],
  \ 'header':  ['fg', 'Comment'] }

nnoremap <Leader>j :FzfGFiles<CR>
nnoremap <Leader>l :FzfLines<CR>
nnoremap <Leader>a :FzfAg<CR>

"""" Syntastic 
let g:syntastic_javascript_checkers = ["eslint"]
let g:syntastic_javascript_eslint_exec = "eslint_d"
let g:syntastic_mode_map = { "mode": "passive"}
" let g:syntastic_mode_map = { "mode": "active"}

" Now we're using ctags-exuberant
" see: http://stackoverflow.com/questions/1790623/how-can-i-make-vims-taglist-plugin-show-useful-information-for-javascript
let g:tlist_javascript_settings = 'javascript;s:string;a:array;o:object;f:function'

"""" YCM You complete me
let g:ycm_key_list_select_completion = ['<C-n>', '<Down>']
let g:ycm_key_list_previous_completion = ['<C-p>', '<Up>']

" Show errors, 'syntastic'
nnoremap <leader>er :Errors<CR>
" Show errors and move into window
nnoremap <leader>err :w<CR>:Errors<CR><C-W>j<CR>



"""" Ultisnips
" better key bindings for UltiSnipsExpandTrigger
let g:UltiSnipsExpandTrigger = "<tab>"
let g:UltiSnipsJumpForwardTrigger = "<tab>"
let g:UltiSnipsJumpBackwardTrigger = "<s-tab>"

" 'snips' is in my dotfiles runtimepath dir
let g:UltiSnipsSnippetsDir = "~/dotfiles/.vim/snips"
let g:UltiSnipsSnippetDirectories=["UltiSnips", "snips"]
let g:UltiSnipsEditSplit= "context"

" Edit Ultisnips ( for current filetype )
nnoremap <leader>eu :UltiSnipsEdit<cr>

"""" Tern
let g:tern_show_argument_hints = "no"
" Remove the scratch/preview window that pops up when I use Tern/Omnicomplete
set completeopt-=preview


"""" General

" Set the current file's path as the current working dir, in vim
nnoremap <Leader><c-j> :cd %:p:h<CR>

" Make sure nothing conflicts with tmux ( c-a, is my tmux prefix )
map <c-a> <nop>

" Select inside function
nmap <leader>v <esc>/{<cr>%v%<s-v>
nmap <leader>x :cclose <bar> :lclose<cr>

" Shortcut to toggle `:SyntasticToggleMode`
nmap <Leader>sn :SyntasticToggleMode<CR>

" Shortcut to rapidly toggle `set list`
nmap <Leader><tab>  :set list!<CR>

" Some useful tips...
" http://sheerun.net/2014/03/21/how-to-boost-your-vim-productivity/

" For consistant behaviour with s-D and s-C
nnoremap Y y$

" Use the global clipboard, by default
set clipboard=unnamedplus

" Search with selection
vnoremap * y/<c-r>0<cr>

" Count search instances
nnoremap <leader>8 :%s///gn<cr>

"Map leader q to quit with saving
nnoremap <leader>q :q<CR>
"Map leader s to save file...
nnoremap <leader>w :w<CR>

"Map 'so' to parse the vim source file
nnoremap <leader>so :source %<CR>

"Map 's.' to run the current shell script
nnoremap <leader>s. :! . %<CR>

" <Ctrl-l> redraws the screen and removes any search highlighting.
nnoremap <leader>/ :nohl \| :redraw!<cr>

" Vundle PluginInstall
nmap <leader>pi :PluginInstall<CR>

" Edit this file, shortcut...

" Edit common files
" .bashrc
nnoremap <silent><leader>eb :tabnew ~/.bashrc<CR>
" .vrimrc
nnoremap <silent><leader>ev :tabnew ~/.vimrc<CR>

" Git status mapping
nnoremap <silent><leader>gs :Gstatus<CR>
nnoremap <silent><leader>gd :Gvdiff<CR>

" open with xdg-open
nnoremap <leader>o :!xdg-open %<CR>

" Dispatch: in ex mode
nnoremap <leader>dd :Dispatch<SPACE>

" Spelling on/off
nnoremap <leader>sp :set spell!<CR>

"""" Local .vimrc files
" Local .vimrc files...
set exrc            " enable per-directory .vimrc files
set secure          " disable unsafe commands in local .vimrc files

"""" Git Gutter
let g:gitgutter_map_keys = 0
nmap gbk <Plug>GitGutterPrevHunk
nmap gbj <Plug>GitGutterNextHunk
nmap gbs <Plug>GitGutterStageHunk
nmap gbr <Plug>GitGutterRevertHunk

"""" Line numbers
" Relative/Absolute lines numbers...
set relativenumber
set number
augroup switchingbuffers
  autocmd InsertEnter * :set number
  autocmd InsertLeave * :set relativenumber
  autocmd FocusLost * :set number
  autocmd FocusGained * :set relativenumber
augroup END

"""" AutoCommands
" my filetype syntax definitions
augroup filetypedetect
  au BufRead,BufNewFile *.hbs,*.volt	set filetype=html
  au BufRead,BufNewFile *.md  		set filetype=ghmarkdown
  au BufRead,BufNewFile *.json          set filetype=json
  au BufRead,BufNewFile *.json          set formatprg=python\ -m\ json.tool
  au BufRead,BufNewFile *.snippets   set noexpandtab
  " au! BufRead,BufNewFile *.xyz		setfiletype drawing
augroup END

"""" Cursor line
" The row where your cursor is, is highlighted. It's warming to me
set cursorline
augroup enter_leave
  autocmd  BufLeave * set nocursorline
  autocmd  BufEnter * set cursorline
augroup END

"""" Colours, colors
syntax on
set background=dark
set novisualbell
" Terminal color mode
set t_Co=256
"
"""" Base 16 
let base16colorspace=256
colorscheme base16-default

" Use the terminal's BG color
highlight Normal ctermbg=NONE
highlight nonText ctermbg=NONE

"""" List Chars
" Use the same symbols as TextMate for tabstops and EOLs
set listchars=tab:▸\ ,eol:¬,nbsp:_,trail:#

"""" Saving
" For saving sane sessions...
set ssop-=options    " do not store global and local values in a session
set ssop-=folds      " do not store folds

"""" Autocomplete in command line...
set wildmode=longest,list,full

"""" Backspace, goes over everything
set backspace=indent,eol,start

"""" Backup files, who needs 'em?
set nobackup
set nowritebackup
set noswapfile

"""" Default Indents ( prefer to use .editorconfig, if you can )
set shiftwidth=2
set softtabstop=2 
set tabstop=2 
set expandtab
set nowrap
set cindent
set autoindent

"""" Search
set hlsearch
set incsearch
set ignorecase    " ignore case when searching
set smartcase     " ignore case if search pattern is all lowercase,
                  "    case-sensitive otherwise

"""" Statusline
" Always show a status line, even if the current window isn't active
set laststatus=2

"""" Search
highlight Search term=underline cterm=underline ctermfg=18 gui=underline guifg=#ab4642

"""" Greviences
" I want comments to only extend, when I hit C-RET, not on RET
