
" no vi compat mode
set nocompatible

" enable filetype
filetype plugin on
filetype indent on

" color
syntax on
colorscheme molokai
set background=dark

" indenting
set noexpandtab
set shiftwidth=8
set softtabstop=8
set tabstop=8
set autoindent
set smartindent

" lines
set number
set cursorline
set linespace=2

" buffers
set hidden
set laststatus=2
set autoread
set so=7
set ruler

" search settings
set hlsearch
set incsearch
set ignorecase
set smartcase
set showmatch
set mat=2

" wildmenu
set wildmenu
set wildignore=*.o,*.swp,*.pyc,.git\*

" enable per-project vimrc
set exrc
set secure

" disable swap files
set nobackup
set noswapfile
set nowritebackup

" keys
imap jj <Esc>

" disable mouse
set mouse=

" etc
set lazyredraw
set magic
set encoding=utf8
set backspace=2

" line moving
nnoremap <C-j> :m .+1<CR>==
nnoremap <C-k> :m .-2<CR>==
inoremap <C-j> <Esc>:m .+1<CR>==gi
inoremap <C-k> <Esc>:m .-2<CR>==gi
vnoremap <C-j> :m '>+1<CR>gv=gv
vnoremap <C-k> :m '<-2<CR>gv=gv
