" https://github.com/junegunn/vim-plug/wiki/tips#automatic-installation
if empty(glob('~/.vim/autoload/plug.vim'))
  silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

" ===============   Plug   ===============
" Use :PlugInstall
call plug#begin('~/.vim/plugged')
Plug 'tpope/vim-surround'
Plug 'vim-airline/vim-airline'
call plug#end()

" ===============  Display  ===============
syntax on
set number		" show number
set relativenumber	" relative numbers
set incsearch		" incremental search
set hlsearch 		" highlight the search
set ignorecase 		" ignore case
set smartcase 		" ...unless the search uses uppercase letters
set showcmd 		" information about the current command going on
set linebreak		" avoid splitting words across two lines
set ruler		" show columns numbers
hi clear SpellBad 	" make highlights less distractive
hi SpellBad cterm=underline

" ===============  Themes  ===============
set t_Co=256
" desert
	" colorscheme desert
" wombat
	" colorscheme wombat256
" solarized theme settings
	" set background=dark
	" set background=light
	" colorscheme solarized
" zenburn
" colorscheme zenburn

" ===============  General settings  ===============
set undofile 			        " undo in different sessions
set undodir=/Users/andre0991/.vim/undos " dir must exist
set mouse+=a 			        " avoid including numbers in mouse selection
" set clipboard=unnamedplus 	        " use system clipboard for all operations (req vim 7.3+)
set clipboard=unnamed		        " use this if the above line does not work properly
set shortmess+=I 		        " remove initial message when opening vim without file
set wildmode=longest,list 	        " autocomplete works like unix instead of DOS
set history=200  			"ex commands max number of entries (default is 20)


" ===============  Identation & readability  ===============
set autoindent		" auto identation
filetype on 		" smart indenting by filetype
filetype indent on	" filetype-specific indenting
filetype plugin on	" filetype-specific plugins

" ===============  Remappings  ===============
" leader is space key
let mapleader=" "
" nmap <silent> <leader><space> :nohlsearch<CR>

