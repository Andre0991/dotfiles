 " set the runtime path to include Vundle and initialize
 set nocompatible
 
 " Vundle requirements and settings  
  filetype off
  set rtp+=~/.vim/bundle/Vundle.vim
  call vundle#begin()
  call vundle#begin('~/.vundle')
  Plugin 'gmarik/Vundle.vim'
  Plugin 'SirVer/ultisnips'
  Plugin 'honza/vim-snippets'
  Plugin 'scrooloose/nerdcommenter'
  Plugin 'altercation/vim-colors-solarized'
  Plugin 'Raimondi/delimitMate'
  Plugin 'Lokaltog/vim-easymotion'
 call vundle#end()        

" display options
	syntax on
	set number		" show number
	set relativenumber	" relative numbers
	set incsearch		" incremental search
	set hlsearch 		" highlight the search
	set ignorecase 		" ignore case
	set smartcase 		" ...unless the search uses uppercase letters
	set showcmd 		" information about the current command going on
	set linebreak		" avoid splitting words across two lines // if doesn't work, also use set nolist
	set ruler		" show columns numbers
	" set t_Co=256		" required by wombat
	
" solarized theme settings
	set background=dark
	colorscheme solarized

" creates a line crossing the column 80
" if exists('+colorcolumn')
  " set colorcolumn=80
" else
  " au BufWinEnter * let w:m2=matchadd('ErrorMsg', '\%>80v.\+', -1)
" endif

" general settings
	 set undofile 			" undo in different sessions
	 set undodir=/Users/andre0991/.vim/undos " (create dir before using this)
	 set mouse+=a 			" avoid that mouse selection includes line numbers
	 set clipboard=unnamed 		" use system clipboard for all operations (requires vim 7.3+)
	 set shortmess+=I 		" removes initial message when opening vim without file
	 set wildmode=longest,list 	" autocomplete works like unix instead of DOS
	 au BufRead *.txt setlocal spell " set spell for *.txt files
 
" identation options
	set autoindent		" auto identation
	filetype on 		" smart indenting by filetype
	filetype indent on
	filetype plugin on


" Uncomment the following to have Vim jump to the last position when opening a file.
" If this doesn't work, a common problem is not having ownership of your ~/.viminfo file. 
" If this is the case, then run:
" sudo chown user:group ~/.viminfo
if has("autocmd")
  au BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"" | endif
endif

" remappings
	let mapleader = " "
	nnoremap <F2> :set invpaste paste?<CR>
	inoremap {<CR>  <CR>{<CR>}<Esc>O
	set pastetoggle=<F2>
	set showmode 		" requires vim >=7

" plugins
	let g:tex_flavor = "latex"	" fix .tex detection
	let NERDSpaceDelims = 1
	map <Leader> <Plug>(easymotion-prefix)
	
 " detect octave files properly
	 augroup filetypedetect
	 au! BufRead,BufNewFile *.m,*.oct set filetype=octave
	 augroup END

" habit breaking, habit making
	noremap <Up> <NOP>
	noremap <Down> <NOP>
	noremap <Left> <NOP>
	noremap <Right> <NOP>

" generates ordered lists
	function! Myfunc(myarg1, myarg2)
		execute "for i in range(a:myarg1, a:myarg2) | put = i.'. ' | endfor"
	endfunc

" customized statusline
	set laststatus=2			  "always show statusline
	set statusline=
	set statusline+=%<\                       " cut at start
	set statusline+=%2*[%n%H%M%R%W]%*\        " flags and buf no
	set statusline+=%-40f\                    " path
	set statusline+=%=%1*%y%*%*\              " file type
	set statusline+=%10((%l,%c)%)\            " line and column
	set statusline+=%P                        " percentage of file

