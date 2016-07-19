" ===============  Vundle  ===============
set nocompatible
filetype off
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin('~/.vundle')
	Plugin 'gmarik/Vundle.vim'
	Plugin 'SirVer/ultisnips'
	Plugin 'honza/vim-snippets'
	Plugin 'scrooloose/nerdcommenter'
	Plugin 'altercation/vim-colors-solarized'
	Plugin 'Raimondi/delimitMate'e
	" Plugin 'Lokaltog/vim-easymotion'
	" Lightweight alternative to easymotion
	" Plugin 'justinmk/vim-sneak'
	Plugin 'bling/vim-airline'
	Plugin 'takac/vim-hardtime'
	Plugin 'tpope/vim-surround'
call vundle#end()


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
    colorscheme zenburn

" ===============  General settings  ===============
set undofile 			" undo in different sessions
set undodir=/Users/andre0991/.vim/undos " dir must exist
set mouse+=a 			" avoid including numbers in mouse selection
" set clipboard=unnamedplus 	" use system clipboard for all operations (req vim 7.3+)
set clipboard=unnamed		" use this if the above line does not work properly
set shortmess+=I 		" remove initial message when opening vim without file
set wildmode=longest,list 	" autocomplete works like unix instead of DOS
set history=200  "ex commands max number of entries (default is 20)

" ===============  Text editing  ===============
" set spell for *.txt files
au BufRead *.txt setlocal spell
set spelllang=en,pt

func! WordProcessorMode()
	setlocal formatoptions=t1
	setlocal textwidth=80
	setlocal smartindent
	setlocal spell spelllang=en_us
	setlocal noexpandtab
endfu
com! WP call WordProcessorMode()

" ===============  Identation & readability  ===============
set autoindent		" auto identation
filetype on 		" smart indenting by filetype
filetype indent on	" filetype-specific indenting
filetype plugin on	" filetype-specific plugins

" creates a line crossing the column 80
function! Column_mark()
	if exists('+colorcolumn')
		set colorcolumn=80
	else
		au BufWinEnter * let w:m2=matchadd('ErrorMsg', '\%>80v.\+', -1)
	endif
endfunc


" ===============  Remappings  ===============
" correct last mispelling (insert mode)
imap <c-l> <c-g>u<Esc>[s1z=`]a<c-g>u
" correct last mispelling (command mode)
nmap <c-l> [s1z=<c-o>]
" leader is space key
let mapleader=" "
" autocomplete code block
inoremap {<CR>  <CR>{<CR>}<Esc>O
" F2 togles paste mode
nnoremap <F2> :set invpaste paste?<CR>
set pastetoggle=<F2>
set showmode
nmap <silent> <leader><space> :nohlsearch<CR>
" <Up> and <Down> can autocomplete commands in ex mode
" when searching through the history. Make C-n and C-p like that.
cnoremap <C-p> <Up>
cnoremap <C-n> <Down>

" habit breaking, habit making
noremap <Up> <NOP>
noremap <Down> <NOP>
noremap <Left> <NOP>
noremap <Right> <NOP>

" Use <leader>b to show buffers,
" See :help 'wildcharm for details.
set wcm=<C-Z>
map <leader>b :b<space><C-Z>


" ===============  Other  ===============
" detect desired filetypes
let g:tex_flavor = "latex"		" fix .tex detection
augroup filetypedetect
	au! BufRead,BufNewFile *.m,*.oct set filetype=octave
	autocmd BufNewFile,BufRead *.md set filetype=markdown
augroup END

" Jump to the last position when opening a file.
" If this doesn't work, you might not have ownership
"  of your ~/.viminfo file. Fix:
" sudo chown user:group ~/.viminfo
if has("autocmd")
	au BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"" | endif
endif

let NERDSpaceDelims = 1			" add extra space after comment
" leader is the key to use easymotion.
" Do not insert a trailing whitespace or tab on the line below.
map <Leader> <Plug>(easymotion-prefix)

" Delete comment character when joining commented lines (from tpope's sensible.vim)
if v:version > 703 || v:version == 703 && has("patch541")
	set formatoptions+=j
endif"

" Activate vim-hardtime
let g:hardtime_default_on = 1
" Activate HardTime notifications
let g:hardtime_showmsg = 1
" Max number of repetative key preses
let g:hardtime_maxcount = 2

" ===============  My functions  ===============
" Reformat text limiting width
function! FormatWidth()
	:set textwidth=80
	:%s/\n/\r\r/g
	:normal gg
	:normal gqG
	:%s/\n\n/\r/g
endfunc
" generates ordered lists from first_item to last_item
function! GenList(first_item, last_item)
	execute "for i in range(a:first_item, a:last_item) | put = i.'. ' | endfor"
endfunc


" ===============  Statusline  ===============
set laststatus=2				    "always show statusline
" customized statusline
	" set statusline=
	" set statusline+=%<\                       " cut at start
	" set statusline+=%2*[%n%H%M%R%W]%*\        " flags and buf no
	" set statusline+=%-40f\                    " path
	" set statusline+=%=%1*%y%*%*\              " file type
	" set statusline+=%10((%l,%c)%)\            " line and column
	" set statusline+=%P                        " percentage of file
" airline
let g:airline_powerline_fonts = 1
" let g:airline_left_sep=''			" change symbol that requires powerline fonts
" let g:airline_right_sep=''			" .

