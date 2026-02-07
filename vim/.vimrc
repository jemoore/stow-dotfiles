""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"                   _
"            __   _(_)_ __ ___  _ __ ___
"            \ \ / / | '_ ` _ \| '__/ __|
"             \ V /| | | | | | | | | (__
"              \_/ |_|_| |_| |_|_|  \___|
"
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Use 4 spaces instead of tabs
set tabstop=4                           " number of visual spaces per tab
set softtabstop=4                       " number of spaces in tab while editing
set shiftwidth=4                        " use 4 spaces when indenting with >
set expandtab                           " tabs are spaces
set smartindent
set mouse=a

set encoding=utf-8
set dictionary+=/usr/share/dict/words

set showmatch                           " highlight matching symbols

" Following 2 lines are 'built-in' partial and fuzzy finder
" :find XX<TAB> to complete partial
" :find *co for fuzzy find
" also works with :b
set path+=**
set wildmenu

let mapleader="\<space>"                       " leader key for custom commands

set exrc
set relativenumber                      " use relative line numbering
set nu                                  " show line numbers
" set nohlsearch                          " no highlighting when searching
set hidden
set noerrorbells
set nowrap
set cursorline

set ignorecase
set smartcase
set incsearch

set noswapfile
set nobackup
set undodir=~/.vim/undodir
set undofile
set tags=./tags;/                     " look for tags file in cur dir, work up to /
set tags+=/some/dir               " add an additional dir to search for tags

set scrolloff=8
set colorcolumn=80                      " display a vertical line at this column(s)
hi ColorColumn ctermbg=lightgrey guibg=lightgrey | " make the vertical line lightgrey

" Enable folding
set foldenable
set foldmethod=syntax
set foldlevel=99
nnoremap <leader><space> za

set cmdheight=2

set splitbelow                          " split below current window
set splitright                          " split right of current window

set updatetime=300                      " Reduce time highlight other refs
set redrawtime=10000                    " More time loading syntax large files

"""""""""""""""
" GVim specific
"""""""""""""""
set guioptions-=m                       " hide menu bar
set guioptions-=T                       " hide toolbar
set guioptions-=r                       " hide scrollbar
" set guifont=DejaVu\ Sans\ Mono\ 12

"""""""""
" Plugins
"""""""""
" download plug.vim if not present
if empty(glob('~/.vim/autoload/plug.vim'))
  silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

call plug#begin('~/.vim/plugged')       " vim-plug plugins will be downloaded here

Plug 'dracula/vim', { 'as': 'dracula' }
Plug 'gruvbox-community/gruvbox'        " Use the best color scheme ever!
Plug 'joshdick/onedark.vim'
Plug 'romgrk/doom-one.vim'
Plug 'chriskempson/base16-vim'
Plug 'vim-airline/vim-airline'          " nice colored bar at the bottom of screen
Plug 'vim-syntastic/syntastic'
Plug 'nvie/vim-flake8'
Plug 'scrooloose/nerdtree' |
            \ Plug 'Xuyuanp/nerdtree-git-plugin' |
            \ Plug 'ryanoasis/vim-devicons' " cooperate with vim-devicons
Plug 'tpope/vim-fugitive'               " use git from vim
Plug 'airblade/vim-gitgutter'           " show git changes as marks along left edge
Plug 'preservim/tagbar'                 " show current file vars, functions, etc... in a window
Plug 'junegunn/fzf'                     " fuzzy finder
Plug 'junegunn/fzf.vim'                 " fuzzy finder integration with vim
Plug 'vimwiki/vimwiki'
Plug 'tpope/vim-markdown'
Plug 'mhinz/vim-startify'               " fancy start screen
Plug 'dhruvasagar/vim-dotoo'            " org mode in vim
nmap <Nop> <Plug>(dotoo-capture)
Plug 'tpope/vim-speeddating', { 'for': [ 'org', 'dotoo', 'rec' ] }
Plug 'tpope/vim-surround'               " ysw, ysiw, yss (,),[,],"
Plug 'tpope/vim-commentary'             " gcc, gc, 5gc
Plug 'ap/vim-css-color'
Plug 'octol/vim-cpp-enhanced-highlight'
Plug 'mboughaba/i3config.vim'
Plug 'sirver/ultisnips'
let g:UltiSnipsExpandTrigger = '<tab>'
let g:UltiSnipsJumpForwardTrigger = '<tab>'
let g:UltiSnipsJumpBackwardTrigger = '<s-tab>'
let g:UltiSnipsSnippetDirectories=["UltiSnips", "MyUltiSnips"]

call plug#end()                         " vim-plug plugins should not be declared below this

" setup color scheme
colorscheme gruvbox
set background=dark

let python_highlight_all=1
syntax on
set nocompatible
filetype plugin on


"""""""""""""""""""
" NERDTree settings
"""""""""""""""""""
function! ToggleNERDTree ()
    NERDTreeToggle
    silent NERDTreeMirror
endfunction

nnoremap <leader>nn :call ToggleNERDTree()<CR>
nnoremap <leader>nf :NERDTreeFind<CR>


"""""""""""""""""""""
" Leader key mappings
"""""""""""""""""""""
" Only write if changes have been made, avoid changing metadata
nnoremap <leader>u :up<CR>
" This is a key combo I used in emacs, hard to break the habit
nnoremap <leader>fs :up<CR>
" Toggle the tagbar window provided by plugin
nnoremap <leader>tb :TagBarToggle<CR>
" Search & Replace
nnoremap <leader>sr :%s//g<Left><Left>
" Toggle auto indent for pasting
set pastetoggle=<F6>
" Open the current file in the default program
nmap <leader>x :!xdg-open %<cr><cr>
" Open buffer list
nmap <leader>b :ls<cr>
" insert current time stamp
nnoremap <leader>ts i[<C-R>=strftime("%Y-%m-%d %a %H:%M")<CR>]<Esc>
" Fast editing of the .vimrc
map <leader>ee :vsp $MYVIMRC<CR>
map <leader>eb :vsp /mnt/Data2/Documents/brain2/brain2.md<CR>
" When vimrc is edited, reload it
autocmd! bufwritepost .vimrc source ~/.vimrc

""""""""""""""
" Key Mappings
""""""""""""""
inoremap jj <ESC>

nnoremap <C-j> <C-W>j
nnoremap <C-k> <C-W>k
nnoremap <C-h> <C-W>h
nnoremap <C-l> <C-W>l
nnoremap <C-J> <C-W>J
nnoremap <C-K> <C-W>K
nnoremap <C-H> <C-W>H
nnoremap <C-L> <C-W>L

" HTML
" type keys quickly to have text replaced
autocmd FileType html inoremap ;h1 <h1></h1><Esc>2b i
autocmd FileType markdown inoremap ;c ```\n```<Esc>k i

" Show tag match in vertical split
map <A-]> :vsp <CR>:exec("tag ".expand("<cword>"))<CR>
" Replace all
nnoremap S :%s//g<Left><Left>

" Make Y behave more like C, D
nnoremap Y y$

" Keep it centered
nnoremap n nzzzv
nnoremap N Nzzzv
nnoremap J mzJ`z

" Undo break points
inoremap , ,<c-g>u
inoremap . .<c-g>u
inoremap ! !<c-g>u
inoremap ? ?<c-g>u

" Jumplist mutations
nnoremap <expr> k (v:count > 5 ? "m'" . v:count : "") . 'k'
nnoremap <expr> j (v:count > 5 ? "m'" . v:count : "") . 'j'

" Moving text
vnoremap J :m '>+1<CR>gv=gv
vnoremap K :m '<-2<CR>gv=gv
inoremap <C-j> <esc>:m .+1<CR>==
inoremap <C-k> <esc>:m .-2<CR>==
nnoremap <leader>k :m .-2<CR>==
nnoremap <leader>j :m .+1<CR>==

" Allow gf to open non-existent files
map gf :edit <cfile><cr>

" Compile code
"" compile current file no warning checks
noremap <F8> <ESC> :w <CR> :!g++ -fsanitize=address -std=c++17 -DONPC -O2 -o %< % && ./%< < inp<CR>
inoremap <F8> <ESC> :w <CR> :!g++ -fsanitize=address -std=c++17 -DONPC -O2 -o "%<" "%" && "./%<" < inp<CR>
"" compile current file with warning checks
noremap <F9> <ESC> :w <CR> :!g++ -fsanitize=address -std=c++17 -Wall -Wextra -Wshadow -DONPC -O2 -o %< % && ./%< <CR>
inoremap <F9> <ESC> :w <CR> :!g++ -fsanitize=address -std=c++17 -Wall -Wextra -Wshadow -DONPC -O2 -o %< % && ./%< <CR>
"" compile current file with warning checks - use input file when running
noremap <F10> <ESC> :w <CR> :!g++ -fsanitize=address -std=c++17 -Wall -Wextra -Wshadow -DONPC -O2 -o %< % && ./%< < inp<CR>
inoremap <F10> <ESC> :w <CR> :!g++ -fsanitize=address -std=c++17 -Wall -Wextra -Wshadow -DONPC -O2 -o "%<" "%" && "./%<" < inp<CR>

"""""""
" Hooks
"""""""
" Delete all trailing whitespace on save
autocmd BufWritePre * %s/\s\+$//e
" Auto generate tags file on file write of certain files
autocmd BufWritePost *.c,*.cpp,*.h,*.hpp,*.py silent! !ctags . &
" Run xrdb whenever Xdefaults or Xresources are updated
autocmd BufWritePost ~/.Xresources,~/.Xdefaults !xrdb %


"""""""""""""""""
" Custom commands
"""""""""""""""""

" A convenience function that can be used to allow easy capture of output to a
" scratch buffer
:command! -nargs=* -complete=shellcmd R new | setlocal buftype=nofile bufhidden=hide noswapfile | r !<args>

""""""""""
" VIM WIKI
""""""""""
" ~~~~~ Ensure files are read as what I want in vimwiki:
let g:vimwiki_global_ext = 0
let g:vimwiki_ext2syntax = {'.md': 'markdown', '.markdown': 'markdown', '.mdown': 'markdown'}
" Makes vimwiki markdown links as [text](text.md) instead of [text](text)
let g:vimwiki_markdown_link_ext = 1
let g:vimwiki_root = '/mnt/Data2/Documents/brain2'
let g:vimwiki_listsyms = '✗○◐●✓'
let g:vimwiki_list = [
     \{'path': '/mnt/Data2/Documents/brain2', 'syntax': 'markdown', 'ext': '.md'},
     \{'path': '/mnt/Data2/Documents/brain2/journal', 'syntax': 'markdown', 'ext':'.md'},
     \{'path': '/mnt/Data2/Documents/brain2/templates', 'syntax': 'markdown', 'ext':'.md'},
     \{'path': '/mnt/Data2/Documents/brain2/attachments', 'syntax': 'markdown', 'ext':'.md'},
     \{'path': '/mnt/Data2/Documents/brain2/wiki', 'syntax': 'markdown', 'ext':'.md'}]

""""""""""""""
" VIM markdown
""""""""""""""
let g:markdown_fenced_languages = ['bash=sh', 'cpp', 'css', 'html', 'javascript', 'js=javascript', 'json=javascript', 'python', 'vim', 'xml']

""""""""""""""
" i3 config
""""""""""""""
aug i3config_ft_detection
      au!
        au BufNewFile,BufRead ~/.config/i3/config set filetype=i3config
aug end

" doom-one color for Neovim terminal
let g:doom_one_terminal_colors = v:true
