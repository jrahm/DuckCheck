"============================================================================
"File:        ducktest.vim
"Description: DuckTest syntax checker for python
"Maintainer:  Joshua Rahm <joshuarahm@gmail.com>
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"
"============================================================================

if exists('g:loaded_syntastic_python_ducktest_checker')
    finish
endif
let g:loaded_syntastic_python_ducktest_checker = 1

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_python_ducktest_GetLocList() dict " {{{1
    let makeprg = self.makeprgBuild({
        \ 'args_before': s:GetVersion(),
        \ 'args': (exists('g:syntastic_python_ducktest_options') ? ' ' . g:syntastic_python_ducktest_options : '') })

    let errorformat =
        \ '%E%f(%l:%c): %m'

    return SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat })
endfunction " }}}1

" Utilities {{{1

function! s:GetVersion() " {{{2
    if exists('g:syntastic_python_ducktest_version_2') && g:syntastic_python_ducktest_version_2
        return '-2'
    else
        return ''
    endif
endfunction " }}}2

" }}}1

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \'filetype': 'python',
    \'name': 'ducktest'})

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set sw=4 sts=4 et fdm=marker:
