let s:quicklisp_setup = expand("~/quicklisp/setup.lisp")

function! s:check_g_lisp_host_prog() abort
  " Check that g:lisp_host_prog is not bound -- otherwise we cannot continue with the remaining health checks
  if exists('g:lisp_host_prog')
    call health#report_warn('g:lisp_host_prog is bound to ' . g:lisp_host_prog . '. Unable to run remaining health checks.')
    return 1
  endif
  return "Using default bindings ('sbcl') for g:lisp_host_prog."
endfunction

function! s:check_lisp_bin() abort
  " Check that SBCL is installed and is executable
  let lisp_bin = exepath('sbcl')
  if empty(lisp_bin)
    call health#report_error('SBCL executable is not in $PATH',
                            \['Install SBCL either using your package manager, or manually from http://www.sbcl.org/ .',
                             \"If you would prefer to use some other Common Lisp implementation, set g:lisp_host_prog in your init.vim to a command which doesn't print anything to the output by itself and loads Quicklisp's setup.lisp and lisp#LispHostScript(). E.g. by default this is set to ['sbcl', '--noinform', '--disable-debugger', '--load', expand('~/quicklisp/setup.lisp'), '--load', lisp#LispHostScript()]."])
    return 1
  endif

  if executable(lisp_bin) != 1
    call health#report_error("`" . lisp_bin . "' is not executable.")
    return 1
  else
    let lisp_bin_version = systemlist(['sbcl', '--version'])[0]
  endif
  return 'Found SBCL version `' . lisp_bin_version . '` at `' . lisp_bin . '`.'
endfunction

function! s:check_quicklisp() abort
  " Check that quicklisp is installed and can be loaded when SBCL starts
  let quicklisp_suggestions = ['Please install Quicklisp by following installation instructions on https://www.quicklisp.org/ .',
                              \"If you installed Quicklisp to a non-default directory, set g:lisp_host_quicklisp_setup to the location of Quicklisp's `setup.lisp` file; e.g. the default is: let g:lisp_host_quicklisp_setup='~/quicklisp/setup.lisp'."]

  if exists('g:lisp_host_quicklisp_setup')
    call health#report_info('g:lisp_host_quicklisp_setup is bound to `' . g:lisp_host_quicklisp_setup . '`.')
    let s:quicklisp_setup = expand(g:lisp_host_quicklisp_setup)
  endif
  call health#report_info('Loading Quicklisp from `' . s:quicklisp_setup . '`.')

  if !filereadable(s:quicklisp_setup)
    call health#report_error('File `' . s:quicklisp_setup . "` either doesn't exist or is not readable.",
                            \quicklisp_suggestions)
    return 1
  endif

  let quicklisp_output = system(['sbcl', '--disable-debugger',
                                \        '--load', s:quicklisp_setup,
                                \        '--eval', '(uiop:quit #+quicklisp 0'.
                                                            \' #-quicklisp 1)'])
  if v:shell_error
    call health#report_error('Quicklisp is either not installed or does not get properly loaded when SBCL is started.',
                            \quicklisp_suggestions + ['Output was: ' . quicklisp_output])
    return 1
  endif
  return 'Quicklisp is installed and gets properly loaded.'
endfunction

function! s:check_cl_neovim() abort
  " Check that cl-neovim can be quickloaded
  let quickload_output = system(['sbcl', '--disable-debugger',
                                \        '--load', s:quicklisp_setup,
                                \        '--eval', "(handler-bind ((error #'(lambda (c)".
                                                                            \"(princ c)".
                                                                            \"(uiop:quit 1))))".
                                                    \"(ql:quickload :cl-neovim)".
                                                    \"(uiop:quit 0))"])
  if v:shell_error
    call health#report_error('Could not quickload cl-neovim.',
                            \['You might need to install libuv1-dev with your package manager or manually build it from https://github.com/libuv/libuv .',
                             \'Output was: ' . quickload_output])
    return 1
  endif
  return 'cl-neovim can be quickloaded.'
endfunction


function! health#lisp#check() abort
  let checks = ['s:check_g_lisp_host_prog', 's:check_lisp_bin', 's:check_quicklisp', 's:check_cl_neovim']

  call health#report_start('Lisp provider')

  for check in checks
    let status = function(check)()
    if status == 1
      return
    else
      call health#report_info(status)
    endif
  endfor

  " TODO: check versions of cl-neovim and cl-messagepack-rpc?
  " TODO: suggestion reporting could be more useful by parsing output and looking for certain words (e.g. uiop:quit fails => update sbcl)
  call health#report_ok('Lisp host should be working correctly!')
endfunction
