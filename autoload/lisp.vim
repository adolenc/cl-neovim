function! lisp#RequireLispHost(host)
  try
    return rpcstart("sbcl", ["--script", "/home/andr3/quicklisp/local-projects/cl-neovim/host.lisp"])
  catch
    echomsg v:exception
  endtry
  throw 'Failed to load lisp host. You can try to see what happened '.
        \ 'by starting Neovim with the environment variable '.
        \ '$NVIM_JL_DEBUG set to a file and opening '.
        \ 'the generated log file. Also, the host stderr will be available '.
        \ 'in Neovim log, so it may contain useful information. '.
        \ 'See also ~/.nvimlog.'
endfunction
