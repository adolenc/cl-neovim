function! lisp#RequireLispHost(host)
  try
    " Collect registered Python plugins into args
    let args = ["--script", "/home/andr3/quicklisp/local-projects/cl-neovim/host.lisp"]
    let lisp_plugins = remote#host#PluginsForHost(a:host.name)
    for plugin in lisp_plugins
      call add(args, plugin.path)
    endfor
    return rpcstart("sbcl", args)
  catch
    echomsg v:exception
  endtry
  throw 'Failed to load lisp host.'
endfunction
