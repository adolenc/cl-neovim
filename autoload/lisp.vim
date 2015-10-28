let s:lisp_host_script = expand('<sfile>:p:h') . "/host.lisp"

function! lisp#RequireLispHost(host)
  " Collect registered Lisp plugins into args
  let args = ["--script", s:lisp_host_script]
  let lisp_plugins = remote#host#PluginsForHost(a:host.name)
  for plugin in lisp_plugins
    call add(args, plugin.path)
  endfor

  try
    let channel_id = rpcstart("sbcl", args)
    if rpcrequest(channel_id, 'poll') == 'ok'
      return channel_id
    endif
  catch
    echomsg v:exception
  endtry
  throw 'Failed to load lisp host.'
endfunction
