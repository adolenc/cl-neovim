let s:lisp_host_script = expand('<sfile>:p:h') . "/host.lisp"

function! lisp#RequireLispHost(host)
  let args = ["--script", s:lisp_host_script]
  let lisp_plugins = remote#host#PluginsForHost(a:host.name)
  let lisp_plugin_paths = []
  for plugin in lisp_plugins
    call add(lisp_plugin_paths, plugin.path)
  endfor

  try
    let channel_id = rpcstart("sbcl", args)

    if rpcrequest(channel_id, 'Poll') == 'ok'
      if $NVIM_LISP_DEBUG != ''
        call rpcrequest(channel_id, 'EnableDebugging', $NVIM_LISP_DEBUG)
      endif
      call rpcrequest(channel_id, 'LoadPlugins', lisp_plugin_paths)
      return channel_id
    endif
  catch
    echomsg v:exception
  endtry
  throw 'Failed to load lisp host.'
endfunction
