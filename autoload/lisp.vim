let s:lisp_host_script = expand('<sfile>:p:h') . "/host.lisp"

function! lisp#GetImplementationCmd()
  " While sbcl should probably be the default implementation everyone
  " writing plugins uses, we do allow user to specify his favourite
  " implementation via g:lisp_implementation variable if he so desires.

  if exists('g:lisp_implementation')
    let implementation = g:lisp_implementation
  else
    let implementation = "sbcl"
  endif

  if type(implementation) == 1
    " For the most common implementations, user shouldn't have to specify all
    " the arguments in a list, but just the name of the implementation as a
    " string.
    let implementation_cmds = {"sbcl": ["sbcl", "--script", "$LISP_HOST"]}
    let implementation_cmd = implementation_cmds[implementation]
  else
    let implementation_cmd = implementation
  endif

  let i = 0
  while i < len(implementation_cmd)
    if implementation_cmd[i] == "$LISP_HOST"
      let implementation_cmd[i] = s:lisp_host_script
    endif
    let i = i + 1
  endwhile

  return implementation_cmd
endfunction

function! lisp#RequireLispHost(host)
  let lisp_plugins = remote#host#PluginsForHost(a:host.name)
  let lisp_plugin_paths = []
  for plugin in lisp_plugins
    call add(lisp_plugin_paths, plugin.path)
  endfor

  let implementation_cmd = lisp#GetImplementationCmd()

  try
    let channel_id = rpcstart(implementation_cmd[0], implementation_cmd[1:])

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
