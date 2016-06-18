let s:lisp_host_script = expand('<sfile>:p:h') . "/host.lisp"

function! lisp#GetImplementationCmd()
  " While sbcl should probably be the default implementation everyone
  " writing plugins uses, we do allow user to specify his favourite
  " implementation via g:lisp_host_prog variable if he so desires.

  if exists('g:lisp_host_prog')
    let implementation = g:lisp_host_prog
  else
    let implementation = "sbcl"
  endif

  if type(implementation) == type("")
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
      if $NVIM_LISP_LOG_FILE != ''
        call rpcrequest(channel_id, 'EnableLogging', $NVIM_LISP_LOG_FILE)
      endif
      call rpcrequest(channel_id, 'LoadPlugins', lisp_plugin_paths)
      return channel_id
    endif
  catch
    echomsg v:exception
  endtry
  throw 'Failed to load lisp host.'
endfunction


""" Using cl-neovim from the REPL
function! lisp#RegisterRepl(channel_id)
  " Register connected Lisp REPL as a fake `host', so that we can later define
  " commands/autocmds/functions on it.
  call remote#host#Register('lisp_repl_' . a:channel_id, '*.lisp', a:channel_id)
endfunction

function! lisp#RegisterReplCallback(channel_id, spec)
  " Register a callback from REPL with neovim via our fake host.
  let type = a:spec.type
  let name = a:spec.name
  let sync = a:spec.sync
  let opts = a:spec.opts
  let rpc_method = ':' . type . ':' . name
  let host = 'lisp_repl_' . a:channel_id

  if type == 'command'
    call remote#define#CommandOnHost(host, rpc_method, sync, name, opts)
  elseif type == 'autocmd'
    let rpc_method .= ':' . get(opts, 'pattern', '*')
    call remote#define#AutocmdOnHost(host, rpc_method, sync, name, opts)
  elseif type == 'function'
    call remote#define#FunctionOnHost(host, rpc_method, sync, name, opts)
  else
    echoerr 'Invalid declaration type: '.type
  endif
endfunction
