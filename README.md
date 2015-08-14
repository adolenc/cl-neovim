## About
This is a [Neovim](http://neovim.io/) client library, which can be used to write neovim plugins using Common Lisp.

A lot of people already implemented libraries for writing neovim plugins in [various different languages](https://github.com/neovim/neovim/wiki/Related-projects#api-clients), but as far as I know this is the first attempt at adding support for Common Lisp.

## Installing package
The simplest way to install the package is to use [quicklisp](https://www.quicklisp.org/). You need to clone 4 repositories into your `~/quicklisp/local-projects` folder:
 - cl-neovim: `$ git clone https://github.com/adolenc/cl-neovim`;
 - cl-messagepack: `$ git clone https://github.com/mbrezu/cl-messagepack`;
 - cl-async: `$ git clone https://github.com/orthecreedence/cl-async` and
 - cl-libuv: `$ git clone https://github.com/orthecreedence/cl-libuv`.

You will also need `sbcl` and `libuv1-dev` which you should be able to install with your package manager.

#### a) Using host for writing plugins
The easiest way to install and test host is to use `plug`. Add

    Plug 'cl-neovim'

into your .nvimrc and symlink `cl-neovim` directory to `~/.nvim/plugged/`:

    $ ln -s ~/quicklisp/local-projects/cl-neovim ~/.nvim/plugged/cl-neovim

This installs the host and a [sample plugin](https://github.com/adolenc/cl-neovim/blob/master/rplugin/lisp/sample-plugin.lisp), which you can test by running `$ nvim -c UpdateRemotePlugins` and restarting neovim. After this, executing `:Cmd`, `:call Func()` or entering a lisp buffer should trigger the current line to be rewritten (but only the first 5 times).

In case you would like to install the host manually, you need to copy `autoload/` and `plugin/` folders into `~/.nvim/` folder:

    $ cp -r ~/quicklisp/local-projects/cl-neovim/autoload ~/quicklis/local-projects/cl-neovim/plugin ~/.nvim/

#### b) Using the package
To use the package, run neovim and make it listen to the right address:

    $ NVIM_LISTEN_ADDRESS=127.0.0.1:7777 nvim

Start your sbcl REPL and enter:

    * (ql:quickload 'cl-neovim)
    * (nvim:connect :host "127.0.0.1" :port 7777)
    * (nvim:command "echo 'Hello from Common Lisp!'")

which should display "Hello from Common Lisp!" into your neovim's prompt. Alternatively you can start `nvim` without specifying `NVIM_LISTEN_ADDRESS`, and connect to it via the named pipe it creates (you can find the address by using `:echo $NVIM_LISTEN_ADDRESS` from inside neovim:

    * (nvim:connect :filename "[path to named pipe]")

## API
#### Exported API
Package basically exports every function exposed by neovim's api. You can find the full listing in [interface.lisp](https://github.com/adolenc/cl-neovim/blob/master/src/interface.lisp#L50-L166) (first string argument is the name).

Some things are renamed for nicer interface though. Specifically:
- underscores are replaced with hyphens;
- names starting with `vim_` have that prefix removed (except for `vim_eval`);
- `get_` and `set_` are removed from names.

For instance, `vim_get_current_line` is now just `current-line` and `buffer_get_line` becomes `buffer-line`.

Setter functions (those with `set` in their names) are implemented as inversions of their respective `get` counterparts via `setf` macro. So, to set current line to "some new line", you would use `(setf (nvim:current-line) "some new line")`.

By default, regular (non-setter) functions are synchronous, meaning they block the execution of the thread until neovim returns the result. You can optionally use asynchronous versions by appending `-a` to the name, and these functions don't block the thread but instead return a promise. If you later on want to use the result, you can use `(nvim:finalize [promise])` to block the thread until result is available. So, for instance, `(nvim:finalize (nvim:current-line-a))` === `(nvim:current-line)`.

Setter functions on the other hand are by default asynchronous, but you can similarly force them to be synchronous by appending `-s` to the name of function call. So, `(setf (nvim:current-line-s) "Wait for me")` blocks the thread until the current line is actually changed.

#### Callbacks
Callbacks for neovim are of the form:
````
callback-type name {sync-specifier}* args {options}? form*

callback-type  ::= defcmd | defautocmd | defunc
sync-specifier ::= :async | :sync
options        ::= (opts [plist])
````
`callback-type` specifies the type of callback registered with neovim: `defcmd` for commands, `defautocmd` for autocommands and `defunc` for functions.

`name` can be a string, in which case it is registered with neovim verbatim, or a symbol, in which case the `hyphen-separated-name` is registered with neovim as `CamelCaseName`.

`sync-specifier` specifies if neovim gets blocked during the call (and expects a result) - `:sync`, or if the result can be discarded, meaning it can run asynchroniously - `:async`. If omitted, `:async` is used.

`options` is used to specify additional options (eg. nargs, range, eval, pattern, ...) for the callback function.

You can find example of a simple plugin (a translation of python example from [:h remote-plugin-example](http://neovim.io/doc/user/remote_plugin.html#remote-plugin-example)) in [rplugin/lisp/sample-plugin.lisp](https://github.com/adolenc/cl-neovim/blob/master/rplugin/lisp/sample-plugin.lisp).

## Contributions
would be awesome. As a relative newcomer to Common Lisp I would be really happy to merge pull requests or just hear your criticism.

## TODO
 - cl-msgpack-rpc should not care about how the parameters it receives from requests/notifications are passed. Instead, it should just pass everything forward and it should be cl-neovim which takes care of properly calling functions;
 - cl-msgpack-rpc should not block when it receives a new message;
 - add more debugging functionality for plugin writers;
 - add a makefile to simplify installation procedure (and also generate interface on the fly by using [src/generate-api.lisp](https://github.com/adolenc/cl-neovim/blob/master/src/generate-api.lisp) instead of having it manually written in the [src/interface.lisp](https://github.com/adolenc/cl-neovim/blob/master/src/interface.lisp).
