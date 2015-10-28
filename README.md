## About
This is a [Neovim](http://neovim.io/) client library, which can be used to write neovim plugins using Common Lisp.

A lot of people already implemented libraries for writing neovim plugins in [various different languages](https://github.com/neovim/neovim/wiki/Related-projects#api-clients), but as far as I know this is the first attempt at adding support for Common Lisp.

## Installing package
The simplest way to install the package is to use [quicklisp](https://www.quicklisp.org/). For now you will need to manually clone 5 repositories into your `~/quicklisp/local-projects` folder:
 - [cl-neovim](https://github.com/adolenc/cl-neovim): `$ git clone https://github.com/adolenc/cl-neovim`;
 - [cl-messagepack](https://github.com/mbrezu/cl-messagepack): `$ git clone https://github.com/mbrezu/cl-messagepack`;
 - [form-fiddle](https://github.com/Shinmera/form-fiddle): `$ git clone https://github.com/Shinmera/form-fiddle`;
 - [cl-async](https://github.com/orthecreedence/cl-async): `$ git clone https://github.com/orthecreedence/cl-async` and
 - [cl-libuv](https://github.com/orthecreedence/cl-libuv): `$ git clone https://github.com/orthecreedence/cl-libuv`.

You will also need `sbcl` and `libuv1-dev` binaries which you should be able to install with your package manager.

#### a) Using host for writing plugins
The easiest way to install and test host is to use [vim-plug](https://github.com/junegunn/vim-plug). Add

    Plug 'cl-neovim'

into your .nvimrc and symlink `cl-neovim` directory to `~/.nvim/plugged/`:

    $ ln -s ~/quicklisp/local-projects/cl-neovim ~/.nvim/plugged/cl-neovim

This installs the host and a [sample plugin](https://github.com/adolenc/cl-neovim/blob/master/rplugin/lisp/sample-plugin.lisp), which you can test by running `$ nvim -c UpdateRemotePlugins` and restarting neovim. After this, executing `:LispSampleCmd`, `:call LispSampleFunc()` or entering a lisp buffer should trigger the line under cursor to be rewritten (but only the first 5 times).

In case you would like to install the host manually, you need to copy `autoload/` and `plugin/` folders into `~/.nvim/` folder:

    $ cp -r ~/quicklisp/local-projects/cl-neovim/autoload ~/quicklis/local-projects/cl-neovim/plugin ~/.nvim/

#### b) Using the package
To use the package, run neovim and make it listen to the some address (can also be a named pipe):

    $ NVIM_LISTEN_ADDRESS=127.0.0.1:7777 nvim

Start your sbcl REPL and enter:

    * (ql:quickload :cl-neovim)
    * (nvim:connect :host "127.0.0.1" :port 7777)
    * (nvim:command "echo 'Hello from Common Lisp!'")

which should display "Hello from Common Lisp!" into your neovim's prompt. Alternatively you can start `nvim` without specifying `NVIM_LISTEN_ADDRESS`, and connect to it via the named pipe it automatically creates (you can find the address by evaluating `:echo $NVIM_LISTEN_ADDRESS` from within neovim):

    * (nvim:connect :file "[path to named pipe]")

## API
#### Exported API
Package basically exports every function exposed by neovim's api. You can find the full listing in [interface.lisp](https://github.com/adolenc/cl-neovim/blob/master/src/interface.lisp#L51-L167) (first string argument is the name).

Some things are renamed for nicer interface though. Specifically:
- underscores are replaced with hyphens;
- names starting with `vim_` have that prefix removed (except for `vim_eval`);
- `get_` and `set_` are removed from names.

For instance, `vim_get_current_line` is now just `current-line` and `buffer_get_line` becomes `buffer-line`.

Setter functions (those with `set` in their names) are implemented as inversions of their respective `get` counterparts via `setf` macro. So, to set current line to "some new line", you would use `(setf (nvim:current-line) "some new line")`.

By default all the calls are synchronous, meaning they block the execution of the thread until neovim returns the result. You can optionally use asynchronous versions by appending `/a` to the name; these calls won't block the thread, but they also ignore all the errors and return values.

#### Callbacks
Callbacks for neovim are of the form:
````
callback-type name sync-specifier? (args) documentation declare-opts? form*

callback-type  ::= defcommand | defautocmd | defun
sync-specifier ::= :sync
args           ::= lambda-list [&opts args-opt*]?
args-opt       ::= option | (option alternative-name)
declare-opts   ::= (declare (opts [declare-opt* | ignore]))
declare-opt    ::= option | (option value)
````
`callback-type` specifies the type of callback registered with neovim: `defcommand` for commands, `defautocmd` for autocommands and `defun` for functions.

`name` can be a string, in which case it is registered with neovim verbatim, or a symbol, in which case the `hyphen-separated-name` is registered with neovim as `CamelCaseName`.

When `sync-specifier` is set to `:sync` neovim gets blocked during the call, otherwise it discards the results and error messages which allows it to run asynchroniously.

`args` is basically just a regular lambda-list, but you can also append it with `&opts` keyword and specify names for arguments for additional options neovim passes along with the regular arguments when called. `args-opt` production's options are, for:
 - commands: `(range | count) | bang | register | vim-eval`;
 - autocmds: none (values from `vim-eval` get passed as normal arguments into lambda-list); and
 - functions: `vim-eval`.

While these are full option names, you can also specify alternative names for them by wrapping them into a list of `(option alternative-name)`. Note that in order to receive these options from neovim, you will have to specify them in `declare-opts`.

`declare-opts` is a declaration used to let neovim know about expected behaviour of the callback and tell it which options you want it to pass along in the calls. Valid options in `declare-opt` are for:
 - commands: `nargs | complete | (range | count) | bang | register | vim-eval`;
 - autocmds: `pattern | vim-eval`; and
 - functions: `range | vim-eval`.

<!--- copied directly from python host: neovim/plugin/decorators.py#L45-L134 -->

Note that you can specify just the name of the option in which case default values are assumed, or an `(option value)` list if you want to assign custom values for options.

In case you want to write plugin or functions which you will manually call via `rpcrequest` or `rpcnotify` (so circumventing the host architecture), you should specify an option `ignore` inside `declare-opts`, so you can just list the arguments for function in the call without having to wrap them into artificial lists.

## Sample plugin
You can find example of a simple plugin (a translation of python example from [:h remote-plugin-example](http://neovim.io/doc/user/remote_plugin.html#remote-plugin-example)) in [rplugin/lisp/sample-plugin.lisp](https://github.com/adolenc/cl-neovim/blob/master/rplugin/lisp/sample-plugin.lisp).

## Contributions
Are very welcome. As a relative newcomer to Common Lisp I would be really happy to merge pull requests or just hear your criticism/ideas to improve the library.

## Support
At the moment I develop this library on Debian GNU/Linux OS and use SBCL and Quicklisp. It should not be too much work to make it work on other systems (I would assume the problems are mainly with the host architecture located in [autoload/](https://github.com/adolenc/cl-neovim/blob/master/autoload/) directory) so if there is request I'd be more than willing to port it (or merge the port in).
