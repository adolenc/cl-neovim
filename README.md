cl-neovim
=========
[![Build Status](https://travis-ci.org/adolenc/cl-neovim.svg?branch=master)](https://travis-ci.org/adolenc/cl-neovim)
[![Coverage Status](https://coveralls.io/repos/github/adolenc/cl-neovim/badge.svg?branch=master)](https://coveralls.io/github/adolenc/cl-neovim?branch=master)
[![Quicklisp dist](http://quickdocs.org/badge/cl-neovim.svg)](http://quickdocs.org/cl-neovim/)
[![MIT licensed](https://img.shields.io/badge/license-MIT-blue.svg)](./LICENSE)

This is a [Neovim](http://neovim.io/) client library, which can be used to write Neovim plugins using Common Lisp.

A lot of people already implemented libraries for writing Neovim plugins in [various different languages](https://github.com/neovim/neovim/wiki/Related-projects#api-clients), but as far as I know this is the first attempt at adding support for Common Lisp.

## Installing package
The simplest way to install the package is to use [Quicklisp](https://www.quicklisp.org/). For now you will need to manually clone this repository into your `~/quicklisp/local-projects` folder:

    $ git clone https://github.com/adolenc/cl-neovim ~/quicklisp/local-projects/cl-neovim/

You will also need `sbcl` (though, as specified later, you can also use other implementations) and `libuv1-dev`, which you should be able to install with your package manager.

After that, evaluate

    * (ql:quickload :cl-neovim)

from the REPL to pull all the dependencies.

## Installing plugin host
The easiest way to install host is to use [vim-plug](https://github.com/junegunn/vim-plug). Add

    Plug 'adolenc/cl-neovim'

into your init.vim and run `:PlugInstall` from within Neovim.

In case you would like to install the host manually, you need to copy `autoload/` and `plugin/` folders into your `.config/nvim/` folder.

Sbcl is used as the default Lisp implementation and plugin writers should try and make sure their plugins work with it. While this is probably not a good idea for many reasons, you can make host (and subsequently all the lisp plugins you have installed) use your favourite implementation by setting `g:lisp_host_prog` variable in your init.vim to a list of `[{cmd}[, {args}...]]`. For it to work properly, command should start the implementation, load `"$LISP_HOST"` file ([autoload/host.lisp](autoload/host.lisp)), and not write anything to standard input/output by itself (e.g. for sbcl this would look like `let g:lisp_host_prog = ["sbcl", "--script", "$LISP_HOST"]`).

## Using the package
To use the package from the REPL, first run Neovim and make it listen to some address like so:

    $ NVIM_LISTEN_ADDRESS=/tmp/nvim nvim

start your Sbcl REPL and enter:

    * (ql:quickload :cl-neovim)
    * (nvim:connect :file "/tmp/nvim") 
    * (nvim:command "echo 'Hello from Common Lisp!'")

which should display "Hello from Common Lisp!" into your Neovim's prompt.

## Example plugins
cl-neovim looks for lisp plugins inside `$VIMRUNTIME/rplugin/lisp/` directory. Note that simply loading plugins in your init.vim is not enough -- the first time around (and every time your callback specifications change) you will need to run `:UpdateRemotePlugins` from within Neovim to register the plugins.

#### Simple plugin
The following is a (slightly convoluted) translation of python example from [:h remote-plugin-example](http://neovim.io/doc/user/remote_plugin.html#remote-plugin-example); simply put it in `$VIMRUNTIME/rplugin/lisp/sample-plugin.lisp`:
```common-lisp
(defpackage #:sample-plugin
  (:use #:cl #:cl-neovim)
  (:shadowing-import-from #:cl #:defun #:eval))
(in-package #:sample-plugin)

(defparameter *calls* 0 "Counter for calls.")

(defun increment-calls ()
  (if (= *calls* 5)
    (error "Too many calls!")
    (incf *calls*)))

(nvim:defcommand/s lisp-sample-cmd (&rest args &opts (range r) bang)
  (declare (opts (nargs "*") (complete "file")))
  (increment-calls)
  (setf (nvim:current-line) (format nil "Command: Called ~A times, args: ~A, range: ~A, bang: ~A" *calls* args r bang)))

(nvim:defautocmd/s buf-enter (filename)
  (declare (opts (pattern "*.lisp") (vim-eval "expand(\"<afile>\")")))
  (increment-calls)
  (setf (nvim:current-line) (format nil "Autocmd: Called ~A times, file: ~A" *calls* filename))) 

(nvim:defun "LispSampleFun" (&rest args &opts (vim-eval line-n))
  (declare (opts (vim-eval "line(\".\")-1")))
  (increment-calls)
  (setf (nvim:current-line) (format nil "Function: Called ~A times, args: ~A, eval: ~A" *calls* args line-n)))
```

#### A more serious plugin
For plugins that require a more serious structure, cl-neovim registers `.asd` files in the root directory of the plugin, which means you can structure them as you wish. The only thing you will need is to add a `rplugin/lisp/[plugin-name].lisp` file which `quickload`s your plugin. For example:

`lisp-sample-plugin.asd`:
```common-lisp
(asdf:defsystem #:lisp-sample-plugin
  :depends-on (#:cl-neovim)
  :serial T
  :components ((:module "src"
                :components ((:file "package")
                             (:file "main")))))
```
`src/package.lisp`:
```common-lisp
(in-package :cl-user)

(defpackage #:lisp-sample-plugin
  (:use #:cl #:cl-neovim)
  (:shadowing-import-from #:cl #:defun #:eval))
```
`src/main.lisp`:
```common-lisp
(in-package #:lisp-sample-plugin)

(nvim:defcommand sample-callback ()
  (setf (nvim:current-line) "Hi nvim!"))
```
`rplugin/lisp/lisp-sample-plugin.lisp`:
```common-lisp
(ql:quickload :lisp-sample-plugin)
```

## Tips for writing plugins
cl-neovim is slightly different from most other Neovim client libraries in that it allows the developer to use the full power of REPL to continuously run and test code; including callbacks. So, while you can simply write plugins by constantly restarting Neovim (and calling `:UpdateRemotePlugins` when necessary), you can be much more efficient by:
- starting Neovim with `NVIM_LISTEN_ADDRESS` specified: `$ NVIM_LISTEN_ADDRESS=/tmp/nvim nvim`;
- connecting to it via REPL: `* (nvim:connect :file "/tmp/nvim")`; and
- writing your plugins as you would write other lisp programs by constantly evaluating your subprograms in REPL.

Evaluating the callbacks in the REPL will get them registered with the connected Neovim instance, and in order to test them, you can trigger them in Neovim and then use `(nvim:listen-once)` in REPL to listen to messages from Neovim. E.g. for the `sample-callback` we specified [above](#a-more-serious-plugin), you would evaluate the `(nvim:defcommand sample-callback ...)` form in the REPL, run `:SampleCallback` from Neovim and evaluate `(listen-once)` in the REPL, after which line under cursor in Neovim should change to "Hi nvim!".

Because `(listen-once)` is slightly more work than one would like, I suggest you trigger the callback from the REPL itself -- that is by calling `(nvim:command "SampleCallback")`, which runs `listen-once` for you behind the scenes.

Only 'printf' debugging is truly supported: printing to standard output from the REPL properly prints to the output, but hides it when plugin is ran using plugin host; that is unless Neovim is started with `$NVIM_LISP_LOG_FILE` set, in which case all the output is redirected to that file. Note that while plugin host should properly close the file when Neovim shuts down, if it for whatever reason fails to do so (or if you want instant updates to log file), simply use `(force-output)` after printing, so you don't lose buffered output.

## Exported symbols
cl-neovim allows you to connect to Neovim using either named pipes via `#'connect` and it's `:file` parameter, or using tcp address if you specify `:host` and `:port` arguments instead. Function also binds the connection to the `*nvim-instance*` variable and returns an instance of `nvim` class, which you can optionally pass as the final argument to all of the functions [below](#neovims-api) in case you want to connect to multiple instances of Neovim at once.

#### Neovim's API
Package basically exports every function exposed by Neovim's api. You can find the full listing in [package.lisp](src/package.lisp).

If you are familiar with the api Neovim exposes, some things in cl-neovim are renamed for nicer interface. Specifically:
- underscores are replaced with hyphens;
- names starting with `vim_` have that prefix removed;
- predicates containing `is_` have that replaced by suffix `-p`;
- `get_` and `set_` are removed from names.

For example, `vim_get_current_line` is now just `current-line`, `buffer_get_line` becomes `buffer-line` and `window_is_valid` is `window-valid-p`.

Setter functions (those with `set` in their names) are implemented as inversions of their respective `get` counterparts via `setf` macro. So, to set current line to "some new line", you would use `(setf (nvim:current-line) "some new line")`.

By default all the calls are synchronous, meaning they block the execution of the thread until Neovim returns the result. You can optionally use asynchronous versions by appending `/a` to the name; these calls won't block the thread, but they also ignore all the errors and return values. If you for whatever reason want to be more explicit, you can also append `/s` to the names, but these functions are in all aspects equivalent to those without `/s`.

If you for whatever reason want to manually call Neovim api functions (that is, by string), you can use `#'call/s` and `#'call/a` for synchronous and asynchronous calls respectively, where the first argument of either call is either a instance of `nvim` class that gets returned by `#'connect`, or `t` (and, equivalently, `*nvim-instance*`) for last connected instance.

#### Callbacks
Callbacks for Neovim are of the form:
````
callback-type name (args) documentation declare-opts? form*

callback-type  ::= defcommand   | defautocmd   | defun    ; asynchronous versions
                 | defcommand/s | defautocmd/s | defun/s  ; synchronous versions
args           ::= lambda-list [&opts args-opt*]?
args-opt       ::= option | (option alternative-name)
declare-opts   ::= (declare (opts declare-opt*))
declare-opt    ::= option | (option value)
````
`callback-type` specifies the type of callback registered with Neovim: `defcommand` for commands, `defautocmd` for autocommands and `defun` for functions. These functions are all asynchronous, meaning Neovim will call them and instantly return control back to the user, completely ignoring their return values and any errors. If you would like Neovim to block user input until your callback is done executing, use the `/s` variants.

`name` can be a string, in which case it is registered with Neovim verbatim, or a symbol, in which case the `hyphen-separated-name` is registered with Neovim as `CamelCaseName`.

`args` is basically just a regular lambda-list, but you can also append it with `&opts` keyword and specify names for arguments for additional options Neovim passes along with the regular arguments when called. `args-opt` production's options are, for:
 - commands: `(range | count) | bang | register | vim-eval`;
 - autocmds: none (values from `vim-eval` get passed as normal arguments into lambda-list); and
 - functions: `vim-eval`.

While these are full option names, you can also specify alternative names for them by wrapping them into a list of `(option alternative-name)`.

`declare-opts` is a declaration used to let Neovim know about expected behaviour of the callback and tell it which options you want it to pass along in the calls. Valid options in `declare-opt` are for:
 - commands: `nargs | complete | (range | count) | bang | register | vim-eval`;
 - autocmds: `pattern | vim-eval`; and
 - functions: `range | vim-eval`.

Note that you can specify just the name of the option in which case some common-sense default values are assumed, or an `(option value)` list if you want to assign custom values for options.

## Contributions
Are very welcome. I would be more than happy to merge pull requests or just hear your criticism/ideas to improve cl-neovim.

## Support
I develop cl-neovim on Debian GNU/Linux OS and use SBCL and Quicklisp.

## License
Copyright (c) 2015 Andrej Dolenc

Licensed under the MIT License.
