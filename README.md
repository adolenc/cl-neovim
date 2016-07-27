cl-neovim
=========
[![Build Status](https://travis-ci.org/adolenc/cl-neovim.svg?branch=master)](https://travis-ci.org/adolenc/cl-neovim)
[![Coverage Status](https://coveralls.io/repos/github/adolenc/cl-neovim/badge.svg?branch=master)](https://coveralls.io/github/adolenc/cl-neovim?branch=master)
[![Quicklisp dist](http://quickdocs.org/badge/cl-neovim.svg)](http://quickdocs.org/cl-neovim/)
[![MIT licensed](https://img.shields.io/badge/license-MIT-blue.svg)](./LICENSE)

cl-neovim is a [Neovim](http://neovim.io/) client library for writing Neovim plugins using Common Lisp.

## Installation
Prerequisites for cl-neovim are `SBCL` along with [Quicklisp](https://www.quicklisp.org/), and `libuv1-dev`, which you should be able to install with your package manager or by manually compiling [libuv](https://github.com/libuv/libuv#build-instructions).

For now you will also need to manually install latest versions of [cl-messagepack](https://github.com/mbrezu/cl-messagepack) and [cl-messagepack-rpc](https://github.com/adolenc/cl-messagepack-rpc), which you can do by cloning the repositories into your `~/quicklisp/local-projects` folder:

    $ git clone https://github.com/mbrezu/cl-messagepack ~/quicklisp/local-projects/cl-messagepack/
    $ git clone https://github.com/adolenc/cl-messagepack-rpc ~/quicklisp/local-projects/cl-messagepack-rpc/

To install cl-neovim itself you have to clone this repository into your `~/quicklisp/local-projects` folder as well:

    $ git clone https://github.com/adolenc/cl-neovim ~/quicklisp/local-projects/cl-neovim/

#### Installing plugin host
The previous step only installed cl-neovim for usage from the REPL. The easiest way to also install lisp host (required to use plugins written in Common Lisp) is to use [vim-plug](https://github.com/junegunn/vim-plug). Add

    Plug 'adolenc/cl-neovim'

into your init.vim, run `:PlugInstall` from within Neovim, restart Neovim and run `:UpdateRemotePlugins`. If everything worked correctly, calling `:Lisp (print 42)` command (after restarting one more time) should output `42` into your prompt.

## Using the package
To use the package from the REPL, first run Neovim and make it listen to some address:

    $ NVIM_LISTEN_ADDRESS=/tmp/nvim nvim

start your SBCL REPL and enter:

    * (ql:quickload :cl-neovim)
    * (nvim:connect :file "/tmp/nvim") 
    * (nvim:command "echo 'Hello from Common Lisp!'")

which should display "Hello from Common Lisp!" into your Neovim's prompt.

## Example plugins
cl-neovim looks for lisp plugins inside `$VIMRUNTIME/rplugin/lisp/` directory. Note that simply loading plugins in your init.vim is not enough -- the first time around (and every time your callback specifications change) you will need to run `:UpdateRemotePlugins` from within Neovim to register the plugins.

#### Simple plugin
The following is a (slightly convoluted) translation of python plugin example from [:h remote-plugin-example](http://neovim.io/doc/user/remote_plugin.html#remote-plugin-example); simply put it in `$VIMRUNTIME/rplugin/lisp/sample-plugin.lisp`:
```common-lisp
(defpackage #:sample-plugin
  (:use #:cl))
(in-package #:sample-plugin)

(defparameter *calls* 0 "Counter for calls.")

(defun increment-calls ()
  (if (= *calls* 5)
    (error "Too many calls!")
    (incf *calls*)))

(nvim:defcommand/s lisp-sample-cmd (&rest args &opts (range r) bang)
  (increment-calls)
  (setf (nvim:current-line) (format nil "Command: Called ~A times, args: ~A, range: ~A, bang: ~A" *calls* args r bang)))

(nvim:defautocmd/s buf-enter (filename)
  (declare (opts (pattern "*.lisp") (eval "expand(\"<afile>\")")))
  (increment-calls)
  (setf (nvim:current-line) (format nil "Autocmd: Called ~A times, file: ~A" *calls* filename))) 

(nvim:defun "LispSampleFun" (&rest args &opts (eval line-n))
  (declare (opts (eval "line(\".\")-1")))
  (increment-calls)
  (setf (nvim:current-line) (format nil "Function: Called ~A times, args: ~A, eval: ~A" *calls* args line-n)))
```

#### A more serious plugin
For plugins that require a more serious structure, cl-neovim registers `.asd` files in the root directory of the plugin, which means you can structure them as you wish. The only thing you will need is to add a `rplugin/lisp/[plugin-name].lisp` file which (quick)loads your plugin. For example:

```common-lisp
;;;; lisp-sample-plugin.asd
(in-package #:cl-user)
(asdf:defsystem #:lisp-sample-plugin
  :depends-on (#:cl-neovim)
  :serial T
  :components ((:module "src"
                :components ((:file "package")
                             (:file "main")))))
```
```common-lisp
;;;; src/package.lisp
(in-package :cl-user)
(defpackage #:lisp-sample-plugin
  (:use #:cl))
```
```common-lisp
;;;; src/main.lisp
(in-package #:lisp-sample-plugin)

(nvim:defcommand sample-callback ()
  (setf (nvim:current-line) "Hi nvim!"))
```
```common-lisp
;;;; rplugin/lisp/lisp-sample-plugin.lisp
(ql:quickload :lisp-sample-plugin)
```

## Exported symbols
cl-neovim allows you to connect to Neovim using either named pipes via `#'connect` and it's `:file` parameter, or using tcp address if you specify `:host` and `:port` arguments instead. Function also binds the connection to the `*nvim-instance*` variable and returns an instance of `nvim` class, which you can optionally pass as the final argument to all of the functions [below](#neovims-api) in case you need to be connected to multiple instances of Neovim at once.

#### Neovim's API
Package basically exports every function exposed by Neovim's api. You can find the full listing in [package.lisp](./src/package.lisp).

If you are familiar with the api Neovim exposes, some things in cl-neovim are renamed for nicer interface. Specifically:
- underscores are replaced with hyphens;
- names starting with `vim` have that prefix removed;
- predicates containing `is` have that replaced by suffix `p`;
- `get` and `set` are removed from names.

For example, `vim_get_current_line` is now just `current-line`, `buffer_get_line` becomes `buffer-line` and `window_is_valid` is `window-valid-p`.

Setter functions (those with `set` in their names) are implemented as inversions of their respective `get` counterparts via `setf` macro. So, to set current line to "some new line", you would use `(setf (nvim:current-line) "some new line")`.

By default all the calls are synchronous, meaning they block the execution of the thread until Neovim returns the result. You can optionally use asynchronous versions by appending `/a` to the name; these calls won't block the thread, but they also ignore all the errors and return values.

If you want to manually call Neovim api functions (that is, by string), you can use `#'call/s` and `#'call/a` for synchronous and asynchronous calls respectively, where the first argument of either call is either a instance of `nvim` class that gets returned by `#'connect`, or `t` (and, equivalently, `*nvim-instance*`) for last connected instance.

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
 - commands: `(range | count) | bang | register | eval`;
 - autocmds: none (values from `eval` get passed as normal arguments into lambda-list); and
 - functions: `eval`.

While these are full option names, you can also specify alternative names for them by wrapping them into a list of `(option alternative-name)`. Unless you explicitly specify differently via `declare-opts`, these options get set to some common-sense default values.

`declare-opts` is a declaration used to let Neovim know about expected behaviour of the callback and explicitly tell it which options you want it to pass along in the calls. Valid options in `declare-opt` are for:
 - commands: `nargs | complete | (range | count) | bang | register | eval`;
 - autocmds: `pattern | eval`; and
 - functions: `range | eval`.

Note that you can specify just the name of the option in which case default values are assumed, or an `(option value)` list if you want to assign custom values for options.

## Tips for writing plugins
cl-neovim is slightly different from most other Neovim client libraries in that it allows the developer to use the full power of REPL to continuously run and test all code, including callbacks. So, while you can simply write plugins by constantly restarting Neovim (and calling `:UpdateRemotePlugins` when necessary), you can be much more efficient by:
- starting Neovim with `NVIM_LISTEN_ADDRESS` specified: `$ NVIM_LISTEN_ADDRESS=/tmp/nvim nvim`;
- connecting to it via REPL: `* (nvim:connect :file "/tmp/nvim")`; and
- writing your plugins as you would write other lisp programs by constantly evaluating your subprograms in REPL.

Evaluating the callbacks in the REPL will get them registered with the connected Neovim instance, and in order to test them, you can trigger them in Neovim and then use `(nvim:listen-once)` in REPL to listen to messages from Neovim. E.g. for the `sample-callback` we specified [above](#a-more-serious-plugin), you would evaluate the `(nvim:defcommand sample-callback ...)` form in the REPL, run `:SampleCallback` from Neovim and evaluate `(listen-once)` in the REPL, after which line under cursor in Neovim should change to "Hi nvim!".

Because `(listen-once)` is slightly more work than one would like, I suggest you trigger the callback from the REPL itself -- that is by calling `(nvim:command "SampleCallback")` (or `(nvim:call-function "SampleCallback" '())` for functions), which runs `listen-once` for you behind the scenes.

## Debugging your plugins
Only 'printf' debugging is truly supported: printing to standard output from the REPL properly prints to the `*standard-output*`, but ignores it when plugin is ran using plugin host; that is unless Neovim is started with `$NVIM_LISP_LOG_FILE` set, in which case all the output is redirected to that file. Note that while plugin host should properly close the file when Neovim shuts down, if it for whatever reason fails to do so (or if you want instant updates to log file), simply use `(force-output)` after printing, so you don't lose buffered output.

Additionally you can set `NVIM_LISP_LOG_LEVEL` to `DEBUG` to log messages passed between cl-neovim and Neovim itself, or to `DEBUG1` to also track actual bytes. If you want to see messages passed between Neovim and cl-neovim running in your REPL, you can manually enable logging by evaluating `(nvim:enable-logging :level :debug)` (or `:debug1` for bytes).

## Running tests
There are two aspects to testing cl-neovim: testing how it works from the REPL and how it works from host. To test REPL, run Neovim with `$ NVIM_LISTEN_ADDRESS=/tmp/nvim nvim`. Then, run REPL with same `NVIM_LISTEN_ADDRESS` specified, e.g. `$ NVIM_LISTEN_ADDRESS=/tmp/nvim sbcl`. After that, evaluate

    * (ql:quickload :cl-neovim)
    * (asdf:test-system :cl-neovim)

to actually run the tests.

On the other hand, to test the plugin host, you need to add

    let g:lisp_host_enable_tests=1

to your init.vim (.nvimrc), run `:UpdateRemotePlugins` from Neovim, restart it, and finally run

    : LispHostRunTests

from Neovim to run the tests. After you are done with testing the host, it is recommended that you remove the `let g:lisp_host_enable_tests=1` line from your init.vim and run `:UpdateRemotePlugins` again, otherwise your Neovim will have a bunch of useless (auto)commands and functions registered.

## Contributions
Are very welcome. I would be more than happy to merge pull requests or just hear your criticism/ideas to improve cl-neovim.

## License
Copyright (c) 2015 Andrej Dolenc

Licensed under the MIT License.
