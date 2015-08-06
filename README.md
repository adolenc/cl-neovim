About
-----
This is a [Neovim](http://neovim.io/) client library, which could eventually be used to write neovim plugins with Common Lisp.

A lot of people already implemented libraries for writing neovim plugins in [various different languages](https://github.com/neovim/neovim/wiki/Related-projects#api-clients), but as far as I know this is the first attempt at adding support for Common Lisp.

Installation
-----------
Right now the simplest way to install the package is to use [quicklisp](https://www.quicklisp.org/). First clone this repository into your `quicklisp/local-projects` folder:

    cd ~/quicklisp/local-projects/ && git clone https://github.com/adolenc/cl-neovim

After this, run neovim and make it listen to the right address:

    NVIM_LISTEN_ADDRESS=127.0.0.1:7777 nvim

The only package `cl-neovim` depends on and is not up-to-date in quicklisp's repository is [cl-messagepack](https://github.com/mbrezu/cl-messagepack), so clone that into `local-projects` as well. After that, run the repl and evaluate:

    (ql:quickload 'cl-neovim)
    (nvim:connect :host "127.0.0.1" :port 7777)
    (nvim:command "echo 'Hello from common lisp!'")

which should display "Hello from common lisp!" into your neovim's prompt.


I'm using SBCL in debian testing for development so it definitely works here. If you manage to make it run in some other implementation, be sure to let me know!

API
---
Package basically exports every function exposed by neovim's api. You can find the full listing in [interface.lisp](https://github.com/adolenc/cl-neovim/blob/master/interface.lisp#L39-L155) (first string argument is the name).

Some things are renamed for nicer interface though. Specifically:
- underscores are replaced with hyphens;
- names starting with `vim_` have that prefix removed (except for `vim_eval`);
- `get_` and `set_` are removed from names.

For instance, `vim_get_current_line` is now just `current-line` and `buffer_get_line` becomes `buffer-line`.

Setter functions (those with `set` in their names) are implemented as inversions of their respective `get` counterparts via `setf` macro. So, to set current line to "some new line", you would use `(setf (nvim:current-line) "some new line")`.

Contributions
-------------
would be awesome. As a relative newcomer to Common Lisp I would be really happy to merge pull requests or just hear your criticism.

TODO
----
- add support for stdio;
- figure out how to actually register with neovim as a host;
- seamlessly convert between vim names and lisp names;
- async versions of exported api commands;
- example of a plugin;
- ...
