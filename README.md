# Emacs Configuration
Paul Perry
pop@paulperry.net

Tested on Emacs 24.3 (9.0)
on OS X 10.9.2 and Ubuntu 12.04

This configuration automatically installs several plugins using ELPA and Marmalade.
It is based on Andrea Crotti's [minimal emacs configuration](https://github.com/AndreaCrotti/minimal-emacs-configuration/) (check his [youtube](https://www.youtube.com/watch?v=0cZ7szFuz18) presentation), with parts of his full [emacs configuration](https://github.com/AndreaCrotti/Emacs-configuration) and parts of Jessica Hamrick's [emacs configuration](https://github.com/jhamrick/emacs/). It adds [exec-path-from-shell](https://github.com/purcell/exec-path-from-shell). 

## Emacs plugins ##

* `python-mode` -- 
* `magit` -- git support
* `auto-complete` -- auto completion
* `autopair` -- paren matching
* `yasnippet` -- code snippet insertions
* `flycheck` -- python checker with flake8
* `jedi` -- general Python support
* `pydoc-info` -- Python documentation
* `markdown-mode` -- support for [Markdown](http://daringfireball.net/projects/markdown/) files
* `ein` -- [IPython notebook](http://ipython.org/notebook) plugin

<!---
* `auctex` -- LaTeX plugin 
* `matlab-mode` -- support Matlab files
-->

Make sure you have `pep8` and `pyflakes` and `pylint` installed (run
`pip install pep8 pyflakes pylint`).

### pychecker ###

`pychecker` is a little script to check that your Python code conforms
to the [PEP8 style guide](http://legacy.python.org/dev/peps/pep-0008/)
using the [pep8](https://pypi.python.org/pypi/pep8) and
[pyflakes](https://pypi.python.org/pypi/pyflakes/0.8.1) Python
packages. This may not be necessary with flycheck and flake8, but if
you want to use the C-c M-w binding to pychecker then you will want to
set this up.

If you do not want this functionality, you can comment out the block
of code in `python-settings.el` that starts with "pyflakes flymake
integration". Otherwise, read on.

1. In your `~/.bashrc`, add `$HOME/bin` to your `$PATH` environment variable like so:
  
  ```
  export PATH="$HOME/bin:$PATH"
  ```

2. Create a file in `~/bin/pychecker` with the following contents:

  ```
  #!/bin/bash
  
  pyflakes "$1"
  pep8 --repeat "$1"
  true
  ```

3. Make it executable with `chmod +x ~/bin/pychecker`.

4. Make sure you have `pep8` and `pyflakes` and `pylint` installed (run `pip
   install pep8 pyflakes pylint`).

5. Now it should work! 


