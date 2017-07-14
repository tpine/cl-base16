cl-base16
===========
A implementation of a [base16](https://github.com/chriskempson/base16) builder for lisp.

## Useage ##
To run the builder run the following:
```lisp
(ql:quickload :cl-base16)

(cl-base16:update)
```

The base directory of the builders scheme.yaml can be changed by setting `*base-dir*` before running update.

The function `apply-scheme` is also exported allowing a system to specify the scheme and template and have the result returned as a string. A example of this functionality is in [base16-stumpwm](https://github.com/tpine/base16-stumpwm) which then uses the result to apply the scheme to StumpWM as it is running.
