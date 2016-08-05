# emacs-codesearch: Tools for using codesearch from Emacs

These extensions allow you to use
[codesearch](https://github.com/google/codesearch) from Emacs.

**THIS IS STILL A WORK IN PROGRESS**

The initial goal here is to combine the functionality of
[codesearch.el](https://github.com/abingham/codesearch.el) and
[helm-codesearch](https://github.com/youngker/helm-codesearch.el). They can and
should share a common core of functionality, primarily that which runs the
codesearch tools and manages the indices. This common functionality will reside
in `codesearch.el`. The various display-oriented functions will live in separate
packages like `helm-codesearch.el. The various display-oriented functions will
live in separate packages like `helm-codesearch.el`.

## For developers

The `codesearch.el` packages exposes two main functions for integration:
`codesearch-run-csearch` and `codesearch-run-cindex`. Currently, each of these
accepts a callback of a single-argument, the full output of the command. These
functions run the commands asynchronously.
