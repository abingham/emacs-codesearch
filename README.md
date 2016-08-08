# emacs-codesearch: Tools for using codesearch from Emacs

These extensions allow you to use
[codesearch](https://github.com/google/codesearch) from Emacs.

This project comprises two main parts. First, `codesearch.el` containts
low-level functionality for interacting with the codesearch tools `cindex` and
`csearch`. These functions execute the tools, help manage your index files, and
provide access to tool output, but they don't provide any sort of user
interface.

The second main part is a collection packages that provide user interfaces for
the codesearch tools. Currently we have two interfaces:

 * `listing-codesearch.el` provides a very simple, listing-oriented display of
   results
 * `helm-codesearch.el` provides integration with the popular
   [helm](https://github.com/emacs-helm/helm) framework

These UI packages can be used independently or together.

## Core functionality from `codesearch.el`

`codesearch.el` is needed for any interaction with the codesearch tools.

### Installation

It can be installed from [melpa](https://melpa.org):
```
M-x package-install codesearch
```

Then just `require` it somewhere in your Emacs configuration:
```
(require 'codesearch)
```

(Note that the various UI packages will also `require` codesearch, so this step
may not be strictly necessary).

### Configuring your index files(s)

The codesearch tools use an index file for storing information about the symbols
found in your source code. `codesearch.el` provides two modes for specifying the
locations of these index files.

The first mode is *global index mode* wherein you specify a single, global index
file that is used for all codesearch operations. To use global index mode, set
the variable `codesearch-global-csearchindex` to the path for you index file.
When set, the specified file will be used for every search and indexing
operation that you perform.

The second indexing mode is *project index mode* where you will use different
index files for different directories. To use this mode, first make sure that
`codesearch-global-csearchindex` is set to `nil`. Then, set the variable
`codesearch-csearchindex` to the *filename* that you use for your indexes. Now
when `codesearch.el` performs operations it will look for the file
`codesearch-csearchindex` in the current directory and its ancestors until one
is found. If none is found, an error is raised.

**NB:** At the time of writing there is no way to *create* new project-specific
indices using `codesearch.el`. You'll need to create those outside of Emacs
until
[the issue is addressed](https://github.com/abingham/emacs-codesearch/issues/4).

### Further configuration

You can also configure other aspects of `codesearch.el`, though that's not
generally necessary. See the configuration group `codesearch` for full details.

### Low-level codsearch commands

The core codesearch package provides a number of commands that you need to use
in order to manage your indices.

 * `codesearch-build-index` - This command adds a directory tree to the
   configured index.
 * `codeseearch-update-index` - This command updates the currently configured
   index by rescanning all of the directories that it already contains.
 * `codesearch-reset` - This removes the currently configured index completely.

## List-oriented UI with `listing-codesearch.el`

`listing-codesearch` provides a simple, list-oriented UI for displaying and
interacting with codesearch results.

### Installation

It can be installed from [melpa](https://melpa.org):
```
M-x package-install listing-codesearch
```

Then just `require` it somewhere in your Emacs configuration:
```
(require 'listing-codesearch)
```

### Configuration

`listing-codesearch` doesn't typically require any configuration, but check the
`listing-codesearch` configuration group for full details.

### Commands

`listing-codesearch` provides two functions for displaying information.

 * `listing-codesearch-search` - Perform a query on the currently configured
   index and lists the results in a buffer. Clicking on individual results in
   the output buffer will take you to those locations in the source code.

 * `listing-codesearch-list-directories` - List the directories currently index
   in the currently configured index.

## Helm integration with `helm-codesearch.el`

**TODO**
