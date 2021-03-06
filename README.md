
# Use Org-mode in your Common Lisp docstrings

To more easily keep your documentation synchronized with the source
code, write your Common Lisp documentation strings in Org-mode format
and use this system to extract each symbol usage into its own separate
Org file. Then your user manual and other documentation can just
include these sampled extracts.  Updates to the manual can be
automated from updates to the docstrings - which we're more likely to
actually remember to do.

Requires `iterate`.

## Main routines

### Function `write-package-files`

Documents a package by writing an Org file for each defined symbol.

    (write-package-files PACKAGE
                         [ :all FLAG ]
                         [ :system ASDF-SYSTEM-NAME ]
                         [ :path DIR ]
                         [ :index FLAG ]
                         [ :index-acc HASH-OR-NIL ]
                         [ :show-package FLAG ]
                         [ :hoist-exported FLAG ]
                         [ :page-title STRNIG ]
                         [ :always-disambiguate FLAG ]
                         [ :section-level NUMBER-OR-NIL ]
                         [ :package-headers FLAG ]
                         [ :usage-headers FLAG ]
                         [ :show-title FLAG ] )

-   The `package` argument should be a package specifier.
-   If the `all` keyword argument is given and is non-nil, then all symbols in
    the package should be documented, instead of just exported symbols.
-   The `path` argument gives the directory where the files should be written.
    This directory will be created if it does not exist.
-   A relative `path` is resolved relative to the location of the ASDF file
    defining the given `system`.
-   When this function is called by another function in this package, `index-acc`
    will be a hash-table used to accumulate symbol references for the index that
    page will create.  When this function is called as an API from outside of this
    system, or if no index will be needed, then the argument should be left `nil`.
-   The `show-package`, `hoist-exported`, and `page-title` keyword arguments
    direct the formatting of the index page which this routine should create when
    it is the entry call to this package.
    -   If `show-package` is non-nil, then the symbol's package name will be
        dsplayed on any index page.
    -   If `hoist-exported` in non-nil, then the list of symbols will be divided
        according to the package which exports each symbol, or by internal symbols
        of all packages.
    -   If non-nil, `page-title` should be a string to be used as the page name.

### Function `write-packages`

Document several packages by making a call to `write-package-files` for each.

    (write-packages PACKAGES
                    [ :default-all FLAG ]
                    [ :default-system ASDF-SYSTEM-NAME ]
                    [ :default-path DIR ]
                    [ :package-extension FLAG ]
                    [ :extension-downcase FLAG ]
                    [ :index FLAG ]
                    [ :index-system ASDF-SYSTEM-NAME ]
                    [ :index-acc HASH-TABLE ]
                    [ :show-package FLAG ]
                    [ :hoist-exported page-title FLAG ]
                    [ :show-title FLAG ]
                    [ :package-headers FLAG ]
                    [ :usage-headers FLAG ] )

-   The `packages` argument is a list giving a specification of the packages to be
    documented.  Each element can be either a package designator or a list whose
    first element is a package designator and other elements are keyword arguments
    accepted by `write-package-files`.  These keywords will be used for the call
    to `write-package-files` for that package.
-   The `default-all`, `default-system`, and `default-path` arguments give the
    default arguments for the calls to `write-package-files`.
-   If `package-extension` is non-nil (its default is `t`), then whenever a
    package spec does not give an explicit path, it should use a subdirectory of
    the default path whose name is taken from the package.  If
    `extension-downcase` is non-nil (its default is `t`), then the package name
    is converted to lower-case for this extension.
-   The `index-acc` is a hash-table used to accumulate symbol references for an
    index page, or `nil` if no index data should be saved.
-   The `show-package`, `hoist-exported`, and `page-title` keyword arguments
    direct the formatting of the index page which this routine should create when
    it is the entry call to this package.
    -   If `show-package` is non-nil, then the symbol's package name will be
        dsplayed on any index page.
    -   If `hoist-exported` in non-nil, then the list of symbols will be divided
        according to the package which exports each symbol, or by internal symbols
        of all packages.
    -   If non-nil, `page-title` should be a string to be used as the page name.

### Function `write-symbol-files`

Writes Org-mode files (in the directory named by `directory-path`) documenting the uses of the given `symbol`.

    (write-symbol-files SYMBOL DIRECTORY-PATH
      [ :index-acc HASH ] [ :always-disambiguate FLAG ] )

-   The `index-acc` is a hash-table used to accumulate symbol references for an index page, or `nil` if no index data should be saved.
-   This function will write a separate file for each *use* of the symbol, disambiguating the file name where necessary with `__fn`, `__var` and so forth.  If `always-disambiguate` is non-nil, then these suffixes will *always* be added to the names of the generated files, even when a symbol has only one usage.

## Nonstandard documentation targets

### Function `documentation*`

Sometimes you may want to maintain documentation attached to
non-standard targets in the source code, and pull those strings into
the manual (for example, command options or available quoted forms).
Some Lisp implementations make it easy to use non-standard targets to
`documentation`; others essentially forbid it.  The `documentation*`
and `(setf documentation*)` functions manage the nonstandard documentation
types on platforms which do not allow it.

    (documentation* X DOC-TYPE)
    (setf (documentation* X DOC-TYPE) DOC-STRING)

At this time, on Allegro Lisp these functions are inlined wrappers for the
standard `documentation` functions, and on other platforms it filters out the
non-standard types and stores them in the `+nonstandard-doc-type+` hash.

Note that when using `(setf documentation*)` to install documentation strings
of non-standard doc-type it is necessary to load the `org-sampler` ASDF system
as a prerequisite of the documented system.  This is in contrast to the use
case of merely assembling documentation separately from loading the system,
in which case there is no reason to `:depends-on` the Org-Sampler system.

## Global switches

### Variable `*section-level*`

If non-nil, then generated Org mode with begin with the indicated level of section header giving the name and use of the definition. If `nil`, no section header is generated.

### Variable `*show-package-header*`

Whether a header line for the package should be written.

### Variable `*show-title*`

Whether an initial comment with the title should be written.

### Variable `*show-usage-header*`

Whether a header line for the usage should be written.

### Variable `*generate-html*`

If non-nil, then an HTML file should be generated from each Org file.

## Self-documentation

### Function `self-document`

Applies `Org-Sampler` to itself in its own directory.
