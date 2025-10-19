# geomeTriD: A R/Bioconductor package for interactive 3D plot of epigenetic data

## Overview

The `geomeTriD` (Three-Dimensional Geometry) Package provides interactive 3D visualization of chromatin structures using the WebGL-based [three.js](https://threejs.org/) or the rgl rendering library. It is designed to identify and explore spatial chromatin patterns within genomic regions. The package generates dynamic 3D plots and HTML widgets that integrate seamlessly with Shiny applications, enabling researchers to visualize chromatin organization, detect spatial features, and compare structural dynamics across different conditions and data types.

`geomeTriD` is available on Bioconductor: [https://bioconductor.org/packages/geomeTriD/](https://bioconductor.org/packages/geomeTriD/)

## Installaton

To install `geomeTriD` package, start R (version "4.5") and enter:

```{r}
if (!require("BiocManager", quietly = TRUE))
    install.packages("BiocManager")

BiocManager::install("geomeTriD")
```

Development version are available by:

```{r}
BiocManager::install('jianhong/geomeTriD')
```

The dependency of this package is R (>= 4.5.0).

## Documentation

To view documentation of `geomeTriD`, start R and enter:

```{r}
browseVignettes('geomeTriD')
```

The documentation are also available online at [Bioconductor](https://bioconductor.org/packages/release/bioc/vignettes/geomeTriD/inst/doc/geomeTriD.html).

### Contributions and Support

If you would like to contribute to this package, the standard workflow
is as follows:

1.  Check that there isn’t already an issue about your idea in the
    [jianhong/geomeTriD/issues](https://github.com/jianhong/geomeTriD/issues)
    to avoid duplicating work. If there isn’t one already, please create
    one so that others know you’re working on this
2.  [Fork](https://help.github.com/en/github/getting-started-with-github/fork-a-repo)
    the [jianhong/geomeTriD](https://github.com/jianhong/geomeTriD)
    to your GitHub account
3.  Make the necessary changes / additions within your forked repository
    following [Bioconductor
    contribution](https://contributions.bioconductor.org/)
4.  Use `devtools::build` and `devtools::check` to check the package
    work properly.
5.  Submit a Pull Request against the `main` or current
    `RELEASE_VERSION` branch and wait for the code to be reviewed and
    merged.

If you’re not used to this workflow with git, you can start with some
[docs from
GitHub](https://help.github.com/en/github/collaborating-with-issues-and-pull-requests)
or even their [excellent `git` resources](https://try.github.io/).

For further information or help, don’t hesitate to get in touch on the
[Bioconductor support site](https://support.bioconductor.org/) with tag
`#geomeTriD` or create a new issues at 
[jianhong/geomeTriD/issues](https://github.com/jianhong/geomeTriD/issues).

### Reporting bug/issues

Many thanks for taking an interest in improving this package. Please
report bug/issues at
[jianhong/geomeTriD/issues](https://github.com/jianhong/geomeTriD/issues).

