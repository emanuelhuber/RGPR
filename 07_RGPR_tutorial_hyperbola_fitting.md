---
layout: page
title: Hyperbola fitting date: 2019-10-18
---

<!--
"/media/huber/Elements/UNIBAS/software/codeR/package_RGPR/RGPR-gh-pages/2014_04_25_frenke"
"G:/UNIBAS/software/codeR/package_RGPR/RGPR-gh-pages/2014_04_25_frenke"
-->

------------------------------------------------------------------------

**Note**:

-   This R-package is still in development, and therefore some of the functions may change in a near future.
-   If you have any questions, comments or suggestions, feel free to contact me (in english, french or german): <emanuel.huber@alumni.ethz.ch>.

Table of Contents
=================

-   [Objectives of this tutorial](#objectives-of-this-tutorial)
-   [Preliminary](#preliminary)
    -   [Install/load `RGPR`](#installload-rgpr)
    -   [The GPR data](#the-gpr-data)
    -   [Basic processing](#basic-processing)
-   [Hyperbola fitting](#hyperbola-fitting)
    -   [Interactive point selection and hyperbola fitting](#interactive-point-selection-and-hyperbola-fitting)

Objectives of this tutorial
===========================

-   Learn how to fit manually a hyperbola to GPR data

Note that his tutorial will not explain you the math/algorithms behind the different processing methods.

Preliminary
===========

-   Read the tutorial [Basic GPR data processing](http://emanuelhuber.github.io/RGPR/01_RGPR_tutorial_basic-processing/) to learn more about the processing methods

Install/load `RGPR`
-------------------

``` r
# install "devtools" if not already done
if(!require("devtools")) install.packages("devtools")
devtools::install_github("emanuelhuber/RGPR")
```

    ##
    ##
       checking for file ‘/tmp/Rtmp8JTyfh/remotes2856766962e7/emanuelhuber-RGPR-189073f/DESCRIPTION’...

    ✔  checking for file ‘/tmp/Rtmp8JTyfh/remotes2856766962e7/emanuelhuber-RGPR-189073f/DESCRIPTION’
    ##

    ─  preparing ‘RGPR’:
    ##

       checking DESCRIPTION meta-information...

    ✔  checking DESCRIPTION meta-information
    ##

    ─  checking for LF line-endings in source and make files and shell scripts
    ##

    ─  checking for empty or unneeded directories
    ##

    ─  looking to see if a ‘data/datalist’ file should be added
    ##

    ─  building ‘RGPR_0.0.6.tar.gz’
    ##


    ##

``` r
library(RGPR)       # load RGPR in the current R session
```

The GPR data
------------

`RPGR` comes along with a GPR data called `frenkeLine00`. Because this name is long, we set `x` equal to `frenkeLine00`:

``` r
x <- frenkeLine00
plot(x)
```

![plot(x)](07_RGPR_tutorial_hyperbola_fitting_tp_files/figure-markdown_github/x-1.png)

Basic processing
----------------

We apply some basic processing to the data with the pipe operator (`%>%`):

``` r
x <- x %>%  estimateTime0(w = 5, method = "MER", FUN = mean) %>%
     time0Cor(method = "pchip")  %>%
     dewow(type = "Gaussian", w = 5)  %>%
     fFilter(f = c(200, 300), type = "low") %>%
     gainSEC(a = 0.003, t0 = 50)

plot(x)
```

![proc](07_RGPR_tutorial_hyperbola_fitting_tp_files/figure-markdown_github/proc-1.png)

Hyperbola fitting
=================

Interactive point selection and hyperbola fitting
-------------------------------------------------

Select points interactively on the plot with

``` r
xy <- locator(type = "l")
```

Fit the corresponding hyperbola:

``` r
hyp <- hyperbolaFit(xy)
```

``` r
plot(x)
points(xy, pch = 20, col = "blue")
hyperbolaPlot(hyp, x = seq(5, 50, by = 0.01), col = "red", lwd = 2)
```

![plot1a](07_RGPR_tutorial_hyperbola_fitting_tp_files/figure-markdown_github/plot1a-1.png)

``` r
plot(x)
points(xy, pch = 20, col = "blue")
hyperbolaPlot(hyp, col = "red", lwd = 2)
```

![plot1b](07_RGPR_tutorial_hyperbola_fitting_tp_files/figure-markdown_github/plot1b-1.png)

``` r
plot(x)
points(xy, pch = 20, col = "blue")
hyperbolaPlot(hyp, col = "red", lwd = 2, ann = TRUE)
```

![plot1c](07_RGPR_tutorial_hyperbola_fitting_tp_files/figure-markdown_github/plot1c-1.png)
