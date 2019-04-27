---
layout: page
title: Plot GPR data
date: 2019-02-21
---

<!--
"/media/huber/Elements/UNIBAS/software/codeR/package_RGPR/RGPR-gh-pages/2014_04_25_frenke"
"G:/UNIBAS/software/codeR/package_RGPR/RGPR-gh-pages/2014_04_25_frenke"
-->

------------------------------------------------------------------------

**Note**:

-   This R-package is still in development, and therefore some of the functions may change in a near future.
-   If you have any questions, comments or suggestions, feel free to contact me (in english, french or german): <emanuel.huber@alumni.ethz.ch>.

Objectives of this tutorial
===========================

-   Learn how to plot GPR data.

Install/load `RGPR`
-------------------

``` r
# install "devtools" if not already done
if(!require("devtools")) install.packages("devtools")
devtools::install_github("emanuelhuber/RGPR")
library(RGPR)       # load RGPR in the current R session
```

The GPR data
------------

`RPGR` comes along with a GPR data called `frenkeLine00`. Because this name is long, we set `A` equal to `frenkeLine00`:

``` r
A <- frenkeLine00
plot(A)
```

![plot(x)](01_RGPR_tutorial_plot-GPR-data_tp_files/figure-markdown_github-tex_math_single_backslash/x-1.png)

Plot the GPR data
=================

2D plot: radargramm
-------------------

To plot the GPR record as a raster image (default mode), enter

``` r
plot(A)
```

![plot(A)](01_RGPR_tutorial_plot-GPR-data_tp_files/figure-markdown_github-tex_math_single_backslash/plot-1.png)

The green line indicates the position of time-zero. The yellow triangle indicates the position of a fiducial marker that was set during the survey to mark something (such as a specific object close to the GPR line, a change in morphology/topography/sedimentology or an intersection with another GPR line). These markers are very useful to add topographic data to the GPR profile, particularly when the fiducial markers correspond to the locations where the (x,y,z) coordinates were measured.

Plot wiggles

``` r
plot(A, type = "wiggles")
```

![plot(A) with wiggles](01_RGPR_tutorial_plot-GPR-data_tp_files/figure-markdown_github-tex_math_single_backslash/plot_wiggles-1.png)

To plot only a part of the GPR data, use `xlim` and `ylim`.

``` r
plot(A, ylim = c(50, 100), xlim = c(30, 40))
```

![plot(A) with xlim and ylim](01_RGPR_tutorial_plot-GPR-data_tp_files/figure-markdown_github-tex_math_single_backslash/plot_xlim_ylim-1.png)

To set the origin of the vertical axis at time-zero, set the argument `relTime0` equal to `TRUE`.

``` r
plot(A, relTime0 = TRUE, ylim = c(0, 200), xlim = c(30, 50))
```

![plot(A) relative to time0](01_RGPR_tutorial_plot-GPR-data_tp_files/figure-markdown_github-tex_math_single_backslash/relTime0-1.png)

Another way to plot only a part of the GPR data is to extract a part of the GPR data. The object `A` can be manipulated in the same way as a matrix without losing the meta-data (e.g., trace coordinates, antenna separation).

To extract the samples 100 to 300 of the $15^{th}$ to $150^{th}$:

``` r
# extract the 100 to 300 samples of the traces 15 to 150
A0 <- A[100:300, 15:150]
A
```

    ## *** Class GPR ***
    ##  name        = LINE00
    ##  filepath    = data-raw/LINE00.DT1
    ##  1 fiducial(s)
    ##  description =
    ##  survey date = 2014-04-25
    ##  Reflection, 100 MHz, Window length = 399.6 ns, dz = 0.4 ns
    ##  223 traces, 55.5 m
    ##  ****************

``` r
A0
```

    ## *** Class GPR ***
    ##  name        = LINE00
    ##  filepath    = data-raw/LINE00.DT1
    ##  description =
    ##  survey date = 2014-04-25
    ##  Reflection, 100 MHz, Window length = 80 ns, dz = 0.4 ns
    ##  136 traces, 33.75 m
    ##  ****************

Check the depth/time and positions values

``` r
depth(A)
pos(A)
```

Plot a section/subset of the GPR record (like zooming)

``` r
# plot the 100 to 300 samples of the traces 15 to 150
plot(A[100:300, 15:150])
```

![plot(A) subset](01_RGPR_tutorial_plot-GPR-data_tp_files/figure-markdown_github-tex_math_single_backslash/plot_subset-1.png)

1D plot: trace plot
-------------------

Plot a signal trace, notice that the signal is clipped to $\pm50\,mV$ (between $0$ and $20\,ns$):

``` r
plot(A[, 15])      # plot the 15th trace of the GPR-line
```

![plot single trace](01_RGPR_tutorial_plot-GPR-data_tp_files/figure-markdown_github-tex_math_single_backslash/plot1D-1.png)

Note: the `@3.5m` in the plot title indicate the relative position of the trace on the GPR profile.

Plot the first 40 trace samples:

``` r
# plot the first 40 samples of the 15th trace of the GPR profile
plot(A[1:40, 15])
```

![plot single trace, fist 40 samples](01_RGPR_tutorial_plot-GPR-data_tp_files/figure-markdown_github-tex_math_single_backslash/plot1D_subset-1.png)

More infos
----------

Check the help for more details on the `plot()` function:

``` r
?plot.GPR
```
