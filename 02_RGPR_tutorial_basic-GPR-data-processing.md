---
layout: page
title: Basic GPR data processing
date: 2020-06-04
---

<!--

-->

------------------------------------------------------------------------

**Note**:

-   This R-package is still in development, and therefore some of the functions may change in a near future.
-   If you have any questions, comments or suggestions, feel free to contact me (in english, french or german): <emanuel.huber@pm.me>.

Table of Contents
=================

-   [Objectives of this tutorial](#objectives-of-this-tutorial)
-   [Preliminary](#preliminary)
    -   [File organisation](#file-organisation)
    -   [Install/load `RGPR` and set the working directory](#installload-rgpr-and-set-the-working-directory)
    -   [Getting help](#getting-help)
-   [Read GPR data](#read-gpr-data)
-   [Basic processing steps](#basic-processing-steps)
    -   [First wave break and time zero estimation](#first-wave-break-and-time-zero-estimation)
    -   [DC shift removal](#dc-shift-removal)
    -   [Time zero correction](#time-zero-correction)
    -   [Dewow](#dewow)
    -   [Frequency filter](#frequency-filter)
    -   [Amplitude gain](#amplitude-gain)
        -   [Power gain](#power-gain)
        -   [Exponential gain](#exponential-gain)
    -   [inverse normal transformations](#inverse-normal-transformations)
    -   [Median filter (spatial filter)](#median-filter-spatial-filter)
    -   [Frequency-wavenumber filter (f-k-filter)](#frequency-wavenumber-filter-f-k-filter)
    -   [Processing overview](#processing-overview)
    -   [Other processing functions](#other-processing-functions)
        -   [Trace average removal](#trace-average-removal)
        -   [Eigen Image Filter](#eigen-image-filter)
        -   [Background matrix substraction](#background-matrix-substraction)
    -   [Save and export](#save-and-export)
    -   [Read the saved GPR data](#read-the-saved-gpr-data)
-   [Some final thoughts](#some-final-thoughts)

Objectives of this tutorial
===========================

-   Learn some basics of ground-penetrating radar data processing with `RGPR`.
-   Learn how to manipulate objects of the class `RGPR`.

Note that his tutorial will not explain you the math/algorithms behind the different processing methods.

Preliminary
===========

-   Download the data [2014\_04\_25\_frenke.zip](http://emanuelhuber.github.io/RGPR/2014_04_25_frenke.zip)
-   Unzip the data

File organisation
-----------------

I suggest to organise your files and directories as follows:

    /2014_04_25_frenke   (project directory with date and location)
        /processing      (here you will save the processed GPR files)
        /rawGPR          (the raw GPR data, never modify them!)
        RGPR_tutorial.R  (this is you R script for this tutorial)

Install/load `RGPR` and set the working directory
-------------------------------------------------

-   Install and load the `RGPR`-package

    ``` r
    # install "devtools" if not already done
    if(!require("devtools")) install.packages("devtools")
    devtools::install_github("emanuelhuber/RGPR")
    library(RGPR)       # load RGPR in the current R session
    ```

-   Set the working directory:

    ``` r
    myDir <- "~/2014_04_25_frenke"  # adapt that to your directory structure
    setwd(myDir)    # set the working directory
    getwd()         # Return the current working directory (just to check)
    ```

Getting help
------------

If you need help about a specific function, check the documentation with either `help("FUNCTION_NAME")` of `?FUNCTION_NAME`, example:

``` r
?traceStat
```

Read GPR data
=============

The raw GPR data are located in the directory `/rawGPR`. The data format is the Sensors & Software format. Each GPR data consists of

-   a header file (extension `.hd`) that can be opened with a normal text editor
-   a binary data file (extension `.DT1`).

To read the GPR data, enter

``` r
x <- readGPR(dsn = "rawGPR/LINE00.DT1")   # the filepath is case sensitive!
class(x)
```

    ## [1] "GPR"
    ## attr(,"package")
    ## [1] "RGPR"

Basic processing steps
======================

First wave break and time zero estimation
-----------------------------------------

Here, we define time-zero, $t_0$ as the time at which the transmitter starts to emit the wave.

Maybe is time zero not correctly set. To get the time-zero for each traces of `x` use the function `time0()`:

``` r
time0(x)
```

The first wave break, $t_{\mathrm{fb}}$, is estimated for each traces (it is the time of the first wave record) with `firstBreak()`:

``` r
tfb <- firstBreak(x, w = 20, method = "coppens", thr = 0.05)
plot(pos(x), tfb, pch = 20, ylab = "first wave break",
     xlab = "position (m)")
```

![plot first wave break time](02_RGPR_tutorial_basic-GPR-data-processing_tp_files/figure-markdown_github/first_wave_break-1.png)

Convert the first wave break time $t_{\mathrm{fb}}$ into time-zero $t_0$ with `firstBreakToTime0()`.

Here we define

$$
t_0 = t_{\mathrm{fb}} - a/c_0
$$

where $a$ is the distance between the transmitter and receiver and $c_0$ is the wave velocity in the media between the transmitter and receiver (in our case, air). The value $a/c_0$ corresponds to the wave travel time from the transmitter to the receiver.

``` r
t0 <- firstBreakToTime0(tfb, x)
time0(x) <- t0     # set time0
```

Note that:

-   if `t0` is too noisy, you can set `time0(x) <- mean(t0)`.
-   you can use `x <- setTime0(x, t0)` instead of `time0(x) <- t0` (both are the same)

Check the results (do you see the difference between time zero in red and first wave break time in blue?):

``` r
plot(x[, 15], xlim = c(0, 100))  # plot the 15th trace of the GPR-line
abline(v = tfb[15], col = "blue")  # first wave break time
```

![plot single trace with time0 and first wave break time](02_RGPR_tutorial_basic-GPR-data-processing_tp_files/figure-markdown_github/time0_check-1.png)

You can apply at once all the previous steps (first wave break estimation + set time-zero) with the function `estimateTime0()` (which has the same arguments as the functions `firstBreak()`, `firstBreakToTime0()` plus an extra argument - `FUN` - for function to apply to the estimated time-zero, e.g. `mean()`; see the documentation), i.e.:

``` r
x <- estimateTime0(x, w = 20, method = "coppens", thr = 0.05, FUN = mean)
```

DC shift removal
----------------

Plot a single trace:

``` r
plot(x[, 15])  # plot the 15th trace of the GPR-line
```

![plot single trace](02_RGPR_tutorial_basic-GPR-data-processing_tp_files/figure-markdown_github/dcShift_plot1D_15-1.png)

Notice how the trace samples before the first wave arrival (before $t = 0\,ns$) are slightly shifted below $0\,mV$? This shift is called direct current offset (DC-offset) and you will remove it from the data. The direct current offset is estimated on trace samples before time-zero.

1.  Determine which samples will be used to estimate the direct current offset (i.e., the samples before the first wave arrival). Identify the samples before $t = 0\,ns$     by ploting the first $n$     samples of the traces. For example, for $n = 110$:

    ``` r
    # plot the first 110 samples of the 15th trace of the GPR profile
    plot(x[1:110, 15])
    ```

![plot single trace, first 110 samples](02_RGPR_tutorial_basic-GPR-data-processing_tp_files/figure-markdown_github/dcShift_plot_first_samples-1.png)

You can visualise the DC-offset on the trace plot by adding an horizontal lines (`abline(h=...)`) with the argument `h` equal the DC-offset, i.e., the mean of the first $110$ samples (`mean(x[1:110,15]`):

    ```r
    plot(x[, 15])  # plot the 15th trace of the GPR-line
    # add a green horizontal line
    abline(h = mean(x[1:110, 15]), col = "green")
    ```

![plot single trace + dc-shift](02_RGPR_tutorial_basic-GPR-data-processing_tp_files/figure-markdown_github/dcShift_check_results_1D-1.png)

1.  Remove the DC-offset estimated on the first n samples usind the function `dcshift()`. This function takes as argument the `GPR` object and the sample index used to estimate the DC shift (in this case, the first $110$     samples):

    ``` r
    x1 <- dcshift(x, u = 1:110)   # new object x1
    ```

Have a look at x1:

``` r
x1
```

    ## *** Class GPR ***
    ##  name        = LINE00
    ##  filepath    = rawGPR/LINE00.DT1
    ##  1 fiducial(s)
    ##  description =
    ##  survey date = 2014-04-25
    ##  Reflection, 100 MHz, Window length = 399.6 ns, dz = 0.4 ns
    ##  223 traces, 55.5 m
    ##  > PROCESSING
    ##    1. time0<-
    ##    2. dcshift//u=1:110
    ##  ****************

Compared with `x` or `print(x)`, three additional lines are displayed. The two last line show the applied processing step:

-   set time zero with `time0<-`
-   time zero correction with `dcshift`. The arguments passed to the function are listed after the double slashes `//`

Each time a GPR object is processed with a function, the name of the function as well as some of its arguments are stored in the GPR object. This enables to track the data processing, i.e., to know exactly which processing steps where applied to the data. This is a first step toward reproducible research.

The processing steps can be extracted with the function `processing()`:

``` r
proc(x1)
```

    ## [1] "time0<-"          "dcshift//u=1:110"

Time zero correction
--------------------

To shift the traces to time-zero, use the function `time0Cor` (the `method` argument defines the type of interpolation method)

``` r
x2 <- time0Cor(x1, method = "pchip")
```

``` r
plot(x2)
```

![plot after time0Cor()](02_RGPR_tutorial_basic-GPR-data-processing_tp_files/figure-markdown_github/time0Cor_check-1.png)

Dewow
-----

Remove the low-frequency components (the so-called "wow") of the GPR record using:

1.  a running median filter (`type = "runnmed"`)
2.  a running mean filter (`type = "runmean"`)
3.  a Gaussian filter (`type = "Gaussian"`)

For the two first cases, the argument `w` is the length of the filter in time units. For the Gaussian filter, `w` is the standard deviation.

``` r
x3 <- dewow(x2, type = "runmed", w = 50)     # dewowing:
plot(x3)                                     # plot the result
```

![plot after dewow](02_RGPR_tutorial_basic-GPR-data-processing_tp_files/figure-markdown_github/dewow-1.png)

Can you see the difference with `x1`? Plot `x3 - x2` to see the removed "wow".

``` r
plot(x3 - x2)                           # plot the difference
```

![plot difference after dewow](02_RGPR_tutorial_basic-GPR-data-processing_tp_files/figure-markdown_github/dewow_diff-1.png)

See the dewowing by comparing the traces before (blue line) and after (red line):

``` r
plot(x2[,15], col = "blue")      # before dewowing
lines(x3[,15], col = "red")      # after dewowing
```

![plot single trace dewow](02_RGPR_tutorial_basic-GPR-data-processing_tp_files/figure-markdown_github/dewow_check_1D-1.png)

Frequency filter
----------------

Let's have a look at the amplitude-frequency and phase-frequency plot (the spectrum given by the Fourier decomposition):

``` r
spec1D(x3)
```

![plot spectrum](02_RGPR_tutorial_basic-GPR-data-processing_tp_files/figure-markdown_github/fFilter_spectrum-1.png)

    ## *** Class GPRset ***
    ##  name        = LINE00
    ##  n sets    = 2
    ##  filepath    = rawGPR/LINE00.DT1
    ##  description =
    ##  survey date = 2014-04-25
    ##  Reflection, 100 MHz, Window length = 177.6 MHz, dz = 0.4 MHz
    ##  223 traces, 55.5 m
    ##  > PROCESSING
    ##    1. time0<-
    ##    2. dcshift//u=1:110
    ##    3. time0Cor//method=pchip
    ##    4. dewow//type=runmed+w=50
    ##    5. spec1D//
    ##  ****************

The curve in red is the averaged amplitude/phase over all the trace amplitudes/phases.

On the first plot, notice

-   a sharp increase of the amplitude between $0\,\mathit{MHz}$     and $10\,\mathit{MHz}$: these frequency correspond to the slowing-varying part of the signal. In this particular case, filtering out this part of the signal results in strong signal distorsion.
-   after a peak at $80\,\mathit{MHz}$     (the returned signal frequency that is lower than the antenna frequency because of frequency-dependent attenuation in the ground), the amplitude decreases.
-   at about $200\,\mathit{MHz}$     the amplitude stays constant (plateau): noise frequency.

Eliminate the high-frequency (noise) component of the GPR record with a bandpass filter. We define as corner frequencies at $150\,\mathit{MHz}$ and $260\,\mathit{MHz}$, and set `plotSpec = TRUE` to plot the spectrum with the signal, the filtered signal and the filter.

``` r
x4 <- fFilter(x3, f = c(150, 260), type = "low", plotSpec = TRUE)
```

![plot frequency filter](02_RGPR_tutorial_basic-GPR-data-processing_tp_files/figure-markdown_github/fFilter_fFilter-1.png)

``` r
plot(x4)
```

![plot frequency filter](02_RGPR_tutorial_basic-GPR-data-processing_tp_files/figure-markdown_github/fFilter_fFilter-2.png)

Let see the difference

``` r
plot(x4 - x3, clip = 50)
```

![plot difference](02_RGPR_tutorial_basic-GPR-data-processing_tp_files/figure-markdown_github/fFilter_diff-1.png)

Ideally, the objective of processing is to remove the noise component without altering the signal component to improve the signal/noise ratio. When ploting the difference in processing (after - before), one should only observe the noise that is filtered out. Here, removing and attenuating some frequencies change the signal amplitude.

Amplitude gain
--------------

Apply a gain to compensate the signal attenuation. Three types of gain are available:

-   power gain (`type = "power"`):

    $$
    x_g(t) = x(t)\cdot t^\alpha
    $$

    with $\alpha = 1$     per default.

-   exponential gain (`type = "exp"`):

    $$
    x_g(t) = x(t)\cdot\exp(\alpha\cdot t)
    $$

-   Automatic gain control (`type = "agc"`): make gain equal to the local root mean squared signal.

We will first apply a power gain and then an exponential gain. To visualise the amplitude envelope of the GPR signal as a function of time, use the function `plotAmpl()` as follows:

``` r
# compute the trace amplitude envelopes (with Hilbert transform)
x4_env <- envelope(x4)
# plot all the trace amplitude envelopes as a function of time
trPlot(x4_env, log = "y", col = rgb(0.2,0.2,0.2,7/100))
# plot the log average amplitude envelope
lines(traceStat(x4_env), log = "y", col = "red", lwd = 2)
```

![plot amplitude](02_RGPR_tutorial_basic-GPR-data-processing_tp_files/figure-markdown_github/plotAmpl-1.png)

On the plot above, there is a sharp increase of the amplitude envelope at the begining of the signal. This sharp increase corresponds to the first wave arrival. Then the amplitude decreases until about $220\,\mathit{ns}$.

### Power gain

From $0\,\mathit{ns}$ to $20\,\mathit{ns}$ the power gain is set equal to the gain at $20\,\mathit{ns}$, i.e., $x_g(20)$ (constant value, `tcst = 20`). The gain is only applied up to $220\,\mathit{ns}$.

``` r
x5 <- gain(x4, type = "power", alpha = 1, te = 220, tcst = 20)
```

Compare the amplitude before (red) and after (green) the power gain:

``` r
plot(traceStat(x4_env), log = "y", col = "red", lwd = 2)
lines(traceStat(envelope(x5)), log = "y", col = "green", lwd = 2)
```

![plot amplitude after power gain](02_RGPR_tutorial_basic-GPR-data-processing_tp_files/figure-markdown_github/ampl_check-1.png)

``` r
plot(x5)      # how does it look after the gain?
```

![plot amplitude after power gain](02_RGPR_tutorial_basic-GPR-data-processing_tp_files/figure-markdown_github/ampl_check-2.png)

### Exponential gain

Ideally, the parameter $\alpha$ in the exponential gain should be close to the slope of the log amplitude decrease. This slope could be estimated by fitting a straight line to the amplitude decrease. After some trials, we apply the exponential gain only between $0\,\mathit{ns}$ (`t0`) and $125\,\mathit{ns}$ (`te` for $t_\mathit{end}$):

``` r
x6 <- gain(x5, type ="exp",  alpha = 0.2, t0 = 0, te = 125)
plot(traceStat(envelope(x6)), log = "y", col = "blue", lwd = 2)
```

![plot ampliude exponential gain](02_RGPR_tutorial_basic-GPR-data-processing_tp_files/figure-markdown_github/gain_exp-1.png)

Oops! Set `alpha` to a smaller value!

``` r
x6 <- gain(x5, type = "exp", alpha = 0.11, t0 = 0, te = 125)
plot(traceStat(x4_env), log = "y", col = "red", lwd = 2)
lines(traceStat(envelope(x5)), log = "y", col = "green", lwd = 2)
lines(traceStat(envelope(x6)), log = "y", col = "blue", lwd = 2)
```

![plot amplitude after exponential gain](02_RGPR_tutorial_basic-GPR-data-processing_tp_files/figure-markdown_github/gain_check2-1.png)

``` r
plot(x6)    # how does it look after the gain?
```

![plot amplitude after exponential gain](02_RGPR_tutorial_basic-GPR-data-processing_tp_files/figure-markdown_github/gain_check2-2.png)

inverse normal transformations
------------------------------

Have a look at the histogram of the values of `x6`

``` r
hist(x6[], breaks = 50)
```

![plot hist](02_RGPR_tutorial_basic-GPR-data-processing_tp_files/figure-markdown_github/invnorm-1.png)

This histogram is very narrow, meaning that a lot of values are very close to zero and therefore many details are not really visible. To widen this histogram, we can transform it to make it more normally distributed with a rank-based inverse normal transformation:

``` r
x7 <- traceScaling(x6, type = "invNormal")
```

Histograms before and after

``` r
par(mfrow = c(1, 2))
hist(x6[], breaks = 50)
hist(x7[], breaks = 50)
```

![plot histogram comparison](02_RGPR_tutorial_basic-GPR-data-processing_tp_files/figure-markdown_github/hist_diff-1.png) Have a look at the results of the transformation:

``` r
plot(x7)
```

![plot inverse normal transformation results](02_RGPR_tutorial_basic-GPR-data-processing_tp_files/figure-markdown_github/invnorm_check-1.png)

Median filter (spatial filter)
------------------------------

A non-linear filter to remove noise:

``` r
x8 <- filter2D(x7, type = "median3x3")
plot(x8)
```

![plot median filter](02_RGPR_tutorial_basic-GPR-data-processing_tp_files/figure-markdown_github/median_filter-1.png)

Let see the difference

``` r
plot(x8 - x7)
```

![plot difference after median filter](02_RGPR_tutorial_basic-GPR-data-processing_tp_files/figure-markdown_github/median_filter_diff-1.png)

Frequency-wavenumber filter (f-k-filter)
----------------------------------------

The function `spec()` with the argument `type = "f-k` returns a list containing the frequencies (f), the wavenumbers (k), the amplitude of the GPR data.

``` r
FKSpec <- spec(x8, type = "f-k")
```

    ## Soon deprecated. Use 'spec1D()' or 'spec2D()' instead

``` r
area <- list(x = c(0, min(FKSpec$wnb), min(FKSpec$wnb), max(FKSpec$wnb), max(FKSpec$wnb), 0),
             y = c(max(FKSpec$fre), 800, 0, 0, 800, max(FKSpec$fre)))
lines(area, type="o")
```

![plot fk-filter](02_RGPR_tutorial_basic-GPR-data-processing_tp_files/figure-markdown_github/fkspec-1.png)

``` r
x9 <- fkFilter(x8, fk = area)
```

With the f-k-filter you can successfully remove the artifacts but still the information gained is very small in this case (the quality of the raw GPR data is already bad):

``` r
plot(x9, clip = 50)
```

![plot fk-spectrum](02_RGPR_tutorial_basic-GPR-data-processing_tp_files/figure-markdown_github/fk_plot-1.png)

``` r
spec(x9, type = "f-k")
```

    ## Soon deprecated. Use 'spec1D()' or 'spec2D()' instead

![plot fk-spectrum](02_RGPR_tutorial_basic-GPR-data-processing_tp_files/figure-markdown_github/fk_plot-2.png)

Let see the difference

``` r
plot(x9 - x8)
```

![plot difference after fk-filter](02_RGPR_tutorial_basic-GPR-data-processing_tp_files/figure-markdown_github/fK_diff-1.png)

Processing overview
-------------------

Let review the processing step applied on the GPR record:

``` r
proc(x9)
```

Other processing functions
--------------------------

### Trace average removal

Check the help page for the function `traceStat`

``` r
?traceStat
```

``` r
x10 <- traceStat(x9, FUN = mean)  # compute average trace of all traces
x10 <- traceStat(x9, FUN = median)  # compute average trace of all traces
x10 <- traceStat(x9, FUN = median)  # compute average trace of all traces
# compute windowed average trace (average of 20 traces)
x10 <- traceStat(x9, w = 20, FUN = median)

plot(x10)
```

### Eigen Image Filter

``` r
?eigenFilter

# remove first eigenimage = keep all except the first one
plot(eigenFilter(x9, eigenvalue = c(2:ncol(x1))))
plot(eigenFilter(x9, eigenvalue = 1)) # the removed eigenvalue

# remove the first two eigenimages
plot(eigenFilter(x9, eigenvalue = c(3:ncol(x1))))
plot(eigenFilter(x9, eigenvalue = 1:2)) # the removed eigenvalue
```

### Background matrix substraction

See See Rashed and Harbi (2014) Background matrix subtraction (BMS): A novel background removal algorithm for GPR data doi: [10.1016/j.jappgeo.2014.04.022](https://doi.org/10.1016/j.jappgeo.2014.04.022)

It is a **slow** function!

``` r
?backgroundSub

x10 <- backgroundSub(x9, width = 21, trim = 0.2, s = 1, eps = 1, itmax = 5)
plot(x10)
plot(x10 - x9)
```

Save and export
---------------

Save the processed GPR record into the directory /processing. Use the `.rds` format (this is a R internal format)

``` r
writeGPR(x9, fPath = file.path(getwd(), "processing", name(x9)),
         type = "rds", overwrite = TRUE)
```

    ## File overwritten

    ## *** Class GPR ***
    ##  name        = LINE00
    ##  filepath    = /mnt/data/RGPR/CODE/RGPR-gh-pages/2014_04_25_frenke/processing/LINE00.rds
    ##  1 fiducial(s)
    ##  description =
    ##  survey date = 2014-04-25
    ##  Reflection, 100 MHz, Window length = 354.8 ns, dz = 0.4 ns
    ##  223 traces, 55.5 m
    ##  > PROCESSING
    ##    1. time0<-
    ##    2. dcshift//u=1:110
    ##    3. time0Cor//method=pchip
    ##    4. dewow//type=runmed+w=50
    ##    5. fFilter//f=150,260+type=low+plotSpec=TRUE
    ##    6. gain//type=power+alpha=1+te=220+tcst=20
    ##    7. gain//type=exp+alpha=0.11+t0=0+te=125
    ##    8. traceScaling//type=invNormal
    ##    9. filter2D//type=median3x3
    ##    10. fkFilter//fk=x<-c(0, -2, -2, 2, 2, 0),y<-c(1250, 800, 0, 0, 800, 1250)
    ##  ****************

Export a high quality PDF:

``` r
plot(x9, type = "wiggles", clip = 30, pdfName = file.path(getwd(), "processing", name(x9)),
          lwd = 0.5, wsize = 2.5)
```

Read the saved GPR data
-----------------------

``` r
procA <- readGPR(fPath = file.path(getwd(), "processing", paste0(name(x9), ".rds")))
```

Some final thoughts
===================

Warning: processing can introduce artifacts in the data and lead to wrong interpretations.

------------------------------------------------------------------------

> What really matters is that the final interpretation is valid, and although processing is important, ultimately, the key to good data interpretation is good data collection in the first place.
> *in Cassidy (2009) Chapter 5 - Ground Penetrating Radar Data Processing, Modelling and Analysis, In Ground Penetrating Radar Theory and Applications, (Eds Harry M. Jol,), Elsevier, Amsterdam, pp: 141-176, ISBN 9780444533487*.

------------------------------------------------------------------------

> A good practical mantra for most users to adopt is if it cannot be seen in the raw data – is it really there? As such, processing steps should be used to improve the raw-data quality, therefore, making interpretation easier. In practice, this means increasing the signal-to-noise ratio of coherent responses and presenting the data in a format that reflects the subsurface conditions accurately.
> *in Cassidy (2009) Chapter 5 - Ground Penetrating Radar Data Processing, Modelling and Analysis, In Ground Penetrating Radar Theory and Applications, (Eds Harry M. Jol,), Elsevier, Amsterdam, pp: 141-176, ISBN 9780444533487*.

------------------------------------------------------------------------

> Processing of GPR data tends to improve the appearance of data, but rarely does processing substantially change the interpretation.
> *in Daniels et al. (1997) Coincident Antenna Three-Dimensional GPR. Journal of Environmental and Engineering Geophysics, Vol. 2, No.1, pp. 1–9*.
