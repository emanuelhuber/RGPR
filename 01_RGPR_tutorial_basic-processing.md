---
layout: page
title: Basic GPR data processing
date: 2018-06-29
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

Read GPR data
=============

The raw GPR data are located in the directory `/rawGPR`. The data format is the Sensors & Software format. Each GPR data consists of

-   a header file (extension `.hd`) that can be opened with a normal text editor
-   a binary data file (extension `.DT1`).

To read the GPR data, enter

``` r
A <- readGPR(fPath = "rawGPR/LINE00.DT1")   # the filepath is case sensitive!
class(A)
```

    ## [1] "GPR"
    ## attr(,"package")
    ## [1] "RGPR"

Plot the GPR data
=================

2D plot: radargramm
-------------------

To plot the GPR record as a raster image (default mode), enter

``` r
plot(A)                                 
```

![plot(A)](01_RGPR_tutorial_basic-processing_files/figure-markdown_github-tex_math_single_backslash/plot-1.png)

The green line indicates the position of time-zero. The yellow triangle indicates the position of a fiducial marker that was set during the survey to mark something (such as a specific object close to the GPR line, a change in morphology/topography/sedimentology or an intersection with another GPR line). These markers are very useful to add topographic data to the GPR profile, particularly when the fiducial markers correspond to the locations where the (x,y,z) coordinates were measured.

Plot wiggles

``` r
plot(A, type = "wiggles")          
```

![plot(A) with wiggles](01_RGPR_tutorial_basic-processing_files/figure-markdown_github-tex_math_single_backslash/plot_wiggles-1.png)

To plot only a part of the GPR data, use `xlim` and `ylim`.

``` r
plot(A, ylim = c(50, 100), xlim = c(30, 40))
```

![plot(A) with xlim and ylim](01_RGPR_tutorial_basic-processing_files/figure-markdown_github-tex_math_single_backslash/plot_xlim_ylim-1.png)

To set the origin of the vertical axis at time-zero, set the argument `relTime0` equal to `TRUE`.

``` r
plot(A, relTime0 = TRUE, ylim = c(0, 200), xlim = c(30, 50))
```

![plot(A) relative to time0](01_RGPR_tutorial_basic-processing_files/figure-markdown_github-tex_math_single_backslash/relTime0-1.png)

Another way to plot only a part of the GPR data is to extract a part of the GPR data. The object `A` can be manipulated in the same way as a matrix without losing the meta-data (e.g., trace coordinates, antenna separation).

To extract the samples 100 to 300 of the 15**<sup>*t**h*</sup> to 150**<sup>*t**h*</sup>:

``` r
# extract the 100 to 300 samples of the traces 15 to 150
A0 <- A[100:300, 15:150]  
A
```

    ## *** Class GPR ***
    ##  name        = LINE00
    ##  filepath    = rawGPR/LINE00.DT1
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
    ##  filepath    = rawGPR/LINE00.DT1
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

![plot(A) subset](01_RGPR_tutorial_basic-processing_files/figure-markdown_github-tex_math_single_backslash/plot_subset-1.png)

1D plot: trace plot
-------------------

Plot a signal trace, notice that the signal is clipped to ±50 *m**V* (between 0 and 20 *n**s*):

``` r
plot(A[, 15])      # plot the 15th trace of the GPR-line   
```

![plot single trace](01_RGPR_tutorial_basic-processing_files/figure-markdown_github-tex_math_single_backslash/plot1D-1.png)

Note: the `@3.5m` in the plot title indicate the relative position of the trace on the GPR profile.

Plot the first 40 trace samples:

``` r
# plot the first 40 samples of the 15th trace of the GPR profile
plot(A[1:40, 15])  
```

![plot single trace, fist 40 samples](01_RGPR_tutorial_basic-processing_files/figure-markdown_github-tex_math_single_backslash/plot1D_subset-1.png)

More infos
----------

Check the help for more details on the `plot()` function:

``` r
?plot.GPR
```

Basic processing steps
======================

DC shift removal
----------------

Plot a single trace:

``` r
plot(A[, 15])  # plot the 15th trace of the GPR-line
```

![plot single trace](01_RGPR_tutorial_basic-processing_files/figure-markdown_github-tex_math_single_backslash/dcShift_plot1D_15-1.png)

Notice how the trace samples before the first wave arrival (before *t* = 0 *n**s*) are slightly shifted below 0 *m**V*? This shift is called direct current offset (DC-offset) and you will remove it from the data. The direct current offset is estimated on trace samples before time-zero.

1.  Determine which samples will be used to estimate the direct current offset (i.e., the samples before the first wave arrival). Identify the samples before *t* = 0 *n**s* by ploting the first n samples of the traces. For example, for *n* = 110:

``` r
# plot the first 110 samples of the 15th trace of the GPR profile
plot(A[1:110, 15]) 
```

![plot single trace, first 110 samples](01_RGPR_tutorial_basic-processing_files/figure-markdown_github-tex_math_single_backslash/dcShift_plot_first_samples-1.png)

1.  Remove the DC-offset estimated on the first n samples usind the function `dcshift()`. This function takes as argument the `GPR` object and the sample index used to estimate the DC shift (in this case, the first 110 samples):

``` r
A1 <- dcshift(A, 1:110)   # new object A1 
```

You can visualise the DC-offset on the trace plot by adding an horizontal lines (`abline(h=...)`) with the argument `h` equal the DC-offset, i.e., the mean of the first 110 samples (`mean(A[1:110,15]`):

``` r
plot(A[, 15])  # plot the 15th trace of the GPR-line 
# add a green horizontal line
abline(h = mean(A[1:110, 15]), col = "green") 
```

![plot single trace + dc-shift](01_RGPR_tutorial_basic-processing_files/figure-markdown_github-tex_math_single_backslash/dcShift_check_results_1D-1.png)

Have a look at A1:

``` r
A1
```

    ## *** Class GPR ***
    ##  name        = LINE00
    ##  filepath    = rawGPR/LINE00.DT1
    ##  1 fiducial(s)
    ##  description = 
    ##  survey date = 2014-04-25
    ##  Reflection, 100 MHz, Window length = 399.6 ns, dz = 0.4 ns
    ##  223 traces, 55.5 m
    ##  ****************

Compared with `A` or `print(A)`, two additional lines are displayed. The second line shows the applied processing step, `dcshift`, with the arguments passed to the function. Each time a GPR object is processed with a function, the name of the function as well as some of its arguments are stored in the GPR object. This enables to track the data processing, i.e., to know exactly which processing steps where applied to the data. This is a first step toward reproducible research.

The processing steps can be extracted with the function `processing()`:

``` r
proc(A1)
```

    ## character(0)

First wave break estimation and time-zero correction
----------------------------------------------------

Here, we define time-zero, *t*<sub>0</sub> as the time at which the transmitter starts to emit the wave.

Maybe is time-zero not correctly set. To get the time-zero for each traces of `A1` use the function `time0()`:

``` r
time0(A1)
```

The first wave break, *t*<sub>*f**b*</sub>, is estimated for each traces (it is the time of the first wave record)

``` r
tfb <- firstBreak(A1)   # take some time
plot(pos(A1), tfb, pch = 20, ylab = "first wave break", 
     xlab = "position (m)")
```

![plot first wave break time](01_RGPR_tutorial_basic-processing_files/figure-markdown_github-tex_math_single_backslash/first_wave_break-1.png)

Convert the first wave break time *t*<sub>*f**b*</sub> into time-zero *t*<sub>0</sub> with `firstBreakToTime0()`.

Here we define *t*<sub>0</sub> = *t*<sub>*f**b*</sub> − *a*/*c*<sub>0</sub>, where *a* is the distance between the transmitter and receiver and *c*<sub>0</sub> is the wave velocity in the media between the transmitter and receiver (in our case, air). The value *a*/*c*<sub>0</sub> corresponds to the wave travel time from the transmitter to the receiver.

``` r
t0 <- firstBreakToTime0(tfb, A1)
time0(A1) <- t0     # set time0 to A1
```

Note that if `t0` is too noisy, you can set `time0(A1) <- mean(t0)`.

Check the results (do you see the difference between time-zero in red and first wave break time in blue?):

``` r
plot(A1[, 15])  # plot the 15th trace of the GPR-line
abline(v = tfb[15], col = "blue")  # first wave break time
```

![plot single trace with time0 and first wave break time](01_RGPR_tutorial_basic-processing_files/figure-markdown_github-tex_math_single_backslash/time0_check-1.png)

To shift the traces to time-zero, use the function `time0Cor` (the `method` argument defines the type of interpolation method)

``` r
A2 <- time0Cor(A1, method = "pchip")
```

``` r
plot(A2)
```

![plot after time0Cor()](01_RGPR_tutorial_basic-processing_files/figure-markdown_github-tex_math_single_backslash/time0Cor_check-1.png)

Dewow
-----

Remove the low-frequency components (the so-called "wow") of the GPR record using:

1.  either a median absolute deviation (MAD) filter (`type = "MAD"`).
2.  or a Gaussian filter (`type = "Gaussian"`). The Gaussian filter is faster than the MAD filter.

In both cases, the argument `w` is the length of the filter in time units.

``` r
A3 <- dewow(A2, type = "MAD", w = 50)     # dewowing: take some time
plot(A3)                                  # plot the result
```

![plot after dewow](01_RGPR_tutorial_basic-processing_files/figure-markdown_github-tex_math_single_backslash/dewow-1.png)

Can you see the difference with `A1`? Plot `A2 - A1` to see the removed "wow".

``` r
plot(A3 - A2)                           # plot the difference
```

![plot difference after dewow](01_RGPR_tutorial_basic-processing_files/figure-markdown_github-tex_math_single_backslash/dewow_diff-1.png)

See the dewowing by comparing the traces before (blue line) and after (red line):

``` r
plot(A2[,15], col = "blue")      # before dewowing
lines(A3[,15], col = "red")      # after dewowing
```

![plot single trace dewow](01_RGPR_tutorial_basic-processing_files/figure-markdown_github-tex_math_single_backslash/dewow_check_1D-1.png)

Frequency filter
----------------

Let's have a look at the amplitude-frequency and phase-frequency plot (the spectrum given by the Fourier decomposition):

``` r
spec(A3)
```

![plot spectrum](01_RGPR_tutorial_basic-processing_files/figure-markdown_github-tex_math_single_backslash/fFilter_spectrum-1.png)

The curve in red is the averaged amplitude/phase over all the trace amplitudes/phases.

On the first plot, notice

-   a sharp increase of the amplitude between 0 *M**H**z* and 10 *M**H**z*: these frequency correspond to the slowing-varying part of the signal. In this particular case, filtering out this part of the signal results in strong signal distorsion.
-   after a peak at 80 *M**H**z* (the returned signal frequency that is lower than the antenna frequency because of frequency-dependent attenuation in the ground), the amplitude decreases.
-   at about 200 *M**H**z* the amplitude stays constant (plateau): noise frequency.

Eliminate the high-frequency (noise) component of the GPR record with a bandpass filter. We define as corner frequencies at 150 *M**H**z* and 260 *M**H**z*, and set `plotSpec = TRUE` to plot the spectrum with the signal, the filtered signal and the filter.

``` r
A4 <- fFilter(A3, f = c(150, 260), type = "low", plotSpec = TRUE)
```

![plot frequency filter](01_RGPR_tutorial_basic-processing_files/figure-markdown_github-tex_math_single_backslash/fFilter_fFilter-1.png)

``` r
plot(A4)
```

![plot frequency filter](01_RGPR_tutorial_basic-processing_files/figure-markdown_github-tex_math_single_backslash/fFilter_fFilter-2.png)

Let see the difference

``` r
plot(A4 - A3, clip = 50)
```

![plot difference](01_RGPR_tutorial_basic-processing_files/figure-markdown_github-tex_math_single_backslash/fFilter_diff-1.png)

Ideally, the objective of processing is to remove the noise component without altering the signal component to improve the signal/noise ratio. When ploting the difference in processing (after - before), one should only observe the noise that is filtered out. Here, removing and attenuating some frequencies change the signal amplitude.

Amplitude gain
--------------

Apply a gain to compensate the signal attenuation. Three types of gain are available:

-   power gain (`type = "power"`):

    *A*<sub>*g*</sub>(*t*)=*A*(*t*)⋅*t*<sup>*α*</sup>

    with $= 1 $ per default.

-   exponential gain (`type = "exp"`): $$ A\_g(t) = A(t)(t) $$
-   Automatic gain control (`type = "agc"`): make gain equal to the local root mean squared signal.

We will first apply a power gain and then an exponential gain. To visualise the amplitude of the GPR signal as a function of time, use the function `plotAmpl()` as follows:

``` r
plotAmpl(A4, col = "black")          # plot amplitude as a function of time
```

![plot amplitude](01_RGPR_tutorial_basic-processing_files/figure-markdown_github-tex_math_single_backslash/plotAmpl-1.png)

On the previous plot, there is a sharp amplitude increase at about 20 *n**s* corresponding to the first wave arrival. Then the amplitude decreases until a plateau at about 200 *n**s*. This plateau corresponds to the signal noise. There is little hope to extract some useful information above 150 *n**s* because above 150 *n**s* the signal/noise ratio is much smaller than 1 (i.e., more noise than signal).

### Power gain

The power gain is set constant until `tcst = 100` ns and applied from 100 *n**s* to `te = 200` ns, with *α* = 1.

``` r
A5 <- gain(A4, type = "power", alpha = 1, te = 150, tcst = 20)
```

Compare the amplitude before and after the power gain:

``` r
plotAmpl(A4, col = "black")   
# set add=TRUE to add the amplitude on the previous plot
plotAmpl(A5, col = "red", add = TRUE)
```

![plot amplitude after power gain](01_RGPR_tutorial_basic-processing_files/figure-markdown_github-tex_math_single_backslash/ampl_check-1.png)

``` r
plot(A5)      # how does it look after the gain?
```

![plot amplitude after power gain](01_RGPR_tutorial_basic-processing_files/figure-markdown_github-tex_math_single_backslash/ampl_check-2.png)

### Exponential gain

Ideally, the parameter *α* in the exponential gain should be close to the slope of the amplitude decrease. This slope could be estimated by fitting a straight line to the amplitude decrease. We only want to apply the filter between 0 *n**s* (`t0`) and 125 *n**s* (`te` for *t*<sub>*e**n**d*</sub> ):

``` r
A6 <- gain(A5, type ="exp",  alpha = 0.2, t0 = 0, te = 125)
plotAmpl(A6, col = "green")
```

![plot ampliude exponential gain](01_RGPR_tutorial_basic-processing_files/figure-markdown_github-tex_math_single_backslash/gain_exp-1.png)

Oops! Set `alpha` to a smaller value!

``` r
A6 <- gain(A5, type = "exp", alpha = 0.11, t0 = 0, te = 125)
plotAmpl(A4, col = "black") 
plotAmpl(A5, col = "red", add = TRUE) 
plotAmpl(A6, col = "green", add = TRUE)
```

![plot amplitude after exponential gain](01_RGPR_tutorial_basic-processing_files/figure-markdown_github-tex_math_single_backslash/gain_check2-1.png)

``` r
plot(A6)    # how does it look after the gain?
```

![plot amplitude after exponential gain](01_RGPR_tutorial_basic-processing_files/figure-markdown_github-tex_math_single_backslash/gain_check2-2.png)

Plot the gained GPR record and clip the amplitude values to 50 *m**V* using the argument `clip`:

``` r
plot(A6, clip = 50)    # how does it look after the gain?
```

![plot after gain](01_RGPR_tutorial_basic-processing_files/figure-markdown_github-tex_math_single_backslash/gain_diff-1.png)

inverse normal transformations
------------------------------

Have a look at the histogram of the values of `A6`

``` r
hist(A6[], breaks = 50)
```

![plot hist](01_RGPR_tutorial_basic-processing_files/figure-markdown_github-tex_math_single_backslash/invnorm-1.png)

This histogram is very narrow, meaning that a lot of values are very close to zero and therefore many details are not really visible. To widen this histogram, we can transform it to make it more normally distributed with a rank-based inverse normal transformation:

``` r
A7 <- traceScaling(A6, type = "invNormal")
```

Histograms before and after

``` r
par(mfrow = c(1, 2))
hist(A6[], breaks = 50)
hist(A7[], breaks = 50)
```

![plot histogram comparison](01_RGPR_tutorial_basic-processing_files/figure-markdown_github-tex_math_single_backslash/hist_diff-1.png) Have a look at the results of the transformation:

``` r
plot(A7)
```

![plot inverse normal transformation results](01_RGPR_tutorial_basic-processing_files/figure-markdown_github-tex_math_single_backslash/invnorm_check-1.png)

Median filter (spatial filter)
------------------------------

A non-linear filter to remove noise:

``` r
A8 <- filter2D(A7, type = "median3x3")
plot(A8)
```

![plot median filter](01_RGPR_tutorial_basic-processing_files/figure-markdown_github-tex_math_single_backslash/median_filter-1.png)

Let see the difference

``` r
plot(A8 - A7)
```

![plot difference after median filter](01_RGPR_tutorial_basic-processing_files/figure-markdown_github-tex_math_single_backslash/median_filter_diff-1.png)

Frequency-wavenumber filter (f-k-filter)
----------------------------------------

The function `spec()` with the argument `type = "f-k` returns a list containing the frequencies (f), the wavenumbers (k), the amplitude of the GPR data.

``` r
FKSpec <- spec(A8, type = "f-k")
area <- list(x = c(0, min(FKSpec$wnb), min(FKSpec$wnb), max(FKSpec$wnb), max(FKSpec$wnb), 0),
             y = c(max(FKSpec$fre), 800, 0, 0, 800, max(FKSpec$fre) ))
lines(area, type="o")
```

![plot fk-filter](01_RGPR_tutorial_basic-processing_files/figure-markdown_github-tex_math_single_backslash/fkspec-1.png)

``` r
A9 <- fkFilter(A8, fk = area)
```

With the f-k-filter you can successfully remove the artifacts but still the information gained is very small in this case (the quality of the raw GPR data is already bad):

``` r
plot(A9, clip = 50)
```

![plot fk-spectrum](01_RGPR_tutorial_basic-processing_files/figure-markdown_github-tex_math_single_backslash/fk_plot-1.png)

``` r
spec(A9, type = "f-k")
```

![plot fk-spectrum](01_RGPR_tutorial_basic-processing_files/figure-markdown_github-tex_math_single_backslash/fk_plot-2.png)

Let see the difference

``` r
plot(A9 - A8)
```

![plot difference after fk-filter](01_RGPR_tutorial_basic-processing_files/figure-markdown_github-tex_math_single_backslash/fK_diff-1.png)

Processing overview
-------------------

Let review the processing step applied on the GPR record:

``` r
proc(A9)
```

    ## [1] "time0<-"

Other processing functions
--------------------------

### Trace average removal

Check the help page for the function `traceAverage`

``` r
?traceAverage
```

Save and export
---------------

Save the processed GPR record into the directory /processing. Use the `.rds` format (this is a R internal format)

``` r
writeGPR(A9, fPath = file.path(getwd(), "processing", paste0(name(A9), ".rds")), 
         format = "rds", overwrite = TRUE)
```

    ## *** Class GPR ***
    ##  name        = LINE00
    ##  filepath    = /media/huber/Elements/UNIBAS/software/codeR/package_RGPR/RGPR-gh-pages/2014_04_25_frenke/processing/LINE00.dt1
    ##  1 fiducial(s)
    ##  description = 
    ##  survey date = 2014-04-25
    ##  Reflection, 100 MHz, Window length = 352 ns, dz = 0.4 ns
    ##  223 traces, 55.5 m
    ##  > PROCESSING
    ##    1. time0<-
    ##  ****************

Export a high quality PDF:

``` r
plot(A9, type = "wiggles", clip = 30, pdfName = file.path(getwd(), "processing", name(A9)),
          lwd = 0.5, wsize = 2.5)
```

Read the saved GPR data
-----------------------

``` r
procA <- readGPR(fPath = file.path(getwd(), "processing", paste0(name(A9), ".rds"))) 
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
