---
layout: page
title: Import GPR data
date: 2019-08-26
---

<!--
"/media/huber/Elements/UNIBAS/software/codeR/package_RGPR/RGPR-gh-pages/2014_04_25_frenke"
"G:/UNIBAS/software/codeR/package_RGPR/RGPR-gh-pages/2014_04_25_frenke"
-->

------------------------------------------------------------------------

**Note**:

-   This R-package is still in development, and therefore some of the functions may change in a near future.
-   If you have any questions, comments or suggestions, feel free to contact me (in english, french or german): <emanuel.huber@alumni.ethz.ch>.

Table of content
================

-   [Objectives of this tutorial](#objectives-of-this-tutorial)
-   [Install/load `RGPR`](#installload-rgpr)
-   [Read/import GPR data](#readimport-gpr-data)

    -   [Sensors and software data (`.dt1`)](#sensors-and-software-data-dt1)
    -   [GSSI data (`.dtz`)](#gssi-data-dtz)
    -   [MALA data (`.rd3`)](#mala-data-rd3)
    -   [ImpulseRadar data (`.iprb`)](#impulseradar-data-iprb)
    -   [RadSys Zond GPR data (`.segy`)](#radsys-zond-gpr-data-segy)
    -   [ASCII data (`.txt`)](#ascii-data-txt)
    -   [ENVI band-sequential files (`.dat`, `.hdr`)](#envi-band-sequential-files-dat-hdr)
    -   [Pickle files (`.pkl`, serialized Python object)](#pickle-files-pkl-serialized-python-object)

-   [Convert a matrix object into a GPR data](#matrix2GPR)

Objectives of this tutorial
===========================

-   Learn some basics of ground-penetrating radar data processing with `RGPR`.
-   Learn how to manipulate objects of the class `RGPR`.

Note that his tutorial will not explain you the math/algorithms behind the different processing methods.

Install/load `RGPR`
===================

``` r
# install "devtools" if not already done
if(!require("devtools")) install.packages("devtools")
devtools::install_github("emanuelhuber/RGPR")
# load RGPR in the current R session
library(RGPR)
```

Read/import GPR data
====================

Use the function `readGPR()` to import GPR data into R. While the filepath (given to the argument `dsn`) is case sensitive, the extension is not. That means that you can write: `XLINE00.DT1` or `XLINE00.dt1`. Note that the filepath must correspond to the binary data (not to the ASCII header file data)

Note that the coordinates collected with GPS in the same time as the GPR data and stored in a specific file are not directly imported by the function `readGPR()`. You must add the coordinates separatly following the explanations of the tutorial '[Adding coordinates to GPR data](http://emanuelhuber.github.io/RGPR/02_RGPR_tutorial_RGPR-survey/)'.

Sensors and software data (`.dt1`)
----------------------------------

Each GPR data consists at least of

-   a header file (extension `.hd`) that can be opened with a normal text editor
-   a binary data file (extension `.DT1`, 16-bit).

To read the GPR data, enter

``` r
x <- readGPR(dsn = "XLINE00.DT1")
plot(x)
```

GSSI data (`.dtz`)
------------------

To read the GPR data, enter

``` r
x <- readGPR(dsn = "XLINE00.dtz")
plot(x)
```

### Multi-channel data

To read multi-channel data, simply specify the channel number you want to read by using the argument `ch` in the function `readGPR()`:

-   read channel 1:

    ``` r
    x <- readGPR(dsn = "YLINE00.dtz", ch = 1)
    plot(x)
    ```

-   read channel 2:

    ``` r
    x <- readGPR(dsn = "YLINE00.dtz", ch = 2)
    plot(x)
    ```

MALA data (`.rd3`)
------------------

Each GPR data consists at least of

-   a header file (extension `.rad`) that can be opened with a normal text editor
-   a binary data file (extension `.rd3`, 16-bit).

To read the GPR data, enter

``` r
x <- readGPR(dsn = "XLINE00.rd3")
plot(x)
```

ImpulseRadar data (`.iprb`)
---------------------------

Each GPR data consists at least of

-   a header file (extension `.iprh`) that can be opened with a normal text editor
-   a binary data file (extension `.iprb`, 16-bit or 32-bit).

To read the GPR data, enter

``` r
x <- readGPR(dsn = "XLINE00.iprb")
plot(x)
```

RadSys Zond GPR data (`.segy`)
------------------------------

Each GPR data consists of

-   a binary data file with the SEG-Y Sounding Data Format (extension `.sgy` or `.segy`, 16-bit or 32-bit).

**NOTE THAT THIS DATA FORMAT IS NOT A VERSION OF THE SEG-Y file format.**

To read the GPR data, enter

``` r
x <- readGPR(dsn = "XLINE00.sgy")
# or
x <- readGPR(dsn = "XLINE00.segy")
plot(x)
```

ASCII data (`.txt`)
-------------------

Either 3-column format (x, t, amplitude) or matrix-format (without header/rownames). `readGPR()` should be able to detect the format as well as the separator.

To read the GPR data, enter

``` r
x <- readGPR(dsn = "XLINE00.txt")
plot(x)
```

ENVI band-sequential files (`.dat`, `.hdr`)
-------------------------------------------

1.  Read the ENVI file

    ``` r
    library(caTools)

    mydata <- read.ENVI("LINE001.dat",
                        headerfile = "LINE001.dat.hdr")
    class(mydata)
    ```

2.  Convert this matrix into a GPR object according to the Section [Convert a matrix object into a GPR data](#convert-a-matrix-object-into-a-gpr-data). Create a list (minimum list format below) and convert it into a GPR object:

    ``` r
    x <- list(data = mydata,
               freq = 250,       # MHz (antenna frequency)
               dx = 0.025,       # metres (spatial sampling)
               dz = 0.1000,      # ns (vertical sampling)
               antsep = 1        # antenna separation 1 m
)

    # convert this list into a GPR object
    gprdata <- as(x, "GPR")
    ```

Pickle files (`.pkl`, serialized Python object)
-----------------------------------------------

1.  Read the pickle file

    ``` r
    # install package reticulare if necessary
    if(!require("reticulate")) install.packages("reticulate")

    mydata <- reticulate::py_load_object("sim.pkl")
    class(mydata)
    ```

2.  Convert this matrix into a GPR object according to the Section [Convert a matrix object into a GPR data](#convert-a-matrix-object-into-a-gpr-data). Create a list (minimum list format below) and convert it into a GPR object:

    ``` r
    x <- list(data = t(mydata),     # transpose the data if necessary
               freq = 250,          # MHz (antenna frequency)
               dx = 0.025,          # metres (spatial sampling)
               dz = 0.1000,         # ns (vertical sampling)
               antsep = 1           # antenna separation 1 m
)

    # convert this list into a GPR object
    gprdata <- as(x, "GPR")
    ```

Convert a matrix object into a GPR data
=======================================

1.  We create a matrix data object:

    ``` r
    mydata <- as.matrix(frenkeLine00)
    ```

2.  Create a list (minimum list format below)

    ``` r
    x <- list(data = mydata,
               freq = 250,      # MHz (antenna frequency)
               dx = 0.025,      # metres (spatial sampling)
               dz = 0.1000,     # ns (vertical sampling)
               antsep = 1       # antenna separation 1 m
)
    ```

3.  Convert this list into a GPR object

    ``` r
    gprdata <- as(x, "GPR")
    # plot to check
    plot(gprdata, main = "test")
    # get an overview of the slots (attributes) of the object
    str(gprdata)
    ```

You can also define some additional attributes, see examples below:

``` r
x2 <- list(data = mydata,
           freq = 250,
           dx = 0.025,
           dz = 0.1000,
           antsep = 1,
           # either 'reflection', 'CMP' or 'WARR'
           surveymode = "reflection",
           date = "25/12/2017",
           # date of the survey: dd/mm/yyyy
           name = "XLINE00",
           description = "A nice survey",
           depthunit = "ns",
           posunit = "m",
           # GPR wave velocity in m/ns, must be a list
           # for the moment, only constant wave velocity
           # are implemented.
           vel = list(0.1)
)


gprdata2 <- as(x2, "GPR")
plot(gprdata2, main = "test2")
```

Instead of giving the spatial (`dx`) and vertical (`dz`) sampling, you can directly give the trace positions (`pos`) and the sample time (`depth`). Example:

``` r
x3 <- list(data = mydata,
           freq = 250,
           # trace position
           pos = seq(from = 0, by = 0.025,
                     length.out = ncol(mydata)),
           # sample position
           depth = seq(from = 0, by = 0.1,
                       length.out = nrow(mydata)),
           antsep = 1
)

gprdata3 <- as(x3, "GPR")
plot(gprdata3, main = "test3")
```
