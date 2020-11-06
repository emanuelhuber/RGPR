---
layout: page
title: Class GPR
date: 2020-08-14
---

------------------------------------------------------------------------

**Note**:

-   This R-package is still in development, and therefore some of the functions may change in a near future.
-   If you have any questions, comments or suggestions, feel free to contact me (in english, french or german): <emanuel.huber@pm.me>.

Table of Contents
=================

-   [Objectives of this tutorial](#objectives-of-this-tutorial)
-   [Preliminary](#preliminary)
-   [An object of the class `RGPR`](#an-object-of-the-class-rgpr)

Objectives of this tutorial
===========================

-   Learn what is an object of the class `RGPR`.

Preliminary
===========

-   Install and load the `RGPR`-package

    ``` r
    # install "devtools" if not already done
    if(!require("devtools")) install.packages("devtools")
    devtools::install_github("emanuelhuber/RGPR")
    library(RGPR)       # load RGPR in the current R session
    ```

-   Load the GPR data `frenkeLine00` that is included in the package `RGPR`:

    ``` r
    data("frenkeLine00")
    # just because it is simplier to enter 'A' instead of 'frenkeLine00'
    A <- frenkeLine00
    ```

An object of the class `RGPR`
=============================

If you just enter the newly created object `A` in R (or enter `print(A)`), R will show you some information on the object A:

``` r
A                 # let's have a look at the object A
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

`A` is an object of the class `GPR` with a complex structure that can be visualised with the `str()` function:

``` r
str(A)            # let's have a look at the structure of A
```

    ## Formal class 'GPR' [package "RGPR"] with 29 slots
    ##..@ version: chr "0.1"
    ##..@ data: num [1:1000, 1:223] -1.05 -1.44 -1.48 -1.5 -1.54...
    ##..@ traces: num [1:223] 1 2 3 4 5 6 7 8 9 10...
    ##..@ depth: num [1:1000] 0 0.4 0.8 1.2 1.6 2 2.4 2.8 3.2 3.6...
    ##..@ pos: num [1:223] 0 0.25 0.5 0.75 1 1.25 1.5 1.75 2 2.25...
    ##..@ time0: num [1:223] 52.2 52.2 52.2 52.2 52.2...
    ##..@ time: num [1:223] 1.4e+09 1.4e+09 1.4e+09 1.4e+09 1.4e+09...
    ##..@ fid: chr [1:223] "" "" "" ""...
    ##..@ ann: chr(0)
    ##..@ coord: logi[0, 0 ]
    ##..@ rec: logi[0, 0 ]
    ##..@ trans: logi[0, 0 ]
    ##..@ coordref: num(0)
    ##..@ freq: num 100
    ##..@ dz: num 0.4
    ##..@ dx: num 0.25
    ##..@ antsep: num 1
    ##..@ name: chr "LINE00"
    ##..@ description: chr ""
    ##..@ filepath: chr "data-raw/LINE00.DT1"
    ##..@ depthunit: chr "ns"
    ##..@ posunit: chr "m"
    ##..@ surveymode: chr "Reflection"
    ##..@ date: chr "2014-04-25"
    ##..@ crs: chr(0)
    ##..@ proc: chr(0)
    ##..@ vel:List of 1
    ##....$: num 0.1
    ##..@ delineations: list()
    ##..@ hd:List of 14
    ##....$ startpos: num 55.5
    ##....$ gprdevice: chr "Data Collected with pE PRO (2011-00114-00)"
    ##....$ ANTENNA_SEPARATION: chr "1.000000"
    ##....$ PULSER_VOLTAGE_V: chr "400"
    ##....$ NUMBER_OF_STACKS: chr "16"
    ##....$ ODOMETER_CAL_tm: chr "979.599976"
    ##....$ STACKING_TYPE: chr "F1, P16, DynaQ OFF"
    ##....$ DVL_Serial: chr "0051-6870-0001"
    ##....$ Control_Mod_Serial: chr "0022-3009-0014"
    ##....$ Transmitter_Serial: chr "0026-3171-0008"
    ##....$ Receiver_Serial: chr "0025-3172-0014"
    ##....$ Start_DVL_Battery: chr "12.68V"
    ##....$ Start_Rx_Battery: chr "12.71V"
    ##....$ Start_Tx_Battery: chr "12.78V 12.78V"

The strings after the `@` are the names of the elements that form `A`. These elements are called `slots`. To get the `slots` names enter:

``` r
slotNames(A)      # return the slot names
```

    ##  [1] "version"      "data"         "traces"       "depth"        "pos"
    ##  [6] "time0"        "time"         "fid"          "ann"          "coord"
    ## [11] "rec"          "trans"        "coordref"     "freq"         "dz"
    ## [16] "dx"           "antsep"       "name"         "description"  "filepath"
    ## [21] "depthunit"    "posunit"      "surveymode"   "date"         "crs"
    ## [26] "proc"         "vel"          "delineations" "hd"

A `RGPR` object consist of: 1. the GPR data (i.e. the measured amplitudes as a function of time for each GPR traces). The data are stored in the slot `data`. 2. meta-data (e.g. position of the traces on the survey line, time of the trace recording, time/depth of each trace sample, time step, etc.)

To access the content of a slot, enter `@` followed by the name of the slot. For example, the `slot` `vel` (the estimated radar wave velocity associated with `A`) is accessed by:

``` r
A@vel              # the slot 'vel' (for velocity)
```

    ## [[1]]
    ## [1] 0.1

Normally you don't need to access the slots directly because the R-package `RGPR` provides enough functions to manipulate the `GPR` object (i.e. to extract the necessary information and to modify the object). For example, the estimated radar wave velocity can be obtained with the function `vel()`:

``` r
vel(A)              # access the slot 'vel'
```

    ## [1] 0.1

<!---
`A` is an object of the class `GPR` with a complex structure:

```r
class(A)          # To which class belong A?
```

```
## [1] "GPR"
## attr(,"package")
## [1] "RGPR"
```

To see the structure of `A` enter

```r
str(A)            # let's have a look at the structure of A
```

The strings after the `@` are the names of the elements that form `A`. These elements are called `slots`. To get the `slots` names enter:

```r
slotNames(A)      # return the slot names
```

To access the content of a slot, enter `@` followed by the name of the slot. For example, the `slot` `vel` (the estimated radar wave velocity) is accessed by:

```r
A@vel              # the slot 'vel' (for velocity)
```

```
## [[1]]
## [1] 0.1
```
-->
<!---
What is the velocity unit? The slot `@depthunit` gives the vertical unit (z, time) and `@posunit` the horizontal unit (x-y):

```r
paste0(A@posunit,"/",A@depthunit)           # the slot 'vel' ('vel' for velocity)
```

```
## [1] "m/ns"
```
-->
<!---
The GPR data are in the slot `@data`. If you enter `A@data` you will notice that `A@data` is a matrix whose columns correspond to the recorded traces and the lines to the time of record. It is not a good practice that the user change the slot contents itself, because some of the slots are related and should not be changed alone. The R-package `RGPR` provides enough function to manipulate the `GPR` object.
-->
Note that an object of the class `RGPR` (e.g. `A`) can be manipulated as a matrix:

``` r
dim(A)            # dimension of the data: col x row
nrow(A)
ncol(A)
Aexp <- exp(A)    # Take the exponential of the GPR data
Asum <- A + Aexp  # Addition
# etc.
```

    ## [1] 1000  223
    ## [1] 1000
    ## [1] 223

Furthermore, it is possible to extract a portion of the GPR data without losing the meta-data contained in the object:

``` r
# Extract a portion of A that only contains
# the time samples 90 to 200 of the traces 5 to 10
B <- A[90:200, 5:10]
```

Note that `B` is also an object of the class `RGPR` consisting of 6 traces with each 111 time samples:

``` r
B
```

    ## *** Class GPR ***
    ##  name        = LINE00
    ##  filepath    = data-raw/LINE00.DT1
    ##  description =
    ##  survey date = 2014-04-25
    ##  Reflection, 100 MHz, Window length = 44 ns, dz = 0.4 ns
    ##  6 traces, 1.25 m
    ##  ****************
