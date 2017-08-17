# RGPR tutorial - `RGPR` class
Emanuel Huber (emanuel.huber@alumni.ethz.ch)  
11 April 2016  
  


`RGPR` is a package for [R](https://cran.r-project.org/) to read, write, analyse and visualise ground-penetrating radar (GPR) data.
  
> R is a [free] programming language and software environment for statistical computing and graphics supported by the R Foundation for Statistical Computing ([Wikipedia > R](https://en.wikipedia.org/wiki/R_%28programming_language%29)).

**Note**: This R-package is still in development, and therefore some of the functions may change in a near future. The R-package `RGPR` is hosted on [GitHub](https://github.com/) at [https://github.com/emanuelhuber/RGPR](https://github.com/emanuelhuber/RGPR). You can contribute to the development of `RGPR`: create an account on [GitHub](https://github.com/), [fork](https://guides.github.com/activities/forking/) `RGPR`, improve it and sumbmit your modifications.

If you have any questions, comments or wishes, etc. feel free to contact me (in english, french or german)

> `emanuel.huber@alumni.ethz.ch`

# Objectives of this tutorial
* Learn what is an object of the class `RGPR`.

In this tutorial the code snippets are in monospaced typewriter font like in the following example:

```r
1 + exp(1:10)
```

The R output are preceded by a double hash (`##`). The following R output is from the code snippet above.

```
##  [1]     3.718282     8.389056    21.085537    55.598150   149.413159
##  [6]   404.428793  1097.633158  2981.957987  8104.083928 22027.465795
```

Create a text file and save it with the `.R` extension (the extension for the R-script files). Then copy the code snippets into your R-script file and adapt them to your needs. To run the code in R, copy the code and paste it into the R console. You can also manually enter the code.

Don't hesitate to consult the help files and to search for help on the internet. For example, to see the help for the function `mean()`, enter:


```r
?mean    # open the help file related to the function mean()
```

# Preliminary
* Read the tutorial [RGPR - Getting started (tutorial 1)](http://emanuelhuber.github.io/RGPR/RGPR_tutorial_installation-load.html)

* Load the packages `RGPR` and set the working directory:

```r
myDir <- "/media/huber/Elements/UNIBAS/software/codeR/package_RGPR/RGPR-gh-pages/2014_04_25_frenke"
setwd(myDir)    # set the working directory
getwd()         # Return the current working directory (just to check)
```

```
## [1] "/media/huber/Elements/UNIBAS/software/codeR/package_RGPR/RGPR-gh-pages/2014_04_25_frenke"
```

* Load the GPR data `frenkeLine00` that is included in the package `RGPR`:

```r
library(devtools)
devtools::install_github("emanuelhuber/RGPR")
library(RGPR)
data("frenkeLine00")
# just because it is simplier to enter 'A' instead of 'frenkeLine00'
A <- frenkeLine00   
```


# An object of the class `RGPR`

If you just enter the newly created object `A` in R (or enter `print(A)`), R will show you some information on the object A:

```r
A                 # let's have a look at the object A
```

```
## *** Class GPR ***
##  name = LINE00
##  filepath = rawGPR/LINE00.DT1
##  1 fiducial(s)
##  description = 
##  survey date =  2014-04-25 
##  Reflection, 100MHz,Window length=399.6ns, dz=0.4ns
##  223 traces,55.5m long
##  ****************
```

`A` is an object of the class `GPR` with a complex structure that can be visualised with the `str()` function:

```r
str(A)            # let's have a look at the structure of A
```

```
## Formal class 'GPR' [package "RGPR"] with 29 slots
##   ..@ version     : chr "0.1"
##   ..@ data        : num [1:1000, 1:223] -1.05 -1.44 -1.48 -1.5 -1.54 ...
##   ..@ traces      : num [1:223] 1 2 3 4 5 6 7 8 9 10 ...
##   ..@ depth       : num [1:1000] 0 0.4 0.8 1.2 1.6 2 2.4 2.8 3.2 3.6 ...
##   ..@ pos         : num [1:223] 0 0.25 0.5 0.75 1 1.25 1.5 1.75 2 2.25 ...
##   ..@ time0       : num [1:223] 52.2 52.2 52.2 52.2 52.2 ...
##   ..@ time        : num [1:223] 1.4e+09 1.4e+09 1.4e+09 1.4e+09 1.4e+09 ...
##   ..@ fid         : chr [1:223] "" "" "" "" ...
##   ..@ ann         : chr(0) 
##   ..@ coord       : logi[0 , 0 ] 
##   ..@ rec         : logi[0 , 0 ] 
##   ..@ trans       : logi[0 , 0 ] 
##   ..@ coordref    : num(0) 
##   ..@ freq        : num 100
##   ..@ dz          : num 0.4
##   ..@ dx          : num 0.25
##   ..@ antsep      : num 1
##   ..@ name        : chr "LINE00"
##   ..@ description : chr ""
##   ..@ filepath    : chr "rawGPR/LINE00.DT1"
##   ..@ depthunit   : chr "ns"
##   ..@ posunit     : chr "m"
##   ..@ surveymode  : chr "Reflection"
##   ..@ date        : chr "2014-04-25"
##   ..@ crs         : chr(0) 
##   ..@ proc        : chr(0) 
##   ..@ vel         :List of 1
##   .. ..$ : num 0.1
##   ..@ delineations: list()
##   ..@ hd          :List of 14
##   .. ..$ startpos          : num 0
##   .. ..$ endpos            : num 55.5
##   .. ..$ gprdevice         : chr "Data Collected with pE PRO (2011-00114-00)"
##   .. ..$ PULSER_VOLTAGE_V  : chr "400"
##   .. ..$ NUMBER_OF_STACKS  : chr "16"
##   .. ..$ ODOMETER_CAL_tm   : chr "979.599976"
##   .. ..$ STACKING_TYPE     : chr "F1, P16, DynaQ OFF"
##   .. ..$ DVL_Serial        : chr "0051-6870-0001"
##   .. ..$ Control_Mod_Serial: chr "0022-3009-0014"
##   .. ..$ Transmitter_Serial: chr "0026-3171-0008"
##   .. ..$ Receiver_Serial   : chr "0025-3172-0014"
##   .. ..$ Start_DVL_Battery : chr "12.68V"
##   .. ..$ Start_Rx_Battery  : chr "12.71V"
##   .. ..$ Start_Tx_Battery  : chr "12.78V 12.78V"
```


The strings after the `@` are the names of the elements that form `A`. These elements are called `slots`. To get the `slots` names enter:

```r
slotNames(A)      # return the slot names
```

```
##  [1] "version"      "data"         "traces"       "depth"       
##  [5] "pos"          "time0"        "time"         "fid"         
##  [9] "ann"          "coord"        "rec"          "trans"       
## [13] "coordref"     "freq"         "dz"           "dx"          
## [17] "antsep"       "name"         "description"  "filepath"    
## [21] "depthunit"    "posunit"      "surveymode"   "date"        
## [25] "crs"          "proc"         "vel"          "delineations"
## [29] "hd"
```

A `RGPR` object consist of:
1. the GPR data (i.e. the measured amplitudes as a function of time for each GPR traces). The data are stored in the slot `data`.
2. meta-data (e.g. position of the traces on the survey line, time of the trace recording, time/depth of each trace sample, time step, etc.)

To access the content of a slot, enter `@` followed by the name of the slot. For example, the `slot` `vel` (the estimated radar wave velocity associated with `A`) is accessed by:

```r
A@vel              # the slot 'vel' (for velocity)
```

```
## [[1]]
## [1] 0.1
```

Normally you don't need to access the slots directly because the R-package `RGPR` provides enough functions to manipulate the `GPR` object (i.e. to extract the necessary information and to modify the object). For example, the estimated radar wave velocity can be obtained with the function `vel()`:

```r
vel(A)              # access the slot 'vel'
```

```
## [1] 0.1
```

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


```r
dim(A)            # dimension of the data: col x row
nrow(A)
ncol(A)
Aexp <- exp(A)    # Take the exponential of the GPR data
Asum <- A + Aexp  # Addition
# etc.
```

```
## [1] 1000  223
## [1] 1000
## [1] 223
```

Furthermore, it is possible to extract a portion of the GPR data without losing the meta-data contained in the object:

```r
# Extract a portion of A that only contains
# the time samples 90 to 200 of the traces 5 to 10
B <- A[90:200, 5:10]          
```

Note that `B` is also an object of the class `RGPR` consisting of 6 traces with each 111 time samples:

```r
B        
```

```
## *** Class GPR ***
##  name = LINE00
##  filepath = rawGPR/LINE00.DT1
##  description = 
##  survey date =  2014-04-25 
##  Reflection, 100MHz,Window length=44ns, dz=0.4ns
##  6 traces,1.25m long
##  ****************
```

