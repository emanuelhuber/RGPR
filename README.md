# RGPR
R package to visualize, analyze, process and interpret *ground-penetrating radar* (GPR) data.

## Note
Currently:

* the only supported binary formats are 
    
    - the Sensors & Software file format (.DT1, .HD)
    - the MALA file format (.rd3, .rad)
    
* RGPR only support reflection data such as surface-based GPR data (no support for cross-borehole GPR data)
* the documentation is still incomplete (but check the tutorials)

This is an ongoing project.
If you have any questions, don't hesitate to contact me:

emanuel.huber@alumni.ethz.ch

Thank you!

## Online tutorials
Check the companion website for more info, tutorials, etc.

http://emanuelhuber.github.io/RGPR/

## How to install/load

```r
library(devtools)
devtools::install_github("emanuelhuber/RGPR")
library(RGPR)

frenkeLine00  # data from the package

plot(frenkeLine00)

```

## Existing functions

### List of the functions from the class `GPR`
```r
library(RGPR)
mtext <-  showMethods(class="GPR", printTo =FALSE )
i <- grepl('Function', mtext) & grepl('package RGPR', mtext) 
fvec <- gsub( "Function(\\:\\s|\\s\\\")(.+)(\\s\\(|\\\")(.+$)",
                                      "\\2", mtext[i] )
fvec
```

### List of the functions from the class `GPRsurvey`
```r
library(RGPR)
mtext <-  showMethods(class="GPRsurvey", printTo =FALSE )
i <- grepl('Function', mtext) & grepl('package RGPR', mtext) 
fvec <- gsub( "Function(\\:\\s|\\s\\\")(.+)(\\s\\(|\\\")(.+$)",
                                      "\\2", mtext[i] )
fvec
```

### Incomplete overview of the RGPR-package
```r
?RGPR
```

## Contributions

Thanks to:

-  @jmerc13