# [RGPR](http://emanuelhuber.github.io/RGPR): a free and open-source software package for ground-penetrating radar (GPR) data processing

**If you have any questions, comments or suggestions, feel free to contact me (in english, french or german):**
**emanuel.huber@pm.me**

> I am developing this package on my free time as a gift to the GPR community. Any support will be appreciated! 


[![](https://bmc-cdn.nyc3.digitaloceanspaces.com/BMC-button-images/custom_images/orange_img.png)](https://www.buymeacoffee.com/EmanuelHuber)

[RGPR](http://emanuelhuber.github.io/RGPR) is a free and open-source software package to read, export, analyse, process and visualise *ground-penetrating radar* (GPR) data. [RGPR](http://emanuelhuber.github.io/RGPR) is written in [R](https://cran.r-project.org/), a high-level  programming language for statistical computing and graphics that is freely available under the GNU General Public License and runs on Linux, Windows and MacOS. [R](https://cran.r-project.org/) is a interpreted scripting language (not compiled) in the same veine as python or matlab.

[RGPR](http://emanuelhuber.github.io/RGPR) was initially developed to compensate for shortcomings of commercial GPR data processing applications. The  ultimate  goal  of  [RGPR](http://emanuelhuber.github.io/RGPR)  is  to  promote  GPR  related research   by   providing   access   to the flexible   and   rich   R environment.    [RGPR](http://emanuelhuber.github.io/RGPR)    has    also    a    didactic    vocation    by encouraging  students  and   researchers  to  learn  about GPR  signal  processing  through  various  tutorials  available  on the [RGPR](http://emanuelhuber.github.io/RGPR) GitHub repository and the R documentation (companion website with tutorials: http://emanuelhuber.github.io/RGPR).

## Table of content

<!--ts-->
   * [How to cite](#how-to-cite)
   * [Notes](#notes)
      * [Supported file formats](#supported-binary-formats) 
      * [Current limitations](#current-limitations)
      * [Yes, you can contribute](#yes-you-can-contribute)
   * [Online tutorials](#online-tutorials)
   * [How to install](#how-to-install)
   * [Function overview](#function-overview)
   * [Contributions](#contributions)
<!--te-->


## How to cite

> E. Huber and G. Hans (2018) RGPR — An open-source package to process and visualize GPR data. 17th International Conference on Ground Penetrating Radar (GPR), Switzerland, Rapperswil, 18-21 June 2018, pp. 1-4.
> doi: [10.1109/ICGPR.2018.8441658](https://doi.org/10.1109/ICGPR.2018.8441658)

[PDF](https://emanuelhuber.github.io/publications/2018_huber-and-hans_RGPR-new-R-package_notes.pdf) [Poster](https://emanuelhuber.github.io/publications/poster_2018_huber-and-hans_RGPR-new-open-source-package.pdf)

Bibtex format

```
@INPROCEEDINGS{huber&hans:2018,
author    = {Emanuel Huber and Guillaume Hans},
booktitle = {2018 17th International Conference on Ground Penetrating Radar (GPR)},
title     = {RGPR — An open-source package to process and visualize GPR data},
year      = {2018},
pages     = {1--4},
doi       = {10.1109/ICGPR.2018.8441658},
ISSN      = {2474-3844}}
```

My current affiliation:

```
Emanuel Huber,
GEOTEST AG
Bernstrasse 165
3052 Zollikofen 
Switzerland
```

## Notes


### Supported file formats (read only):

- [X] [Sensors & Software](https://www.sensoft.ca) file format (**\*.dt1**, **\*.hd**, **\*.gps**).
- [X] [MALA](https://www.malagpr.com.au) file format (**\*.rd3**, **\*.rd7**, **\*.rad**, **\*.cor**).
- [X] [ImpulseRadar](https://www.impulseradar.se) file format (**\*.iprb**, **\*.iprh**, **\*.cor**, **\*.time**, **\*.mrk**).
- [X] [GSSI](https://www.geophysical.com) file format (**\*.dzt**, **\*.dzx**).
- [X] [Geomatrix Earth Science Ltd](https://www.geomatrix.co.uk/) file format (Utsi Electronics format) for the **GroundVue 3**, **7**, **100**, **250** and **400** as well as for the **TriVue** devices (**\*.dat**, **\*.hdr**, **\*.gpt**, **\*.gps**).
- [x] [Radar Systems, Inc.](http://www.radsys.lv) Zond file format (**\*.sgy**). **WARNING: it is not a version of the SEG-Y file format**.
- [X] [IDS](https://idsgeoradar.com/) file format (**\*.dt**, **\*.gec**).
- [X] [Transient Technologies](https://viy.ua/) file format (**\*.sgpr**).
- [X] [US Radar](https://usradar.com/) file format (**\*.RA1**, **\*.RA2** or **\*.RAD**)
- [X] [SEG-Y](https://en.wikipedia.org/wiki/SEG-Y) file format developed by the Society of Exploration Geophysicists (SEG) for storing geophysical data (**\*.sgy**), also used by [Easy Radar USA](https://easyradusa.com)
- [X] [SEG-2](https://seg.org/Portals/0/SEG/News%20and%20Resources/Technical%20Standards/seg_2.pdf) Pullan, S.E., 1990, Recommended standard for seismic (/radar) files in the personal computer environment: Geophysics, 55, no. 9, 1260–1271(**\*.sg2**). Also used by US Radar with extensions \*.RA1, \*.RA2, \*. RAD. 
- [X] [GPRmax](https://www.gprmax.com/): hdf5 file format with extension \*.out (not well tested)
- [X] [3dradar](http://3d-radar.com/): the manufacturer does not want to reveal the binary file format **\*.3dra**. **Workaround**: export the GPR data in binary VOL format (**\*.vol**)  with the examiner software -> **still experimental**
- [X] **R** internal format (**\*.rds**).
- [X] serialized **Python** object (**\*.pkl**).
- [X] [ENVI band sequential file format](https://www.harrisgeospatial.com/docs/ENVIImageFiles.html) (**\*.dat**, **\*.hdr**).
- [X] ASCII (**\*.txt**): 
  	- either 3-column format (x, t, amplitude) 
    - or matrix-format (without header/rownames)
- [ ] [Terra Zond](http://terrazond.ru/) binary file format (**\*.trz**) -> **we are working on it**
    
See tutorial [Import GPR data](http://emanuelhuber.github.io/RGPR/00_RGPR_import_data/).

    
Do you miss your preferred file format? Send me the file format description with a test file and I will adapt the RGPR-package to support this file format. 


### Supported export file formats

- [X] [Sensors & Software](https://www.sensoft.ca) file format (**\*.dt1**, **\*.hd**).
- [X] R internal format (**\*.rds**).
- [X] ASCII (**\*.txt**): 
- [X] [SEG-Y](https://en.wikipedia.org/wiki/SEG-Y) file format (**\*.sgy**)


### Yes, you can contribute

**This is an ongoing project.**

To report bugs and contribute to the development of [RGPR](http://emanuelhuber.github.io/RGPR), see [how to contribute](https://github.com/emanuelhuber/RGPR/blob/master/CONTRIBUTING.md).

If you have any questions, comments or suggestions, feel free to contact me (in english, french or german):

**emanuel.huber@pm.me**

Thank you!

## Online tutorials
Check the companion website for more info, tutorials, etc.

http://emanuelhuber.github.io/RGPR

## How to install

You must first install [R](https://cran.r-project.org/). Then, in R console, enter the following:

```r
if(!require("devtools")) install.packages("devtools")
devtools::install_github("emanuelhuber/RGPR")
library(RGPR)

frenkeLine00  # data from the package

plot(frenkeLine00)

```

Alternatively, you can download the package as a zip file and install it in R
following these instructions:

https://riptutorial.com/r/example/5556/install-package-from-local-source

## Function overview

**NOTE: this overview is not up to date!! More functions than those listed below are available!** 

The documentation is still incomplete (but check the tutorials, http://emanuelhuber.github.io/RGPR and do not hesitate to contact me if you need addtional informations)

### Input/output functions
* `readGPR()`: reads various GPR file formats (Sensors & Software, MALA, SEG-Y, ImpulseRadar, GSSI, Utsi Electronic, IDS, Tansient technology, serialized Python object, ENVI band sequential file format, ASCII, etc.)
* `writeGPR()`: writes various GPR file format (Sensors & Software, R-format, ASCII, 'xyz')
* `exportPDF()`: exports high quality pdf graphic
* `exportDelineations()`: exports delineations
* `exportFID()`: exports fiducial markers as ASCII-file
* `exportCoord()`: exports coordinates as SpatialLines, SpatialPoints or ASCII-file
* `exportProc()`: exports the processing steps as ASCII-file

### Plot functions

* GPR data
  * 1D/2D: `plot()`, `contour()`, `lines()`, `points()`
  * 3D (plot in openGL): `plot3DRGL()`
  * superposition of all traces: `trPlot()`
* Amplitude: `plotAmpl()`
* Envelope: `plotEnvelope()`
* Spectrum:
  * 1D frequency spectrum: `spec()`
  * 2D frequency spectrum (frequency-wavenumber): `spec(x, type = "f-k")`
* Delineations
  * 2D: `plotDelineations()`
  * 3D: `plot3DDelineations()`
* Velocity layers: `plotVelocityLayers()`
* Structure tensor: `plotTensor()`
* Color palette: `plotPal()`



### GPR data positioning and referencing

* Trace position reversal: `reverse()`
* Vertical trace shift: `traceShift()`
* Georeference coordinates (based on center and rotation angle): `georef()` 
* Interpolate trace position (x, y, z) from known positions: `interpPos()`
* Estimate shift between two parallel profiles: `shiftEst()`

### GPR data analysis and processing
####  GPR data analysis and transforms
* Trace amplitude: `ampl()`, plot trace amplitude: `plotAmpl()`
* Average trace: `traceAverage()`
* Spectrum (f-x and f-k): `spec(x, type = c("f-x", "f-k"))`
* Structure tensor: `strTensor()`, plot structure tensor: `plotTensor()`

#### GPR data interpolation
* Trace interpolation at regularly spaced positions: `regInterpPos()`
* Upsampling (time and position): `upsample()`
* Relative position on the radargramm: `relPos()`

#### GPR signal correction
* DC-shift correction: `dcshift()`
* Low-frequency ('wow') component removal: `dewow()` (type = "MAD", "Gaussian")
* First-break picking: `firstBreak()` (method = "coppens", "threshold",  "MER")
* Shift the traces vertically such that they start at time zero: `time0Cor()`
* Constant offset time correction: `timeCorOffset()` 

#### GPR signal attenuation compensation (gain)
* Linear, power, exponential, ang agc gain: `gain()` (type = "power", "exp", "agc")


#### GPR signal enhancement
* Clip the GPR signal values: `clip()`
* Gamma correction of the GPR signal values: `gammaCorrection()`
* Trace scaling: `traceScaling()`
* Trace filters (1D): `filter1D()`: type = "median", "hampel", "Gaussian"
* Radargramm filters (2D): `filter2D()`: type = "median3x3", "adimpro"
* Trace frequency filter (1D): `fFilter()`: freqency filter, type = 'low','high','bandpass'
* Frequency-wavenumber filter (2D): `fkFilter()`
* Trace (1D) and radargramm (2D) convolution: `conv1D()` and `conv2D()` 
* Deconvolution: `deconv()` (type = "spiking", "wavelet", "min-phase",
  "mixed-phase")
* Phase rotation `rotatePhase()`

#### GPR signal velocity 
* Common-mid point analysis (CMP): `CMPAnalysis()` (method = "semblance", "winsemblance",
  "wincoherence")
* Normal Move-Out correction (NMO): `NMOCor()`

#### GPR data topographic correction and migration 
* Topography correction and topographic Kirchhoff migration: `migration()` (type = "static", "kirchhoff")

#### Generic processing functions
* Apply many processing steps: `papply()`

###  GPR data delineation and mapping
* `delineate()`
* `rmDelineations()<-`
* `delineations()`
* `addDelineation()`
* `plotDelineations3D()`
* `plotDelineations()`
* `identifyDelineation()`


### Miscellaneous
* Operators: `+`, `-`, `*`, `/`, `^`
* Mathematical functions: `max()`, `min()`, `mean()`, `median()`, `summary()`, `range()`, `abs()`, `sign()`, `sqrt()`, `ceiling()`, ...
* Matrix functions: `length()`, `nrow()`, `ncol()`, `dim()`
* Coercion: `as.matrix()`, `as.numeric()`, `as.double()`, `as.list()`, `as.SpatialLines()`, `as.SpatialPoints()`

### Setter/getter functions
* `depthunit()` & `depthunit()<-`
* `description()` & `description()<-`
* `ann()` & `ann()<-`
* `coord()` & `coord()<-`
* `crs()` & `crs()<-`
* `fid()` & `fid()<-`
* `filepath()` & `filepath()<-`
* `pos()` & `pos()<-`
* `posunit()` & `posunit()<-`
* `proc<-()` & `processing` 
* `name()` & `name()<-`
* `values()` & `values()<-`
* `vel()` & `vel()<-` 
* `gethd()`
* `svDate()` & `svDate()<-`


### List of the functions from the class `GPR`
```r
library(RGPR)
mtext <-  showMethods(class="GPR", printTo =FALSE )
i <- grepl('Function', mtext) & grepl('package RGPR', mtext) 
fvec <- gsub( "Function(\\:\\s|\\s\\\")(.+)(\\s\\(|\\\")(.+$)", "\\2", mtext[i] )
fvec
```

### List of the functions from the class `GPRsurvey`
```r
library(RGPR)
mtext <-  showMethods(class="GPRsurvey", printTo =FALSE )
i <- grepl('Function', mtext) & grepl('package RGPR', mtext) 
gvec <- gsub( "Function(\\:\\s|\\s\\\")(.+)(\\s\\(|\\\")(.+$)", "\\2", mtext[i] )
gvec
```

### Incomplete overview of the RGPR-package
```r
?RGPR
```

## Contributions

Thanks to:

-  @jmerc13
