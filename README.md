# [RGPR](http://emanuelhuber.github.io/RGPR)

## Open-source GPR processing in [R](https://cran.r-project.org/)

**Read · Process · Visualise · Analyse · Interpret**

[RGPR](http://emanuelhuber.github.io/RGPR) is a **free and open-source R package** for reading, processing, visualising and analysing ground-penetrating radar (GPR) data.

Work with data from different GPR systems, build your own processing workflows, automate repetitive tasks and keep your entire workflow in code.

**No proprietary processing software required. No black box. Your data, your workflow, your code.**


[![Documentation](https://img.shields.io/badge/docs-online-blue)](https://emanuelhuber.github.io/RGPR/)
[![GitHub stars](https://img.shields.io/github/stars/emanuelhuber/RGPR?style=social)](https://github.com/emanuelhuber/RGPR)



---

## Why RGPR?

GPR processing often involves many small steps: filtering, gain, background removal, time-zero correction, migration, coordinate correction and more.

With graphical software, these steps can be difficult to reproduce or automate.

With RGPR, the workflow is simply **R code**:

```r
library(RGPR)

gpr <- readGPR("profile.DT1")

gpr <- gpr |>
  dcshift() |>
  dewow() |>
  gain(type = "agc") |>
  fFilter(type = "bandpass")

plot(gpr)
```

The same workflow can be:

* **reproduced** on another dataset;
* **automated** for many profiles;
* **shared** with colleagues;
* **modified** when your processing needs change;
* **tracked** in Git;
* **combined** with the rest of the R ecosystem.

### Open source means control

RGPR is open source. You can inspect how processing is performed, modify existing functions, add your own methods and contribute improvements to the project.

**If you have any questions, comments or suggestions, feel free to contact me (in English, French or German):**
**emanuel.huber@pm.me**

> I am developing this package on my free time as a gift to the GPR community. 
> Any support will be appreciated! 


[![](https://bmc-cdn.nyc3.digitaloceanspaces.com/BMC-button-images/custom_images/orange_img.png)](https://www.buymeacoffee.com/EmanuelHuber)

[Buy me a coffee with Paypal](https://www.paypal.com/donate/?hosted_button_id=ZGSWR9SLV4MM2)

## Table of content

## Table of content

<!--ts-->

	* [Features](#features)
	  * [📂 Read GPR data](#-read-gpr-data)
	    * [Supported file formats (read only)](#supported-file-formats-read-only)
	    * [Supported export file formats](#supported-export-file-formats)
	    * [Format currently not supported](#format-currently-not-supported)
	  * [📡 Process radargrams](#-process-radargrams)
	  * [🚀 Build reproducible processing pipelines](#-build-reproducible-processing-pipelines)
	  * [📐 Velocity analysis and migration](#-velocity-analysis-and-migration)
	  * [🗺️ Work with spatial GPR surveys](#-work-with-spatial-gpr-surveys)
	  * [🧊 Explore GPR data in 3D](#-explore-gpr-data-in-3d)
	  * [✏️ Interpret your data](#-interpret-your-data)
	* [Installation](#installation)
	* [Try RGPR in five minutes](#try-rgpr-in-five-minutes)
	* [Documentation](#documentation)
	  * [Getting started](#getting-started)
	  * [Spatial GPR](#spatial-gpr)
	  * [Advanced processing](#advanced-processing)
	* [Open source and collaboration](#open-source-and-collaboration)
	* [Reproducibility](#reproducibility)
	* [How to cite](#how-to-cite)
	  * [How to cite](#how-to-cite)
	  * [Bibtex format](#bibtex-format)
	* [License](#license)
	* [Get involved](#get-involved)

<!--te-->





---

# Features

## 📂 Read GPR data


RGPR aims to make your data accessible regardless of the software originally used to acquire it.


### Supported file formats (read only):

- [X] [Sensors & Software](https://www.sensoft.ca) file format (**\*.dt1**, **\*.hd**, **\*.gps**).
- [X] [MALA](https://www.guidelinegeo.com/mala-ground-penetrating-radar-gpr/) file format (**\*.rd3**, **\*.rd7**, **\*.rad**, **\*.cor**).
- [X] [ImpulseRadar](https://www.impulseradar.se) file format (**\*.iprb**, **\*.iprh**, **\*.cor**, **\*.time**, **\*.mrk**).
- [X] [GSSI](https://www.geophysical.com) file format (**\*.dzt**, **\*.dzx**).
- [X] [Geomatrix Earth Science Ltd](https://www.geomatrix.co.uk/) file format (Utsi Electronics format) for the **GroundVue 3**, **7**, **100**, **250** and **400** as well as for the **TriVue** devices (**\*.dat**, **\*.hdr**, **\*.gpt**, **\*.gps**).
- [x] [Radar Systems, Inc.](http://www.radsys.lv) Zond file format (**\*.sgy**). **WARNING: it is not a version of the SEG-Y file format**.
- [X] [IDS](https://idsgeoradar.com/) file format (**\*.dt**, **\*.gec**).
- [X] [Transient Technologies](https://viy.ua/) file format (**\*.sgpr**).
- [X] [US Radar](https://usradar.com/) file format (**\*.RA1**, **\*.RA2** or **\*.RAD**)
- [X] [SEG-Y](https://en.wikipedia.org/wiki/SEG-Y) file format developed by the Society of Exploration Geophysicists (SEG) for storing geophysical data (**\*.sgy**), also used by [Easy Radar USA](https://easyradusa.com)
- [X] [Geotech OKO](https://geotechru.com/) file format (**\*.GPR**, **\*.GPR2**).
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
    
See tutorial [Import GPR data](https://emanuelhuber.github.io/RGPR/00_RGPR_tutorial_import-GPR-data/).

    
### Supported export file formats

- [X] [Sensors & Software](https://www.sensoft.ca) file format (**\*.dt1**, **\*.hd**).
- [X] R internal format (**\*.rds**).
- [X] ASCII (**\*.txt**): 
- [X] [SEG-Y](https://en.wikipedia.org/wiki/SEG-Y) file format (**\*.sgy**)


### Format currently not supported

If your GPR format is not currently supported, contributions are welcome.

When possible, provide:

1. a small example dataset;
2. information about the file format;
3. information about the acquisition system;
4. an example of the expected result.

---

## 📡 Process radargrams

A broad collection of processing tools is available:

* time-zero correction
* first-break estimation
* DC-shift correction
* dewow
* background removal
* trace averaging
* frequency filtering
* f-k filtering
* median filtering
* gain functions
* eigenimage filtering
* phase rotation
* convolution
* deconvolution
* resampling

Processing functions can be combined into workflows and applied repeatedly to multiple datasets.

---

## 🚀 Build reproducible processing pipelines

Instead of manually repeating the same processing steps, define them once:

```r
pipeline <- list(
  dcshift,
  dewow,
  function(x) gain(x, type = "agc"),
  function(x) fFilter(x, type = "bandpass")
)

processed <- papply(gpr, pipeline)
```

Your processing recipe becomes part of your project rather than a sequence of clicks that has to be remembered.

---

## 📐 Velocity analysis and migration

Tools are available for:

* CMP/WARR analysis
* velocity estimation
* NMO correction
* Kirchhoff migration
* topographic migration
* hyperbola fitting

```r
plot(gpr)

# Estimate or select velocity

gpr_mig <- migration(gpr, ...)
```

---

## 🗺️ Work with spatial GPR surveys

Combine individual profiles into spatial surveys using `GPRsurvey`.

Work with:

* trace coordinates;
* GPS information;
* coordinate reference systems;
* survey geometry;
* profile positions;
* spatial interpolation;
* time/depth slices.

---

## 🧊 Explore GPR data in 3D

Combine spatially distributed GPR profiles and create 3D representations of your data.

```r
cube <- interpSlices(SU, dx = 0.05, dy = 0.05, dz = 0.05, h = 6)

plot(cube)
```

Explore GPR volumes, time/depth slices and interpreted features in 3D.

---

## ✏️ Interpret your data

Delineate and analyse features directly from GPR profiles.

Use interpretations to:

* trace reflections;
* identify horizons;
* extract coordinates;
* analyse interpreted features;
* visualise interpretations in 2D and 3D.

---



# Installation

You must first install [R](https://cran.r-project.org/). Then, in R console, enter the following:

Install the development version from GitHub:

```r
install.packages("remotes")

remotes::install_github("emanuelhuber/RGPR")
```

---

## Try RGPR in five minutes

RGPR includes example GPR datasets so that you can start without finding your own data. In R console, enter the following:

```r
library(RGPR)

data(frenkeLine00)

plot(frenkeLine00)
```

Apply a simple processing workflow:

```r
gpr <- frenkeLine00 |>
  dcshift() |>
  dewow()

plot(gpr)
```

From there, explore filtering, gain, migration, spatial positioning, interpolation and interpretation.

---

# Documentation

The documentation contains tutorials and examples covering the main RGPR workflow.

**[Read the RGPR documentation →](https://emanuelhuber.github.io/RGPR/)**

## Getting started

* [Import GPR data](https://emanuelhuber.github.io/RGPR/00_RGPR_tutorial_import-GPR-data/)
* [Plot GPR data](01_RGPR_tutorial_plot-GPR-data)
* [Basic GPR data processing](02_RGPR_tutorial_basic-GPR-data-processing)
* [Pipe processing](03_RGPR_tutorial_processing-GPR-data-with-pipe-operator)




## Spatial GPR

* [Add coordinates to GPR data](04_RGPR_tutorial_GPR-data-survey)
* [Time/depth slice interpolation](05_RGPR_tutorial_GPR-data-time-slice-interpolation-3D)


## Advanced processing

* [GPR data migration](07_RGPR_tutorial_GPR-data-migration)
* [Hyperbola fitting](09_RGPR_tutorial_hyperbola_fitting)
* [Deconvolution](10_RGPR_mixed-phase-wavelet-deconvolution)




# Open source and collaboration

RGPR is developed openly and welcomes contributions from the GPR community.

Contributions can include:

* bug reports;
* new file-format readers;
* processing algorithms;
* performance improvements;
* tests;
* documentation;
* examples;
* datasets;
* tutorials;
* translations.

You don't need to be an expert R developer to contribute.

A small example dataset, a bug report or an improvement to the documentation can be just as valuable as a new algorithm.

---

# Reproducibility

A major advantage of using RGPR is that your processing workflow can live alongside your data and analysis code.

For example:

```text
my-gpr-project/
│
├── data/
│   ├── raw/
│   └── processed/
│
├── R/
│   └── processing.R
│
├── figures/
│
├── results/
│
└── README.md
```

Your processing is no longer hidden inside a software project file.

It is code that can be inspected, version-controlled, shared and rerun.

---

# Citation


## How to cite

> E. Huber and G. Hans (2018) RGPR — An open-source package to process and visualize GPR data. 17th International Conference on Ground Penetrating Radar (GPR), Switzerland, Rapperswil, 18-21 June 2018, pp. 1-4.
> doi: [10.1109/ICGPR.2018.8441658](https://doi.org/10.1109/ICGPR.2018.8441658)

[PDF](https://emanuelhuber.github.io/publications/2018_huber-and-hans_RGPR-new-R-package_notes.pdf) [Poster](https://emanuelhuber.github.io/publications/poster_2018_huber-and-hans_RGPR-new-open-source-package.pdf)

## Bibtex format

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
---

# License

RGPR is free and open-source software released under the GNU General Public License.

---

# Get involved

Have an idea?

Found a bug?

Need support for another GPR format?

Want to contribute?

**Open an issue, start a discussion or submit a pull request.**

[GitHub →](https://github.com/emanuelhuber/RGPR)

**RGPR is built openly, for everyone working with GPR data.**

