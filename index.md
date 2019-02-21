---
layout: default
title: Home
date: 2018-02-12
---

<!--
# RGPR: a free and open-source software package to process and visualise <acronym title="Ground Penetrating Radar">GPR</acronym> data
-->

# RGPR: a free and open-source software package for ground-penetrating radar (<acronym title="Ground Penetrating Radar">GPR</acronym>) data processing

<p class="message">
<acronym title="a R package for Ground Penetrating Radar data processing">RGPR</acronym> is a free and open-source software package to read, export, analyse, process and visualise <strong>ground-penetrating radar (GPR)</strong> data.
 </p>
 
<p class="message"> 
<acronym title="a R package for Ground Penetrating Radar data processing">RGPR</acronym> is written in <a href="https://cran.r-project.org/" title="R Cran">R</a>, a high-level  programming language for statistical computing and graphics that is freely available under the GNU General Public License and runs on Linux, Windows and MacOS. <a href="https://cran.r-project.org/" title="R Cran">R</a> is a highly versatile and extensible language to which   C,   C++  and   Fortran   code  can  be   linked  and  run. Furthermore,  the  R  developer  community  is  very  active  and more  than  10'000  packages  are  hosted  on  the  official  global package    repository    CRAN    (Comprehensive    R    Archive Network, <a href="https://cran.r-project.org/" title="R Cran">https://cran.r-project.org</a>). In 2017, <a href="https://cran.r-project.org/" title="R Cran">R</a> was   ranked  as   the   sixth   top programming   language   by   the   Institute   of   Electrical   and  Electronics Engineers.
</p>

<p class="message">    
<acronym title="a R package for Ground Penetrating Radar data processing">RGPR</acronym> is hosted on <a href="https://github.com/" title="GitHub">GitHub</a> at <a href="https://github.com/emanuelhuber/RGPR" title="RGPR on GitHub">https://github.com/emanuelhuber/RGPR</a>. 
</p>

<p>This R package is still in development, and therefore some of the functions may change in a near future. </p>

<p>To report bugs, contribute to the development of <acronym title="R-package for Ground Penetrating Radar data processing">RGPR</acronym>, see <a href="https://github.com/emanuelhuber/RGPR/blob/master/CONTRIBUTING.md" title="how to contribute">how to contribute</a>.</p>

<p>If you have any questions, comments or suggestions, feel free to contact me (in english, french or german):<br/><a href="mailto:emanuel.huber@alumni.ethz.ch">emanuel.huber@alumni.ethz.ch</a></p>


## Tutorials

1. [Import GPR data](00_RGPR_import_data)
2. [Basic GPR data processing](01_RGPR_tutorial_basic-processing)
3. [Pipe processing](01_zRGPR_tutorial_processing-with-pipe-operator)
4. [Add coordinates to GPR data](02_RGPR_tutorial_RGPR-survey)
5. [GPR data migration](03_RGPR_tutorial_migration)
6. [Some explanations on the class GPR](04_RGPR_tutorial_RGPR-object)


## How to install/load


1. Download R from the [R Cran website](http://cran.r-project.org) and install it.
2. Optionally install a R-editor:
  * [Rstudio](https://www.rstudio.com/) (*Recommended*)
  * [RKward](https://rkward.kde.org/)
  * [Notepad++](https://notepad-plus-plus.org/) combined with [NppToR](https://sourceforge.net/projects/npptor/)
3. Open the R-editor (e.g., Rstudio)
4. Install `RGPR`

    ```r
    if(!require("devtools")) install.packages("devtools")
    devtools::install_github("emanuelhuber/RGPR")
    ```


**New to R?** Try this *free* interactive course from DataCamp: [Introduction to R](https://www.datacamp.com/courses/free-introduction-to-r)

## Notes

### Supported file formats:

- [x] Sensors & Software file format (**\*.dt1**, **\*.hd**).
- [x] MALA file format (**\*.rd3**, **\*.rad**).
- [x] RadSys Zond GPR file format (**\*.sgy**). **WARNING: it is not like the usual SEG-Y file format**).
- [x] ImpulseRadar format (**\*.iprb**, **\*.iprh**).
- [X] GSSI file format (**\*.dtz**).
- [X] ASCII (**\*.txt**): 
  	
  	- either 3-column format (x, t, amplitude) 
    - or matrix-format (without header/rownames)
    
- [X] R internal format (**\*.rds**).
- [X] ENVI band sequential file format (**\*.dat**, **\*.hdr**).
- [ ] SEG-Y -> **we are working on it**
    

    
Do you miss your preferred file format? Send me the file format description with a test file and I will adapt the RGPR-package to support this file format. 

### Current limitations

RGPR only support reflection data such as surface-based GPR data (no support for cross-borehole GPR data)

## Coming soon

We are developing a <a href="https://github.com/emanuelhuber/RGPR/tree/develop">new version of RGPR</a> that will handle three-dimensional GPR data as well as multi-dimensional transformations, see <a href="2018_huber-and-hans_RGPR-new-R-package_notes.pdf">our objectives</a>.

<!--
$$\forall x \in R$$
-->
