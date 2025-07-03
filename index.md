---
layout: default
title: Home
date: 2018-02-12
---

<!--
# RGPR: a free and open-source software package to process and visualise <acronym title="Ground Penetrating Radar">GPR</acronym> data
-->

# RGPR: a free and open-source software package for ground-penetrating radar (<acronym title="Ground Penetrating Radar">GPR</acronym>) data processing

<!--
<p class="message"> 
I am developing this package on my free time as a gift to the GPR community. Any support will be appreciated!
</p>


[![](https://bmc-cdn.nyc3.digitaloceanspaces.com/BMC-button-images/custom_images/orange_img.png)](https://www.buymeacoffee.com/EmanuelHuber)

-->
<p class="message"> 
 am developing this package on my free time as a gift to the GPR community. Any support will be appreciated! 
</p>

[![](https://bmc-cdn.nyc3.digitaloceanspaces.com/BMC-button-images/custom_images/orange_img.png)](https://www.buymeacoffee.com/EmanuelHuber)

[Buy me a coffee with Paypal](https://www.paypal.com/donate/?hosted_button_id=ZGSWR9SLV4MM2)


## About

<p>
<acronym title="a R package for Ground Penetrating Radar data processing">RGPR</acronym> is a free and open-source software package to read, export, analyse, process and visualise <strong>ground-penetrating radar (GPR)</strong> data.
 </p>
 
<p> 
<acronym title="a R package for Ground Penetrating Radar data processing">RGPR</acronym> is written in <a href="https://cran.r-project.org/" title="R Cran">R</a>, a high-level  programming language for statistical computing and graphics that is freely available under the GNU General Public License and runs on Linux, Windows and MacOS. <a href="https://cran.r-project.org/" title="R Cran">R</a> is a highly versatile and extensible language to which   C,   C++  and   Fortran   code  can  be   linked  and  run. Furthermore,  the  R  developer  community  is  very  active  and more  than  10'000  packages  are  hosted  on  the  official  global package    repository    CRAN    (Comprehensive    R    Archive Network, <a href="https://cran.r-project.org/" title="R Cran">https://cran.r-project.org</a>). In 2017, <a href="https://cran.r-project.org/" title="R Cran">R</a> was   ranked  as   the   sixth   top programming   language   by   the   Institute   of   Electrical   and  Electronics Engineers.
</p>

<p>    
<acronym title="a R package for Ground Penetrating Radar data processing">RGPR</acronym> is hosted on <a href="https://github.com/" title="GitHub">GitHub</a> at <a href="https://github.com/emanuelhuber/RGPR" title="RGPR on GitHub">https://github.com/emanuelhuber/RGPR</a>. 
</p>

<p>This R package is still in development, and therefore some of the functions may change in a near future. </p>

<p>To report bugs, contribute to the development of <acronym title="R-package for Ground Penetrating Radar data processing">RGPR</acronym>, see <a href="https://github.com/emanuelhuber/RGPR/blob/master/CONTRIBUTING.md" title="how to contribute">how to contribute</a>.</p>


<p>If you have any questions, comments or suggestions, feel free to contact me (in english, french or german):<br/><a href="mailto:emanuel.huber@pm.me">emanuel.huber@pm.me</a></p>


## Tutorials

1. [Import GPR data](00_RGPR_tutorial_import-GPR-data)
2. [Plot GPR data](01_RGPR_tutorial_plot-GPR-data)
3. [Basic GPR data processing](02_RGPR_tutorial_basic-GPR-data-processing)
4. [Pipe processing](03_RGPR_tutorial_processing-GPR-data-with-pipe-operator)
5. [Add coordinates to GPR data](04_RGPR_tutorial_GPR-data-survey)
6. [Time/depth slice interpolation](05_RGPR_tutorial_GPR-data-time-slice-interpolation-3D)
7. [GPR data migration](07_RGPR_tutorial_GPR-data-migration)
8. [Hyperbola fitting](09_RGPR_tutorial_hyperbola_fitting)
9. [Deconvolution](10_RGPR_mixed-phase-wavelet-deconvolution)
10. [Some explanations on the classes of RGPR](70_RGPR_tutorial_class-GPR)
11. [Free GPR data to download](80_RGPR_GPR-data-free-to-download)
12. [RGPR in the web](99_GPR_on_the_web)


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


    Alternatively, you can download the package as a zip file and install it in R
    following these instructions:

    [https://riptutorial.com/r/example/5556/install-package-from-local-source](https://riptutorial.com/r/example/5556/install-package-from-local-source)
    
    or these:
    
    https://stackoverflow.com/questions/30989027/how-to-install-a-package-from-a-download-zip-file/30989367#30989367


**New to R?** Try this *free* interactive course from DataCamp: [Introduction to R](https://www.datacamp.com/courses/free-introduction-to-r)

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
- [X] [US Radar](https://usradar.com/) file format (**\*.RA1**, **\*.RA2** or **\*.RAD**).
- [X] [Geotech OKO](https://geotechru.com/) file format (**\*.GPR**, **\*.GPR2**).
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




## Coming soon

We are developing a <a href="https://github.com/emanuelhuber/RGPR/tree/develop">new version of RGPR</a> that will handle three-dimensional GPR data as well as multi-dimensional transformations, see <a href="2018_huber-and-hans_RGPR-new-R-package_notes.pdf">our objectives</a>.

Note that you can already interpolate GPR data (2D) into a 3D cube with the current RGPR version! Do not hesitate to contact me if you need more informations...

<!--
$$\forall x \in R$$
-->
