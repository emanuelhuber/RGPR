.. documentation master file, created by sphinx-quickstart 
   You can adapt this file completely to your liking, but it should at least
   contain the root `toctree` directive.

reStructuredText
================================

.. raw:: html

    <style> .red {color:red} </style>

.. role:: red

This main document is in `'reStructuredText' ("rst") format
<https://www.sphinx-doc.org/en/master/usage/restructuredtext/index.html>`_,
which differs in many ways from standard markdown commonly used in R packages.
``rst`` is richer and more powerful than markdown. The remainder of this main
document demonstrates some of the features, with links to additional ``rst``
documentation to help you get started. The definitive argument for the benefits
of ``rst`` over markdown is the `official language format documentation
<https://www.python.org/dev/peps/pep-0287/>`_, which starts with a very clear
explanation of the `benefits
<https://www.python.org/dev/peps/pep-0287/#benefits>`_.

Examples
--------

All of the following are defined within the ``docs/index.rst`` file. Here is
some :red:`coloured` text which demonstrates how raw HTML commands can be
incorporated. The following are examples of ``rst`` "admonitions":

.. note::

    Here is a note

    .. warning::

        With a warning inside the note

.. seealso::

    The full list of `'restructuredtext' directives <https://www.sphinx-doc.org/en/master/usage/restructuredtext/directives.html>`_ or a similar list of `admonitions <https://restructuredtext.documatt.com/admonitions.html>`_.

.. centered:: This is a line of :red:`centered text`

.. hlist::
   :columns: 3

   * and here is
   * A list of
   * short items
   * that are
   * displayed
   * in 3 columns

The remainder of this document shows three tables of contents for the main
``README`` (under "Introduction"), and the vignettes and R directories of
a package. These can be restructured any way you like by changing the main
``docs/index.rst`` file. The contents of this file -- and indeed the contents
of any `readthedocs <https://readthedocs.org>`_ file -- can be viewed by
clicking *View page source* at the top left of any page.

.. toctree::
   :maxdepth: 1
   :caption: Introduction:


   intro.md







.. toctree::
   :maxdepth: 1
   :caption: Functions

   angle <functions/angle.md>
   ann <functions/ann.md>
   antsep <functions/antsep.md>
   antSepFromAntFreq <functions/antSepFromAntFreq.md>
   apply-GPRvirtual-method <functions/apply-GPRvirtual-method.md>
   Arith-methods <functions/Arith-methods.md>
   as.sf <functions/as.sf.md>
   as.spatialLines <functions/as.spatialLines.md>
   as.spatialPoints <functions/as.spatialPoints.md>
   atan2 <functions/atan2.md>
   bits2volt <functions/bits2volt.md>
   buffer <functions/buffer.md>
   checkArg <functions/checkArg.md>
   clipData <functions/clipData.md>
   clippedData <functions/clippedData.md>
   CMPhyperbolas <functions/CMPhyperbolas.md>
   colSums <functions/colSums.md>
   Compare-methods <functions/Compare-methods.md>
   Complex-methods <functions/Complex-methods.md>
   contour <functions/contour.md>
   convertTimeToDepth <functions/convertTimeToDepth.md>
   convexhull <functions/convexhull.md>
   coordinates <functions/coordinates.md>
   crs <functions/crs.md>
   crsUnit <functions/crsUnit.md>
   declip <functions/declip.md>
   depth0 <functions/depth0.md>
   depthToTime <functions/depthToTime.md>
   detectASCIIProp <functions/detectASCIIProp.md>
   dim-GPRvirtual-method <functions/dim-GPRvirtual-method.md>
   dropDuplicatedCoords <functions/dropDuplicatedCoords.md>
   findClosestCoord <functions/findClosestCoord.md>
   freqFromString <functions/freqFromString.md>
   georef <functions/georef.md>
   getAntFreqGSSI <functions/getAntFreqGSSI.md>
   getFName <functions/getFName.md>
   getGPR <functions/getGPR.md>
   getLonLatFromGPGGA <functions/getLonLatFromGPGGA.md>
   getUTMzone <functions/getUTMzone.md>
   getVel <functions/getVel.md>
   GPR-class <functions/GPR-class.md>
   GPRcoercion <functions/GPRcoercion.md>
   GPRcube-class <functions/GPRcube-class.md>
   GPRset-class <functions/GPRset-class.md>
   GPRslice-class <functions/GPRslice-class.md>
   GPRsurvey-class <functions/GPRsurvey-class.md>
   GPRsurvey <functions/GPRsurvey.md>
   GPRvirtual-class <functions/GPRvirtual-class.md>
   hyperbolicTWT <functions/hyperbolicTWT.md>
   interpCoords <functions/interpCoords.md>
   interpSlices <functions/interpSlices.md>
   intersect <functions/intersect.md>
   is.finite-GPRvirtual <functions/is.finite-GPRvirtual.md>
   is.na <functions/is.na.md>
   isCMP <functions/isCMP.md>
   isCRSGeographic <functions/isCRSGeographic.md>
   isZTime <functions/isZTime.md>
   length-GPR-method <functions/length-GPR-method.md>
   length-GPRcube-method <functions/length-GPRcube-method.md>
   length-GPRsurvey-method <functions/length-GPRsurvey-method.md>
   line2user <functions/line2user.md>
   lines <functions/lines.md>
   Logic-methods <functions/Logic-methods.md>
   logicalNegation-GPRvirtual <functions/logicalNegation-GPRvirtual.md>
   lonLatToUTM <functions/lonLatToUTM.md>
   Math-methods <functions/Math-methods.md>
   Math2-methods <functions/Math2-methods.md>
   mean-GPRvirtual-method <functions/mean-GPRvirtual-method.md>
   median-GPRvirtual-method <functions/median-GPRvirtual-method.md>
   metadata <functions/metadata.md>
   ncol-GPRvirtual-method <functions/ncol-GPRvirtual-method.md>
   NMO <functions/NMO.md>
   NMOcorrect <functions/NMOcorrect.md>
   NMOstack <functions/NMOstack.md>
   NMOstreching <functions/NMOstreching.md>
   nrow-GPRvirtual-method <functions/nrow-GPRvirtual-method.md>
   obbox <functions/obbox.md>
   palCol <functions/palCol.md>
   palGPR <functions/palGPR.md>
   pathLength <functions/pathLength.md>
   pathRelPos <functions/pathRelPos.md>
   plot <functions/plot.md>
   plotCMPhyperbolas <functions/plotCMPhyperbolas.md>
   plotTr <functions/plotTr.md>
   plotVel <functions/plotVel.md>
   points <functions/points.md>
   print.GPR <functions/print.GPR.md>
   print.GPRcube <functions/print.GPRcube.md>
   print.GPRslice <functions/print.GPRslice.md>
   print.GPRsurvey <functions/print.GPRsurvey.md>
   proc <functions/proc.md>
   project <functions/project.md>
   readDT1 <functions/readDT1.md>
   readDZG <functions/readDZG.md>
   readDZT <functions/readDZT.md>
   readDZX <functions/readDZX.md>
   readFID <functions/readFID.md>
   readGPGGA <functions/readGPGGA.md>
   readGPR <functions/readGPR.md>
   readGPS <functions/readGPS.md>
   readHD <functions/readHD.md>
   readTopo <functions/readTopo.md>
   relPos <functions/relPos.md>
   RGPR-package <functions/RGPR-package.md>
   setDefaultListValues <functions/setDefaultListValues.md>
   setDots <functions/setDots.md>
   setVel <functions/setVel.md>
   shift <functions/shift.md>
   shiftToTime0 <functions/shiftToTime0.md>
   show-GPR-method <functions/show-GPR-method.md>
   show-GPRcube-method <functions/show-GPRcube-method.md>
   show-GPRslice-method <functions/show-GPRslice-method.md>
   show-GPRsurvey-method <functions/show-GPRsurvey-method.md>
   simWavelet <functions/simWavelet.md>
   smooth2D <functions/smooth2D.md>
   spunit <functions/spunit.md>
   subset-GPR <functions/subset-GPR.md>
   subset-GPRcube <functions/subset-GPRcube.md>
   subset-GPRset <functions/subset-GPRset.md>
   subset-GPRsurvey <functions/subset-GPRsurvey.md>
   summary-GPRvirtual-method <functions/summary-GPRvirtual-method.md>
   Summary-methods <functions/Summary-methods.md>
   timeToDepth <functions/timeToDepth.md>
   trimStr <functions/trimStr.md>
   UMTStringToEPSG <functions/UMTStringToEPSG.md>
   UTMToEPSG <functions/UTMToEPSG.md>
   UTMTolonlat <functions/UTMTolonlat.md>
   vel <functions/vel.md>
   velDix <functions/velDix.md>
   velInterp <functions/velInterp.md>
   velPick <functions/velPick.md>
   velSmooth <functions/velSmooth.md>
   velSpectrum <functions/velSpectrum.md>
   verboseF <functions/verboseF.md>
   window <functions/window.md>
   writeGPR <functions/writeGPR.md>
   xpos <functions/xpos.md>
