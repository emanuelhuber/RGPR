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
   :caption: Tutorials:
   
   Plot GPR data <tutorials/tutorials_01_plotGPR>























































.. toctree::
   :maxdepth: 1
   :caption: Functions

   amplEnv <functions/amplEnv.md>
   analyticSignal <functions/analyticSignal.md>
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
   createCubeFromGrid <functions/createCubeFromGrid.md>
   crs <functions/crs.md>
   crsUnit <functions/crsUnit.md>
   declip <functions/declip.md>
   delineation <functions/delineation.md>
   depth0 <functions/depth0.md>
   depthToTime <functions/depthToTime.md>
   detectASCIIProp <functions/detectASCIIProp.md>
   dewow <functions/dewow.md>
   dim-GPRvirtual-method <functions/dim-GPRvirtual-method.md>
   dot-adaptiveStripeSmoothing <functions/dot-adaptiveStripeSmoothing.md>
   dot-destripe <functions/dot-destripe.md>
   dot-detect_format <functions/dot-detect_format.md>
   dot-estimateStripeStrength <functions/dot-estimateStripeStrength.md>
   dot-fft_destripe <functions/dot-fft_destripe.md>
   dot-finalize_gridCoords_GPRsurvey <functions/dot-finalize_gridCoords_GPRsurvey.md>
   dot-finalize_replace_write_hdf5 <functions/dot-finalize_replace_write_hdf5.md>
   dot-h5_atomic_replace <functions/dot-h5_atomic_replace.md>
   dot-h5_line_group_id <functions/dot-h5_line_group_id.md>
   dot-h5_lock_acquire <functions/dot-h5_lock_acquire.md>
   dot-h5_lock_release <functions/dot-h5_lock_release.md>
   dot-h5_resolve_line_ids <functions/dot-h5_resolve_line_ids.md>
   dot-h5_safe_name <functions/dot-h5_safe_name.md>
   dot-h5_temp_path <functions/dot-h5_temp_path.md>
   dot-h5_update_survey_with_source <functions/dot-h5_update_survey_with_source.md>
   dot-h5_update_survey <functions/dot-h5_update_survey.md>
   dot-h5_verify_checksums <functions/dot-h5_verify_checksums.md>
   dot-h5_walk_and_read <functions/dot-h5_walk_and_read.md>
   dot-h5_write_data_array <functions/dot-h5_write_data_array.md>
   dot-h5_write_matrix <functions/dot-h5_write_matrix.md>
   dot-h5_write_r_object <functions/dot-h5_write_r_object.md>
   dot-h5_write_vector <functions/dot-h5_write_vector.md>
   dot-kirMigTopo <functions/dot-kirMigTopo.md>
   dot-make_grid_line_coords <functions/dot-make_grid_line_coords.md>
   dot-maybe_interp_gps <functions/dot-maybe_interp_gps.md>
   dot-maybe_set_cmp_mode <functions/dot-maybe_set_cmp_mode.md>
   dot-normalise_dsn <functions/dot-normalise_dsn.md>
   dot-normalize_grid_argument <functions/dot-normalize_grid_argument.md>
   dot-normalize_grid_reverse <functions/dot-normalize_grid_reverse.md>
   dot-normalizeMarkers <functions/dot-normalizeMarkers.md>
   dot-read_dat <functions/dot-read_dat.md>
   dot-read_dt1 <functions/dot-read_dt1.md>
   dot-read_dzt <functions/dot-read_dzt.md>
   dot-read_GPR_line_hdf5 <functions/dot-read_GPR_line_hdf5.md>
   dot-read_intersections_hdf5 <functions/dot-read_intersections_hdf5.md>
   dot-read_ipr <functions/dot-read_ipr.md>
   dot-read_rd3 <functions/dot-read_rd3.md>
   dot-read_rds <functions/dot-read_rds.md>
   dot-read_survey_group_hdf5 <functions/dot-read_survey_group_hdf5.md>
   dot-read_txt <functions/dot-read_txt.md>
   dot-read_vol <functions/dot-read_vol.md>
   dot-readRFDate <functions/dot-readRFDate.md>
   dot-replace_lines_cross_file_hdf5 <functions/dot-replace_lines_cross_file_hdf5.md>
   dot-replace_lines_same_file_hdf5 <functions/dot-replace_lines_same_file_hdf5.md>
   dot-replace_one_GPRsurvey_line_hdf5 <functions/dot-replace_one_GPRsurvey_line_hdf5.md>
   dot-resolve_gssi_antfreq <functions/dot-resolve_gssi_antfreq.md>
   dot-validate_grid_line_ids <functions/dot-validate_grid_line_ids.md>
   dot-validate_optional_grid_argument <functions/dot-validate_optional_grid_argument.md>
   dot-validate_required_grid_argument <functions/dot-validate_required_grid_argument.md>
   dot-write_GPR_line_hdf5 <functions/dot-write_GPR_line_hdf5.md>
   dot-write_GPRsurvey_coords_hdf5 <functions/dot-write_GPRsurvey_coords_hdf5.md>
   dot-write_intersections_hdf5 <functions/dot-write_intersections_hdf5.md>
   dot-write_survey_group_hdf5 <functions/dot-write_survey_group_hdf5.md>
   dot-writeGPR_h5 <functions/dot-writeGPR_h5.md>
   dropDuplicatedCoords <functions/dropDuplicatedCoords.md>
   estimateTime0 <functions/estimateTime0.md>
   filter1D <functions/filter1D.md>
   filter2D <functions/filter2D.md>
   filterEigen <functions/filterEigen.md>
   filterFreq <functions/filterFreq.md>
   findClosestCoord <functions/findClosestCoord.md>
   findIntersection <functions/findIntersection.md>
   firstBreakToTime0 <functions/firstBreakToTime0.md>
   freqFromString <functions/freqFromString.md>
   freqSpectrum <functions/freqSpectrum.md>
   freqSpectrum2D <functions/freqSpectrum2D.md>
   gainAGC <functions/gainAGC.md>
   gainEnv <functions/gainEnv.md>
   gainSEC <functions/gainSEC.md>
   georef <functions/georef.md>
   getAntFreqGSSI <functions/getAntFreqGSSI.md>
   getDepth <functions/getDepth.md>
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
   GPRsurveyInit <functions/GPRsurveyInit.md>
   GPRvirtual-class <functions/GPRvirtual-class.md>
   gridCoords <functions/gridCoords.md>
   hyperbolicTWT <functions/hyperbolicTWT.md>
   instAmpl <functions/instAmpl.md>
   instPhase <functions/instPhase.md>
   int32touint32 <functions/int32touint32.md>
   interpCoords <functions/interpCoords.md>
   interpRegRaster <functions/interpRegRaster.md>
   interpSlices <functions/interpSlices.md>
   intersect <functions/intersect.md>
   is.finite-GPRvirtual <functions/is.finite-GPRvirtual.md>
   is.na <functions/is.na.md>
   isCMP <functions/isCMP.md>
   isCRSGeographic <functions/isCRSGeographic.md>
   isH5Backed <functions/isH5Backed.md>
   isSamplingRegular <functions/isSamplingRegular.md>
   isZTime <functions/isZTime.md>
   length-GPR-method <functions/length-GPR-method.md>
   length-GPRcube-method <functions/length-GPRcube-method.md>
   length-GPRsurvey-method <functions/length-GPRsurvey-method.md>
   line2user <functions/line2user.md>
   lines <functions/lines.md>
   loadCube <functions/loadCube.md>
   Logic-methods <functions/Logic-methods.md>
   logicalNegation-GPRvirtual <functions/logicalNegation-GPRvirtual.md>
   lonLatToUTM <functions/lonLatToUTM.md>
   materialize <functions/materialize.md>
   Math-methods <functions/Math-methods.md>
   Math2-methods <functions/Math2-methods.md>
   mean-GPRvirtual-method <functions/mean-GPRvirtual-method.md>
   median-GPRvirtual-method <functions/median-GPRvirtual-method.md>
   metadata <functions/metadata.md>
   migrate <functions/migrate.md>
   ncol-GPRvirtual-method <functions/ncol-GPRvirtual-method.md>
   NMO <functions/NMO.md>
   NMOcorrect <functions/NMOcorrect.md>
   NMOstack <functions/NMOstack.md>
   NMOstreching <functions/NMOstreching.md>
   nrow-GPRvirtual-method <functions/nrow-GPRvirtual-method.md>
   obbox <functions/obbox.md>
   palCol <functions/palCol.md>
   palGPR <functions/palGPR.md>
   papply <functions/papply.md>
   pathLength <functions/pathLength.md>
   pathRelPos <functions/pathRelPos.md>
   pickFirstBreak <functions/pickFirstBreak.md>
   plot <functions/plot.md>
   plotCMPhyperbolas <functions/plotCMPhyperbolas.md>
   plotTr <functions/plotTr.md>
   plotVel <functions/plotVel.md>
   plotVelLayers <functions/plotVelLayers.md>
   points <functions/points.md>
   print.GPR <functions/print.GPR.md>
   print.GPRcube <functions/print.GPRcube.md>
   print.GPRslice <functions/print.GPRslice.md>
   print.GPRsurvey <functions/print.GPRsurvey.md>
   proc <functions/proc.md>
   project <functions/project.md>
   radon <functions/radon.md>
   radoninv <functions/radoninv.md>
   rayCMP <functions/rayCMP.md>
   raytrace <functions/raytrace.md>
   raytracehalf <functions/raytracehalf.md>
   readCOR <functions/readCOR.md>
   readDT <functions/readDT.md>
   readDT1 <functions/readDT1.md>
   readDZG <functions/readDZG.md>
   readDZT <functions/readDZT.md>
   readDZX <functions/readDZX.md>
   readFID <functions/readFID.md>
   readGEC <functions/readGEC.md>
   readGPGGA <functions/readGPGGA.md>
   readGPR <functions/readGPR.md>
   readGPRsurvey <functions/readGPRsurvey.md>
   readGPS <functions/readGPS.md>
   readGPSUSRADAR <functions/readGPSUSRADAR.md>
   readHD <functions/readHD.md>
   readIPRCOR <functions/readIPRCOR.md>
   readSEG2 <functions/readSEG2.md>
   readSGY <functions/readSGY.md>
   readTopo <functions/readTopo.md>
   readUSRadar <functions/readUSRadar.md>
   readUtsiGPS <functions/readUtsiGPS.md>
   readUtsiGPT <functions/readUtsiGPT.md>
   readVOL <functions/readVOL.md>
   refracAng <functions/refracAng.md>
   register_gpr_format <functions/register_gpr_format.md>
   relPos <functions/relPos.md>
   resampleRegGrid <functions/resampleRegGrid.md>
   resolve_companion_files <functions/resolve_companion_files.md>
   reverse <functions/reverse.md>
   RGPR-package <functions/RGPR-package.md>
   rmDCShift <functions/rmDCShift.md>
   rmStripes <functions/rmStripes.md>
   robustSmooth <functions/robustSmooth.md>
   rotatePhase <functions/rotatePhase.md>
   scale1D <functions/scale1D.md>
   scaleCol <functions/scaleCol.md>
   setDefaultListValues <functions/setDefaultListValues.md>
   setDots <functions/setDots.md>
   setVel <functions/setVel.md>
   shift <functions/shift.md>
   shiftGridLine <functions/shiftGridLine.md>
   shiftTopo <functions/shiftTopo.md>
   shiftToTime0 <functions/shiftToTime0.md>
   show-GPR-method <functions/show-GPR-method.md>
   show-GPRcube-method <functions/show-GPRcube-method.md>
   show-GPRslice-method <functions/show-GPRslice-method.md>
   show-GPRsurvey-method <functions/show-GPRsurvey-method.md>
   simCMP <functions/simCMP.md>
   simWavelet <functions/simWavelet.md>
   smooth2D <functions/smooth2D.md>
   spunit <functions/spunit.md>
   subset-GPR <functions/subset-GPR.md>
   subset-GPRcube <functions/subset-GPRcube.md>
   subset-GPRset <functions/subset-GPRset.md>
   subset-GPRsurvey <functions/subset-GPRsurvey.md>
   summary-GPRvirtual-method <functions/summary-GPRvirtual-method.md>
   Summary-methods <functions/Summary-methods.md>
   switchPolarity <functions/switchPolarity.md>
   time0 <functions/time0.md>
   timeToDepth <functions/timeToDepth.md>
   trapply <functions/trapply.md>
   trimStr <functions/trimStr.md>
   UMTStringToEPSG <functions/UMTStringToEPSG.md>
   unscale <functions/unscale.md>
   UTMToEPSG <functions/UTMToEPSG.md>
   UTMTolonlat <functions/UTMTolonlat.md>
   vel <functions/vel.md>
   velDix <functions/velDix.md>
   velInterp <functions/velInterp.md>
   velLayers <functions/velLayers.md>
   velPick <functions/velPick.md>
   velSetLayers <functions/velSetLayers.md>
   velSmooth <functions/velSmooth.md>
   velSpectrum <functions/velSpectrum.md>
   verboseF <functions/verboseF.md>
   wapply <functions/wapply.md>
   window <functions/window.md>
   writeGPR <functions/writeGPR.md>
   writeProfileVTK <functions/writeProfileVTK.md>
   writeVTK <functions/writeVTK.md>
   xpos <functions/xpos.md>
