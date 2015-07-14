


cat("******* RGPR *******\n")
cat("version 0.0.8\n")
cat("15.03.2015\n")
cat("Emanuel Huber, emanuel.huber@unibas.ch\n")
cat("********************\n")

#---changes v0.0.8.1---#
#	- GPR::gain -> rename gain_geospreading in power!
#	- lines.GPR
#	- GPR::exportProc
#	- GPR::exportCoord

#---changes v0.0.8---#
# GPR::reverse (check pos)
# plotWig > add_ann
#---changes v0.0.7---#
# local orientation added
# deconvolution
# fx-filter
# medianFilter


# THINK: GPR is EITHER RASTER OR VECTOR...
# THINK: PRIVATE FUNCTION .myfunction()

# FIX ME!
#	- GPR::exportCoord & GPRsurvey::exportCoord -> export as points -> use point.data.frame (add z-information)
#	- GPRsurvey::plot3D -> use function "inPoly" to plot in 3D only a selection of traces from GPRsurvey
#	- GPR::migration/topoShift -> integrate the function time2depth or depth2time!!!!
#	- GPR::delineation -> use the same scheme for both raster and wiggles!
#	- GPR::interpTraces -> use a raster to get the v-elevation!
#	- plot.GPR function
#		-> check if all the "..." parameters corresponds to the possible paramters. If not > error!
#		-> check option image and use the "raster" option. Check grid with smooth image...
#	- GPR::gain -> t0=NULL then t0 <- mean(time0)
#	- gain: t0=NULL then t0 <- mean(time0)
#	- scaling after gain!
# 	- different taper window: cos, triang, hamming, bartlett, limtaper, hann, flattop....
#	- GPR::spec(type "f-t") -> use power and log scale (?)
#	- GPR::plotAmpl -> option log y-axis
#	- check how time0 is used
#	- GPR::exportPDF -> add processing steps!
#	- consistency of the function arguments:
#		- path, filename, filepath...

# FIX ME!!
#		-> check FIX ME!!
#		-> plot3D -> add a zlim or a max depth!!!
#		-> add x@pts="numeric"  points number ??
#		-> export(type=PDF) use function plot (wiggles)!
#		-> with generic method: use exactly the same arguments
#				use args(plot) to know the arguments
#		-> rename "fid" into  "fiducial marks"	=> fid
# Idea for trace to trace processing:
# -> list of all function with their argument to use
# -> use a FUN_WRAP function : apply(GPR$data,2,FUN_WRAP)
# -> FUN_WRAP process each single trace according to the list of function and theirs args.


#======== TODO =========#
# check book "Near-surface Geophysics, vol 13"

#--- STRUCTURE ---#
# - keep @ann > for intersections
# - add @version
# - add @vDatum	> for vertical geodesic datum


#--- declipping ---#
# - least-square polynomial interpolation
# - AR/ARMA model
# - PCA/SVD decomposition
# - DCT (cf. R package)
# - deconvolution/convolution

#--- gain functions ---#
# - rms AGC
# - instantaneous AGC
# - automatic gain > check MATGPR
# - tpow gain (frequency-dependent gain, Fowler and Clearbout

#--- editing functions ---#
# - merge (or cbind)
# - rbind()
# - align GPR profile with FFT

#--- resolution ---#
# - bicubique interpolation
# - L1-DFT-based interpolation (L1 norm, sparsity)
# - anti-alisasing filter

#--- migration ---#
# - Kirchhoff
# - SAR (cf book with matlab)
# - f-k migration
# - phase-shift migration

#--- PEF-filter ---#
# - f-xy filter
# - t-x
# - f-x
# - ...

#--- deconvolution ---#
# - despiking
# - wiener
# - BP
# - mixed-phase

#--- denoising ---#
# - denoising tensor field based on Riemannian geometry (Mathematical Methods for Signal and Image Analysis and Representation)
# - adaptative smoothing
# - hampel filter
# - bp_salsa_d
# - n-pt median filter (scalar median filter SMF)
# - correction for trace-to-trace jitter
# - Radon transform

#--- local orientation ---#
# - local orientation
# - angular unconformity

#--- plot functions ---#
# - plot3Draster > to plot raster data in 3D with the GPR profiles
# - plot3D > check alternative to RGL for high-quality figures

#--- attributes ---#
# - instantaneous (phase, amplitude, cf. Hilbert)
# - coherency
# - energy


#--- CMP ---#
# - 

#--- miscs ---#
# - function to read NMEA-GPS string
# - GPRsurvey::bbox
# - GPRsurvey::convexhull
# - GPRsurvey -> intersections (also as spatialPoints)
# - pre-defined color bar with funny names...
# - GPRsurvey -> add spatial data (e.g. borehole) > closest distance...
# - time function ->  gpr <- readGPR(file.choose());	as.POSIXct(gpr@time, origin = "1970-01-01")
# - proc(gpr) -> return x@proc
# - vel(gpr) -> return x@vel



# TAPER WINDOWS
# tapertype = MinPhase.tapertype;
# tabpZL = nlags;

# switch lower(tapertype)
    # case 'cos'
        # taper = (sin(linspace(0,pi/2,tabpZL)).^2)';
        # ACFoutput(1:tabpZL) = ACFoutput(1:tabpZL).*taper;
        # ACFoutput(end-tabpZL+1:end) = ACFoutput(end-tabpZL+1:end).*flipud(taper);
    # case 'triang'
        # taper = triang(2*tabpZL+1);
        # ACFoutput = ACFoutput.*taper;
    # case 'hamming'
        # taper = hamming(2*tabpZL+1);
        # ACFoutput = ACFoutput.*taper;
    # case 'bartlett'
        # taper = bartlett(2*tabpZL+1);
        # ACFoutput = ACFoutput.*taper;
    # case 'none'
        # % taper = ones(2*tabpZL+1,1);
        # % ACFoutput = ACFoutput-ACFoutput(end);
    # case 'limtaper'
        # % taper = ones(size(ACFoutput));
        # % taper = cs_taper(taper,'hann',round(length(ACFoutput)*0.25));
        # % ACFoutput = cs_taper(ACFoutput,'cos',round(length(ACFoutput)*0.25));
        # ACFoutput = cs_taper(ACFoutput,'cos',round(length(ACFoutput)*0.05));
    # case 'hann'
        # ACFoutput = cs_taper(ACFoutput,'hann');
    # case 'flattop'
        # taper = flattopwin(2*tabpZL+1);
        # ACFoutput = ACFoutput.*taper;
# end
# -------------------------------------------#
# --------- load_install_package ------------#
# -------------------------------------------#
# @name	load_install_package (load or install automatically a package)
# @description This function load or install automatically a package
# @usage load_install_package(c("foreach","MASS","doParallel"))

# @date 08.11.2012 19:45
# @auteur Emanuel Huber
# @source = http://www.r-bloggers.com/loading-andor-installing-packages-programmatically/
# @param [c(text)] 	package_names 				(each column represent a trace, each row a time step / depth step)
# @return void
# -------------------------------------------
load_install_package <- function(package_names){
  for(package_name in package_names){
    if(!is_installed(package_name)){
       install.packages(package_name,repos="http://lib.stat.cmu.edu/R/CRAN")
    }
    library(package_name,character.only=TRUE,quietly=TRUE,verbose=FALSE)
  }
}

is_installed <- function(mypkg) is.element(mypkg, installed.packages()[,1])


requiredPackage = c('MASS','signal', 'colorspace','Cairo','rgeos','sp','rgl', 'rgdal','EMD','mmand','e1071','adimpro')
load_install_package(requiredPackage)
cat('> Package(s) loaded (if...): ',requiredPackage,' \n')
	
	# library(signal) # unwrap()
	# # library(pracma) # topoShift > interp1 
	# <<<<<<library(fields) # plotRaster > image.plot (barscale) 
	# library(colorspace)	# diverge_hcl
	# library(Cairo)	# diverge_hcl
	# library(rgeos)
	# library(sp)
	# mmand::gaussianSmooth
	# e1071::kurtosis
	

	
	
	
	
source("R/global.R")
source("R/ClassGPR.R")
source("R/ClassGPRsurvey.R")

	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
