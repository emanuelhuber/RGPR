


cat("******* RGPR *******\n")
cat("version 0.0.8\n")
cat("15.03.2015\n")
cat("Emanuel Huber, emanuel.huber@unibas.ch\n")
cat("********************\n")


############## CHANGES ##################
#--- changes v0.0.9---#
#	- add fx: pal(), colGPR()
#	- filepath instead of filename
#   - slot @ntr removed (not needed! x@ntr <- ncol(x@data))
# 	- slot @w removed (not needed! x@w <- (nrow(x@data)-1)*x@dz)
#	- slot @version added 
#	- slot @filename renamed as @filepath
#	- new function: GPRsurvey:: write
#	- handle two types of dates (readGPR): %Y %m %d and %d %m %Y with any kind of separator

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


############## RCODE : TO CHECK/THINK ###############
# THINK: use Roxygen...
# When plot with par(mfrow=c(2,1)).... reset the "par" after the plot (1) save op <- par(), 2) plot and 3) reset par: par(op).

# CONVENTIONS: empty slot should have length = 0 (use character(0) instead of "")
# CONVENTIONS: with generic method: use exactly the same arguments
#				use args(plot) to know the arguments
# CONVENTIONS: filepath (NOT filePath, fileName, filename, path)
# CONVENTIONS: PRIVATE FUNCTION .myfunction()
# CONVENTIONS: rename "fid" into  "fiducial marks"	=> fid

# check lockBinding  (bibliotheque/documents/R/manuel-S4)

# function invisible() > Return a (temporarily) invisible copy of an object.
# used in plot function

# DON'T WANT TO PASS "..."-ARGUMENTS TO A FUNCTION?
# SOLUTION: use a wrapper function, where the args after ... are the args
# that you don't want to have in the function. E.G:
# lPoints <- function(..., log, axes, frame.plot, panel.first, panel.last) {
#     points(...)
# }



# read/write SEGy

# FIX ME!
#	- GPRsurvey::plot3D -> use function "inPoly" to plot in 3D only a selection of traces from GPRsurvey
#						-> add a zlim or a max depth!!!
#	- GPR::plot3D -> add a zlim or a max depth!!!
#	- GPR::exportCoord & GPRsurvey::exportCoord -> export as points -> use point.data.frame (add z-information)
#	- GPR::migration/topoShift -> integrate the function time2depth or depth2time!!!!
#	- GPR::delineation -> use the same scheme for both raster and wiggles!
#	- GPR::fkFilter -> add as argument "slopes"
#	- GPR::interpTraces -> use a raster to get the v-elevation!
#	- plot.GPR function
#		-> check if all the "..." parameters corresponds to the possible paramters. If not > error!
#		-> check option image and use the "raster" option. Check grid with smooth image...
#	- GPR::gain -> t0=NULL then t0 <- mean(time0)
#	- GPR::spec(type "f-t") -> use power and log scale (?)
#	- GPR::plotAmpl -> option log y-axis
#	- GPR::exportPDF -> add processing steps!
#	- scaling after gain!
# 	- different taper window: cos, triang, hamming, bartlett, limtaper, hann, flattop....
#	- check how time0 is used
#	- GPR::export(type=PDF) use function plot (wiggles)!



#--- STRUCTURE ---#
# - add @vDatum	> for vertical geodesic datum
# (- delete @depthunit, @posunit & replace by @units@depth and @units@pos)
# - delete @dx > not needed! x@dx <- mean(diff(x@pos))
# - delete @dz > not needed! x@dz <- mean(diff(x@depth))

# NEW CLASS: 	"GPRGrid" > based on survey but for x- and y-lines
# 				2D plot: the lines start from the same position
# 				can cut slices etc.


################## GPR PROCESSING ######################3

# check book "Near-surface Geophysics, vol 13"

# Idea for trace to trace processing:
# -> list of all function with their argument to use
# -> use a FUN_WRAP function : apply(GPR$data,2,FUN_WRAP)
# 		-> FUN_WRAP process each single trace according to the list of function and theirs args.

#--- declipping ---#
# - least-square polynomial interpolation
# - AR/ARMA model
# - PCA/SVD decomposition
# - DCT (cf. R package)
# - deconvolution/convolution
# - interpolation par cubic splines (see bibliotheque/document/courbes de bezier+++ )

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


requiredPackage = c('base','MASS','signal', 'colorspace','Cairo','rgeos','sp','rgl', 'rgdal','EMD','mmand','e1071','adimpro')
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

	
	
	
	
# FILE FORMAT
# 1		Trace number (1,2,3,4....,n)
# 2		Trace position (0, 0.25, 0.5, 0.75,...)
# 3		Samples: # Points/Traces (500,500,500,500,...)
# 4 	Topo: elevation data (145.23, 146.1,146.45,...)
# 5 	NA1: time? (0.000000e+00 3.713441e-43 7.426882e-43 1.112631e-42,...)
# 6		Bytes (2,2,2,2,...)
# 7		tracenb: in fact time windows in ps!!! (800,800,800,...)
# 8		number of stacks (32,32,32,32,...)
# 9		window: in fact NA1 (0,0,0,0,0,...)  > GPS x position 
# 10	NA2: (0,0,0,0,0,...)  > GPS x position (double*8 number)
# 11	NA3: (0,0,0,0,0,...)  > GPS y position (double*8 number)
# 12	NA4: (0,0,0,0,0,...)  > GPS y position (double*8 number)
# 13	NA5: (0,0,0,0,0,...)  > GPS z position (double*8 number)
# 14	NA6: (0,0,0,0,0,...)  > GPS z position (double*8 number)
# 15	receiver x position
# 16	receiver y position
# 17	receiver z position
# 18	transmitter x position
# 19	transmitter y position
# 20	transmitter z position
# 21	time-zero
# 22	zero-flag: 0=data okay, 1=zero data
# 23	NA not used (0,0,0,...)
# 24	time of the day data collected in seconds pas midnight
# 25	x8: comment flag: 1 = comment attached
# 26-28 com: comment

	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
