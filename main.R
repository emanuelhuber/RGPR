


cat("******* RGPR *******\n")
cat("version 0.0.8\n")
cat("15.03.2015\n")
cat("Emanuel Huber, emanuel.huber@unibas.ch\n")
cat("********************\n")

#---changes v0.0.8---#
# gpr:reverse (check pos)
# plotWig > add_ann
#---changes v0.0.7---#
# local orientation added
# deconvolution
# fx-filter
# medianFilter

# TODO
#	- function "lines" to plot a GPR trace on a previous plot
#			plot(gpr[,100])
#			lines(gpr[,101],col="red")
#	- as.numeric, as.vector
#	- use function "inPoly" to plot in 3D only a selection of traces from GPRsurvey
#	- migration/topoShift > integrate the function time2depth or depth2time!!!!
#	- by delineation > use the same scheme for both raster and wiggles!
#	- check option image and use raster! check grid with smooth image...
#	- scaling after gain!
#	- rms agc, instantaneous gain 
#	- rename gain_geospreading in power!
#	- dewow: method: "hampel", "gaussian"
#	- export topot -> shapefile, lines
#	- plot function: check if all the "..." parameters corresponds to the possible paramters. If not -> error!
# 	- different taper window: cos, triang, hamming, bartlett, limtaper, hann, flattop....
#	- fx spec > use power! (log scale)
#	- check how time0 is used
#	- gain: t0=NULL then t0 <- mean(time0)

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

# FIX ME!!
#		-> check FIX ME!!
# 		-> write(gpr...) -> update gpr@filename
#		-> read(gpr,... -> update gpr@filename
#		-> exportPDF: add processing steps!
#		-> plot3D -> add a zlim or a max depth!!!
#		-> add x@pts="numeric"  points number ??
#		-> export(type=PDF) use function plot (wiggles)!
#		-> with generic method: use exactly the same arguments
#				use args(plot) to know the arguments
#		-> rename "fid" into  "fiducial marks"	=> fid
#		-> as.numeric, as.vector
# Idea for trace to trace processing:
# -> list of all function with their argument to use
# -> use a FUN_WRAP function : apply(GPR$data,2,FUN_WRAP)
# -> FUN_WRAP process each single trace according to the list of function and theirs args.

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

	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
