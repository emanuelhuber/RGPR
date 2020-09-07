  
.read_RDS <- function(path){
  x <- readRDS(path)
  if(inherits(x, "GPRvirtual") || inherits(x, "GPRsurvey")){
    x@path <- path
  }else if(inherits(x, "list")){
    if(x[["version"]] == "0.1"){
      for(i in seq_along(x[['delineations']])){
        x[['delineations']][[i]][, 5] <- -x[['delineations']][[i]][, 5]
      }
    }
    if( any(x[["version"]] == c("0.1", "0.2"))){
      y <- new("GPR",
               #--- class GPRvirtual
               version = "0.3",
               name = x[['name']],
               path = x[['filepath']],
               desc = x[['description']],
               mode = x[['surveymode']],
               date = x[['date']],
               freq = x[['freq']],
               
               data  = x[['data']],
               dunit = "mV",                          # FIXME???
               dlab  = "amplitude",
               
               spunit = x[['posunit']],
               crs    = x[['crs']],
               
               xunit = x[['posunit']],
               xlab  = "position",                    # FIXME???
               
               zunit = x[['depthunit']],
               zlab  = "two-way travel time",         # FIXME???
               
               vel = list(v = x[['vel']][[1]]),                
               
               proc         = x[['proc']],
               delineations = x[['delineations']],
               md           =  x[['hd']],
               
               #--- class GPR
               z0      = x[['time0']],
               time    = x[['time']],              
               antsep  = x[['antsep']],
               markers = trimStr(x[['fid']]),
               ann     = trimStr(x[['ann']]),
               
               coord = x[['coord']],
               rec   = x[['rec']],      
               trans = x[['trans']],
               
               x = x[['pos']],
               z = x[['depth']]
               
               # angles      = ...
      )
    }else{  # version 0.3
      x <- new("GPR",
               #--- class GPRvirtual
               version = "0.3",
               name = x[['name']],
               path = x[['path']],
               desc = x[['desc']],
               mode = x[['mode']],
               date = x[['date']],
               freq = x[['freq']],
               
               data  = x[['data']],
               dunit = x[['dunit']],           # FIXME???
               dlab  = x[['dlab']],
               
               spunit = x[['spunit']],
               crs    = x[['crs']],
               
               xunit = x[['xunit']],
               xlab  = x[['xlab']],         # FIXME???
               
               zunit = x[['zunit']],
               zlab  = x[['zlab']],         # FIXME???
               
               vel = x[['vel']],                
               
               proc         = x[['proc']],
               delineations = x[['delineations']],
               md           = x[['md']],
               
               #--- class GPR
               z0      = x[['z0']],
               time    = x[['time']],              
               antsep  = x[['antsep']],
               markers = x[['markers']],
               ann     = x[['ann']],
               
               coord = x[['coord']],
               rec   = x[['rec']],      
               trans = x[['trans']],
               
               x = x[['x']],
               z = x[['z']],
               
               angles = x[['angles']]
      )
    }
    x@path <- path
  }
  return(x)
}