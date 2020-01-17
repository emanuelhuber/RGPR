setMethod("Arg", "GPR", function(z){
  z@data <- as.matrix(Arg(z@data))
  return(z)
})