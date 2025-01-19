devtools::install_github("datastorm-open/DependenciesGraphs")
library(DependenciesGraphs)
# Prepare data
dep <- envirDependencies("package:RGPR")

# visualization
plot(dep)
