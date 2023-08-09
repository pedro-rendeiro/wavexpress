# INSTALL PACKAGES
# install.packages("DT")
# install.packages("shiny")
# install.packages("shinythemes")

# Load shiny (to execute runApp and runExample)
library(shiny)

# Apps
runApp("main")
runApp("test")
runApp("widgets")
runApp("censusVis")

# Examples
runExample("03_reactivity")
