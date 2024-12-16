loadOrInstall <- function (packageName, type = "CRAN") {
    
    isPackageInstalled <- packageName %in% rownames(installed.packages())
    
    if (!isPackageInstalled) {
        
        if (type == "CRAN") {
            
            install.packages(packageName)
            
        } else if (type == "bioc") {
            
            BiocManager::install(packageName)
            
        }
        
    }
    
    library(packageName, character.only = TRUE)
}

cranPackages <- c(
    "shiny",
    "shinythemes",
    "shinyjs",
    "ggplot2",
    "openxlsx",
    "DT",
    "pheatmap",
    "NbClust",
    "stringi",
    "BiocManager",
    "png",
    "shinyWidgets"
)

biocPackages <- c(
  "M3C"
)

loadOrInstall("BiocManager")

for (package in cranPackages) {
    loadOrInstall(package)
}

for (package in biocPackages) {
    loadOrInstall(package, type = "bioc")
}

