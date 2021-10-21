my_packages <- c("rvest","httr","tidyverse", "readxl", "writexl")
install_if_missing <- function(p) {
  if(p %in% rownames(installed.packages())==FALSE){
    install.packages(p)}
}
invisible(sapply(my_packages, install_if_missing))