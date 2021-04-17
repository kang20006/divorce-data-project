#init.R
#install package
my_packages = c("MASS", "data.table","shiny","shinythemes")

install_if_missing = function(p) {
  if (p %in% rownames(installed.packages()) == FALSE) {
    install.packages(p)
  }
}

invisible(sapply(my_packages, install_if_missing))
