# Works with Github actions to install the built package.
# Could not get Rscript -e to work with Windows, necessitating this file.
install.packages("../SampleSelectR", repos=NULL, type="source", lib="./package_binary")
