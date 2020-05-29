(Preferred) Automatically install package from github (on Windows) use RTools:

**Windows**: [Rtools.exe](https://cran.r-project.org/bin/windows/Rtools/). 

#RSForInvt
Install the latest version from github with devtools. You may have to manually install a few packages that aren't brought in by the package for some reason. Lubridate was an example package that wouldn't import with the package.
```r
install.packages("lubridate")
devtools::install_github("jstrunk001/RSForInvt")
```




Manual way to install this package:

Download this git repository to a local directory, zip the repository (file folder named "RSForInvt" -> "RSForInvt.zip"), and then use the R package load interface to install from .zip package file.
