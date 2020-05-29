You will need Rtools to install this package

**Windows**: [Rtools.exe](https://cran.r-project.org/bin/windows/Rtools/). 

#RSForInvt
Install the latest version from github with devtools. You may have to manually install a few packages that aren't brought in by the package for some reason. Lubridate was an example package that wouldn't import with the package.
```r

#wonky package that e.g. won't load automajically
install.packages("lubridate")

install.packages("remotes")
remotes::install_github("jstrunk001/RSForInvt")

```




Install from local directory
```r

install.packages("remotes")
remotes::install_local("c:\\temp\\RSForInvt.zip")

```

