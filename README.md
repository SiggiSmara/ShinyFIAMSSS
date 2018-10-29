# ShinyFIAMSSS
Flow Injection Analysis Mass Spectrometry System Suitability

A tool to assess system suitability of a FIA MS (MRM type) setup

## Origins
The origins of this package is to support the system suitability
checking of the FIA part of the Biocrates assay 
(http:www.biocrates.com) as analyzed on a Sciex mass spectrometer.

There are functions on the data side that are specific for that
origin and may or may not be generally applicable.

## Application
Allowing an easier way to assess both the daily variance of
spiked analytes and blanks as well as how they compare over time.

## Install the package
The package relies on several packages, most importantly both shiny
and xcms. It also allows on the package version of shiny-directory-input 
that can currently found on github here
https://github.com/SiggiSmara/shiny-directory-input/tree/package

The shinyDirectoryInput package needs to be installed via devtools
prior to installing this one:

```r
devtools::install_github('SiggiSmara/shiny-directory-input', ref="package")
```
once that is installed install this package in a similar manner:
```r
devtools::install_github('SiggiSmara/ShinyFIAMSSS')
```

## RUn the app
There is only one exported function in the package: `runMainProgram`. It requires
that the main working directory be supplied to it where the setup files, the mzML files,
and the results are stored.
```r
library(ShinyFIAMSSS)
runMainProgram('/path/to/workig/directory')
```

