# ifCNV-R
 ifCNV R package
 
 # Installation
 The package is available on CRAN. To install type:
 
 
 `
install.packages("ifCNVR")
`

# Example

See the example.R file reproduced below:

```R
library(ifCNVR)

pathToBam <- "/path/to/bam/files/"
pathToBed <- "/path/to/bed/file.bed"
pathToBedtools <- "/path/to/bedtools/"
outputPath <- "/path/to/html/report.html"

# After installing bedtools (https://bedtools.readthedocs.io/en/latest/content/installation.html), 
# type `which bedtools` in your console to get the pathToBedtools

# Create reads matrix
readsMatrix <- CreateReadsMatrix(pathToBam, pathToBed, pathToBedtools)

# Detect CNV positive samples 
CNVpos <- abSamples(readsMatrix)

# Detect CNV positive targets
CNVtar <- abTargets(readsMatrix, CNVpos)

# Create the results table
resTable <- calculateScore(readsMatrix, CNVpos, CNVtar)

# Generate the html report
generateReport(outputFile = outputPath, readsMatrix, resTable, CNVpos)

```


# User's Guide

A html User's Guide with more details is also available
