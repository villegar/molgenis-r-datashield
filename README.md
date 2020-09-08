# DSMolgenisArmadillo

[![Build Status](https://jenkins.dev.molgenis.org/buildStatus/icon?job=molgenis%2Fmolgenis-r-datashield%2Fmaster)](https://jenkins.dev.molgenis.org/job/molgenis/job/molgenis-r-datashield/job/master/)
[![codecov](https://codecov.io/gh/molgenis/molgenis-r-datashield/branch/master/graph/badge.svg)](https://codecov.io/gh/molgenis/molgenis-r-datashield)


A DSI implementation for the [Armadillo DataSHIELD Service](https://github.com/molgenis/molgenis-service-datashield).
**Part of the MOLGENIS suite**

## Overview
You can use DSMolgenisArmadillo to analyse data shared in Armadillo servers using DataSHIELD. DataSHIELD allows execution of a subset of analysis methods available in R. Methods such as:

`ds.mean()`
`ds.glm()`
`ds.lmerSLMA()`

For more detailed documentation check: [https://cran.datashield.org/](https://cran.datashield.org/).

## Installation
The DataSHIELD Armadillo package is available on the MOLGENIS CRAN (https://registry.molgenis.org/repository/R). You can install it by executing the following code-block:

```R
install.packages("DSI")
install.packages("DSMolgenisArmadillo", repos = "https://registry.molgenis.org/repository/R", dependencies = TRUE)
```

Make sure you install the DataSHIELD client (`dsBaseClient`) to perform the actual analysis. This needs to be a client which is version 6.0.0 or higher.

```R
# install the DataSHIELD client
install.packages("dsBase", repos = c("http://cran.datashield.org", "http://cran.us.r-project.org"), dependencies = TRUE)
```

## Usage
To use the DataSHIELD Armadillo client and perform analysis in DataSHIELD there a few basic steps you need to take.

### Get the Armadillo token

```R
# Load the necessary packages.
library(dsBaseClient)
library(DSMolgenisArmadillo)

# specify server url
armadillo_url <- "https://armadillo.dev.molgenis.org"

# get token from central authentication server
token <- armadillo.get_token(armadillo_url)
```

### Building the login frame
You need to specify the project, the folder and the table name(s) you want to access.

```R
# build the login dataframe
builder <- DSI::newDSLoginBuilder()
builder$append(server = "armadillo",
               url = armadillo_url,
               token = token,
               table = "gecko/2_1-core-1_0/nonrep",
               driver = "ArmadilloDriver")

# create loginframe
logindata <- builder$build()
```

### Login and assign the data
Assigning the data means that you will assign the data to a symbol in the analysis environment.

```R
# login into server
conns <- datashield.login(logins = logindata, assign = TRUE)
```

### Performing analysis
DataSHIELD has a range of methods you can use to perform analysis. Check: the [dsBaseClient](https://cran.datashield.org/web/#client-packages) documentation to see which methods are available.

```R
# calculate the mean
ds.mean("core_nonrep$coh_country", datasources = conns)

# create a histogram
ds.histogram(x = "core_nonrep$coh_country", datasources = conns)
```

## Documentation
Check the [package documentation](https://molgenis.github.io/molgenis-r-datashield/articles/DSMolgenisArmadillo.html) for details.

