# DSMolgenisArmadillo

[![Build Status](https://jenkins.dev.molgenis.org/buildStatus/icon?job=molgenis%2Fmolgenis-r-datashield%2Fmaster)](https://jenkins.dev.molgenis.org/job/molgenis/job/molgenis-r-datashield/job/master/)
[![codecov](https://codecov.io/gh/molgenis/molgenis-r-datashield/branch/master/graph/badge.svg)](https://codecov.io/gh/molgenis/molgenis-r-datashield)


A DSI implementation for the [Armadillo DataSHIELD Service](https://github.com/molgenis/molgenis-service-datashield).
**Part of the MOLGENIS suite**

## Overview
You can use DSMolgenisArmadillo to analyse data shared in Armadillo servers using DataSHIELD. DataSHIELD allows exceution of a subset of analysis methods available in R. Methods such as:

`ds.mean()`
`ds.glm()`
`ds.SLMA()`

For more detailed documentation check: [https://cran.datashield.org/](https://cran.datashield.org/).

## Installation
The DataSHIELD Armadillo package is available on the MOLGENIS CRAN (https://registry.molgenis.org/repository/R). You can install it by executing the following code-block:

```{r, install Armadillo DataSHIELD package, eval = FALSE}
install.packages("DSI")
install.packages("DSMolgenisArmadillo", repos = "https://registry.molgenis.org/repository/R", dependencies = TRUE)
```

Make sure you install the DataSHIELD client (`dsBaseClient`) to perform the actual analysis. This needs to be a client which is version 6.0 or higher.

```{r, install the Armadillo DataSHIELD client}
# install the DataSHIELD client
install.packages("dsBase", repos = c("http://cran.datashield.org", "http://cran.us.r-project.org"), dependencies = TRUE)
```

> **Experimental versions**
>
> If you are interested in using exeperimental versions you can use our own CRAN repository:
>
>```{r, install an experimental version of the Armadillo DataSHIELD client, eval = FALSE}
> # install experimental Armadillo client
> install.packages("DSMolgenisArmadillo", repos = "https://registry.molgenis.org/repository/r-hosted-snapshots")
> ```

## Usage
```{r, basic use of the client}



```



Check the howto here: [howto](https://molgenis.github.io/molgenis-r-datashield/articles/DSMolgenisArmadillo.html).

