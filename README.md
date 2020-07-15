# DSMolgenisArmadillo

[![Build Status](https://jenkins.dev.molgenis.org/buildStatus/icon?job=molgenis%2Fmolgenis-r-datashield%2Fmaster)](https://jenkins.dev.molgenis.org/job/molgenis/job/molgenis-r-datashield/job/master/)
[![codecov](https://codecov.io/gh/molgenis/molgenis-r-datashield/branch/master/graph/badge.svg)](https://codecov.io/gh/molgenis/molgenis-r-datashield)


A DSI implementation for the [Armadillo DataSHIELD Service](https://github.com/molgenis/molgenis-service-datashield).
**Part of the MOLGENIS suite**

## Usage
We have constructed a sample program to get your first login, data assigend and calculation of mean.

```{r}
# create loginframe
builder <- DSI::newDSLoginBuilder()
builder$append(server = "armadillo",
               url = "http://aramdillo.dev.molgenis.org?workspace=gecko/patient",
               user = "admin",
               password = "admin",
               token = "",
               table = "datashield.PATIENT",
               driver = "ArmadilloDriver")

# login into datashield service
armadillos <- datashield.login(logins=logindata,assign=F)

# assign some data in R
datashield.assign.table(conns = armadillos, table = "datashield.PATIENT", symbol = "D")

# Do some statistical analysis, execute:
ds.mean('D$age')
```

## Development

### Testing
The test are run each time you file a pull request on github. You can run the test locally as well using the following commands:

```R

# run all tests

devtools::test()

# run a single test file
# make sure you have the test file open
# e.g. test-ArmadilloResult.R

devtools::test_file()
```

### Vignettes
We use vignettes to expose the usage of the package. It includes examples of regular use cases and shows you how to setup the package.

The vignettes are automatically build and you can run them locally with the following command:

```R
devtools::build_vignettes()
```


