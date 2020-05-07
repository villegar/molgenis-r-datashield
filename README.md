# DSMolgenisArmadillo



[![codecov](https://codecov.io/gh/molgenis/molgenis-r-datashield/branch/master/graph/badge.svg)](https://codecov.io/gh/molgenis/molgenis-r-datashield)


A DSI implementation for the [Armadillo DataSHIELD Service](https://github.com/molgenis/molgenis-service-datashield).
**Part of the MOLGENIS suite**

## Usage
We have constructed a sample program to get your first login, data assigend and calculation of mean.

```
# create loginframe
server <- c("armadillo") 
url <- c("https://armadillo.dev.molgenis.org")
user <- c("admin") 
password <- c("admin")
driver <- c("ArmadilloDriver")
table <- c("datashield.PATIENT") 
logindata <- data.frame(server,url,user,password,table,driver)

# login into datashield service
armadillos <- datashield.login(logins=logindata,assign=F)

# assign some data in R
datashield.assign.table(conns = armadillos, table = "datashield.PATIENT", symbol = "D")

# Do some statistical analysis, execute:
ds.mean('D$age')
```

