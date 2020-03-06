# DSMolgenis
A DSI implementation for the [MOLGENIS DataSHIELD Service](https://github.com/molgenis/molgenis-service-datashield)

## Usage
We have constructed a sample program to get your first login, data assigend and calculation of mean.

```
# create loginframe
server <- c("molgenis") 
url <- c("https://datashield.dev.molgenis.org")
user <- c("admin") 
password <- c("admin")
driver <- c("MolgenisDriver")
table <- c("datashield.PATIENT") 
logindata <- data.frame(server,url,user,password,table,driver)

# login into datashield service
molgenisses <- datashield.login(logins=logindata,assign=F)

# assign some data in R
datashield.assign.table(conns = molgenisses, table = "datashield.PATIENT", symbol = "D")

# Do some statistical analysis, execute:
ds.mean('D$age')
```

