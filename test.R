library(DSI)
library(DSMolgenis)
library(dsBaseClient)

# create loginframe
server <- c("molgenis")
url <- c("http://localhost:8080/DIABETES/patient")
user <- c("admin")
password <- c("admin")
driver <- c("MolgenisDriver")
table <- c("datashield.PATIENT")
logindata <- data.frame(server,url,user,password,table,driver)

##############
# Workspaces #
##############
conns <- datashield.login(logins=logindata,assign=F)
datashield.assign.table(conns = conns, table = "datashield.PATIENT", symbol = "D")
datashield.symbols(conns)

# logout without saving workspace
datashield.logout(conns)
datashield.symbols(conns) # error 

# login again, D should not be available
conns <- datashield.login(logins=logindata,assign=F)
datashield.symbols(conns)

# assign symbol, logout and save workspace
conns <- datashield.login(logins=logindata,assign=F)
datashield.assign.table(conns = conns, table = "datashield.PATIENT", symbol = "J")
datashield.logout(conns, save="my-workspace")
conns <- datashield.login(logins=logindata, assign=FALSE, restore="my-workspace")
datashield.symbols(conns) # should contain 'J'

# save to already existing workspace
conns <- datashield.login(logins=logindata,assign=F)
datashield.assign.table(conns = conns, table = "datashield.PATIENT", symbol = "H")
datashield.workspace_save(conns, "my-overwrite")
datashield.workspace_save(conns, "my-overwrite")

# list workspaces
conns <- datashield.login(logins=logindata,assign=F)
dsListWorkspaces(conns$molgenis)
datashield.workspaces(conns) # weird output (bug in dsBaseClient?)

# remove workspace
conns <- datashield.login(logins=logindata,assign=F)
datashield.workspace_save(conns, "removeMe")
dsListWorkspaces(conns$molgenis)
datashield.workspace_rm(conns, "removeMe")
dsListWorkspaces(conns$molgenis)

#####################
# Symbol assignment #
#####################
conns <- datashield.login(logins=logindata,assign=F)
datashield.assign.table(conns = conns, table = "datashield.PATIENT", symbol = "D")
datashield.assign.table(conns = conns, table = "datashield.PATIENT", symbol = "E")

datashield.symbols(conns)
datashield.rm(conns = conns, 'E')
datashield.symbols(conns)

datashield.assign.expr(conns = conns, symbol = 'K', "c(10,50,100)")
ds.mean("K")
ds.mean("D$age")

##############################
# Tables / methods /packages #
##############################
conns <- datashield.login(logins=logindata,assign=F)
datashield.assign.table(conns = conns, table = "datashield.PATIENT", symbol = "D")

# check tables
dsListTables(conns$molgenis)
dsHasTable(conns$molgenis, "datashield.PATIENT")
dsHasTable(conns$molgenis, "datashield.NOTPATIENT")

# check packages
conns <- datashield.login(logins=logindata,assign=F)
dsListPackages(conns$molgenis)

##################
# Error handling #
##################
conns <- datashield.login(logins=logindata,assign=F)

# bad requests (sync and async):
datashield.assign.expr(conns = conns, symbol = 'K', "c(10,50,100")
dsAssignExpr(conns$molgenis, symbol = "K", "c(x", async=FALSE)
dsAssignExpr(conns$molgenis, symbol = "K", "c(x", async=TRUE)

dsAggregate(conns$molgenis, "c(4", async=FALSE)
dsAggregate(conns$molgenis, "c(4", async=TRUE)

# R error -> 500
result <- dsAggregate(conns$molgenis, "X", async=TRUE)
dsFetch(result)

dsAggregate(conns$molgenis, "X", async=FALSE)

# R error -> 201 (async assign)
datashield.assign.expr(conns = conns, symbol = 'K', "X")
