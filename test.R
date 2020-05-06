library(DSMolgenisArmadillo)
library(dsBaseClient)

dsGetInfo(armadillo())

# create loginframe
server <- c("armadillo")
url <- c("http://localhost:8080?workspace=DIABETES/patient&workspace=GECKO/customer")
user <- c("admin")
password <- c("admin")
driver <- c("ArmadilloDriver")
table <- c("datashield.PATIENT")
logindata <- data.frame(server, url, user, password, table, driver)

##############
# Workspaces #
##############
conns <- datashield.login(logins = logindata, assign = F)
dsGetInfo(conns$armadillo)
datashield.assign.table(conns = conns, table = "datashield.PATIENT", symbol = "D", async = FALSE)
datashield.symbols(conns)

# logout without saving workspace
datashield.logout(conns)
datashield.symbols(conns) # error

# login again, D should not be available
conns <- datashield.login(logins = logindata, assign = F)
datashield.symbols(conns)

# assign symbol, logout and save workspace
conns <- datashield.login(logins = logindata, assign = F)
datashield.assign.table(conns = conns, table = "datashield.PATIENT", symbol = "J")
datashield.logout(conns, save = "my-workspace")
conns <- datashield.login(logins = logindata, assign = FALSE, restore = "my-workspace")
datashield.symbols(conns) # should contain 'J'

# save to already existing workspace
conns <- datashield.login(logins = logindata, assign = F)
datashield.assign.table(conns = conns, table = "datashield.PATIENT", symbol = "H")
datashield.workspace_save(conns, "my-overwrite")
datashield.workspace_save(conns, "my-overwrite")

# list workspaces
conns <- datashield.login(logins = logindata, assign = F)
dsListWorkspaces(conns$armadillo)
datashield.workspaces(conns) # weird output (bug in dsBaseClient?)

# remove workspace
conns <- datashield.login(logins = logindata, assign = F)
datashield.workspace_save(conns, "removeMe")
dsListWorkspaces(conns$armadillo)
datashield.workspace_rm(conns, "removeMe")
dsListWorkspaces(conns$armadillo)


#####################
# Symbol assignment #
#####################
conns <- datashield.login(logins = logindata, assign = F)
datashield.assign.table(conns = conns, table = "datashield.PATIENT", symbol = "D")
datashield.assign.table(conns = conns, table = "datashield.PATIENT", symbol = "E", async = TRUE)

datashield.symbols(conns)
datashield.rm(conns = conns, "E")
datashield.symbols(conns)

datashield.assign.expr(conns = conns, symbol = "K", "c(10,50,100)")
ds.mean("K", datasources = conns)

datashield.assign.expr(conns = conns, symbol = "H", "c(10,50,100)", async = FALSE)
ds.mean("H", datasources = conns)


##############################
# Tables / methods /packages #
##############################
conns <- datashield.login(logins = logindata, assign = F)
datashield.assign.table(conns = conns, table = "datashield.PATIENT", symbol = "D")

# check tables
dsListTables(conns$armadillo)
dsHasTable(conns$armadillo, "datashield.PATIENT")
dsHasTable(conns$armadillo, "datashield.NOTPATIENT")

# check packages
conns <- datashield.login(logins = logindata, assign = F)
dsListPackages(conns$armadillo)

# check methods
datashield.methods(conns = conns, type = "aggregate")
datashield.methods(conns = conns, type = "assign")

##################
# Error handling #
##################
conns <- datashield.login(logins = logindata, assign = F)

# bad requests (sync and async):
datashield.assign.expr(conns = conns, symbol = "K", "c(10,50,100")
dsAssignExpr(conns$armadillo, symbol = "K", "c(x", async = FALSE)
dsAssignExpr(conns$armadillo, symbol = "K", "c(x", async = TRUE)

dsAggregate(conns$armadillo, "c(4", async = FALSE)
dsAggregate(conns$armadillo, "c(4", async = TRUE)

# R error -> 500
result <- dsAggregate(conns$armadillo, "X", async = TRUE)
dsFetch(result)

dsAggregate(conns$armadillo, "X", async = FALSE)

# R error -> 201 (async assign)
datashield.assign.expr(conns = conns, symbol = "K", "X")

# invalid method type
datashield.methods(conns = conns, type = "assigregate")
