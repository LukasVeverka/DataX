# Dependencies
library(odbc)
library(DBI)
library(dbplyr)
library(tidyverse)

# Drivery pro připojení k databázi
odbc::odbcListDrivers()

# Pokud v seznamu není "MySQL ODBC 8.0 Unicode Driver" tak je potřeba nainstalovat z odkazu níže
# https://dev.mysql.com/downloads/connector/odbc/

# Define server connection
con <- DBI::dbConnect(odbc::odbc(),
                      Driver   = "MySQL ODBC 8.0 Unicode Driver",
                      Server   = "compass-db-server.cgbcuxp6tr07.eu-central-1.rds.amazonaws.com",
                      UID      = "studentX",
                      PWD      = "MojeOblibenaHodina",
                      Port     = 3306)

# Příklad dotazování "SQL style"
moje_data <- dbGetQuery(con,'
                          SELECT *
                          FROM MP_university_DATAX.MP_database
                        ')

# Příklad dotazování "R style"
moje_data <- 
  tbl(con, in_schema("MP_university_DATAX","MP_database")) %>% 
  collect()
