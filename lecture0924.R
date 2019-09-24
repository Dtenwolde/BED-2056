# Paste your R code here...

require(RMySQL) || {install.packages("RMySQL"); require(RMySQL)}

con<-dbConnect(RMySQL::MySQL(),
               host      = "titlon.uit.no",
               user      = "dwo009@uit.no",
               password  = "TS!JcBdn!070YNmUdsyJe",
               db        = "OSE")

dbListTables(con)
dbListFields(con,"equity")
rs = dbSendQuery(con, "SELECT * FROM equity WHERE Name='Equinor'")
titlon_data=fetch(rs,-1)

# -------------------------------------------------------------------------

?fetch

#' Congratulations, you are now connected to Titlon, and are ready to run SQL queries.
#' Our connection is labelled `con`.

library(tidyverse)

browseURL("https://rviews.rstudio.com/2017/05/17/databases-using-r/")

browseURL("https://db.rstudio.com/dplyr/")

#' A very useful function in DBI is dbListTables(), which retrieves the names of available tables.
#' In this case "con"
dbListTables(con)

#' Another useful function is the dbListFields, which returns a vector with all of the column names in a table.
dbListFields(con, "equity")

#' How many rows of data is there?
tally(tbl(con, "equity")) # could be useful to know before loading the data locally

#' Now that we've copied the data, we can use tbl() to take a reference to it:
#' This is probably the most important function so far.
equity <- tbl(con, "equity")
equity

#' We can now use dplyr lingo on this database object, end with collect() to retrieve data locally
names <- equity %>% select(Symbol, Name) %>% collect()
unique(names$Name)

#' Finding a specific Symbol
names %>% filter(Name=="Equinor")

#' Number of obsevations per symbol
table <- fct_count(names$Symbol)
table

#' How many sectors are there?
sectors <- equity %>% select(Sector) %>% collect()
fct_count(sectors$Sector)

#' When used against a database, the previous function is converted to a SQL query that works with MS SQL Server.
#' The `show_query()`` function displays the translation.
show_query(tally(tbl(con, "equity")))


#' 1) Use the "bonds" table.
bonds <- tbl(con, "bonds")

#' a) What are the variables in the "bonds" table?
dbListFields(con, "bonds")
#' b) How many observations is there in the "bonds" table?
tally(tbl(con, "bonds"))
#' c) Download the bonds table only for EqName "Norsk Hydro".
bondsNHydro <- bonds %>% filter(EqName == "Norsk Hydro") %>% collect()
#' d) Fix the dates (hint: use lubridate package). Remove any duplicate dates.
library("lubridate")
bondsNHydro$Date <- ymd(bondsNHydro$Date)
#' 2) Use the "equity" table.
equity <- tbl(con, "equity")
#' b) What are the variables in the "equity" table?
dbListFields(con, "equity")
#' c) Extract Equinor and Norsk Hydro data from the equity database.
Equinor_nHydro <- equity %>% filter(name == "Equinor" | name == "Norsk Hydro") %>% collect()
#' d) Fix the dates (hint: use lubridate package). Remove any duplicate dates.
Equinor_nHydro$Date <- ymd(Equinor_nHydro$Date)
Equinor_nHydro <- distinct(Equinor_nHydro,Date, .keep_all = TRUE)
#' e) Find the average AdjustedPrice per month, per stock, and plot it over time.
Equinor_nHydro %>% select(Date, AdjustedPrice, Name) 


#' f) Plot the daily AdjustedPrice of these two stocks over time.
#' g) Plot the OfficialNumberOfTrades against the AdjustedPrice, using colors for each year.
