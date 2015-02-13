library(plyr)
library(tidyr)
library(dplyr)
library("RODBC")
library("lubridate")
library( ReporteRs )

#########################################################################################################
## Initiate report variables
#########################################################################################################

sampleYear = 2014

#########################################################################################################
# Open SQL channel and fetch
#########################################################################################################


channel <- odbcConnect("discountasp", uid = "SQL2008_508574_uwmep_user", pwd = "Manis9") #this creates a connection to the database and reads the server from discountasp.dsn, and provides user credentials.

UTM <-sqlFetch(channel, "UTMs")  # read data from SQL database

glimpse(UTM)

UTM$Yr1 = as.character(UTM$Year1)
UTM$Yr2 = as.character(UTM$Year2)
UTM$Yr3 = as.character(UTM$Year3)


UTMsampleYear <- UTM %>%
  filter(Year1 == sampleYear | Year2 == sampleYear | Year3 == sampleYear ) %>%
  mutate(YearsSampled = paste(Yr1, Yr2, Yr3, sep = ', ')) %>%
  select(-Year1:-Prelim, -Yr1:-Yr3)
  

#########################################################################################################
options('ReporteRs-fontsize'=11, 'ReporteRs-default-font'='Times New Roman')
#########################################################################################################



reportout = docx()
reportout = addSection( reportout, landscape = FALSE)
reportout = addFlexTable(reportout, vanilla.table(UTMsampleYear))
#reportout = addSection( reportout )
writeDoc( reportout, file = "Sampling.docx")