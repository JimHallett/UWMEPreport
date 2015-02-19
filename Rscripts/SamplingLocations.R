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
  select(-Station, -Year1:-Yr3, -Easting) %>%
  arrange(Unit, Habitat) %>%
  group_by(Owner, Unit, Habitat, YearsSampled) %>%
  count(Owner, Unit, Habitat, YearsSampled)



UTMsampleYear$YearsSampled <- gsub(', NA', '', UTMsampleYear$YearsSampled)

#########################################################################################################
options('ReporteRs-fontsize'=11, 'ReporteRs-default-font'='Times New Roman')
#########################################################################################################


reportout = docx(template = "Annual Progress Report 2014.docx") 



Table1 <-   FlexTable(UTMsampleYear, header.columns = FALSE)

Table1 = addHeaderRow(Table1, 
                      value = c( "Owner", "Management unit", "Habitat", "Years sampled", "Number of sampling points"),
                     cell.properties = cellProperties(padding.bottom = 3, padding.top = 3))


setFlexTableBorders(Table1,inner.vertical=borderProperties(style = "none"),
                    inner.horizontal=borderProperties(style = "none"),
                    outer.vertical=borderProperties(style = "none"),
                    outer.horizontal=borderProperties(),
                    body=TRUE,header=TRUE,footer=FALSE)
spanFlexTableRows(Table1, j = "Owner", runs = as.character( UTMsampleYear$Owner ) )

Table1[, 1:4] <-   parProperties(text.align = "left")
Table1[, 5] <-   parProperties(text.align = "center")




#Table1 <- borderNone()
Table1 <-   setZebraStyle(Table1, odd = '#eeeeee', even = 'white' ) 

reportout = addFlexTable(reportout, Table1, bookmark = "Table1") 
  writeDoc( reportout, file = "Report2015.docx", par.properties = parProperties(text.align = "center"),)

# 
# reportout = docx(template = "Annual Progress Report 2014.docx") %>%
#   # addSection( landscape = FALSE) %>%
#   addFlexTable( vanilla.table(UTMsampleYear), bookmark = "Table1", add.rownames = FALSE,
#                 body.cell.props = parLeft) %>%
#   writeDoc( file = "Report2015.docx")