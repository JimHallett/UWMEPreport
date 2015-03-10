#library(plyr)
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

mammals <- sqlFetch(channel, "Mammal view") 

UTM <-sqlFetch(channel, "UTMs")  # read UTM data from SQL database

close(channel)

#######################################################################################
## code for table 1 Reference locations
#######################################################################################
UTM$Yr1 = as.character(UTM$Year1)
UTM$Yr2 = as.character(UTM$Year2)
UTM$Yr3 = as.character(UTM$Year3)

UTMsampleYear <- UTM %>%   #filter(Year1 == sampleYear | Year2 == sampleYear | Year3 == sampleYear ) %>%
  filter(`Site type` == "Reference") %>%
  mutate(YearsSampled = paste(Yr1, Yr2, Yr3, sep = ', ')) %>%
  select(-Station, -Year1:-Yr3, -Easting) %>%
  arrange(Habitat, Unit, Owner, YearsSampled) %>%
  group_by(Habitat, Unit, Owner, YearsSampled) %>%
  count(Habitat, Unit, Owner, YearsSampled)

UTMsampleYear$YearsSampled <- gsub(', NA', '', UTMsampleYear$YearsSampled)     # Gets rid of NA strings

#######################################################################################
## code for table 2 Permanent locations
#######################################################################################

UTMPsampleYear <- UTM %>%   #filter(Year1 == sampleYear | Year2 == sampleYear | Year3 == sampleYear ) %>%
  filter(`Site type` == "Permanent") %>%
  mutate(YearsSampled = paste(Yr1, Yr2, Yr3, sep = ', ')) %>%
  select(-Station, -Year1:-Yr3, -Easting) %>%
  arrange(Habitat, Unit, Owner, YearsSampled) %>%
  group_by(Habitat, Unit, Owner, YearsSampled) %>%
  count(Habitat, Unit, Owner, YearsSampled)

UTMPsampleYear$YearsSampled <- gsub(', NA', '', UTMPsampleYear$YearsSampled)     # Gets rid of NA strings

#######################################################################################
## code for table  Mammal captures
#######################################################################################


mammalsampleYear <- mammals %>%
  filter(year(Date) == sampleYear) %>%
  select(-Owner, -Date, -Station, -Northing, -Easting, -`Common Name`, -Sex:-Year) %>%
  arrange(Unit, Habitat, `Scientific Name`) %>%
  group_by(Unit, Habitat, `Scientific Name`) %>%
  count(Unit, Habitat, `Scientific Name`) %>%
  spread('Scientific Name', n)

mammalsampleYear[is.na(mammalsampleYear)] <- 0                              # Replace NA with 0 throughout



#########################################################################################################
options("ReporteRs-fontsize"=11, "ReporteRs-default-font"="Times New Roman")
#########################################################################################################


########################################################################################################
## Table 1 Reference points
########################################################################################################

Table1 <-   FlexTable(UTMsampleYear, header.columns = FALSE)

Table1 = addHeaderRow(Table1, 
          value = c( "Habitat", "Management unit", "Owner", "Years sampled", "Sampling points (n)"),
          cell.properties = cellProperties(padding.bottom = 3, padding.top = 3))

setFlexTableBorders(Table1,inner.vertical=borderProperties(style = "none"),
                    inner.horizontal=borderProperties(style = "none"),
                    outer.vertical=borderProperties(style = "none"),
                    outer.horizontal=borderProperties(),
                    body=TRUE,header=TRUE,footer=FALSE)

#spanFlexTableRows(Table1, j = "Owner", runs = as.character( UTMsampleYear$Owner ) )

Table1[, 1:4] <-   parProperties(text.align = "left")
Table1[, 5] <-   parProperties(text.align = "center")
Table1 <-   setZebraStyle(Table1, odd = '#eeeeee', even = 'white' ) 

########################################################################################################
## Table 2 Permanent points
########################################################################################################

Table2 <-   FlexTable(UTMPsampleYear, header.columns = FALSE)

Table2 = addHeaderRow(Table2, 
                      value = c( "Habitat", "Management unit", "Owner", "Years sampled", "Sampling points (n)"),
                      cell.properties = cellProperties(padding.bottom = 3, padding.top = 3))

setFlexTableBorders(Table2,inner.vertical=borderProperties(style = "none"),
                    inner.horizontal=borderProperties(style = "none"),
                    outer.vertical=borderProperties(style = "none"),
                    outer.horizontal=borderProperties(),
                    body=TRUE,header=TRUE,footer=FALSE)

#spanFlexTableRows(Table2, j = "Owner", runs = as.character( UTMsampleYear$Owner ) )

Table2[, 1:4] <-   parProperties(text.align = "left")
Table2[, 5] <-   parProperties(text.align = "center")
Table2 <-   setZebraStyle(Table2, odd = '#eeeeee', even = 'white' ) 

########################################################################################################
## Table mammals
########################################################################################################

TableMammal <-   FlexTable(mammalsampleYear, header.columns = TRUE)

setFlexTableBorders(TableMammal,inner.vertical=borderProperties(style = "none"),
                    inner.horizontal=borderProperties(style = "none"),
                    outer.vertical=borderProperties(style = "none"),
                    outer.horizontal=borderProperties(),
                    body=TRUE,header=TRUE,footer=FALSE)

TableMammal[, 1:2] <-   parProperties(text.align = "left")
TableMammal[, 3:10] <-   parProperties(text.align = "center")
TableMammal <-   setZebraStyle(TableMammal, odd = '#eeeeee', even = 'white' ) 



reportout = docx(template = "Annual Progress Report 2014.docx")        #Give the report a start

########################################################################################################
# Output the report
########################################################################################################
fig1 <- file.path( getwd(), "SiteLocations65.jpg")
fig2 <- file.path( getwd(), "Fig2KalmapsSmall.jpg")
fig3 <- file.path( getwd(), "KalID2014crop.jpg")
fig4 <- file.path( getwd(), "SpeciesComp65.jpg")
fig5 <- file.path( getwd(), "fig5.jpg")

reportout <- addImage(reportout, fig1, ppi = 300, bookmark = "Fig1", par.properties = parCenter() )
reportout <- addImage(reportout, fig2, ppi = 266, bookmark = "Fig2", par.properties = parCenter() )
#reportout <- addImage(reportout, fig3, bookmark = "Fig3", par.properties = parCenter() )

reportout = addFlexTable(reportout, Table1, bookmark = "Table1") 
reportout = addFlexTable(reportout, Table2, bookmark = "Table2") 
reportout = addFlexTable(reportout, TableMammal, bookmark = "Mammals") 
  writeDoc( reportout, file = "Report2015.docx", par.properties = parProperties(text.align = "center"),)
