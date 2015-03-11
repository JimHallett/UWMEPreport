library(RODBC)
library(vegan)
#library(BiodiversityR)

#rm(list=ls())

channel <- odbcConnect("discountasp", uid = "SQL2008_508574_uwmep_user", pwd = "Manis9") #this creates a connection to the database and reads the server from discountasp.dsn, and provides user credentials.

smtot <-sqlFetch(channel, "RNCoverinvasivesaveraged")  # read view from SQL database

smshrub <- subset(smtot, Habitat == "Grassland Steppe")


#ref <- sqlFetch(channel, "RNshrub")  # read view from SQL database
ref <- sqlFetch(channel, "RNgrasssteppe")  # read view from SQL database
#ref <- sqlFetch(channel, "RNriparianshrub")  # read view from SQL database

newsm <- paste(smshrub[,3]) # concatenate year and station
df <- subset(smshrub, select = c(4,7)) # extract scientific_name and count
final <- data.frame(df, newsm) # create data frame
plt <- final[,3] # plots
abu <- final[,2] # abundances
spc <- final[,1] # species
plt.codes <- levels(factor(plt))  #names of plotswee
spc.codes <- levels(factor(spc))  #name of species
# -------------------------------------------------------------------------------------
#Creates community matrix of species x locations
#--------------------------------------------------------------------------------------
taxa <- matrix(0,nrow=length(plt.codes),ncol=length(spc.codes))  # creates blank matrix
row <- match(plt,plt.codes)
col <- match(spc,spc.codes)
for (i in 1:length(abu)) {
  taxa[row[i],col[i]] <- abu[i]
}
taxa <- data.frame(taxa)
names(taxa) <- spc.codes
row.names(taxa) <- plt.codes


#----------nmds

taxa.dist <- vegdist(taxa, distance = "bray")
par(lwd = 3)
plot(taxa.ord <- metaMDS(taxa.dist), type="text", display="sites")
ordiellipse(taxa.ord, ref$Sitetype, label = TRUE)
grass.ano <- anosim(taxa.dist, ref$Sitetype)


