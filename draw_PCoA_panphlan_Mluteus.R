#Load libraries
library(devtools)
#install_github('wilkox/doPCoA')
library(doPCoA)
library(ggplot2)
library(wilkoxmisc)

#Read samples in UniFrac distances
UniFrac <- read.dist("binary_jaccard_panphlan_joined_tables_mluteus_new.txt")
Meta <- read.tidy("panphlan_output_sample_list_w_city.txt")
UniFracMatrix <- as.matrix(UniFrac)

#Check that UniFrac matrix rows match samples table (for ANOSIM)
sum(row.names(UniFracMatrix)==AllSamples$Sample) == length(AllSamples$Sample)
          
#Merge Dataframe with metadata
AllSamples <-data.frame(Sample = row.names(UniFracMatrix))
AllSamples <- merge(AllSamples, Meta, by = "Sample", all.x = TRUE)

write.tidy(AllSamples,"panphlan_merged_distance_taxonomy_ref_info.txt")
AllSamples <- read.tidy("panphlan_merged_distance_taxonomy_ref_info.txt")    


#Perform ANOSIM
UniFrac <- as.dist(UniFrac)
ANOSIM <- anosim(UniFrac, grouping = AllSamples$Ground)

#See ANOSIM options
ls(ANOSIM)
    
#Load ANOSIM statistics and signif
ANOSIM$statistic
ANOSIM$signif

#Do PCoA (not ANOSIM)
PCoAList <- do_PCoA(UniFrac)


PCoA <- PCoAList$Coordinates
PCoAV1 <- PCoAList$Variance1
PCoAV2 <- PCoAList$Variance2

#Merge PCoA with metadata
PCoA <- merge(PCoA, AllSamples, by.x = "Sample", all.x = TRUE)

#Specify X and Y axis
xlab <- paste0("PCoA1 (Variance Explained: ", PCoAV1, "%)")
ylab <- paste0("PCoA2 (Variance Explained: ", PCoAV2, "%)")
write.tidy(PCoA,"PCoA.txt")

PCoA <- read.tidy("PCoA.txt")
PCoA$City <- factor(PCoA$City, levels = c("Denver","Hong Kong","London","New York", "Oslo","Stockholm","Reference"))


Plot <- ggplot(PCoA, aes(x = PCoA1, y = PCoA2, colour=Clade))

Plot <- ggplot(PCoA, aes(x = PCoA1, y = PCoA2, colour=City))
Plot <- Plot + geom_point(size=1.5)
Plot <- Plot + xlab(xlab)
Plot <- Plot + ylab(ylab)
Plot <- Plot + theme_classic()
Plot <- Plot + scale_color_manual(values=c("#cdc673","#8a2be2","#ee7600","#6e8b3d","#1874cd","#ee6363","black"))
Plot <- Plot + theme(axis.title=element_text(size=18, face="bold"))
Plot <- Plot + theme(axis.text=element_text(size=14, face="bold"))
Plot <- Plot + theme(legend.title = element_blank(), legend.position="bottom")
Plot <- Plot + theme(legend.text = element_text(size=14, face = "bold"))
ggsave("panphlan_mluteus_w_ref.pdf",width=8,height=7,units="in",useDingbats=FALSE)
