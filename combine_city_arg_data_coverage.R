#Merge rpkm coverage data from all cities

Denver <- read.tidy("rpkm_coverage_combined_Denver.txt")
Oslo <- read.tidy("rpkm_coverage_combined_Oslo.txt")

London <- read.tidy("rpkm_coverage_combined_London.txt")
NYC <- read.tidy("rpkm_coverage_combined_NYC.txt")

Stockholm <- read.tidy("rpkm_coverage_combined_Stockholm.txt")
HKG <- read.tidy("rpkm_coverage_combined_HKG.txt")

Combined <- cbind(Denver,HKG,London,NYC,Oslo,Stockholm)

ARG <- read.tidy("../resfam_ARG/plasmid_w_ARG_data_new.txt")[c("contig_name","context","ResfamID","TargetName","Mechanism Classification","Drug")]
Merge <- merge(ARG,Combined,by.x="contig_name",by.y="Contig",all.x=TRUE)

#Merge ARG with read-length information
Length <- read.tidy("../resfam_ARG/RF_length.txt")
Merge <- merge(Merge,Length,by.x="ResfamID",by.y="Accession",all.x=TRUE)
write.tidy(Merge,"rpkm_coverage_combined_w_rf_length.txt")

#Organize table - plotting RPKM overall and by city
library(dplyr)
library(reshape2)

Merge <- read.tidy("rpkm_coverage_combined_w_rf_length_adjusted.txt")
Merge <- melt(Merge)
Merge <- Merge[ -c(7:11) ] 
Removed <- Merge[-which(Merge$value == "0") ,]
Removed <- Removed[-which(Removed$context == "unclassified") ,]

#Generate single plot comparing chromosome and plasmid
Plot <- ggplot(Removed,aes(x=Drug,y=value,fill=context))
Plot <- Plot + geom_violin()+ theme_classic() + scale_y_log10()
Plot <- Plot + coord_flip()
ggsave("plasmid_ARG_coverage_overall.pdf",width=7,height=9,units="in")

#Open metadata
Denver <- read.tidy("../../MetaSUB_Meta/air_new/meta/metadata_Denver_new.txt")[c("SL","City")]
Denver <- merge(Meta,Removed,by.x="SL",by.y="variable",all.x=TRUE)
Denver$log <- log10(Denver$value)
write.tidy(Denver,"rpkm_coverage_combined_w_rf_length_adjusted_Denver.txt")
Plot <- ggplot(Denver, aes(x=Drug,y=value, fill=context))
Plot <- Plot + geom_violin() + theme_classic() + scale_y_log10()
Plot <- Plot + coord_flip()
ggsave("plasmid_ARG_coverage_Denver.pdf",width=7,height=9,units="in")

HKG <- read.tidy("../../MetaSUB_Meta/air_new/meta/metadata_HKG_new.txt")[c("Sample","City")]
HKG <- merge(HKG,Removed,by.x="Sample",by.y="variable",all.x=TRUE)
HKG$log <- log10(HKG$value)
write.tidy(HKG,"rpkm_coverage_combined_w_rf_length_adjusted_HKG.txt")
Plot <- ggplot(HKG, aes(x=Drug,y=value, fill=context))
Plot <- Plot + geom_violin() + theme_classic() + scale_y_log10()
Plot <- Plot + coord_flip()
ggsave("plasmid_ARG_coverage_HKG.pdf",width=7,height=9,units="in")

London <- read.tidy("../../MetaSUB_Meta/air_new/meta/metadata_London_new.txt")[c("SL","City")]
London <- merge(London,Removed,by.x="SL",by.y="variable",all.x=TRUE)
London$log <- log10(London$value)
write.tidy(London,"rpkm_coverage_combined_w_rf_length_adjusted_London.txt")
Plot <- ggplot(London, aes(x=Drug,y=value, fill=context))
Plot <- Plot + geom_violin() + theme_classic() + scale_y_log10()
Plot <- Plot + coord_flip()
ggsave("plasmid_ARG_coverage_London.pdf",width=7,height=9,units="in")

NYC <- read.tidy("../../MetaSUB_Meta/air_new/meta/metadata_NYC_new.txt")[c("SL","City")]
NYC <- merge(NYC,Removed,by.x="SL",by.y="variable",all.x=TRUE)
NYC$log <- log10(NYC$value)
write.tidy(NYC,"rpkm_coverage_combined_w_rf_length_adjusted_NYC.txt")
Plot <- ggplot(NYC, aes(x=Drug,y=value, fill=context))
Plot <- Plot + geom_violin() + theme_classic() + scale_y_log10()
Plot <- Plot + coord_flip()
ggsave("plasmid_ARG_coverage_NYC.pdf",width=7,height=9,units="in")

Oslo <- read.tidy("../../MetaSUB_Meta/air_new/meta/metadata_Oslo_new.txt")[c("SL","City")]
Oslo <- merge(Oslo,Removed,by.x="SL",by.y="variable",all.x=TRUE)
Oslo$log <- log10(Oslo$value)
write.tidy(Oslo,"rpkm_coverage_combined_w_rf_length_adjusted_Oslo.txt")
Plot <- ggplot(Oslo, aes(x=Drug,y=value, fill=context))
Plot <- Plot + geom_violin() + theme_classic() + scale_y_log10()
Plot <- Plot + coord_flip()
ggsave("plasmid_ARG_coverage_Oslo.pdf",width=7,height=9,units="in")

Stockholm <- read.tidy("../../MetaSUB_Meta/air_new/meta/metadata_Stockholm_new.txt")[c("SL","City")]
Stockholm <- merge(Stockholm,Removed,by.x="SL",by.y="variable",all.x=TRUE)
Stockholm$log <- log10(Stockholm$value)
write.tidy(Stockholm,"rpkm_coverage_combined_w_rf_length_adjusted_Stockholm.txt")
Plot <- ggplot(Stockholm, aes(x=Drug,y=value, fill=context))
Plot <- Plot + geom_violin() + theme_classic() + scale_y_log10()
Plot <- Plot + coord_flip()
ggsave("plasmid_ARG_coverage_Stockholm.pdf",width=7,height=9,units="in")

#Comobine information from all city to create single file/plot
Denver <- read.tidy("rpkm_coverage_combined_w_rf_length_adjusted_Denver.txt")[c("SL","City","context","Drug","value")]
HKG <- read.tidy("rpkm_coverage_combined_w_rf_length_adjusted_HKG.txt")[c("Sample","City","context","Drug","value")]
colnames(HKG)[1] <- "SL"
London <- read.tidy("rpkm_coverage_combined_w_rf_length_adjusted_London.txt")[c("SL","City","context","Drug","value")]
NYC <- read.tidy("rpkm_coverage_combined_w_rf_length_adjusted_NYC.txt")[c("SL","City","context","Drug","value")]
Oslo <- read.tidy("rpkm_coverage_combined_w_rf_length_adjusted_Oslo.txt")[c("SL","City","context","Drug","value")]
Stockholm <- read.tidy("rpkm_coverage_combined_w_rf_length_adjusted_Stockholm.txt")[c("SL","City","context","Drug","value")]
Combined <- rbind(Denver,HKG,London,NYC,Oslo,Stockholm)

#Calculate total ARG levels of drug classes
Aggregate <- aggregate(London$value, by=list(Category=London$Drug), FUN=sum)

#Plot drug by context and city
Plot <- ggplot(Combined,aes(x=Drug,y=value,color = City))
Plot <- Plot + geom_violin() + theme_classic() + scale_y_log10() + facet_grid(context~City) #geom_jitter(size=0.1)
Plot <- Plot + coord_flip() + theme(legend.position="none")
ggsave("plasmid_ARG_coverage_city_combined.pdf",width=11,height=6,units="in",useDingbats=FALSE)