library(dplyr)

Table <- read.tidy("merged_abundance_table_species_core_cities.cast.txt")
dplyr <- melt(Table)

Meta <- read.tidy("metadata_new.txt")[c("Sample","City")]

Merge <- merge(dplyr,Meta, by.x="variable",by.y="Sample",all.x=TRUE)

Merge$Sample <- factor(Merge$Sample,levels=c("Cutibacterium acnes","Micrococcus luteus","Enhydrobacter aerosaccus","Brachybacterium unclassified","Nocardioides unclassified","Dietzia unclassified","Corynebacterium tuberculostearicum","Deinococcus unclassified","Corynebacterium pseudogenitalium","Pseudomonas unclassified","Propionibacterium granulosum","Massilia unclassified","Gardnerella vaginalis","Kocuria rhizophila","Kocuria unclassified","Acinetobacter unclassified","Staphylococcus hominis"))
Plot <- ggplot(Merge,aes(x=value,colour=City)) + geom_density() + facet_wrap(~Sample,scales="free") 
Plot <- Plot + xlab(paste0("Relative Abundance (%)")) + ylab(paste0("Density")) + theme_minimal() + theme(legend.position="bottom", legend.title=element_blank())
ggsave("core_density_17.pdf",width=8,height=6,units="in")