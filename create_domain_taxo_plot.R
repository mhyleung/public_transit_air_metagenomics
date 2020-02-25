#Open bacterial table

#Bacteria
#Create domain overview plot
Domain <- read.tidy("merged_abundance_table_all_cities_domain.txt")
Domain <- melt(Domain)
Domain$Sample <- factor(Domain$Sample, levels = c("Bacteria","Virus","Fungi","Archaea"))

#Merge with city metadata
Meta <- read.tidy("metadata_new.txt")
Merge <- merge(Domain,Meta, by.x="variable",by.y="Sample", all.x=TRUE)

Plot <- ggplot(Merge,aes(x=variable,y=value,fill=Sample))
Plot <- Plot + geom_bar(stat="identity",width=1) + scale_fill_brewer(palette="Set2") + theme_classic() + scale_y_continuous(expand=c(0,0))
Plot <- Plot + theme(axis.text.x=element_blank(), legend.title=element_blank(),axis.ticks.x = element_blank()) + facet_wrap(~City,scale="free_x",ncol=2)
Plot <- Plot + ylab(paste0("Overall relative abundance (%)")) + xlab(paste0("Sample")) + theme(legend.position="bottom")
ggsave("domain_relative_abundance.pdf",width=5,height=8,units="in")