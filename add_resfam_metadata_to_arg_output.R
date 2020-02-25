#Open ResFam metadata table
Meta <- read.tidy("180102_resfams_metadata_updated_v1.2.2.txt")

#Open ARG output
Table <- read.tidy("assembly_combined_secondary_out_simple.txt")

#Merge
Merge <- merge(Table,Meta,by="ResfamID",all.X=TRUE)
write.tidy(Merge,"resfam_output_w_meta.txt")


#Plot ARG/plasmid-positive/negative tally
Table <- read.tidy("plasmid_w_ARG_data.txt")

Plot <- ggplot(Table,aes(x=ARG,fill=context))
Plot <- Plot + geom_bar() + theme_classic()
ggsave("no_contig_plasmid_chromo.pdf",width=7,height=7,units="in")