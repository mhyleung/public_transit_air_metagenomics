#Open ARG data
ARG <- read.tidy("resfam_output_w_meta.txt")

#Open Plasmid data
Plas <- read.tidy("../plasflow_R3/plasflow_out_all_contigs_new_tax_only.txt")

#Merge
Merge <- merge(Plas,ARG, by.y="Contig",by.x="contig_name", all.x=TRUE)

write.tidy(Merge,"plasmid_w_ARG_data.txt")

#Plot ARG mechanism by genomic context based on tallied data
Merge <- read.tidy("plasmid_w_ARG_data_tally.txt")
Merge <- Merge[-which(Merge$Mechanism == "No ARG"), ]
Merge <- Merge[-which(Merge$Context == "Unclassified") ,]

Merge$Mechanism <- factor(Merge$Mechanism, levels = c("ABC Transporter","Acetyltransferase","Adenylyltransferase","Aminotransferase","Antibiotic Target","Beta-Lactamase",
                                                      "D-ala D-ala Ligase","Gene Modulating Resistance","Gylcopeptide Resistance","Methyltransferase","MFS Transporter","Nucleotidyltransferase","Other Efflux","Phosphotransferase","RND Antibiotic Efflux","rRNA Methyltransferase","Stress Response","Target Overexpression","Target Protection","Other"))
Plot <- ggplot(Merge,aes(x=Mechanism,y=Number,fill=Context))
Plot <- Plot + geom_bar(stat="identity") + facet_wrap(~Context, ncol=1) 
Plot <- Plot + theme_classic() + scale_y_continuous(expand = c(0, 0)) + theme(axis.text.x=element_text(angle=270,vjust=0.5,hjust=0))
Plot <- Plot + ylab(paste0("No. of Contigs")) + xlab(paste0("Resistance Mechanism"))
ggsave("arg_mechanism_context.pdf",width=9,height=6,units="in")

#Combine plasmid_w_ARG data with ARG drug classes
Table <- read.tidy("resfam_output_w_meta.txt")
Class <- read.tidy("rf_drugclass.txt")
Merge <- merge(Table,Class,by.x="ResfamID", by.y="RF", all.x=TRUE)
write.tidy(Merge,"resfam_output_w_meta.txt")



#Contig merge with plasmid data
Plas <- read.tidy("../plasflow_R3/plasflow_out_all_contigs_new_tax_only.txt")
ARG <- read.tidy("contig_list_w_ARG.txt")
Merge <- merge(Plas,ARG,by.x="contig_name",by.y="Contig",all.y=TRUE)
write.tidy(Merge,"plasmid_w_ARG_data_new.txt")

#Combine contig/ARG/plasmid data with resfam data
Merge <- read.tidy("plasmid_w_ARG_data_new.txt")
ARG <- read.tidy("resfam_output_w_meta.txt")

Merge <- merge(Merge,ARG,by.y="Contig", by.x="contig_name",all.x=TRUE)
write.tidy(Merge,"plasmid_w_ARG_data_new.txt")
