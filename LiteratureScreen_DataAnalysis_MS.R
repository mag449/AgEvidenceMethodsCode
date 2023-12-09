#Data analysis and figures for literature screen manuscript ----
#AgEvidence: A dataset to explore agro-ecological effects of conservation agriculture
#Lesley Atwood, Maria Gannett, Stephen A. Wood
#Data and library loading ----
rm(list=ls(all=TRUE))

#Load libraries
library(dplyr)
library(ggplot2)
library(stringr)
library(data.table)
library(fossil)
library(igraph)
library(tidyr)

#Results and Reference tabs from each conservation ag practice
#Cover crops
CC<-read.csv("C:/Users/Maria/Documents/AgEvidence/RelationalDataTables/CoverCrops_AgE_US/Results.csv")
CC_ref<-read.csv("C:/Users/Maria/Documents/AgEvidence/RelationalDataTables/CoverCrops_AgE_US/Reference.csv")
CC_ref$practice<-"Cover crops"

#Crop rotation
CR<-read.csv("C:/Users/Maria/Documents/AgEvidence/RelationalDataTables/CropRotation_AgE_US/Results.csv")
CR_ref<-read.csv("C:/Users/Maria/Documents/AgEvidence/RelationalDataTables/CropRotation_AgE_US/Reference.csv")
CR_ref<-CR_ref[!apply(is.na(CR_ref) | CR_ref == "", 1, all),] #gets rid of some phantom (all NA) rows
CR_ref$practice<-"Crop rotation"

#Nutrient management
NM<-read.csv("C:/Users/Maria/Documents/AgEvidence/RelationalDataTables/NutrientMgmt_AgE_US/Results.csv")
NM_ref <- read.csv("C:/Users/Maria/Documents/AgEvidence/RelationalDataTables/NutrientMgmt_AgE_US/Reference.csv")
NM_ref$practice<-"Nutrient management"

#Pest management
PM<-read.csv("C:/Users/Maria/Documents/AgEvidence/RelationalDataTables/PestMgmt_AgE_US/Results.csv")
PM_ref<-read.csv("C:/Users/Maria/Documents/AgEvidence/RelationalDataTables/PestMgmt_AgE_US/Reference.csv")
PM_ref$practice<-"Pest management"

#Reduced tillage
TL<-read.csv("C:/Users/Maria/Documents/AgEvidence/RelationalDataTables/Tillage_AgE_US/Results.csv")
TL_ref<-read.csv("C:/Users/Maria/Documents/AgEvidence/RelationalDataTables/Tillage_AgE_US/Reference.csv")
TL_ref$practice<-"Tillage"

#Get rid of readername and dateread for CC, CR, and NM to be able to combine with the other reference datasets
CC_ref<-select(CC_ref,-c("readername","dateread"))
CR_ref<-select(CR_ref,-c("readername","dateread"))
NM_ref<-select(NM_ref,-c("readername","dateread"))
TL_ref<-select(TL_ref,-c("readername","dateread"))

#Combine all the reference datasets
Refs<-rbind(CC_ref,CR_ref,NM_ref,PM_ref,TL_ref)

#Clean CC data ----
#Create a column for percent change
#Use ifelse to isolate the calculation for values measured in percent
CC<-CC %>%
  mutate(per_change = ifelse(rv_units=="percent",
                             ((trt2_value*100)-(trt1_value*100)),
                             ((trt2_value)-(trt1_value))/((trt1_value))*100)) %>%
  mutate(per_change = round(per_change, digits = 2))

#Delete any -Infs, Infs, and NaNs
CC[is.na(CC) | CC=="-Inf"] = NA 
CC[is.na(CC) | CC=="Inf"] = NA
CC[is.na(CC) | CC=="NaN"] = NA

CC_stats<-CC %>%
  summarize(mean_change=mean(CC$per_change,na.rm=TRUE))

#CC dataset check for normality ----
#Look at the distribution
ggplot(CC, aes(x=per_change)) + geom_histogram(binwidth=10,color = "white", fill = "#A2998B")+
  geom_vline(aes(xintercept=mean_change),CC_stats,color="#4472C4", linetype="dashed", linewidth=1)+
  xlim(-500,500)+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),panel.background=element_blank(),axis.line=element_line(color="black"))

#Identify potential outliers using z-scores
#Calculate z-scores of percent change values
CC$z <- scale(CC$per_change)
#Calculate z-scores of control and treatment values
CC$z_t1<-scale(CC$trt1_value)
CC$z_t2<-scale(CC$trt2_value)

#Look at the distribution of z-scores
hist(CC$z)
hist(CC$z_t1)
hist(CC$z_t2)

#Identify values with z-scores above 3.29 and below -3.29 so we can double check them from the primary literature
CC_Out<-filter(CC,CC$z > 3.29|CC$z < -3.29)
CC_Out_t<-filter(CC,CC$z_t1 > 3.29|CC$z_t1 < -3.29|CC$z_t2 > 3.29|CC$z_t2 < -3.29)


#Clean CR data ----
#Create a column for percent change
#Use ifelse to isolate the calculation for values measured in percent
CR<-CR %>%
  mutate(per_change = ifelse(rv_units=="percent",
                             ((trt2_value*100)-(trt1_value*100)),
                             ((trt2_value)-(trt1_value))/((trt1_value))*100)) %>%
  mutate(per_change = round(per_change, digits = 2))

#Delete any -Infs, Infs, and NaNs
CR[is.na(CR) | CR=="-Inf"] = NA
CR[is.na(CR) | CR=="Inf"] = NA
CR[is.na(CR) | CR=="NaN"] = NA

CR_stats<-CR %>%
  summarize(mean_change=mean(CR$per_change,na.rm=TRUE))

#CR dataset check for normality ----
#Look at the distribution
ggplot(CR, aes(x=per_change)) + geom_histogram(binwidth=10,color = "white", fill = "#A2998B")+
  geom_vline(aes(xintercept=mean_change),CR_stats,color="#4472C4", linetype="dashed", linewidth=1)+
  xlim(-500,500)+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),panel.background=element_blank(),axis.line=element_line(color="black"))

#Identify potential outliers using z-scores
#Calculate z-scores of percent change values
CR$z <- scale(CR$per_change)
#Calculate z-scores of control and treatment values
CR$z_t1<-scale(CR$trt1_value)
CR$z_t2<-scale(CR$trt2_value)

#Look at the distribution of z-scores
hist(CR$z)
hist(CR$z_t1)
hist(CR$z_t2)

#Identify values with z-scores above 3.29 and below -3.29 so we can double check them from the primary literature
CR_Out<-filter(CR,CR$z > 3.29|CR$z < -3.29)
CR_Out_t<-filter(CR,CR$z_t1 > 3.29|CR$z_t1 < -3.29|CR$z_t2 > 3.29|CR$z_t2 < -3.29)


#Clean NM data ----
#Create a column for percent change
#Use ifelse to isolate the calculation for values measured in percent
NM<-NM %>%
  mutate(per_change = ifelse(rv_units=="percent",
                             ((trt2_value*100)-(trt1_value*100)),
                             ((trt2_value)-(trt1_value))/((trt1_value))*100)) %>%
  mutate(per_change = round(per_change, digits = 2))

#Delete any -Infs, Infs, and NaNs
NM[is.na(NM) | NM=="-Inf"] = NA 
NM[is.na(NM) | NM=="Inf"] = NA
NM[is.na(NM) | NM=="NaN"] = NA

NM_stats<-NM %>%
  summarize(mean_change=mean(NM$per_change,na.rm=TRUE))

#NM dataset check for normality ----
#Look at the distribution
ggplot(NM, aes(x=per_change)) + geom_histogram(binwidth=10,color = "white", fill = "#A2998B")+
  geom_vline(aes(xintercept=mean_change),NM_stats,color="#4472C4", linetype="dashed", linewidth=1)+
  xlim(-500,500)+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),panel.background=element_blank(),axis.line=element_line(color="black"))

#Identify potential outliers using z-scores
#Calculate z-scores of percent change values
NM$z <- scale(NM$per_change)
#Calculate z-scores of control and treatment values
NM$z_t1<-scale(NM$trt1_value)
NM$z_t2<-scale(NM$trt2_value)

#Look at the distribution of z-scores
hist(NM$z)
hist(NM$z_t1)
hist(NM$z_t2)

#Identify values with z-scores above 3.29 and below -3.29 so we can double check them from the primary literature
NM_Out<-filter(NM,NM$z > 3.29|NM$z < -3.29)
NM_Out_t<-filter(NM,NM$z_t1 > 3.29|NM$z_t1 < -3.29|NM$z_t2 > 3.29|NM$z_t2 < -3.29)


#Clean PM data ----
#Create a column for percent change
#Use ifelse to isolate the calculation for values measured in percent
PM<-PM %>%
  mutate(per_change = ifelse(rv_units=="percent",
                             ((trt2_value*100)-(trt1_value*100)),
                             ((trt2_value)-(trt1_value))/((trt1_value))*100)) %>%
  mutate(per_change = round(per_change, digits = 2))

#Delete any -Infs, Infs, and NaNs
PM[is.na(PM) | PM=="-Inf"] = NA 
PM[is.na(PM) | PM=="Inf"] = NA
PM[is.na(PM) | PM=="NaN"] = NA

PM_stats<-PM %>%
  summarize(mean_change=mean(PM$per_change,na.rm=TRUE))

#PM dataset check for normality ----
#Look at the distribution
ggplot(PM, aes(x=per_change)) + geom_histogram(binwidth=10,color = "white", fill = "#A2998B")+
  geom_vline(aes(xintercept=mean_change),PM_stats,color="#4472C4", linetype="dashed", linewidth=1)+
  xlim(-500,500)+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),panel.background=element_blank(),axis.line=element_line(color="black"))

#Identify potential outliers using z-scores
#Calculate z-scores of percent change values
PM$z <- scale(PM$per_change)
#Calculate z-scores of control and treatment values
PM$z_t1<-scale(PM$trt1_value)
PM$z_t2<-scale(PM$trt2_value)

#Look at the distribution of z-scores
hist(PM$z)
hist(PM$z_t1)
hist(PM$z_t2)

#Identify values with z-scores above 3.29 and below -3.29 so we can double check them from the primary literature
PM_Out<-filter(PM,PM$z > 3.29|PM$z < -3.29)
PM_Out_t<-filter(PM,PM$z_t1 > 3.29|PM$z_t1 < -3.29|PM$z_t2 > 3.29|PM$z_t2 < -3.29)


#Clean TL data ----
#Create a column for percent change
#Use ifelse to isolate the calculation for values measured in percent
TL<-TL %>%
  mutate(per_change = ifelse(rv_units=="percent",
                             ((trt2_value*100)-(trt1_value*100)),
                             ((trt2_value)-(trt1_value))/((trt1_value))*100)) %>%
  mutate(per_change = round(per_change, digits = 2))

#Delete any -Infs, Infs, and NaNs
TL[is.na(TL) | TL=="-Inf"] = NA 
TL[is.na(TL) | TL=="Inf"] = NA
TL[is.na(TL) | TL=="NaN"] = NA

TL_stats<-TL %>%
  summarize(mean_change=mean(TL$per_change,na.rm=TRUE))

#TL dataset check for normality ----
#Look at the distribution
ggplot(TL, aes(x=per_change)) + geom_histogram(binwidth=10,color = "white", fill = "#A2998B")+
  geom_vline(aes(xintercept=mean_change),TL_stats,color="#4472C4", linetype="dashed", linewidth=1)+
  xlim(-500,500)+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),panel.background=element_blank(),axis.line=element_line(color="black"))

#Identify potential outliers using z-scores
#Calculate z-scores of percent change values
TL$z <- scale(TL$per_change)
#Calculate z-scores of control and treatment values
TL$z_t1<-scale(TL$trt1_value)
TL$z_t2<-scale(TL$trt2_value)

#Look at the distribution of z-scores
hist(TL$z)
hist(TL$z_t1)
hist(TL$z_t2)

#Identify values with z-scores above 3.29 and below -3.29 so we can double check them from the primary literature
TL_Out<-filter(TL,TL$z > 3.29|TL$z < -3.29)
TL_Out_t<-filter(TL,TL$z_t1 > 3.29|TL$z_t1 < -3.29|TL$z_t2 > 3.29|TL$z_t2 < -3.29)


#Figure out which references are in multiple datasets (Fig. 1) ----
DRefs1 <- Refs[duplicated(Refs$title, fromLast=TRUE), ]
DRefs2 <- Refs[duplicated(Refs$title), ]
DRefs3<-left_join(DRefs1,DRefs2, by = c("title"))
DRefs3$practice2<-str_c(DRefs3$practice.x,"_", DRefs3$practice.y)

table(DRefs3$practice2)

#Number of papers published each year by Ag practice (Fig. 2) ----
ragg::agg_png("Published_AgPractices_Filtered.png",width=6.5,height=4.5,units="in",pointsize=12,res=300)
ggplot(Refs, aes(pubyear,fill = practice)) + #factor(citation_pub_year) <- put this back in if really needs to be discrete
  geom_bar(stat="count", position = "stack") + 
  scale_fill_manual(limits=c("Cover crops","Crop rotation","Nutrient management","Pest management","Tillage"),
                    values=c("#A2998B","#E75480","#4472C4","#FFC000","#90BB40"),
                    name="Conservation agriculture practice")+
  scale_x_continuous(n.breaks=20,name="",limits = c(1980,2020))+ 
  scale_y_continuous(bquote('Papers included in AgEvidence (#)'))+ 
  geom_hline(aes(yintercept=0))+
  theme(axis.text=element_text(size=12))+
  theme(axis.text.x=element_text(angle=52,hjust=1))+
  theme(axis.title=element_text(size=12))+
  theme(legend.text=element_text(size=12))+
  theme(legend.title=element_text(size=12))+
  theme(plot.title=element_text(size=12))+
  theme(legend.position = c(0.25,0.75))+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),panel.background=element_blank(),axis.line=element_line(color="black"))
dev.off() 

#Explore the response variable categories ----
#Group level 3 groupings
CC_groups<-CC %>%
  select(paper_id,group_level1,group_level2,group_level3,per_change)
CC_groups$practice<-"Cover crops"
CR_groups<-CR %>%
  select(paper_id,group_level1,group_level2,group_level3,per_change)
CR_groups$practice<-"Crop rotation"
NM_groups<-NM %>%
  select(paper_id,group_level1,group_level2,group_level3,per_change)
NM_groups$practice<-"Nutrient management"
PM_groups<-PM %>%
  select(paper_id,group_level1,group_level2,group_level3,per_change)
PM_groups$practice<-"Pest management"
TL_groups<-TL %>%
  select(paper_id,group_level1,group_level2,group_level3,per_change)
TL_groups$practice<-"Tillage"

AllGroups<-rbind(CC_groups, CR_groups, NM_groups, PM_groups, TL_groups)

GroupsSum<-AllGroups %>%
  group_by(group_level1) %>%
  summarise(levels = n_distinct(group_level2)) %>%
  drop_na()

GroupsSum1<-AllGroups %>%
  group_by(group_level1,group_level2) %>%
  summarise(levels = n_distinct(group_level3)) %>%
  drop_na()

GroupsSum2<-AllGroups %>%
  group_by(group_level1,group_level2,group_level3) %>%
  summarise(obs=n())

GroupsSum3<-AllGroups %>%
  group_by(group_level1,practice) %>%
  summarise(papers = n_distinct(na.omit(paper_id))) %>%
  drop_na()

GroupsSum4<-GroupsSum3 %>%
  group_by(practice) %>%
  summarise(total=sum(na.omit(papers))) %>%
  ungroup()

GroupsSum3<-left_join(GroupsSum3,GroupsSum4,by="practice")
GroupsSum3$perc_data<-(GroupsSum3$papers/GroupsSum3$total)*100

#Percent of papers that report response variables within each category (Fig. 3) ----
ragg::agg_png("PercentPapers_ResponseVariable.png",width=5.5,height=6,units="in",pointsize=12,res=300)
ggplot(GroupsSum3, aes(x=practice,y=perc_data,fill=group_level1)) + 
  geom_bar(stat="identity", position = "stack") + 
  scale_fill_manual(limits=c("Climate Mitigation","Crop Yields","Economics","Other Soil Properties","Pests","Soil Nutrients","Water Quality"),
                    values=c("#972019","#7bcabb","#d8832b","grey","#165f99","pink","#DEBF4b"),
                    name="Response variable catagory\nof reported data")+
  geom_text(aes(label = papers),
            position = position_stack(vjust = .5))+ #vjust=-0.25
  scale_y_continuous(bquote('Papers included in AgEvidence (%)'))+ 
  scale_x_discrete(bquote(''))+ 
  geom_hline(aes(yintercept=0))+
  theme(axis.text=element_text(size=12))+
  theme(axis.text.x=element_text(angle=52,hjust=1))+
  theme(axis.title=element_text(size=12))+
  theme(legend.text=element_text(size=12))+
  theme(legend.title=element_text(size=12))+
  theme(plot.title=element_text(size=12))+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),panel.background=element_blank(),axis.line=element_line(color="black"))
dev.off() 

#Explore how frequently authors publish and with who (Fig. 4) ----
#Make the authors into a network analysis
#Lines connecting authors should be the width of how frequently they publish together
#Circles (authors) should be the size of how frequently they publish

#Create a table of authors and paper ids
#Separate authors and keep paper_ids
Authors<-setDT(Refs)[, strsplit(as.character(authors), "\\., | \\& | II," ), by = paper_id]

#Get rid of all &s that came with separating author lists
Authors$Authors<-gsub("\\&|and", "", Authors$V1)

#Get rid of any leading white spaces ("left") or trailing white spaces ("right")
Authors$Authors<- str_trim(Authors$Authors, "left")
Authors$Authors<- str_trim(Authors$Authors, "right")

#Delete a comma if it ends with one
Authors<-Authors %>%
  mutate(Authors = ifelse(grepl(",$", Authors), sub(",$", "", Authors), Authors)) 

#Now replace all second commas with a ;
Authors$AuthorsSemi <- gsub("^(.*?),(.*?),", "\\1;\\2,", Authors$Authors)

#Now split the AuthorsSemi again, but by ;
Authors2 <- setDT(Authors)[, .(author = unlist(strsplit(as.character(AuthorsSemi), ";"))), by = paper_id]

#Add a period if it doesn't already end with one
Authors2<-Authors2 %>%
  mutate(author = ifelse(!grepl("\\.$", author), paste0(author, "."), author))

#Get rid of any leading white spaces ("left") or trailing white spaces ("right") again (since we split again)
Authors2$author<- str_trim(Authors2$author, "left")
Authors2$author<- str_trim(Authors2$author, "right")

#Delete any rows with just initials
Authors2=Authors2[!grepl("^[A-Z]\\.$|^[A-Z]\\.[A-Z]\\.$",Authors2$author),]

#Delete second initial if there is one
Authors2$author<-sub("^(.*?), ([A-Z]\\.)(.*?$)", "\\1, \\2", Authors2$author)

#Make a new data frame with conservation ag practices to be able to attach to the new long format data frame
Ag_practice<-Refs[,c("paper_id","title","practice")]

#Join the two data frames
Authors2 <- left_join(Ag_practice,Authors2, by="paper_id")  

#Make sure authors are only listed once for each study
Authors2 <- unique( Authors2[ , c("paper_id","title","practice","author") ] ) 

#Make a new column with most common practice the author published in
mode <- function(x) { names(which.max(table(x))) }

Authors3 <- Authors2 %>% 
  mutate(practice = factor(practice)) %>%
  group_by(author) %>%
  summarise(mode=mode(practice))

#Instead of using mode, just assign "multiple" to authors who have published across multiple conservation practice
Authors3a <- Authors2 %>%
  group_by(author,practice) %>%
  summarise(obs=n())

Authors3b <- Authors3a %>%
  mutate(practice2 = case_when(
    author == lead(author) & practice != lead(practice) ~ "Multiple",
    author == lag(author) & practice != lag(practice) ~ "Multiple",
    TRUE ~ as.character(practice)))

Authors3c <- Authors3b %>%
  group_by(author,practice2) %>%
  summarise(obs=n())

#Re-calculate the unique number of studies included in AgEvidence
length(unique(Authors2$paper_id)) #424

#Create a column with frequency with which authors published
Author_count<-as.data.frame(table(Authors2$author))
colnames(Author_count)<-c("author","Freq")
Author_count$author<-as.character(Author_count$author)

Author_count<-Author_count %>%
  mutate(Pubs_bin=case_when
         (Freq>=30~"30 or more",
           Freq<30 & Freq>=20~"20 to 29",
           Freq<20 & Freq>=10~"10 to 19",
           Freq<10 & Freq>=3~"3 to 9",
           Freq<3 ~"Less than 3"))

Author_count2<-Author_count %>% 
  group_by(Pubs_bin) %>% 
  summarise(across(author, list))

#Make a new data frame with author publication frequencies to be able to attach to the new long format data frame
Author_freq<-Author_count[,c("author","Freq")]

#Join the two data frames again
Authors2 <- left_join(Author_freq,Authors2, by="author")
Authors2 <- left_join(Authors3,Authors2, by="author")

#For the presence/absence data only look at authors who published at least 3 times
#Delete entries where the author published 2 or fewer times
Authors4 <- subset(Authors2, Freq > 2)
Author4_count<-as.data.frame(table(Authors4$author))
colnames(Author4_count)<-c("author","Freq")
Author4_count<-inner_join(Author4_count, Authors3c, by="author")

#Create a matrix of authors and papers (presence/absence)
Authors4<-as.matrix(Authors4)
M<-as.matrix(create.matrix(Authors4,tax.name="paper_id",locality="author"))


#Network using the igraph package:

#Generate co-occurrence matrix with crossproduct
co_mat <- t(M) %*% M

#Set diagonal values to 0
diag(co_mat) <- 0

#Assign dim names
dimnames(co_mat) <- list(colnames(M), colnames(M))

#Number of publications
NumPubs<-colSums(co_mat)

#Create graph from adjacency matrix
g <- graph_from_adjacency_matrix(co_mat, mode = "upper", weighted = TRUE)

#Assign nodes weight equal to species frequency
g <- set.vertex.attribute(g, "v_weight", value = colSums(M))

#Assign color attribute to nodes
g <- set.vertex.attribute(g, "practice", value = Author4_count$practice2)
#Ag practice colors
colrs<-adjustcolor(c("#C3B1E1","#E75480","#90BB40","#FFC000","#A2998B","#4472C4"), alpha=0.6)
colrs_index <- match(V(g)$practice, unique(V(g)$practice))
node_colors <- colrs[colrs_index]

#igraph plot of co-occurence network:
ragg::agg_png("AuthorsOver2Pubs.png",width=9,height=9,units="in",pointsize=12,res=300)
plot(g, vertex.size = V(g)$v_weight * .75, edge.width = E(g)$weight * 1,
     layout=layout_nicely,
     vertex.color=node_colors,
     vertex.frame.width=0.75,
     vertex.label=NA,
     vertex.frame.color="black",
     layout = layout_nicely(g),margin=0)
legend(x=-1.3, y=-0.7, c("Multiple","Crop rotation","Tillage","Pest managment", "Cover crops","Nutrient management"), pch=21,
       col="#777777", pt.bg=colrs, pt.cex=2, cex=.8, bty="n", ncol=1)
dev.off() 

#Calculate network analysis stats:

#Edge density
#The proportion of present edges from all possible edges in the network
ED<-edge_density(g, loops=F)

#Average path length (not directional)
#the mean of the shortest distance between each pair of nodes in the network
mean_distance(g, directed=F)
