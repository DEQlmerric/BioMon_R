# Author: Shannon Hubler
# Initial date: 4/5/21

# Purpose: show how well RCA-2020 represents sites across the landscape
#       1) bring in RCA-2020 final outputs (one.table_rule.all)--contains final ref designation, plus disturbance metrics
#       2) bring in StreamCat data--contains natural and disturbance metrics
#       3) join by COMID

library(readxl)
library(tidyverse)

# RCA-2020 outputs

rca2020 <- read.csv('//deqlab1/GIS_WA/Project_Working_Folders/Reference/2020/_Final outputs/one.table_rule.all.csv')

# remove sites with missing or invalid COMIDs

rca2020 <- rca2020[!is.na(rca2020$COMID),]
rca2020 <- rca2020[ rca2020$COMID >0, ]

# StreamCat

cat <- read.csv('//deqlab1/GIS_WA/Project_Working_Folders/StreamCat/StreamCat_all_OR.csv')
cat_def <- read_excel('//deqlab1/gis_wa/Project_Working_Folders/StreamCat/StreamCat_metrics_definitions_2.xlsx') # StreamCat metrics, definitions, and class codings
cat_def <-cat_def[,-1]


# winnow down StreamCat (cat) to only those metrics at Ws scale--take the definitions df (cat_def), 
# filter out only Ws metrics (based on Ws-Cat)
# also filter out only for natural variables --unimpacted by human activities
cat_def_Ws.Nat <- cat_def %>% 
  filter(Ws_Cat == 'Ws') %>%
  filter(Nat_Hum == 'natural')

#Get a vector of Metric.Name from cat_def_Ws that also exist as column in cat
cat_met.name <- cat_def_Ws.Nat$Metric.Name[cat_def_Ws.Nat$Metric.Name %in% names(cat)]


cat_Ws.Nat <- cat %>%
  select(COMID, cat_met.name)


# join rca and cat together

ref.cat <- left_join(rca2020, cat_Ws.Nat, by=c('COMID'))

ref.cat <- unique(ref.cat)  
  
  
###################################  
##
####   PCA
##
####################################


# remove columns with "zero variance"
which(apply(ref.cat, 2, var)==0)  



# missing data blows up the PCA -- remove rows with missing data

# change NAs from Disturb.score and BPJ.final -- these can be changed to some sort of dummy/marker value

ref.cat$Disturb.score[is.na(ref.cat$Disturb.score)] <- -999
ref.cat$BPJ_final[is.na(ref.cat$BPJ_final)] <- "Not Assessed"

# remove incomplete cases
ref.cat_complete <- ref.cat[complete.cases(ref.cat), ]

@@@@@@
  @@@@@@@   removes 84 records without COMIDs matching....several are ref sites, need to include to show better coverage
@@@@@@
  
                # data transformations for PCA: normal distributions an assumption for linear relationships
                #source("//deqlab1/Biomon/R Stats/chris parker scripts/transform.variables[1]_SH divide by 100.r")
                #transform.view(ref.cat[,c(27:65)])
  

            
# create a new df for assessing PCA with transformed variables
ref.cat_trans <- ref.cat_complete

# remove variables with little info  
ref.cat_trans <- ref.cat_trans %>%   
    select (-c(PctCarbResidWs, PctGlacTilClayWs,	PctGlacTilLoamWs,	PctGlacTilCrsWs, PctGlacLakeCrsWs,
             PctGlacLakeFineWs,	PctHydricWs,	PctEolCrsWs,	PctEolFineWs,	PctSalLakeWs,	PctAlluvCoastWs,
             PctCoastCrsWs,	PctWaterWs,	AgKffactWs))


ref.cat_complete <- ref.cat_complete %>%   
  select (-c(PctCarbResidWs, PctGlacTilClayWs,	PctGlacTilLoamWs,	PctGlacTilCrsWs, PctGlacLakeCrsWs,
             PctGlacLakeFineWs,	PctHydricWs,	PctEolCrsWs,	PctEolFineWs,	PctSalLakeWs,	PctAlluvCoastWs,
             PctCoastCrsWs,	PctWaterWs,	AgKffactWs))

  
# data transformations


# use a formula and dplyr for multiple columns
log.10p1 <- function(x, na.rm = FALSE) (log10(x +1)) #, na.rm = na.rm))
log.10 <- function(x, na.rm = FALSE) (log10(x))      #, na.rm = na.rm))
asin.sqrt.100 <- function(x, na.rm = FALSE) (asin(sqrt(x/100))) #, na.rm = na.rm)))
sqrt2 <- function(x, na.rm = FALSE) (sqrt(x))         #, na.rm = na.rm))



ref.cat_trans <- ref.cat_trans %>% 
  mutate_at(c('WsAreaSqKm', 'ElevWs','MgOWs'), log.10p1)  %>% 
  mutate_at(c('SWs', 'NWs','HydrlCondWs'), log.10)  

# summary(ref.cat$PctNonCarbResidWs); summary(ref.cat_complete$PctNonCarbResidWs) 
# verify it worked

ref.cat_trans <- ref.cat_trans %>% 
  mutate_at(c('CompStrgthWs'), sqrt2)  

ref.cat_trans <- ref.cat_trans %>% 
  mutate_at(c('PctNonCarbResidWs', 'PctSilicicWs'), asin.sqrt.100)  



  
# run PCA______choose transformed (ref.cat_trans) or non-transformed (ref.cat_complete)
library(FactoMineR)  
library("factoextra")
library("corrplot")
pca.allsites <- PCA(ref.cat_complete[,c(27:51)], graph = FALSE)

summary(pca.allsites)
str(pca.allsites)
print(pca.allsites)
eig.val <- get_eigenvalue(pca.allsites) # Extract the eigenvalues/variances of principal components
fviz_eig(pca.allsites, addlabels = TRUE, ylim = c(0, 50) ) # Visualize the eigenvalues


# Extract the results for individuals and variables, respectively.
ind <- get_pca_ind(pca.allsites); 
var <- get_pca_var(pca.allsites) 

# Visualize the results individuals and variables, respectively.
fviz_pca_ind(pca.allsites)
fviz_pca_var(pca.allsites, col.var = "blue") 

fviz_pca_biplot(pca.allsites) # Make a biplot of individuals and variables.


corrplot(var$cos2, is.corr=FALSE)  # ???????

# Total cos2 of variables on Dim.1 and Dim.2
fviz_cos2(pca.allsites, choice = "var", axes = 1:2)


# Color by cos2 values: quality on the factor map
fviz_pca_var(pca.allsites, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE # Avoid text overlapping
)


fviz_pca_var(pca.allsites, alpha.var = "cos2") # Change the transparency by cos2 values

corrplot(var$contrib, is.corr=FALSE)  

fviz_contrib(pca.allsites, choice = "var", axes = 1, top = 15)
fviz_contrib(pca.allsites, choice = "var", axes = 2, top = 15)
fviz_contrib(pca.allsites, choice = "var", axes = 1:2, top = 15)

# The most important (or, contributing) variables can be highlighted on the correlation plot as follow:
fviz_pca_var(pca.allsites, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
)

            
 fviz_pca_ind(pca.allsites, col.ind = "cos2", 
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
              repel = TRUE # Avoid text overlapping (slow if many points)
             )




grp <- as.factor(ref.cat$Eco3)
# Color variables by groups
fviz_pca_var(pca.allsites, col.var = grp, 
             palette = c("#0073C2FF", "#EFC000FF", "#868686FF"),
             legend.title = "Cluster")

@@@@@@@@ doesnt work.  grouping var not the same length as # rows in pca



# Color by groups


# Eco3
fviz_pca_ind(pca.allsites,
             geom.ind = "point", # show points only (nbut not "text")
             col.ind = ref.cat_trans$Eco3, # color by groups
             palette = 'lancet', #c('violet', 'blue', 'green', 'gray', 'orange', 'red', 'pink', 'black', 'forest green'),
             addEllipses = TRUE, # Concentration ellipses
             legend.title = "Groups"
)

# Ref 2020
fviz_pca_ind(pca.allsites,
             geom.ind = 'point', # show points only (nbut not "text")
             col.ind = ref.cat_trans$Ref2020_FINAL, # color by groups
             palette = c('forest green', 'blue', 'red'),
             addEllipses = TRUE, # Concentration ellipses
             legend.title = 'Ref Class 2016')

# Owner
fviz_pca_ind(pca.allsites,
             geom.ind = 'point', # show points only (nbut not "text")
             col.ind = ref.cat_trans$owner, # color by groups
             palette = c('forest green', 'blue', 'red'),
             #addEllipses = TRUE, # Concentration ellipses
             legend.title = 'Ref class 2020')






# scientific journal palettes from ggsci R package, e.g.: "npg", "aaas", "lancet", "jco", "ucscgb", "uchicago", "simpsons" and "rickandmorty".


# Change the size of arrows an labels
fviz_pca_var(pca.allsites, arrowsize = 1, labelsize = 5, 
             repel = TRUE)



fviz_pca_biplot(pca.allsites, 
                # Fill individuals by groups
                geom.ind = "point",
                pointshape = 21,
                pointsize = 2,
                fill.ind = ref.cat_trans$Eco3,
                col.ind = "black",
                # Color variable by groups
                #col.var = factor(c("sepal", "sepal", "petal", "petal")),
                
                legend.title = list(fill = "Eco3", color = "Clusters"),
                repel = TRUE        # Avoid label overplotting
)+
  ggpubr::fill_palette("lancet")+      # Indiviual fill color
  ggpubr::color_palette("npg")      # Variable colors     # Variable colors





fviz_pca_ind(pca.allsites, 
             # Fill individuals by groups
             geom.ind = "point",
             pointshape = 21,
             pointsize = 2,
             fill.ind = ref.cat_trans$Eco3, 
             addEllipses = TRUE,
             #col.ind = "black",
             # Color variable by groups
             #col.var = factor(c("sepal", "sepal", "petal", "petal")),
             
             legend.title = list(fill = "Eco3"), #, color = "Clusters"),
             repel = TRUE        # Avoid label overplotting
)+
  ggpubr::fill_palette("lancet") #+      # Indiviual fill color
# ggpubr::color_palette("npg")      # Variable colors     # Variable colors


fviz_pca_ind(pca.allsites,
             geom.ind = "point", # show points only (but not "text")
             pointshape = 21,
             pointsize = ref.cat_trans$Ref2020_FINAL,
             fill.ind = ref.cat_trans$Eco3, # color by groups
             palette = c('purple', 'blue', 'green', 'orange', 'red', 'gray', 'pink', 'black', 'forest green'), 
             addEllipses = FALSE, # Concentration ellipses
             legend.title = "Groups")+
  scale_shape_manual(values=c(19,20,21))



fviz_pca_biplot(pca.allsites,
                geom.ind = "point", # show points only (nbut not "text")
                pointshape = 21 ,
                pointsize = ref.cat_trans$Ref2020_FINAL,
                fill.ind = ref.cat_trans$Eco3, # color by groups
                palette = c('purple', 'blue', 'green', 'orange', 'red', 'gray', 'black','yellow',  'forest green'), 
                addEllipses = FALSE, # Concentration ellipses
                select.var = list(cos2  = 0.5),  # opnly include variables with cos2 > 0.5
                legend.title = "Groups"
) 



# from CA F&G code
library(ggplot2)

pca.plot<-
  ggplot(stations, aes(x=Nat.PC1, y=Nat.PC2))+
  geom_point()+
  theme_bw()

ggsave(pca.plot, filename="PeteFigures011013/pca.plot.eps",width=10, height=8, units="in") 
# ggsave(pca.plot, filename="PeteFigures011013/pca.plot.jpg",width=10, height=8, units="in") 

pca.plot.refs<-
  pca.plot+
  geom_point(data=subset(stations, SiteStatus=="Reference"), shape=21, fill="gray90", size=4)

ggsave(pca.plot.refs, filename="PeteFigures011013/pca.plot.refs.eps",width=10, height=8, units="in") 


 



# Summary stats


ref.cat_nat <- ref.cat_complete[ , c(25, 27:51)] 

ref.cat_nat <- ref.cat_nat %>% 
  pivot_longer(!Ref2020_FINAL, names_to = "Metric", values_to = "Value")




sum.stats <- ref.cat_nat %>%
  group_by(Metric, Ref2020_FINAL)%>%
  summarise(
    n = n(),
    min = min(Value),
    max = max(Value),
    mean = mean(Value),
    SD = sd(Value)
    )




sum.stats_long <- sum.stats %>%
  pivot_longer(!c(Metric, Ref2020_FINAL), names_to = "stat", values_to = 'Value')


sum.stats_final <- sum.stats_long %>% 
  pivot_wider(names_from = c(Ref2020_FINAL, stat), values_from = Value)

write.csv(sum.stats_final, 'Reference/PCA/sum.stats.csv')
