#
# Author: Shannon Hubler, Bug Overlord
# Original Code = Bio Tools R upgrade_v3.4_noBCG.R

# Adapted to a stand-alone set of code, just for calculating bug metrics
# This code would typically follow running "Data_from_AWQMS.R"

# 12.14.2020
 

# optimally, this would be turned into a function, to roll through all samples at once

library(reshape)
library(plyr)






## calculate total abundance from raw data
tot.abund<-aggregate(b_t_s$Count,  list(Sample=b_t_s$Sample), sum)  
colnames(tot.abund)[colnames(tot.abund)=="x"] <- "total.abundance"



# create a dataframe of relative abundances
rel.abund<-b_t_s[, c("Sample", "Taxon", "Count")]
rel.abund<-merge(rel.abund, tot.abund)
rel.abund<-ddply(.data = rel.abund, .(Sample, Taxon), plyr::summarize, rel.abund=Count/total.abundance)



                                              

# create a dataframe of relative abundances, but only for unique taxa -- used for Diversity/Evenness metrics
b_t_s.unique<- subset(b_t_s, UniqueTaxon=='UniqueTaxon')
tot.abund.unique <- aggregate(b_t_s.unique$Count,  list(Sample=b_t_s.unique$Sample), sum)  
rel.abund.unique<-b_t_s.unique[, c("Sample", "Taxon", "Count")]
rel.abund.unique<-merge(rel.abund.unique, tot.abund.unique)
colnames(rel.abund.unique)[colnames(rel.abund.unique)=="x"] <- "tot.abund.unique"
rel.abund.unique<-ddply(.data = rel.abund.unique, .(Sample, Taxon), plyr::summarize, rel.abund.unique=Count/tot.abund.unique)



###
####
##  ## METALS TOLERANCE INDEX
####
###


# need to drop taxa without tolerances, then re-adjust relative abundances
colnames(b_t_s)

myvars <- c("Sample", "Taxon", "Count", "MTI")
rel.abund.mti <- b_t_s[myvars]
# drop taxa missing MTI
#rel.abund.mti<-rel.abund.mti[complete.cases(rel.abund.mti),]
rel.abund.mti<-subset(rel.abund.mti, MTI != 666)     # remove '666' from d.f (they are taxa not used in the index)

#calculate total abundance
tot.abund.mti<-aggregate(rel.abund.mti$Count,  list(Sample=rel.abund.mti$Sample), sum)  
setnames(tot.abund.mti, old=c('x'), new=c('tot.abund.mti'))
rel.abund.mti<-merge(rel.abund.mti, tot.abund.mti, by='Sample')
rel.abund.mti$rel.abund<-(rel.abund.mti$Count/rel.abund.mti$tot.abund.mti)
rel.abund.mti$tol_X_ra<-(rel.abund.mti$MTI * rel.abund.mti$rel.abund)

#calculate MTI: sum_across.taxa(rel abund * tol)

MTI<-aggregate(rel.abund.mti$tol_X_ra,  list(Sample=rel.abund.mti$Sample), sum) 
setnames(MTI, old=c('x'), new=c('MTI'))



###
####
##  ## Total Richness
####
###


b_t_s$Unique.num<-ifelse(b_t_s$UniqueTaxon=='UniqueTaxon', 1,0)
total.richness<-aggregate(b_t_s$Unique.num,  list(Sample=b_t_s$Sample), sum)  
colnames(total.richness)[colnames(total.richness)=="x"] <- "total.richness"

 
###
####
##  ## Order_Family
####
###
 
# richness of all Orders --separatetly
order.rich<-count(b_t_s, vars=c('Sample','UniqueTaxon', 'Order'))
order.rich<- subset(order.rich, UniqueTaxon=='UniqueTaxon')
order.rich<-cast(order.rich, Sample ~ Order)
colnames(order.rich) <- paste(colnames(order.rich), "rich",  sep = ".") #add '.rich' to note the type of metric
order.rich[is.na(order.rich)] <- 0
colnames(order.rich)[colnames(order.rich)=="Sample.rich"] <- "Sample"

order5.rich<-subset(order.rich, select=c(Sample, Coleoptera.rich, Diptera.rich, Ephemeroptera.rich, Plecoptera.rich, Trichoptera.rich)) # limit families to 3




# richness of all Families --separately  
family.rich<-count(b_t_s, vars=c('Sample','UniqueTaxon', 'Family'))
family.rich<- subset(family.rich, UniqueTaxon=='UniqueTaxon')
family.rich<-cast(family.rich, Sample ~ Family)
colnames(family.rich) <- paste(colnames(family.rich), "rich",  sep = ".") #add '.rich' to note the type of metric
family.rich[is.na(family.rich)]<- 0
colnames(family.rich)[colnames(family.rich)=="Sample.rich"] <- "Sample"    

family3.rich<-subset(family.rich, select=c(Sample, Baetidae.rich, Chironomidae.rich, Hydropsychidae.rich)) # limit families to 3



# percent Order
# first calculate Order abundance
a<-ddply(.data=b_t_s, .(Sample, Order), summarize, Count=sum(Count))  # sum counts, across the  orders separately
a<-merge(tot.abund, a, all.x=TRUE)                              # bring in total abundance
pct_Order<-ddply(.data=a, .(Sample, Order), summarize, pct_ = (Count/total.abundance)*100 )  # calc % Abund: across station & order
pct_Order<-cast(pct_Order, Sample ~ Order)                    # matrify pct
colnames(pct_Order) <- paste("pct", colnames(pct_Order),   sep = "_")  # add prefix to denote metric type
colnames(pct_Order)[colnames(pct_Order)=="pct_Sample"] <- "Sample"
pct_Order[is.na(pct_Order)]<- 0

pct_order5<-subset(pct_Order, select=c(Sample, pct_Coleoptera, pct_Diptera, pct_Ephemeroptera, pct_Plecoptera, pct_Trichoptera)) # limit orders



###
####
##  ## EPT 
####
###

# richness

EPT.richness<-count(b_t_s, vars=c('Sample','UniqueTaxon', 'Order'))
EPT.richness<- subset(EPT.richness, UniqueTaxon=='UniqueTaxon')
EPT.richness<-subset(EPT.richness,  Order=='Ephemeroptera' | Order=='Plecoptera' |Order=='Trichoptera')
EPT.richness<-aggregate(EPT.richness$freq,  list(Sample=EPT.richness$Sample), sum)  
colnames(EPT.richness)[colnames(EPT.richness)=="x"] <- "EPT.rich"

# % EPT
pct_EPT<-ddply(.data=pct_Order, .(Sample), summarize, 
               pct_EPT = sum(pct_Ephemeroptera, pct_Plecoptera, pct_Trichoptera)
)



##
###
####
#     Voltinism = # of broods per year
####
###
##

# richness

voltine.rich<-count(b_t_s, vars=c('Sample','UniqueTaxon', 'Voltine'))
voltine.rich<- subset(voltine.rich, UniqueTaxon=='UniqueTaxon')
voltine.rich<-cast(voltine.rich, Sample ~ Voltine) 
voltine.rich[is.na(voltine.rich)]<- 0
setnames(voltine.rich, old = c('MV', 'SV', 'UV'), new = c('multivoltine.rich','semivoltine.rich', 'univoltine.rich'))
colnames(voltine.rich)
voltine.rich<-voltine.rich %>%
  select(Sample, multivoltine.rich, semivoltine.rich, univoltine.rich)


# pct_Voltinism

a<-ddply(.data=b_t_s, .(Sample, Voltine), plyr::summarize, Count=sum(Count)) #sum counts, across the voltinism categories separately
a<-merge(tot.abund, a, all.x=TRUE)
pct_Voltine<-ddply(.data=a, .(Sample, Voltine), plyr::summarize, pct = Count/total.abundance*100)
pct_Voltine<-cast(pct_Voltine, Sample ~ Voltine) 
pct_Voltine<-subset(pct_Voltine, select=c(Sample, MV, SV, UV)) # remove blanks or unknowns
setnames(pct_Voltine, old = c('MV', 'SV', 'UV'), new = c('pct_multivoltine','pct_semivoltine', 'pct_univoltine'))




##
###
####
#     Functional Feeding Groups
####
###
##

# richness
ffg.rich<-count(b_t_s, vars=c('Sample','UniqueTaxon', 'FFG'))
ffg.rich<- subset(ffg.rich, UniqueTaxon=='UniqueTaxon')
ffg.rich<-cast(ffg.rich, Sample ~ FFG) 
colnames(ffg.rich) <- paste(colnames(ffg.rich), "rich",  sep = ".")
colnames(ffg.rich)[colnames(ffg.rich)=="Sample.rich"] <- "Sample"
ffg.rich <- ffg.rich %>%
  select(-V1.rich)


# pct_FFG

a<-ddply(.data=b_t_s, .(Sample, FFG), plyr::summarize, Count=sum(Count)) #sum counts, across the FFG categories separately
a<-merge(tot.abund, a, all.x=TRUE)
pct_FFG<-ddply(.data=a, .(Sample, FFG), plyr::summarize, pct = Count/total.abundance*100)
pct_FFG<-cast(pct_FFG, Sample ~ FFG) 
colnames(pct_FFG) <- paste("pct", colnames(pct_FFG),   sep = "_")  # add prefix to denote metric type
colnames(pct_FFG)[colnames(pct_FFG)=="pct_Sample"] <- "Sample"
ffg.rich[is.na(ffg.rich)]<- 0
pct_FFG<-pct_FFG %>%
  select(-pct_V1)

##
###
####
#     Dominance
####
###
##

# % Dominant - Top 5 taxa
z<-ddply(.data=b_t_s, .(Sample),  plyr::summarize, Count=tail(sort(Count),5))
z<-merge(z, tot.abund)
z.1<-ddply(.data = z, .(Sample), plyr::summarize, pct.abund=Count/total.abundance*100)
pct_Dom.5<-ddply(.data=z.1, .(Sample), plyr::summarize, pct_Dom.5=sum(pct.abund))


# % Dominant - Top 3 taxa
z<-ddply(.data=b_t_s, .(Sample),  plyr::summarize, Count=tail(sort(Count),3))
z<-merge(z, tot.abund)
z.1<-ddply(.data = z, .(Sample), plyr::summarize, pct.abund=Count/total.abundance*100)
pct_Dom.3<-ddply(.data=z.1, .(Sample), plyr::summarize, pct_Dom.3=sum(pct.abund))

# % Dominant - Top 1 taxon
z<-ddply(.data=b_t_s, .(Sample),  plyr::summarize, Count=max(Count))
z<-merge(z, tot.abund)
pct_Dom.1<-ddply(.data = z, .(Sample), plyr::summarize, pct_Dom.1=Count/total.abundance*100)


# combine all dominance together
pct_Dom<- merge(pct_Dom.5, pct_Dom.3)
pct_Dom<- merge(pct_Dom, pct_Dom.1)



##
###
####
#     Diversity/Evenness
####
###
##

@@
@@@@@  Need to double check SH.div -- not working for single sample on Lady Cr.
@@


SH.div<-ddply(.data=rel.abund.unique, .(Sample), plyr::summarize, Shannon_diversity= -(sum(rel.abund.unique*log(rel.abund.unique))))
Simp.div<-ddply(.data=rel.abund.unique, .(Sample), plyr::summarize, Simpson_diversity= 1-sum(rel.abund.unique^2))
summary(Simp.div)


# Evenness


@@
  @@@@@  Need to double check SH.div -- not working for single sample on Lady Cr.
@@
  


even<-merge(SH.div, total.richness)
even<-ddply(.data=even, .(Sample), plyr::summarize, Evenness= Shannon_diversity/(log(total.richness)))
summary(even)  


##
###
####
#     Non-Insect
####
###
##


# richness of all Non-Insects  
noninsect.rich<-count(b_t_s, vars=c('Sample','UniqueTaxon', 'Class'))
noninsect.rich<- subset(noninsect.rich, UniqueTaxon=='UniqueTaxon' & Class != 'Insecta')
noninsect.rich<-ddply(.data=noninsect.rich, .(Sample), plyr::summarize, Non.Insect_richness=sum(freq))
summary(noninsect.rich)


# percent Non-Insects
# first calculate Order abundance
a<-ddply(.data=b_t_s, .(Sample, Class), plyr::summarize, Count=sum(Count))  # sum counts, across the  orders separately
a<- subset(a, Class != 'Insecta')
pct_Non.Insect<-ddply(.data=a, .(Sample), plyr::summarize, Count=sum(Count))
pct_Non.Insect<-merge(tot.abund, pct_Non.Insect, all.x=TRUE)                              # bring in total abundance
pct_Non.Insect<-ddply(.data = pct_Non.Insect, .(Sample), plyr::summarize, pct_Non.Insect=Count/total.abundance*100)
pct_Non.Insect[is.na(pct_Non.Insect)] <- 0


####
### ##
####  ##
###     ####        Combine all metrics together        #### ###
####  ##
### ##
####


list.of.data.frames = list(total.richness, order5.rich, family3.rich, 
                           pct_order5, EPT.richness, pct_EPT, 
                           voltine.rich, pct_Voltine,
                           ffg.rich, pct_FFG, pct_Dom, SH.div, 
                           Simp.div, even, noninsect.rich, pct_Non.Insect,
                           MTI)



metrics<-merge(list.of.data.frames[1], list.of.data.frames[2], by='Sample', all.x = TRUE)

for(i in 3:length(list.of.data.frames)){
  metrics <- merge(metrics, list.of.data.frames[i], by='Sample', all.x = TRUE)
  
}



