#To test if SARs differs between regions in the most recent years of the record, we first grouped the CWT-based SAR data separately by smolt age (Yearling/Subyearling), region, and rearing type (hatchery/wild). For each of these groupings, we pooled all data in the 2010-2014 period across all populations in a region, and then resampled with replacement the pooled data N=10,000 times, each time drawing a sample of the same size as the original pooled data. For each group, we calculated the N median SARs, and then calculated the ratio of those N medians with those from each of the other regions in turn. The empirical distribution of the N ratios allows for a formal statistical test of the proposition that median SARs in the two regions are equal (i.e. that the ratios are not different from one). 



# Libraries ---------------------------------------------------------------
library(tidyverse)
library(ggpubr) #for ggarrange
library(ggthemes) #for economist palette
library(scales) #for show_col



# Options -----------------------------------------------------------------

Sys.setenv(TZ="GMT")
options(tibble.print_max = Inf)




# Import data and prepare-------------------------------------------------------------

sars<-read.csv("Welch_2020_Review_of_Coastwide_Decline_SAR_Data.csv", header=TRUE)
head(sars)


sars$SmoltAge<-factor(sars$SmoltAge)
sars$SmoltAge<-recode(sars$SmoltAge, "0"="Subyearling",
                      "1"="Yearling")


#pull out the PIT-based SARs
sars2<-filter(sars, Reference!="McCann et al. 2018")


#define the time period
sarsub<-filter(sars2, SmoltYear>=2010 & 
                      SmoltYear <=2014)




# Resample ----------------------------------------------------------------

#Need to make the samples the same size as the original sample. It turns out that the default number in the "sample" function is the full set, but I kept this to use for subsetting and QA.
counts<-sarsub %>%
  group_by(Region,
           SmoltAge,
           Rear) %>%
  summarise(count=n())


#define output dataframe
smp<-data.frame()

for (i in 1:nrow(counts)) { #for every Region, SmoltAge, and Rear group
  
  temp<-counts[i,] #pull out the group to use for subsetting
  
  data<-merge(temp, sarsub) #use that group to subset the data
  
  smptemp<-replicate(10000, sample(x=data$SAR, replace=TRUE))
  
  smptemp_df<-as.data.frame(smptemp) #output of replicate needs to be converted to dataframe
  
  smptemp_df2<-merge(temp, smptemp_df) #merge to add the Region, Smoltage, and Rear information
  
  smptemp_long<-gather(smptemp_df2,  key="rep", value="SAR", -Region, -SmoltAge, -Rear, -count) #Output from replicate has a column for each replicate. Gather to long format.

  smp<-bind_rows(smp, smptemp_long) #bind to the main results dataframe

}




# Median of all samples ------------------------------------

smpsum<-smp %>%
  group_by(Region,
           SmoltAge,
           Rear,
           rep) %>%
  summarise(mediansar=median(SAR)) %>%
  ungroup()




# Normalize all regions by all regions ------------------------------------

#get list of regions
regs<-levels(sarsub$Region)

#define output dataframe
resampled<-data.frame()

for (i in 1:length(regs)) #for each region
{
  regnorm<-smpsum %>%
    filter(Region==regs[i]) #subset of normalizing region
  
  regother<-smpsum %>%
    filter(Region!=regs[i]) #subset of all the other regions
  
  temp<-merge(regother, regnorm, by=c("SmoltAge", "Rear","rep"), suffixes = c("_other", "_norm")) #merge these together so each replicate for each region is paired with the corresponding replicate from the normalizing region
  
  resampled<-rbind(resampled, temp) #bind back to results dataframe
  
}


#calculate the normalized SAR
resampled$norm<-resampled$mediansar_other/resampled$mediansar_norm




# Pretty up ---------------------------------------------------------------

#sort Regions north to south
resampled$Region_other<-factor(resampled$Region_other, levels=c("SEAK",
                                                                  "NCBC",
                                                                  "WCVI",
                                                                  "SOG",
                                                                  "PS",
                                                                  "WAC",
                                                                  "LCOL",
                                                                  "MCOL",
                                                                  "UCOL",
                                                                  "SNAK",
                                                                  "ORC",
                                                                  "CA"))

resampled$Region_norm<-factor(resampled$Region_norm, levels=levels(resampled$Region_other))





# Plots -------------------------------------------------------------------

#--------------colours

cols<-c(economist_pal()(9), "lightgoldenrod1", "darkgoldenrod1","white")
show_col(cols)
coluse<-cols[c(1,8,11,9)]

outsize=0.25


#--------------set theme defaults
theme_update(strip.background=element_rect(fill=NA),
             panel.spacing.x=unit(0.5, "lines"),
             panel.background=element_rect(fill=NA),
             strip.text.x=element_text(margin = margin(b=3)),
             legend.title = element_blank(),
             legend.key = element_blank(),
             panel.grid.major.x =element_line(colour = "grey85", size=0.25),
             panel.grid.major.y=element_line(colour = "grey85", size=0.25),
             panel.grid.minor=element_line(colour = NA))  



#--------------calc stats for pointrange plot
stat<-resampled %>%
  group_by(Region_other,
           Region_norm,
           SmoltAge,
           Rear) %>%
  summarise(mediannorm=median(norm),
            low90=quantile(norm, 0.05),
            high90=quantile(norm, 0.95),
            low95=quantile(norm, 0.025),
            high95=quantile(norm, 0.975))




#--------------all regions pointrange plot
tiff(file="Pointrange.tif",width=180, height=190,  units="mm", res=800, bg = "transparent", compression="lzw")

#plotting the hatchery populations only because there are only a few wild populations
ggplot(stat[stat$Rear=="H",], aes(x=Region_other, y=mediannorm, ymin=low90, ymax=high90, colour=SmoltAge, pch=SmoltAge)) +
  
  #to add the w stocks without causing a shift in the data points
  geom_pointrange(data=stat[stat$Rear=="W",], fatten=2, position=position_dodge(width=0.9)) +
  
  #add a horizontal line at 1% to show where the regions are equal
  geom_hline(aes(yintercept=1), lty=2) +
  
  geom_pointrange(fatten=2, position=position_dodge(width=0.9)) +
  
  facet_wrap(~Region_norm, nrow=4) +
  
  theme(plot.margin=unit(c(0.25,0.25,0.25,0.25), "cm"),
        legend.position = c(0.15, 0.975),
        legend.margin= margin(l=3, t=-5),
        axis.title.x = element_blank(), 
        axis.text.x=element_text(angle=90, hjust=1, vjust=0.5)) +
  
  #add in the x and y axes
  annotate("segment", x=-Inf, xend=Inf, y=0.01, yend=0.01)+
  annotate("segment", x=-Inf, xend=-Inf, y=0.01, yend=50) +
  
  #annotations to add * for the Wild stocks
  geom_text(aes(y=high90, x=Region_other), label="*", data=stat[stat$Rear=="W",], colour="black", size=5) +
  
  #display on log scale
  scale_y_continuous(expand=c(0,0), trans="log2", breaks=c(0.01,0.04,0.16, 0.64, 2.56 ,10.24, 40.96), labels=c(0.01,0.04,0.16, 0.64, 2.56 ,10.24, 40.96)) +
  
  scale_colour_manual(values=coluse, guide=guide_legend(nrow=1)) +
  scale_shape_manual(values=c(0,19), guide=guide_legend(nrow=1)) +
  
  ylab("Normalized CWT SAR")  

dev.off()

