################################################################################
## COMPARISONS OF DHS AND PHIA DATA FROM MALAWI FOR WOMEN
################################################################################

# The DHS survey included data collection in the 0-4y group, in the 15-49y group
# among women. # In the PHIA survey, data collection covered the 0-64y groups.


if (!require("pacman")) {install.packages("pacman")}
pacman::p_load(tidyverse,haven,plyr,survey,srvyr,fastDummies,naniar,
               lubridate, usethis, lemon, ggpattern, ggnewscale,scales)

###########################
# PREPARATION OF DHS DATA
###########################

malawi.dhs<-read_dta("MWPR7AFL.DTA") |>
            select(starts_with("hv00"),
                   starts_with("hv02"),
                   starts_with("hv10")) |>
            mutate(wt = hv005/1000000) |>
            filter(hv103==1) |> # keep only de facto population
            replace_with_na(replace = list(hv105=98)) |>
            mutate(sex = case_when(hv104==2~"Female",
                                   hv104==1~"Male"),
                   sex = as.factor(sex),
                   agecat = cut(hv105,
                                c(-Inf,seq(4,79,by=5),Inf), 
                                labels = seq(0,80,by = 5)))
            
###########################    
# PREPARATION OF PHIA DATA
###########################

# For PHIA, we first need to combine the separate datasets

malawi.phia.hh<-read_dta("Mphia2015hh.dta") |>
                select(country,householdid,urban,rostercount,hhwt0,varstrat, 
                       varunit)

malawi.phia.ch<-read_dta("Mphia2015childind.dta") |>
                select(householdid,personid,indstatus,gender,age,livehere,
                       sleephere) 

malawi.phia.ad<-read_dta("Mphia2015adultind.dta") |>
                select(householdid,personid,indstatus,gender,age,livehere,
                       sleephere)


malawi.phia<-bind_rows(malawi.phia.ch,malawi.phia.ad) |>
             join(malawi.phia.hh,
                  by = "householdid") |>
             filter(sleephere==1) |>
             mutate(agecat = cut(age,
                                 c(-Inf,seq(4,79,by=5),Inf),
                                 labels = seq(0,80,by = 5)),
                    sex = case_when(gender==1~"Male",
                                    gender==2~"Female"),
                    sex = as.factor(sex))
  

################################################################################
## ANALYSIS
################################################################################

# We calculate the age distribution in each dataset, using the survey parameters 

women.dhs<-subset(malawi.dhs,sex=="Female") |> 
           as_survey_design(ids=hv021,
                            strata=hv023,
                            weights=wt) |>
           group_by(agecat) |>
           dplyr::summarize(proportion = survey_mean(),
                            total = survey_total()) |>
           rename(dhs = proportion,
                  dhs_se = proportion_se) 

women.phia<-subset(malawi.phia,sex=="Female") |> 
            as_survey_design(ids=varunit,
                             strata=varstrat,
                             weights=hhwt0,
                             nest=TRUE) |>
            group_by(agecat) |>
            dplyr::summarize(proportion = survey_mean()) |>
            rename(phia = proportion,
                   phia_se = proportion_se)
              
# We then calculate the ratios of these age-specific proportions and we 
# estimate the standard errors of these ratios, using formula (20) in
# Howard Seltman's notes

age.ratios<-join(women.phia,women.dhs, 
                 by = c("agecat")) |>
            mutate(ratio = phia/dhs,
                   var=(phia^2/dhs^2)*((phia_se^2/phia^2)+(dhs_se^2/dhs^2)),
                   se = sqrt(var),
                   upper = ratio+1.96*se,
                   lower = ratio-1.96*se,
                   agestart = (as.numeric(agecat)-1)*5+2.5)

###########################
# Data visualization
###########################

figure1<-age.ratios |>
         filter(agecat!="80") |>
         ggplot(aes(y = agestart, x=ratio))+
         geom_rect(data = data.frame(ymin = c(0,5,15,50,65), 
                                     ymax = c(5,15,50,65,80),
                                     elig = c("Eligible in DHS & PHIA",
                                              "Eligible only in PHIA",
                                              "Eligible in DHS & PHIA",
                                              "Eligible only in PHIA",
                                              "Not eligible")),
                   aes(ymin = ymin, ymax = ymax, 
                       fill = elig,
                       xmin = 0.5, xmax = 2),
                   alpha = 0.5,
                   inherit.aes = FALSE)+
                   scale_fill_brewer(name = "Eligibility for additional\nData collection:",
                                     palette = "Paired",
                                     guide = guide_legend(reverse = TRUE))+
          geom_point(shape = 1, size = 2)+
          geom_errorbar(aes(xmin=lower, xmax = upper),
                        width=0)+
          geom_vline(xintercept = 1, 
                     color="black", 
                     linetype="dashed")+
          xlab("Relative age composition")+ 
          ylab("Age\ngroup")+
          theme_light()+
          theme(axis.title.y = element_text(angle = 0, vjust = 0.5),
                legend.background = element_rect(color = "black"))+
          scale_x_continuous(limits=c(0.5,2), 
                             breaks=c(0.5,0.8,1,1.25,2),
                             transform = "log",
                             position = "top")+
          scale_y_continuous(breaks = seq(0,80, by = 5),
                             labels = seq(0,80, by = 5),
                             minor_breaks = NULL)


figure1 + geom_segment(x = -0.1, xend = -0.65, y = -3.2, yend = -3.2,
                       linewidth = 0.1,
                       arrow = arrow(length = unit(5, "pt")))+
          geom_segment(x = 0.1, xend = 0.65, y = -3.2, yend = -3.2,
                       linewidth = 0.1,
                       arrow = arrow(length = unit(5, "pt")))+
          annotate("text", x = 0.71, y = -1.2, label="% in DHS > % in PHIA",
                   size = 3, fontface = "italic")+
          annotate("text", x = 1.4, y = -1.2, label="% in DHS < % in PHIA",
                   size = 3, fontface = "italic")


