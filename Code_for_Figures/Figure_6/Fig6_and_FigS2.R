# load required libraries
library(tidyverse)
library(gmodels)
library(ggplot2)
library(readr)
library(plotly)
library(MASS)
library(ggdark)
library(dplyr)
library(cowplot)
library(gridExtra)

#source("g_by_back_plotter.R")


#Set up some cool functions
variable_names <- list(
  "WT" = expression(bolditalic("Zic2")^bolditalic("+/+")),
  "HET" = expression(bolditalic("Zic2")^bolditalic("Ku/+"))
)

variable_labeller <- function(variable,value){return(variable_names[value])}

back_names <- list(
  "C3H" = "C3H/HeH",
  "C57BL6" = "C57BL6/N"
)

back_labeller <- function(variable,value){return(back_names[value])}

pheno_names <- list(
  "Anophthalmia" = expression(bold("Anophthalmia")), 
  "Curly_Tail" = expression(bold("Curly Tail")), 
  "Forked_Kinked_Hooked_Tail" = expression(bold("Other Tail Abnormalities")),
  "Fused_Ventrally" = expression(bold("Forebrain Fused Ventrally")),
  "Reduced_Forebrain" = expression(bold("Reduced Forebrain")),
  "Reduced_Lat_Vents" = expression(bold("Reduced Lateral Ventricles")),
  "Reduced_Fissure" = expression(bold("Reduced Interhemispheric Fissure")),
  "Smooth_Pointed_Philthrum" = expression(bold("Smooth Pointed Philthrum")),
  "Spina_Bifida" = expression(bold("Spina Bifida"))

)

pheno_labeller <- function(variable,value){return(pheno_names[value])}

geno_pheno <- expand.grid(variable_names, pheno_names)

geno_pheno_labeller <- function(variable,value1,value2){return(geno_pheno[value1, value2])}


geno_back_pheno <- expand.grid(back_names,variable_names, pheno_names)



geno_back_pheno_labeller <- function(variable,value1,value2,value3){return(geno_back_pheno[value1, value2])}


geno_back_pheno_names <- list(
  "C3H.WT.Anophthalmia" = expression(bolditalic("Zic2")^bolditalic("+/+")~bold("C3H/HeH Anophthalmia")),
  "C57BL6.WT.Anophthalmia" = expression(bolditalic("Zic2")^bolditalic("+/+")~bold("C57BL6/N Anophthalmia")),
  "C3H.HET.Anophthalmia" = expression(bolditalic("Zic2")^bolditalic("Ku/+")~bold("C3H/HeH Anophthalmia")),
  "C57BL6.HET.Anophthalmia" = expression(bolditalic("Zic2")^bolditalic("Ku/+")~bold("C57BL6/N Anophthalmia")),
  "C3H.WT.Curly_Tail" = expression(bolditalic("Zic2")^bolditalic("+/+")~bold("C3H/HeH Curly Tail")),
  "C57BL6.WT.Curly_Tail" = expression(bolditalic("Zic2")^bolditalic("+/+")~bold("C57BL6/N Curly Tail")),
  "C3H.HET.Curly_Tail" = expression(bolditalic("Zic2")^bolditalic("Ku/+")~bold("C3H/HeH Curly Tail")),
  "C57BL6.HET.Curly_Tail" = expression(bolditalic("Zic2")^bolditalic("Ku/+")~bold("C57BL6/N Curly Tail")),
  "C3H.WT.Fused_Ventrally" = expression(bolditalic("Zic2")^bolditalic("+/+")~bold("C3H/HeH Fused Ventrally")),
  "C57BL6.WT.Fused_Ventrally" = expression(bolditalic("Zic2")^bolditalic("+/+")~bold("C57BL6/N Fused Ventrally")),
  "C3H.HET.Fused_Ventrally" = expression(bolditalic("Zic2")^bolditalic("Ku/+")~bold("C3H/HeH Fused Ventrally")),
  "C57BL6.HET.Fused_Ventrally" = expression(bolditalic("Zic2")^bolditalic("Ku/+")~bold("C57BL6/N Fused Ventrally")),
  "C3H.WT.Reduced_Fissure" = expression(bolditalic("Zic2")^bolditalic("+/+")~bold("C3H/HeH Reduced Fissure")),
  "C57BL6.WT.Reduced_Fissure" = expression(bolditalic("Zic2")^bolditalic("+/+")~bold("C57BL6/N Reduced Fissure")),
  "C3H.HET.Reduced_Fissure" = expression(bolditalic("Zic2")^bolditalic("Ku/+")~bold("C3H/HeH Reduced Fissure")),
  "C57BL6.HET.Reduced_Fissure" = expression(bolditalic("Zic2")^bolditalic("Ku/+")~bold("C57BL6/ Reduced Fissure")),
  "C3H.WT.Smooth_Pointed_Philthrum" = expression(bolditalic("Zic2")^bolditalic("+/+")~bold("C3H/HeH Smooth Pointed Philthrum")),
  "C57BL6.WT.Smooth_Pointed_Philthrum" = expression(bolditalic("Zic2")^bolditalic("+/+")~bold("C57BL6/N Smooth Pointed Philthrum")),
  "C3H.HET.Smooth_Pointed_Philthrum" = expression(bolditalic("Zic2")^bolditalic("Ku/+")~bold("C3H/HEH Smooth_Pointed Philthrum")),
  "C57BL6.HET.Smooth_Pointed_Philthrum" = expression(bolditalic("Zic2")^bolditalic("Ku/+")~bold("C57BL6/N Smooth_Pointed Philthrum")),
  "C3H.WT.Forked_Kinked_Hooked_Tail" = expression(bolditalic("Zic2")^bolditalic("+/+")~bold("C3H/HeH Other Tail Abnormalities")),
  "C57BL6.WT.Forked_Kinked_Hooked_Tail" = expression(bolditalic("Zic2")^bolditalic("+/+")~bold("C57BL6/N Other Tail Abnormalities")),
  "C3H.HET.Forked_Kinked_Hooked_Tail" = expression(bolditalic("Zic2")^bolditalic("Ku/+")~bold("C3H/HeH Other Tail Abnormalities")),
  "C57BL6.HET.Forked_Kinked_Hooked_Tail" = expression(bolditalic("Zic2")^bolditalic("Ku/+")~bold("C57BL6/N Other Tail Abnormalities")),
  "C3H.WT.Spina_Bifida" = expression(bolditalic("Zic2")^bolditalic("+/+")~bold("C3H/HeH Spina Bifida")),
  "C57BL6.WT.Spina_Bifida" = expression(bolditalic("Zic2")^bolditalic("+/+")~bold("C57BL6/N Spina Bifida")),
  "C3H.HET.Spina_Bifida" = expression(bolditalic("Zic2")^bolditalic("Ku/+")~bold("C3H/HeH Spina Bifida")),
  "C57BL6.HET.Spina_Bifida" = expression(bolditalic("Zic2")^bolditalic("Ku/+")~bold("C57BL6/N Spina Bifida"))
)



#Just d

#This is the only way to call to make it work!
inter_names <- list(
  "WT.C3H" = expression(bolditalic("Zic2")^bolditalic("+/+")~bold("C3H/HeH")),
  "HET.C3H" = expression(bolditalic("Zic2")^bolditalic("Ku/+")~bold("C3H/HeH")),
  "WT.C57BL6" = expression(bolditalic("Zic2")^bolditalic("+/+")~bold("C57BL6/N")),
  "HET.C57BL6" = expression(bolditalic("Zic2")^bolditalic("Ku/+")~bold("C57BL6/N"))
)

inter_names2 <- list(
  "C3H.WT" = expression(bolditalic("Zic2")^bolditalic("+/+")~bold("C3H/HeH")),
  "C3H.HET" = expression(bolditalic("Zic2")^bolditalic("Ku/+")~bold("C3H/HeH")),
  "C57BL6.WT" = expression(bolditalic("Zic2")^bolditalic("+/+")~bold("C57BL6/N")),
  "C57BL6.HET" = expression(bolditalic("Zic2")^bolditalic("Ku/+")~bold("C57BL6/N"))
)


# read csv files
overt_phenotypes <- read_csv("211110_overt_phenotypes.csv")

overt_phenotypes <- arrange(transform(overt_phenotypes, Genotype=factor(Genotype,levels=c("WT","HET")))) 

overt_phenotypes <- arrange(transform(overt_phenotypes, Background=factor(Background,levels=c("C3H","C57BL6"))))

#Factorise my binary values

cols <- colnames(overt_phenotypes[c(5,6,7,10,11,12,13,14,15)])

overt_phenotypes[cols] <- lapply(overt_phenotypes[cols], factor)



#G_by_back_logistic_regression_function

log_ress <- function(y, dataset){
  model <- glm(y~Genotype*Background, data = dataset, family = binomial(link='logit'))
  print(anova(model, test="Chisq"))
}

lapply(overt_phenotypes[c(5,6,7,10,11,12,13)], function(y)
  log_ress(y, overt_phenotypes))




#Figure 6
percent.data <- data.frame()

for (ph in c(5,6,7,10,11,12,13,14,15)){
  percents <- overt_phenotypes[c(2,3,ph)] %>% 
    group_by(Genotype,Background) %>%
    count(overt_phenotypes[colnames(overt_phenotypes[ph])]) %>%
    na.omit() %>%
    mutate(Percent=n/sum(n, na.rm = T)*100) %>%
    dplyr::select(-4)
  print(percents)
  if (ph == 5){percent.data <- percents}
  else{percent.data <- merge(percent.data, percents, all.x = T, all.y = T)}
}


percent.data <- percent.data %>% gather(key = Phenotype, value = value, na.rm = T, 
                                        c("Anophthalmia","Fused_Ventrally", "Reduced_Fissure", 
                                          "Smooth_Pointed_Philthrum", "Curly_Tail", "Smooth_Pointed_Philthrum",
                                          "Forked_Kinked_Hooked_Tail", "Spina_Bifida", "Reduced_Forebrain",
                                          "Reduced_Lat_Vents"))


pdf("Figure_6.pdf", onefile = T, paper="a4", width=10, height=13)

ggplot(percent.data, aes(x=interaction(Genotype,Background),y=Percent, fill=interaction(Background,value)))+
  geom_col()+
  scale_x_discrete(name="", limits=rev, labels=unlist(inter_names))+
  coord_flip()+
  facet_wrap(~Phenotype, ncol=1, labeller = pheno_labeller)+
  scale_fill_manual(name="", limits=c("C3H.N","C57BL6.N","C3H.Y", "C57BL6.Y"), 
                    labels=c("C3H/HeH Background: \nPhenotype Absent",
                             "C57BL6/N Background: \nPhenotype Absent", 
                             "C3H/HeH Background: \nPhenotype Present",
                             "C57BL6/N Background: \nPhenotype Present"),
                    values = c("#d16163",  "#d16196", "#61aed1", "#618dd1"), guide=guide_legend(ncol=2) )+
  ylab(expression(bold("Percentage(%)")))+
  dark_theme_linedraw()+
  theme(legend.position = "bottom", legend.text = element_text(size=10), legend.direction = "vertical",
        strip.background = element_rect(fill="black", colour = "black", size=0.1),
        strip.text.x = element_text(color = "white", size=10), legend.key.size = unit(0.8, 'cm'))


dev.off()

# Figure S2

#Calculate percentages 
pheno_percent <- na.omit(overt_phenotypes[c(1,2,3,5,6,7,10,11,12,13)]) %>%
  group_by(Genotype,Background) %>%
  count(Anophthalmia, Curly_Tail, Reduced_Fissure, Smooth_Pointed_Philthrum, Fused_Ventrally, Forked_Kinked_Hooked_Tail, Spina_Bifida, na.rm=T) %>%
  mutate(sum=sum(n), percent=(n)/sum*100)


#Figure_S2A
p1<- ggplot(pheno_percent, aes(x=interaction(Background,Genotype), y=percent, 
                          fill=interaction(Reduced_Fissure, Curly_Tail, Fused_Ventrally,
                                           Anophthalmia, Smooth_Pointed_Philthrum, 
                                           Forked_Kinked_Hooked_Tail, Spina_Bifida)))+
  geom_bar(stat='identity')+
  coord_flip()+
  labs(tag = "A")+
  scale_x_discrete("", labels=unlist(inter_names2), limits=c("C57BL6.HET", "C3H.HET","C57BL6.WT", "C3H.WT"))+
  scale_fill_discrete(name="Phenotype", labels=c("No Abnormalities", 
                                                "Reduced Fissure Only",
                                                 "Curly Tail Only",
                                                 "Reduced Fissure and Curly Tail",
                                                 "Anophthalmia Only",
                                                 "Reduced Interhemispheric Fissure and Anopthalmia",
                                                 "Reduced Interhemispheric Fissure, Curly Tail and Anopthalmia",
                                                 "Fused Ventrally and Anopthalmia",
                                                 "Reduced Interhemispheric Fissure, Fused Ventrally and Anopthalmia",
                                                 "Reduced Interhemispheric Fissure, Fused Ventrally and Smooth Pointed Philthrum",
                                                 "Other Tail Defects",
                                                 "Reduced Interhemispheric Fissure and Curly Tail",
                                                 "Curly Tail and Other Tail Defects",
                                                 "Reduced Interhemispheric Fissure, Curly Tail and Other Tail Defects"))+
  dark_theme_linedraw()+
  theme(legend.position = "bottom", legend.direction = "vertical", legend.text = element_text(size=5))


#Figure_S2B
pheno_percent2 <- na.omit(overt_phenotypes[c(1,2,3,5,6,7,10,11,12,13)]) %>%
  group_by(Genotype,Background) %>%
  count(Anophthalmia, Curly_Tail, Smooth_Pointed_Philthrum, Fused_Ventrally, Spina_Bifida, na.rm=T) %>%
  mutate(sum=sum(n), percent=(n)/sum*100)

p2<- ggplot(pheno_percent2, aes(x=interaction(Background, Genotype), y=percent, 
                           fill=interaction(Curly_Tail, Fused_Ventrally, Anophthalmia, Smooth_Pointed_Philthrum, Spina_Bifida)))+
  geom_bar(stat='identity')+#, aes(fill =  heat.colors(15)))+ 
  scale_fill_discrete(name="Phenotype", labels=c("No Abnormalities", 
                                                 "Curly Tail Only", 
                                                 "Anophthalmia Only",
                                                 "Curly Tail and Anopthalmia",
                                                 "Forebrain Fused Ventrally and Anopthalmia", 
                                                 "Forebrain Fused Ventrally and Smooth Pointed Philthrum",
                                                 "Curly Tail and Spina Bifida")
                    )+
  geom_text(aes(x=interaction(Background,Genotype), y=c(98, 98, 98, 27,
                                                        12, 98, 43, 32,
                                                        8, 18, 23), label =n))+ 
  scale_x_discrete("", labels=unlist(inter_names2), limits=c("C57BL6.HET", "C3H.HET","C57BL6.WT", "C3H.WT"))+
  labs(tag = "B")+
  ylab("Percentage (%)")+
  coord_flip()+
  dark_theme_linedraw()+
  theme(legend.position = "bottom", legend.direction = "vertical")



  

pdf("Figure_S2.pdf", onefile = T, paper="a4", width=10, height=13)

p1 <- as_grob(p1)
p2 <- as_grob(p2)

grid.arrange(grobs=list(p1, p2), ncol=1, nrow=2)

dev.off()



