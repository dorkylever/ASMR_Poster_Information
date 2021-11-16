# C3H Zic2 Kumba data analyis by Kyle Drover - ANU

#Required libraries
library(dplyr)
library(readr)
library(tidyverse)
library(gmodels)
library(lme4)
library(lmerTest)
library(emmeans)
library(MASS)
library(effects)
library(ggpmisc)
library(car)
library(WRS2)
library(FSA)
library(ggplot2)
library(ggforce)
library(cowplot)

#Setting up some cool functions
variable_names <- list(
  "WT" = expression(bolditalic("Zic2")^bolditalic("+/+")),
  "HET" = expression(bolditalic("Zic2")^bolditalic("Ku/+"))
)

variable_labeller <- function(variable,value){return(variable_names[value])}

staging_names <- list(
  "1" = expression(bold("E14-E14.25")),
  "1.38888888888889" = expression(bold("E14-E14.75")),
  "2" = expression(bold("E14.25-E14.75")) )

staging_name_labeller <- function(variable,value){return(staging_names[value])}

scale_to_mm3 <- function(x) {
  #voxel size is 40um 
  um3_conv_factor <- 40.0^3  # To convert voxels to um3
  um3_to_mm3_conv_factor <- 1e9
  return((x * um3_conv_factor)/um3_to_mm3_conv_factor)
}

normalise <- function(x, na.rm = FALSE) (x/organ_info$WEV)

#Filter for columns of interest
full_vars <- c('Genotype','Treatment','Asymmetric_Brain','WEV','Embryo','X3', 'X17','x18', 'X23','X24','X40','X53','X41','X46','X44','X51','X55','X65')


#Reading and setting up dataset
organ_info <- read_csv("210727_org_info.csv")
organ_info <- arrange(transform(organ_info, genotype=factor(Genotype,levels=c("WT","HET"))) )
organ_info<- organ_info[, (colnames(organ_info) %in% full_vars)]

# staging

staging_vols <- read_csv("staging_info_volume_all_groups.csv")

staging_vars <- c('vol', 'stage_group')
staging_info <- staging_vols[, (colnames(staging_vols) %in% staging_vars)]


# rename stuff
names(staging_info)[names(staging_info) == "vol"] <- "Embryo"
names(staging_info)[names(staging_info) == "stage_group"] <- "Stage_Group"

# merge df

full_info <- merge(staging_info, organ_info, by = "Embryo", all.x = FALSE, all.y = TRUE)


full_info <- full_info %>% 
  mutate_at(vars(contains('X')), scale_to_mm3) %>%
  mutate_at(vars(contains('WEV')), scale_to_mm3) %>%
  mutate_at(vars(contains('X')), normalise) 


#Figure 3B
box_plots <- lapply(full_info[6:17], function(data) 
  ggplot(full_info,aes(x=Genotype,y=data,color=Genotype,label=Embryo))+
    geom_boxplot()+
    #geom_point()+
    #geom_smooth(method = "lm")+
    #facet_grid(~Genotype, labeller = variable_labeller, scales = "free_x")+
    #geom_text(aes(label=ifelse(data > quantile(data, 0.975),as.character(Embryo),'' )), hjust=0, vjust=0)+
    #geom_text(aes(label=ifelse(data < quantile(data, 0.025),as.character(Embryo),'' )), hjust=0, vjust=0)+
    scale_color_manual(name="Genotype", labels=unlist(variable_names), values=c("#61aed1","#d16163"))+
    ylab(bquote(atop("Lateral Ventricle Volume Normalised", "To Whole Embryo Volume (mm" ^ 3*")")))+
      #expression("Lateral Ventricle Volume Normalised \n To Whole Embryo Volume (WEV)"~"mm"^3) )+ 
    scale_x_discrete(name="", labels=unlist(variable_names), limits=c("WT","HET") )+
    scale_y_continuous(limits = c(0, 9e-07))+
    theme(axis.line.y = element_line(arrow = grid::arrow(length = unit(0.3, "cm"), 
                                                         ends = "last"), size = 0.1),
          axis.line.x = element_line(arrow = grid::arrow(length = unit(0.3, "cm"), 
                                                         ends = "last"), size = 0.1) 
          )+
    xlab(bquote("Whole Embryo Volume"*" (mm" ^ 3*")"))+
    dark_theme_linedraw())

box_plots[[2]]

#Figure 3C
list_plots <- lapply(full_info[6:17], function(data) 
  ggplot(full_info,aes(x=WEV,y=data,colour=Genotype,label=Embryo))+
    #geom_boxplot()+
    geom_point()+
    geom_smooth(method = "lm")+
    #facet_grid(~Genotype, labeller = variable_labeller, scales = "free_x")+
    #geom_text(aes(label=ifelse(data > quantile(data, 0.975),as.character(Embryo),'' )), hjust=0, vjust=0)+
    #geom_text(aes(label=ifelse(data < quantile(data, 0.025),as.character(Embryo),'' )), hjust=0, vjust=0)+
    scale_color_manual(name="Genotype", labels=unlist(variable_names), values=c("#61aed1","#d16163"))+
    ylab(bquote(atop("Lateral Ventricle Volume Normalised", "To WEV (mm" ^ 3*")")))+
    #xlab(expression("Whole Embryo Volume (WEV) (mm"^3))+ 
    #scale_x_continuous(limits=c(0, 220))+
    scale_y_continuous(limits = c(0, 9e-07))+
    theme(axis.line.y = element_line(arrow = grid::arrow(length = unit(0.3, "cm"), 
                                                         ends = "last"), size = 0.1),
          axis.line.x = element_line(arrow = grid::arrow(length = unit(0.3, "cm"), 
                                                         ends = "last"), size = 0.1) 
    )+
    xlab(bquote("Whole Embryo Volume"*" (mm" ^ 3*")"))+
    dark_theme_linedraw())



list_plots[[2]]


#Statistical Test for lateral ventricles
vent_lm <-lm(X17~Genotype+WEV, data=full_info)

summary(vent_lm)

plot(vent_lm)

# making the files: 
pdf("Fig_3.pdf", onefile = T, paper="a4r", width=13, height=10)

p1 <- as_grob(as_grob(box_plots[[2]]))

p2 <- as_grob(as_grob(line_plots[[2]]))


grid.arrange(grobs=list(p1, p2), ncol=2, nrow=1)

dev.off()

