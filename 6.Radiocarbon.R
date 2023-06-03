#Script for performing type II linear regression for 14C keeling plot and jackknifing data to determine Intercept error
#Author: Allison Welch
#Date: 6/1/2023

rm(list = ls())

#packages 
{
  library(lmodel2)
  library(ggplot2)
  library(tidyverse)
  library(readxl)
  library(readr)
}

####################
##Read in own data##
####################
keel <- read_csv("Data/keelingplottemp.csv")
keel <- keel %>% rename(Inverse_TC = `1/TC`) #skip this - go to line 24

###########################################################################
#If you don't want to go through and replace all variable names do it here#
###########################################################################
#keel <- keel %>% rename(Inverse_TC = your 1/TC column,
#                       (Del14C.corr = your Del14C column)

#prelim plot
ggplot(data = keel, aes(x = Inverse_TC, y = Del14C.corr)) +
  geom_point() +
  labs(title = "Keeling Plot --",
       x = bquote(1/TC~(1/µg~m^-3)),
       y = bquote(paste("∆"^14, "C (‰)"))
    ) +
  theme_bw()
# ggsave("Keeling Plot.png", height = 6, width = 6, dpi = 700)


#Subset data to what I need for jackknife: 
jack_data <- keel %>% select(ID, Inverse_TC, Del14C.corr) #I have an ID column that's just 1:nrow to keep track of things; can delete
#if no ID column in your data


#For each row in data frame, I want to take out that row and save copy of data frame
{
  #It seems to work better if I convert the data frame to a matrix first
  jack_data <- as.matrix(jack_data)
  
  #Define function variables
  name = "jack_data_" 
  val = c(1:nrow(keel)) #number of new datasets
  
  #create empty list to add data frames to
  list.dfs <- list()
  
  #Iterate through matrix, creating n-1 obs data frames
  #reference: SabDeM on stack exchange https://stackoverflow.com/questions/30835815/save-imported-csv-data-in-vector-r/30835924#30835924
  for(i in 1:nrow(jack_data)){ 
    list.dfs[[i]] <- data.frame(assign(paste(name, val, sep = "")[i], jack_data[-i, ])) 
  }
  rm(name, val)
}

#Now I can similarly iterate through data frames, performing type II regression on each
{
  #create empty list for model outputs
  mod_summary <- list()
  #also want another list just for coefficients to make plotting easier
  mod_coefs <- list()
  
  #Wanted statistics: coefs, confidence intervals, r2, p
  
  for (i in 1:length(list.dfs)){
    mod <- lmodel2(Del14C.corr ~ Inverse_TC, data = list.dfs[[i]])
    mod_summary[[i]] <- list(mod$regression.results[3, 1:4],
               mod$confidence.intervals[3, 2:5],
               rsquare = mod$rsquare,
               p = mod$P.param)
    mod_coefs[[i]] <- mod$regression.results[3, 2:3] #save as data.frame
  }
  rm(mod)
}

#Average slope
lm_avg <- lmodel2(Del14C.corr ~ Inverse_TC, data = keel)
lm_avg_slope <- round(lm_avg$regression.results$Slope[[3]], digits = 1) #SMA result
lm_avg_int <- round(lm_avg$regression.results$Intercept[[3]], digits = 1) #SMA result

#Create coefficients data frame
mod_coefs_df <- bind_rows(mod_coefs, .id = "column_label")
#calculate intercept uncertainty
int_un <- round(sd(mod_coefs_df$Intercept), digits = 1)


#Assign color palette
library(rcartocolor)
palette <- carto_pal(n = nrow(keel), name = "Prism")

#add to color grouping to data frame
keel$color_group <- palette

#Now I'm gonna print a geom_line list because I don't want to have to type all of that
for (i in 1:nrow(keel)){
  print(paste("geom_abline(slope = mod_coefs[[", i, "]]$Slope, intercept = mod_coefs[[", i, "]]$Intercept, color = palette[[", i, "]]) +"))
}

#copy print output
#search and delete [1]

#plot!!
my_annotation <- paste("f(x) = ", lm_avg_slope, "x + ", lm_avg_int, "± ", int_un)

ggplot()+
  geom_point(data = keel, aes(x = Inverse_TC, y =  Del14C.corr, color = color_group), size = 5) +
  geom_abline(slope = mod_coefs[[ 1 ]]$Slope, intercept = mod_coefs[[ 1 ]]$Intercept, color = palette[[ 1 ]]) +
  geom_abline(slope = mod_coefs[[ 2 ]]$Slope, intercept = mod_coefs[[ 2 ]]$Intercept, color = palette[[ 2 ]]) +
  geom_abline(slope = mod_coefs[[ 3 ]]$Slope, intercept = mod_coefs[[ 3 ]]$Intercept, color = palette[[ 3 ]]) +
  geom_abline(slope = mod_coefs[[ 4 ]]$Slope, intercept = mod_coefs[[ 4 ]]$Intercept, color = palette[[ 4 ]]) +
  geom_abline(slope = mod_coefs[[ 5 ]]$Slope, intercept = mod_coefs[[ 5 ]]$Intercept, color = palette[[ 5 ]]) +
  geom_abline(slope = mod_coefs[[ 6 ]]$Slope, intercept = mod_coefs[[ 6 ]]$Intercept, color = palette[[ 6 ]]) +
  geom_abline(slope = mod_coefs[[ 7 ]]$Slope, intercept = mod_coefs[[ 7 ]]$Intercept, color = palette[[ 7 ]]) +
  geom_abline(slope = mod_coefs[[ 8 ]]$Slope, intercept = mod_coefs[[ 8 ]]$Intercept, color = palette[[ 8 ]]) +
  geom_abline(slope = mod_coefs[[ 9 ]]$Slope, intercept = mod_coefs[[ 9 ]]$Intercept, color = palette[[ 9 ]]) +
  geom_abline(slope = mod_coefs[[ 10 ]]$Slope, intercept = mod_coefs[[ 10 ]]$Intercept, color = palette[[ 10 ]]) +
  annotate("text", x = max(keel$Inverse_TC)*0.75, y = max(keel$Del14C.corr)*0.75, label = my_annotation, size = 5) + #add linear regression to plot
  labs(title = "Keeling Plot --",
       x = bquote(1/TC~(1/µg~m^-3)),
       y = bquote(paste("∆"^14, "C (‰)"))
  ) +
  scale_color_manual(values = palette) +
  theme_bw(base_size = 15) +
  theme(legend.position = "none",
        panel.grid = element_line(color = "gray95"))

# ggsave("__Keeling Plot.png", height = 6, width = 6, dpi = 500)





#Code graveyard
# 
#figuring out lmodel2 loop
# colnames(mod_summary) <- colnames(mod_y)
# 
# 
# mod <- lmodel2(`Del14C.corr` ~ `X1.TC`, data = list.dfs[[2]])
# mod_results <- mod$regression.results[3,1:4]
# mod_CI <- mod$confidence.intervals[3,2:5]
# mod_stat <- data.frame(rsquare = mod$rsquare, p = mod$P.param)
# mod_y <- data.frame(mod_results, mod_CI, mod_stat)
# mod_x <- rbind(mod_x, mod_y)
# 
# mod_summary[[2]] = list(mod$regression.results[3, 1:4],
#                         mod$confidence.intervals[3, 2:5],
#                         rsquare = mod$rsquare,
#                         p = mod$P.param)

#   }
# }