##############################
##  Title: 1_prep.R
##  Note: Prepare the data and functions
##        See data access note below
##  Author: Marshall A. Taylor
##############################

### BEGIN ###

#######################
## Necessary functions
#######################

# install.packages("pacman")
pacman::p_load(tidyverse,
               poLCA,
               ggplotify,
               ggpubr,
               reshape2,
               install = T)


#######################
## Load in the data
#######################

narc.data <- read.csv(file="data/NarcDys.csv", header = T) # Please contact Terence E. 
                          # McDonnell (terence.e.mcdonnell@nd.edu) for details on 
                          # how to access these data.


#######################
## Prepare DV variables (make them dichotomous)
#######################

narc.data$postpol2 <- NA
narc.data$postpol2[narc.data$postpol <= 2] <- 1
narc.data$postpol2[narc.data$postpol > 2] <- 2

narc.data$smsup2 <- NA
narc.data$smsup2[narc.data$smsup == 0] <- 1
narc.data$smsup2[narc.data$smsup >= 1] <- 2

narc.data$smnsup2 <- NA
narc.data$smnsup2[narc.data$smnsup == 0] <- 1
narc.data$smnsup2[narc.data$smnsup >= 1] <- 2

narc.data$frreply2 <- NA
narc.data$frreply2[narc.data$frreply <= 2] <- 1
narc.data$frreply2[narc.data$frreply >= 3] <- 2

narc.data$frshar2 <- NA
narc.data$frshar2[narc.data$frshar <= 2] <- 1
narc.data$frshar2[narc.data$frshar >= 3] <- 2

narc.data$smissue2 <- NA
narc.data$smissue2[narc.data$smissue == 0] <- 1
narc.data$smissue2[narc.data$smissue >= 1] <- 2

narc.data$contact2 <- NA
narc.data$contact2[narc.data$contact <= 2] <- 1
narc.data$contact2[narc.data$contact == 3] <- 2

narc.data$demo2 <- NA
narc.data$demo2[narc.data$demo <= 2] <- 1
narc.data$demo2[narc.data$demo == 3] <- 2

narc.data$fund2 <- NA
narc.data$fund2[narc.data$fund <= 2] <- 1
narc.data$fund2[narc.data$fund == 3] <- 2

narc.data$media2 <- NA
narc.data$media2[narc.data$media <= 2] <- 1
narc.data$media2[narc.data$media == 3] <- 2

narc.data$petition2 <- NA
narc.data$petition2[narc.data$petition <= 2] <- 1
narc.data$petition2[narc.data$petition == 3] <- 2

narc.data$rallyever2 <- NA
narc.data$rallyever2[narc.data$rallyever <= 2] <- 1
narc.data$rallyever2[narc.data$rallyever == 3] <- 2


#######################
## Prepare predictors
#######################

#Recode gender
narc.data$gender2 <- ifelse(narc.data$gender==0, 0,
                            ifelse(narc.data$gender==1, 1,
                                   ifelse(narc.data$gender==2, 0,
                                          ifelse(narc.data$gender==3, 1, NA
                                          ))))

#Lose -99, IDK, and other categories on partaff
narc.data$partaff[which(narc.data$partaff=="-99" | narc.data$partaff==8)] <- NA

#Reverse code informed and bin it (sparse larger caetegory entries)
narc.data$informed.bin <- NA
narc.data$informed.bin[which(narc.data$informed < 4)] <- 1
narc.data$informed.bin[which(narc.data$informed >= 4)] <- 0

#Reverse code informed
narc.data$uninformed.temp <- NA
narc.data$uninformed.temp[which(narc.data$uninformed == 1)] <- 7
narc.data$uninformed.temp[which(narc.data$uninformed == 2)] <- 6
narc.data$uninformed.temp[which(narc.data$uninformed == 3)] <- 5
narc.data$uninformed.temp[which(narc.data$uninformed == 4)] <- 4
narc.data$uninformed.temp[which(narc.data$uninformed == 5)] <- 3
narc.data$uninformed.temp[which(narc.data$uninformed == 6)] <- 2
narc.data$uninformed.temp[which(narc.data$uninformed == 7)] <- 1
narc.data$uninformed <- narc.data$uninformed.temp
narc.data$uninformed.temp <- NULL

#Hashtags
narc.data$hash <- NA
narc.data$hash <- narc.data$htmaga+narc.data$htres+narc.data$httrain+narc.data$htimp+
  narc.data$htwom+narc.data$htnmp+narc.data$htblack+narc.data$htblue+narc.data$htall+
  narc.data$htnod+narc.data$htknee+narc.data$htnfl+narc.data$htmetoo+narc.data$httcot+
  narc.data$htocra+narc.data$htoth

#Justices
narc.data$justice <- NA
narc.data$justice <- narc.data$scjust1+narc.data$scjust2+narc.data$scjust3+
  narc.data$scjust4+narc.data$scjust5+narc.data$scjust6+narc.data$scjust7+
  narc.data$scjust8+narc.data$scjust9

#Recode the race variables
narc.data$nonwhite <- 0
narc.data$nonwhite[which((narc.data$black==1 | narc.data$asian==1 |
                            narc.data$indian==1 | narc.data$islander==1 |
                            narc.data$hisp==1) &
                           narc.data$white==0)] <- 1
narc.data$nonwhite[is.na(narc.data$white+narc.data$black+narc.data$asian+
                           narc.data$indian+narc.data$islander+
                           narc.data$hisp)] <- NA

#Medium
narc.data$medium <- (narc.data$medium2+narc.data$medium4+narc.data$medium5+narc.data$medium6)/(narc.data$medium1+narc.data$medium2+
                                               narc.data$medium3+narc.data$medium4+
                                               narc.data$medium5+narc.data$medium6+
                                               narc.data$medium7+narc.data$medium8+
                                               narc.data$medium9+narc.data$medium10+
                                               narc.data$medium11+narc.data$medium12+
                                               narc.data$medium13)

#Create separate partisanship and ideological extremity variables
narc.data$partisan <- cut(narc.data$conservative, 3)
narc.data$partisan <- factor(narc.data$partisan, levels=c("(0.994,3]","(3,5]","(5,7.01]"), 
                             labels=c("liberal","moderate","conservative"))

narc.data$ideo <- scales::rescale(narc.data$conservative, to=c(-1,1))
narc.data$ideo <- abs(narc.data$ideo)

### END ###
