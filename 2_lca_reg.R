##############################
##  Title: 2_lca_reg.R
##  Note: LCA regressions
#3        See data access note below
##  Author: Marshall A. Taylor
##############################

### BEGIN ###

#######################
## Load in data
#######################

#LCA results can vary somewhat from run to run, so in order to produce the tables
  #and figures exactly, load in the RData here and move straight to the tables and plotting

#Read in RData in necessary
# readRDS("data/lca_final.rds") -- # Please contact Marshall A. Taylor (mtaylor2@nmsu.edu)
                          #for details on how to access this RData file.


#######################
## LCA models
#######################

#Get the equations
f.null <- cbind(postpol2, smsup2, smnsup2, frreply2, frshar2, smissue2,
           contact2, demo2, fund2, media2, #petition2, 
           rallyever2) ~ 1

f <- cbind(postpol2, smsup2, smnsup2, frreply2, frshar2, smissue2,
           contact2, demo2, fund2, media2, #petition2, 
           rallyever2) ~ factor(toomuch)+factor(toofast)+govsay+
          factor(informed.bin)+congress+factor(gender2)+factor(nonwhite)+yrbrn+income+
          relevel(factor(urban), ref=2)+relevel(factor(partisan), ref="moderate")+
          scale(ideo)+justice+intpol+medium


#Estimate LCAs with different ks
for (i in 1:6) { #The higher ks will take a while
  if (i == 1) {
    set.seed(123)
    assign(paste0("model.",i), poLCA(f.null, narc.data, maxiter = 5000, 
                                     nrep = 10, na.rm = T, nclass = i, 
                                     verbose = F, graphs = F))
  }
    else {
      set.seed(123)
      assign(paste0("model.",i), poLCA(f, narc.data, maxiter = 5000, 
                                       nrep = 10, na.rm = T, nclass = i, 
                                       verbose = F, graphs = F))
  }
}

#Compare model fits
models <- grep("^model.*\\.[1-6]$", names(.GlobalEnv), value = T)
models <- do.call("list", mget(models))

  #BIC
for (i in models) {
  i[c("bic")] %>% print()
} #model.3 = best fit

  #Normalized Shannon entropy
for (i in models) {
  (poLCA.entropy(i)/
     log(prod(sapply(model.1$probs, ncol)))) %>% print()
} #Ranges from 0 to 1, with 1 = max dispersion


#Select a three-class solution
for (i in sample(1:100, 20, replace = F)) {
  set.seed(i)
  assign(paste0("temp.",i), poLCA(f, narc.data, na.rm = T, nclass = 3,
                                  maxiter = 5000, nrep = 10, 
                                  graphs = F, verbose = F))
} # 42, 47, and 73 put armchair citizens as the reference 
  # category without needing to make a call to LCA.reorder

#Compare model fits (very little difference, unsurprisingly)
temp.10[c("bic")] #best fit
temp.16[c("bic")]
temp.27[c("bic")]
temp.3[c("bic")]
temp.36[c("bic")]
temp.47[c("bic")]
temp.48[c("bic")]
temp.50[c("bic")]
temp.55[c("bic")]
temp.59[c("bic")]
temp.60[c("bic")]
temp.69[c("bic")]
temp.7[c("bic")]
temp.70[c("bic")]
temp.83[c("bic")]
temp.89[c("bic")]
temp.91[c("bic")]
temp.95[c("bic")]

#Get the model
lca.model <- temp.10


#Estimates class population shares
lca.model$P %>% round(., 4)

#Conditional item response probabilities
lca.model$probs

#Regression results
lca.model

#G^2
lca.model$Gsq

#Entropy R^2
machine_tolerance <- sqrt(.Machine$double.eps) #From: https://gist.github.com/daob/c2b6d83815ddd57cde3cebfdc2c267b3
entropy.R2 <- function(fit) {
  entropy <- function(p) {
    p <- p[p > machine_tolerance] # since Lim_{p->0} p log(p) = 0
    sum(-p * log(p))
  }
  error_prior <- entropy(fit$P) # Class proportions
  error_post <- mean(apply(fit$posterior, 1, entropy))
  R2_entropy <- (error_prior - error_post) / error_prior
  R2_entropy
}

entropy.R2(lca.model)


#######################
## Plots
#######################

#Figure 1
  #Get the individual stacked bar plots
names(lca.model$probs) <- c("postpol", "smsup", "smnsup", "frreply", "frshar", "smissue",
                                     "contact", "demo", "fund", "media", "rallyever") 
for (i in 1:length(lca.model$probs)) {
  
  temp <- as.data.frame(lca.model$probs[i]) %>% t() %>% melt()
  
  temp$Var1 <- c(1,2,1,2,1,2)
    
    assign(paste0("p",i), ggplot(temp, aes(x = Var2, y = value, fill = factor(Var1))) +
             geom_bar(stat = "identity", color = "black", 
                      width = .5) +
             scale_fill_manual(breaks = c(1,2), values = c("gray50","gray75"),
                               name = "Level", labels = c("No/Little Engagement","High Engagement")) +
             guides(fill = F) +
             labs(x = "", y = "",
                  title = names(lca.model$probs[i])) +
             scale_x_discrete(labels = c("Armchair\nCitizens","Actives","Passives")) +
             theme_bw() +
             theme(axis.text.x = element_text(size = 12, face = "bold"),
                   #axis.title.y = element_text(size = 12, face = "bold"),
                   #legend.title = element_text(face = "bold"),
                   plot.title = element_text(face = "bold")))
  
}

  #Legend
temp <- ggplot(temp, aes(x = Var2, y = value, fill = factor(Var1))) +
  geom_bar(position = "fill", stat = "identity", color = "black") +
  scale_fill_manual(breaks = c(1,2), values = c("gray50","gray75"),
                    name = "Level", labels = c("No/Little Engagement","High Engagement")) +
  #guides(fill = F) +
  labs(x = "", y = "Conditional Item Response Probability",
       title = names(lca.model$probs[i])) +
  scale_x_discrete(labels = c("Armchair\nCitizens","Actives","Passives")) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"),
        legend.title = element_text(face = "bold", size = 12),
        legend.text = element_text(size = 12),
        plot.title = element_text(face = "bold"))

g_legend <- function(a.gplot){ 
  tmp <- ggplot_gtable(ggplot_build(a.gplot)) 
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box") 
  legend <- tmp$grobs[[leg]] 
  return(legend)} 

legend <- g_legend(temp) 
legend <- as.ggplot(legend)

rm(temp)

  #Arrange them
plot.1 <- ggarrange(p1,p2,p3,p4,p5,p6, align = "hv")
plot.1 <- annotate_figure(plot.1, top = text_grob("Social Media Use Variables", face = "bold", size = 16, hjust = 1.95))

plot.2 <- ggarrange(p7,p8,p9,p10,p11,legend, align = "hv")
plot.2 <- annotate_figure(plot.2, top = text_grob("Offline Political Behavior Variables", face = "bold", size = 16, hjust = 1.52))

final.plot <- ggarrange(plot.1, plot.2, align = "hv", nrow = 2)
final.plot <- annotate_figure(final.plot, left = text_grob("Conditional Item Response Probability", 
                                                           size = 12, face = "bold", rot = 90))

png("figures/fig1.png", units = "in", height = 10, width = 12, res = 800)
  final.plot
dev.off()

#Figure 2
val.toofast <- cbind(1, mean(lca.model$x$factor.toomuch.1), c(0,1), mean(lca.model$x$govsay), mean(lca.model$x$factor.informed.bin.1),
      mean(lca.model$x$congress), mean(lca.model$x$factor.gender2.1), mean(lca.model$x$factor.nonwhite.1),
      mean(lca.model$x$yrbrn), mean(lca.model$x$income), mean(lca.model$x$relevel.factor.urban...ref...2.0),
      mean(lca.model$x$relevel.factor.urban...ref...2.2), mean(lca.model$x$relevel.factor.partisan...ref....moderate..liberal),
      mean(lca.model$x$relevel.factor.partisan...ref....moderate..conservative), 0, mean(lca.model$x$justice),
      mean(lca.model$x$intpol), mean(lca.model$x$medium))

exb.val.toofast <- exp(val.toofast %*% lca.model$coeff)

predict.toofast <- cbind(1, exb.val.toofast)/(1 + rowSums(exb.val.toofast))


val.toomuch <- cbind(1, c(0,1), mean(lca.model$x$factor.toofast.1), mean(lca.model$x$govsay), mean(lca.model$x$factor.informed.bin.1),
     mean(lca.model$x$congress), mean(lca.model$x$factor.gender2.1), mean(lca.model$x$factor.nonwhite.1),
     mean(lca.model$x$yrbrn), mean(lca.model$x$income), mean(lca.model$x$relevel.factor.urban...ref...2.0),
     mean(lca.model$x$relevel.factor.urban...ref...2.2), mean(lca.model$x$relevel.factor.partisan...ref....moderate..liberal),
     mean(lca.model$x$relevel.factor.partisan...ref....moderate..conservative), 0, mean(lca.model$x$justice),
     mean(lca.model$x$intpol), mean(lca.model$x$medium))

exb.val.toomuch <- exp(val.toomuch %*% lca.model$coeff)

predict.toomuch <- cbind(1, exb.val.toomuch)/(1 + rowSums(exb.val.toomuch))

predict <- rbind(predict.toomuch, predict.toofast) %>% as.data.frame()

rownames(predict) <- c("toomuch.0","toomuch.1","toofast.0","toofast.1")
colnames(predict) <- c("armchair","active","passive")
predict$var <- rownames(predict)

predict <- reshape2::melt(predict)

names <- list("armchair" = "Armchair Citizens",
     "active" = "Actives",
     "passive" = "Passives")

name_labeller <- function(variable, value){
  return(names[value])
}

predict$diff <- NA

for (i in seq(2, nrow(predict), 2)) {
  
  predict[i,]$diff <- predict[i,]$value - predict[i-1,]$value
  
}

plot <- predict %>%
  filter((var == "toofast.0" | var == "toofast.1") & !is.na(diff)) %>%
  ggplot(aes(x = var, y = diff, fill = variable)) +
  geom_col(color = "black", position = position_dodge(.7), width = .5) +
  geom_hline(yintercept = 0, color = "black") + 
  xlab("") +
  ylab('Difference in Predicted Probability of Class Membership\n("Too Fast" = 1 - "Too Fast" = 0)') +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.title = element_text(face = "bold"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  scale_fill_manual(breaks = c("armchair","active","passive"),
                    values = c("#dcdbdb","#9d9d9d","#353535"),
                    labels = c("Armchair Citizens","Actives","Passives"),
                    name = "") +
  coord_flip()

png("figures/fig2.png", res = 750, units = "in", height = 4, width = 4)
  plot
dev.off()

### END ###
