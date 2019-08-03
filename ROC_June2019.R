# packages
library("ggplot2")
library("pROC")

##############Import text files###############
disguise_short <- read.delim("disguise_short.txt")
disguise_long <- read.delim("disguise_long.txt")
undisguise_short <- read.delim("undisguise_short.txt")
undisguise_long <- read.delim("undisguise_long.txt")

##############Create ROC curves###############
disg_s <- roc(controls = disguise_short$false.ID, cases = disguise_short$correct.ID
              [!is.na(disguise_short$correct.ID)], direction = "<", partial.auc = c(1, .75))
disg_l <- roc(controls = disguise_long$false.ID, cases = disguise_long$correct.ID
              [!is.na(disguise_long$correct.ID)], direction = "<", partial.auc = c(1, .75))
undisg_s <- roc(controls = undisguise_short$false.ID, cases = undisguise_short$correct.ID
              [!is.na(undisguise_short$correct.ID)], direction = "<", partial.auc = c(1, .75))
undisg_l <- roc(controls = undisguise_long$false.ID, cases = undisguise_long$correct.ID
              [!is.na(undisguise_long$correct.ID)], direction = "<", partial.auc = c(1, .75))

##############Run pAUC analyses###############
# short encoding disguise vs undisguise .75 specificity 
roc.test(disg_s, undisg_s, reuse.auc = FALSE, paired = FALSE, partial.auc = c(1, .75),
         partial.auc.focus = "sp", method = "bootstrap")
# long encoding disguise vs. undisguise .75 specificity
roc.test(disg_l, undisg_l, reuse.auc = FALSE, paired = FALSE, partial.auc = c(1, .75),
         partial.auc.focus = "sp", method = "bootstrap")

##############Plot ROC using ggplot2###############
short <- ggroc(list(Disguised = disg_s, Undisguised = undisg_s), legacy.axes = TRUE) + 
  labs(title = "Disguised vs. Undisguised Lineups", subtitle = "Under Short Encoding Conditions", 
       x = "False Alarm Rate", y = "Hit Rate", color = "Lineup Type") + geom_segment(aes (x = 0, 
       xend = .4, y = 0, yend = .4), color="grey", linetype="dashed")  + xlim(0,.4) + 
  geom_point(size = 1.5) + theme_classic()
short

long <- ggroc(list(Disguised = disg_l, Undisguised = undisg_l), legacy.axes = TRUE) +
  labs(title = "Disguised vs. Undisguised Lineups", subtitle = "Under Long Encoding Conditions", 
       x = "False Alarm Rate", y = "Hit Rate", color = "Lineup Type") + geom_segment(aes (x = 0, 
       xend = .3, y = 0, yend = .4), color="grey", linetype="dashed") + xlim(0, .3) + 
  geom_point(size = 1.5) + theme_classic()
long
