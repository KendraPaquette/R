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
              [!is.na(disguise_short$correct.ID)], direction = "<")
disg_l <- roc(controls = disguise_long$false.ID, cases = disguise_long$correct.ID
              [!is.na(disguise_long$correct.ID)], direction = "<")
undisg_s <- roc(controls = undisguise_short$false.ID, cases = undisguise_short$correct.ID
              [!is.na(undisguise_short$correct.ID)], direction = "<")
undisg_l <- roc(controls = undisguise_long$false.ID, cases = undisguise_long$correct.ID
              [!is.na(undisguise_long$correct.ID)], direction = "<")

##############Run pAUC analyses###############
# short encoding disguise vs undisguise .81 specificity (specificity = 1 - FA rate)
roc.test(disg_s, undisg_s, reuse.auc = FALSE, paired = FALSE, partial.auc = c(1, .81),
         partial.auc.focus = "sp", method = "bootstrap")
# long encoding disguise vs. undisguise .9 specificity
roc.test(disg_l, undisg_l, reuse.auc = FALSE, paired = FALSE, partial.auc = c(1, .9),
         partial.auc.focus = "sp", method = "bootstrap")

##############Plot ROC curves###############
# long encoding disguise and undisguise
plot.roc(disg_l, type = "l", xlab = "False Alarm Rate", ylab = "Hit Rate", legacy.axes = TRUE)
plot.roc(disg_l, type = "p", add = TRUE)
plot.roc(undisg_l, type = "l", add = TRUE, col = "Red")
plot.roc(undisg_l, type = "p", add = TRUE, col = "Red")
# short encoding disguise and undisguise
plot.roc(disg_s, type = "l", xlab = "False Alarm Rate", ylab = "Hit Rate", legacy.axes = TRUE)
plot.roc(disg_s, type = "p", add = TRUE)
plot.roc(undisg_s, type = "l", add = TRUE, col = "Red")
plot.roc(undisg_s, type = "p", add = TRUE, col = "Red")