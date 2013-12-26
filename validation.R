# USHMM/CPG STATISTICAL RISK ASSESSMENT
# MODEL VALIDATION
# 
# Jay Ulfelder
# ulfelder@gmail.com

#################################
# Load and Prep Data
#################################

# Clean up workspace
rm(list=ls(all=TRUE))

# Load data with relevant lags and transformations
dat <- read.table("ushmm.cooked.csv", sep = "\t", header = TRUE)

# Subset to 1960-2012 because of missingness, esp from WDI
dat <- subset(dat, year >= 1960 & year <= 2012)

###############################
# Model Formulae
###############################

f.cwar <- formula(pri.onset2 ~ popcat.1 + log(xxxcimr.1) + polcat.1 + log1p(pol.durable.1) +
                  regcon.1 + rsccat.1 + slowgrowth.1 + civconc.1 )

f.coup <- formula(cou.try ~ gdpcat.1 + regcon.1 + postcw + cou.tries5d + polcat.1 +
                  log1p(pol.durable.1) + slowgrowth.1 + io.iccpr1.1)

f.threat <- formula(mkl.start ~ cwar.p + coup.p)

f.cnc <- formula(mkl.start ~ popcat.1 + xconccnc.1 + secstrc3.1 + I(xconccnc.1 * secstrc3.1) +
                 elfcat.1 + traderesc.1 )

f.pitf <- formula(pitf.st ~ log(xxxcimr.1) + pitfcat.1 + dispota4.1 + regcon.1 + as.factor(region) )

f.harff <- formula(mkl.start ~ log(pitf.dur.min.1 + 1) +
                               sftpuhvl.1 + I(log(pitf.dur.min.1 + 1) * sftpuhvl.1) +
                               mkl.ever.1 + I(log(pitf.dur.min.1 + 1) * mkl.ever.1) +
                               elceliti.1 + I(log(pitf.dur.min.1 + 1) * elceliti.1) +
                               autocracy.1 + I(log(pitf.dur.min.1 + 1) * autocracy.1) +
                               elcelethc.1 + I(log(pitf.dur.min.1 + 1) * elcelethc.1) +
                               log(wdi.trade.1) + I(log(pitf.dur.min.1 + 1) * log(wdi.trade.1)) )

f.rf <- formula(as.factor(mkl.start) ~ mkl.ongoing.1 + mkl.ever.1 + ageln.1 + 
                          reg.eap + reg.eur + reg.mna + reg.sca + reg.amr +
                          popcat.1 + xxxcimr.1 +
                          slowgrowth.1 + wdi.trade.1 + rsccat.1 +
                          io.wto.1 + io.iccpr1.1 + postcw +
                          polcat.1 + xconccnc.1 + poldurln.1 + secstrc3.1 + 
                          dispota4.1 + elfcat.1 + elcelethc.1 + elceliti.1 +
                          cou.tries5d + sftpuhvl.1 + regcon.1 + civconc.1 +
                          areg.dur.1 + ewar.dur.1 + rwar.dur.1)


###############################
# Cross-Validation
###############################

library(caret)
dat1 <- subset(dat, is.na(mkl.start) == FALSE)
y <- dat1$mkl.start
dat1$k <- createFolds(y, k = 10, list = FALSE)

predit <- function(x) {
     train <- subset(df, k != x)
     test <- subset(df, k == x)
     test$cnc.p <- predict(glm(f.cnc, family = binomial, data = train, na.action = na.exclude), newdata = test, type = "response")
     test$cwar.p <- predict(glm(f.cwar, family = binomial, data = train, na.action = na.exclude), newdata = test, type = "response")
     test$coup.p <- predict(glm(f.coup, family = binomial, data = train, na.action = na.exclude), newdata = test, type = "response")
     train$cwar.p <- predict(glm(f.cwar, family = binomial, data = train, na.action = na.exclude), type = "response")
     train$coup.p <- predict(glm(f.coup, family = binomial, data = train, na.action = na.exclude), type = "response")
     test$threat.p <- predict(glm(f.threat, family = binomial, data = train, na.action = na.exclude), newdata = test, type = "response")
     test$pitf.p <- predict(glm(f.pitf, family = binomial, data = subset(train, pitf.on.1==0),
          na.action = na.exclude), newdata = test, type = "response")
     test$pitf.p.2 <- ifelse( test$pitf.on.1==1, 1,
          ifelse(is.na(test$pitf.p)==TRUE, NA, test$pitf.p )  )
     test$harff.cond.p <- predict(glm(f.harff, family = binomial, data = subset(train, pitf.on==1),
          na.action = na.exclude), newdata = test, type = "response")
     test$harff.p <- test$pitf.p.2 * test$harff.cond.p
     require(randomForest)
     test$rf.p <- predict(randomForest(f.rf, data = train, na.action="na.exclude", ntree = 1000, mtry = 4, cutoff=c(0.4,0.6)),
          newdata = test, type = "prob", na.action = "na.exclude")[,2]
     test$mean.p <- (test$cnc.p + test$threat.p + test$harff.p + test$rf.p)/4
     out <- subset(test, select = c(sftgcode, year, mkl.start, mean.p, cnc.p, cwar.p, coup.p, threat.p, pitf.p, harff.p, rf.p, k))
     return(out)
     }

df <- dat1
test1 <- predit(1)
test2 <- predit(2)
test3 <- predit(3)
test4 <- predit(4)
test5 <- predit(5)
test6 <- predit(6)
test7 <- predit(7)
test8 <- predit(8)
test9 <- predit(9)
test10 <- predit(10)
out <- rbind(test1, test2, test3, test4, test5,
             test6, test7, test8, test9, test10)

###############################
# Assessing Accuracy
###############################

# Distribution of AUC scores by fold

fun.auc <- function(df, x) {
                            require(verification)
                            mean <- roc.area(df$mkl.start[out$k==x], df$mean.p[out$k==x])
                            cnc <- roc.area(df$mkl.start[out$k==x], df$cnc.p[out$k==x])
                            harff <- roc.area(df$mkl.start[out$k==x], df$harff.p[out$k==x])
                            threat <- roc.area(df$mkl.start[out$k==x], df$threat.p[out$k==x])
                            rf <- roc.area(df$mkl.start[out$k==x], df$rf.p[out$k==x])
                            all <- c(x, mean$A, cnc$A, harff$A, threat$A, rf$A)
                            names(all) <- c("fold", "mean", "cnc", "harff", "threat", "RF")
                            return(all)
                           }

auc1 <- fun.auc(out, 1)
auc2 <- fun.auc(out, 2)
auc3 <- fun.auc(out, 3)
auc4 <- fun.auc(out, 4)
auc5 <- fun.auc(out, 5)
auc6 <- fun.auc(out, 6)
auc7 <- fun.auc(out, 7)
auc8 <- fun.auc(out, 8)
auc9 <- fun.auc(out, 9)
auc10 <- fun.auc(out, 10)
auctab <- as.data.frame(rbind(auc1, auc2, auc3, auc4, auc5, auc6, auc7, auc8, auc9, auc10))

png(file = "ushmm.statrisk.validation.auc.by.fold.png",
     width=12, height=12, units='cm', bg='white', res=150)
plot(density(auctab$mean), xlim=c(0.5,1), ylim=c(0,8), lwd = 2.5, main = "",
     bty = "n", mai = c(1.5, 0.5, 0.25, 0.25), xaxt = "n", yaxt = "n", xlab = "AUC", ylab = "" )
axis(1, tick = FALSE)
lines(density(auctab$cnc), col = "red", lwd = 1.5)
lines(density(auctab$harff), col = "blue", lwd = 1.5)
lines(density(auctab$threat), col = "forestgreen", lwd = 1.5)
lines(density(auctab$RF), col = "goldenrod", lwd = 1.5)
text(x = 0.95,y = 8, "mean", col = "black", adj = 0)
text(x = 0.95,y = 7.5, "C&C", col = "red", adj = 0)
text(x = 0.95,y = 7, "Harff", col = "blue", adj = 0)
text(x = 0.95,y = 6.5, "threat", col = "forestgreen", adj = 0)
text(x = 0.95,y = 6, "RF", col = "goldenrod", adj = 0)
dev.off()

# ROC curves by model

library(ROCR)
mean.pred <- prediction(out$mean.p, out$mkl.start)
mean.roc <- performance(mean.pred, "tpr", "fpr")
mean.auc <- performance(mean.pred, measure = "auc")
cnc.pred <- prediction(out$cnc.p, out$mkl.start)
cnc.roc <- performance(cnc.pred, "tpr", "fpr")
cnc.auc <- performance(cnc.pred, measure = "auc")
harff.pred <- prediction(out$harff.p, out$mkl.start)
harff.roc <- performance(harff.pred, "tpr", "fpr")
harff.auc <- performance(harff.pred, measure = "auc")
threat.pred <- prediction(out$threat.p, out$mkl.start)
threat.roc <- performance(threat.pred, "tpr", "fpr")
threat.auc <- performance(threat.pred, measure = "auc")
rf.pred <- prediction(out$rf.p, out$mkl.start)
rf.roc <- performance(rf.pred, "tpr", "fpr")
rf.auc <- performance(rf.pred, measure = "auc")

png(file = "ushmm.statrisk.validation.roc.png",
     width=12, height=12, units='cm', bg='white', res=150)
plot(mean.roc, col = "black", lwd=2, add = FALSE)
plot(harff.roc, col = "blue", add = TRUE)
plot(cnc.roc, col = "red", add = TRUE)
plot(threat.roc, col = "forestgreen", add = TRUE)
plot(rf.roc, col= "goldenrod", add = TRUE)
text(x=1,y=0.20,
     labels = paste("RF", substring(as.character(rf.auc@y.values),1,5), sep=' = '),
     pos=2, cex=0.75, col = "goldenrod")
text(x=1,y=0.15,
     labels = paste("Threat", substring(as.character(threat.auc@y.values),1,5), sep=' = '),
     pos=2, cex=0.75, col = "forestgreen")
text(x=1,y=0.10,
     labels = paste("C&C", substring(as.character(cnc.auc@y.values),1,5), sep=' = '),
     pos=2, cex=0.75, col = "red")
text(x=1,y=0.05,
     labels = paste("Harff", substring(as.character(harff.auc@y.values),1,5), sep=' = '),
     pos=2, cex=0.75, col = "blue")
text(x=1,y=0,
     labels = paste("Mean", substring(as.character(mean.auc@y.values),1,5), sep=' = '),
     pos=2, cex=0.75, col = "black")
dev.off()

# Separation plots

library(separationplot)
png(file= "ushmm.validation.sep.mean.png",
      bg='white', width = 5.25, height = 1.25, unit = 'in', res=150)
par(mar=c(1,1,1,1))
par(cex=0.7)
separationplot(as.vector(as.matrix( subset(out, is.na(mean.p) == FALSE, select = mean.p) ) ),
              as.vector(as.matrix( subset(out, is.na(mean.p) == FALSE, select = mkl.start) ) ),
              heading = "Mean", newplot = FALSE)
dev.off()

library(separationplot)
png(file= "ushmm.validation.sep.cnc.png",
      bg='white', width = 5.25, height = 1.25, unit = 'in', res=150)
par(mar=c(1,1,1,1))
par(cex=0.7)
separationplot(as.vector(as.matrix( subset(out, is.na(cnc.p) == FALSE, select = cnc.p) ) ),
              as.vector(as.matrix( subset(out, is.na(cnc.p) == FALSE, select = mkl.start) ) ),
              heading = "Colaresi & Carey", newplot = FALSE)
dev.off()

library(separationplot)
png(file= "ushmm.validation.sep.threat.png",
      bg='white', width = 5.25, height = 1.25, unit = 'in', res=150)
par(mar=c(1,1,1,1))
par(cex=0.7)
separationplot(as.vector(as.matrix( subset(out, is.na(threat.p) == FALSE, select = threat.p) ) ),
              as.vector(as.matrix( subset(out, is.na(threat.p) == FALSE, select = mkl.start) ) ),
              heading = "Elite Threat", newplot = FALSE)
dev.off()

library(separationplot)
png(file= "ushmm.validation.sep.harff.png",
      bg='white', width = 5.25, height = 1.25, unit = 'in', res=150)
par(mar=c(1,1,1,1))
par(cex=0.7)
separationplot(as.vector(as.matrix( subset(out, is.na(harff.p) == FALSE, select = harff.p) ) ),
              as.vector(as.matrix( subset(out, is.na(harff.p) == FALSE, select = mkl.start) ) ),
              heading = "PITF/Harff", newplot = FALSE)
dev.off()

library(separationplot)
png(file= "ushmm.validation.sep.rf.png",
      bg='white', width = 5.25, height = 1.25, unit = 'in', res=150)
par(mar=c(1,1,1,1))
par(cex=0.7)
separationplot(as.vector(as.matrix( subset(out, is.na(rf.p) == FALSE, select = rf.p) ) ),
              as.vector(as.matrix( subset(out, is.na(rf.p) == FALSE, select = mkl.start) ) ),
              heading = "Random Forests", newplot = FALSE)
dev.off()

####################################
# Write out predicted probabilities
####################################

write.csv(out, "kfold.predictions.csv", quote = FALSE, row.names = FALSE)
