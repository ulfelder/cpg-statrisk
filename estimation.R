# USHMM/CPG STATISTICAL RISK ASSESSMENT
# MODEL AND RESPONSE ESTIMATION
# 
# Jay Ulfelder
# ulfelder@gmail.com

#################################
# Load and Prep Data
#################################

# Clean up workspace
rm(list=ls(all=TRUE))

# Load data with relevant lags and transformations
dat <- read.table("ushmm.cooked.csv",
            sep = "\t", header = TRUE)

# Subset to 1960-2012 b/c of WDI.
dat <- subset(dat, year>=1960 & year<=2012)

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

#############################
# Model & Respons Estimation
#############################

# Colaresi & Carey

cnc <- glm(f.cnc, family = binomial, data = dat, na.action = na.exclude)
dat$cnc.p <- predict(cnc, newdat = dat, type = "response")

# Elite Threat

coup <- glm(f.coup, family = binomial, data = dat, na.action = na.exclude)
dat$coup.p <- predict(coup, newdat = dat, type = "response")
cwar <- glm(f.cwar, family = binomial, data = dat, na.action = na.exclude)
dat$cwar.p <- predict(cwar, newdat = dat, type = "response")
threat <- glm(f.threat, family = binomial, data = dat, na.action = na.exclude)
dat$threat.p <- predict(threat, newdat = dat, type = "response")

# PITF/Harff Conditional

pitf <- glm(f.pitf, family = binomial, data = subset(dat, pitf.on.1==0), na.action = na.exclude)
dat$pitf.p <- predict(pitf, newdata = dat, type = "response")
dat$pitf.p.2 <- ifelse(dat$pitf.on.1==1, 1, ifelse(is.na(dat$pitf.p)==TRUE, NA, dat$pitf.p )  )
harff <- glm(f.harff, family = binomial, data = subset(dat, pitf.on==1), na.action = na.exclude)
dat$harff.cond.p <- predict(harff, newdata = dat, type = "response")
dat$harff.p <- dat$pitf.p.2 * dat$harff.cond.p

# Random Forest

require(randomForest)
rf <- randomForest(f.rf, data = dat, na.action="na.exclude", ntree = 1000, mtry = 4, cutoff=c(0.4,0.6))
dat$rf.p <- predict(rf, newdata = dat, type = "prob", na.action = "na.exclude")[,2]

# Unweighted Average

dat$mean.p <- (dat$cnc.p + dat$threat.p + dat$harff.p + dat$rf.p)/4

#############################
# Write out results
#############################

save(dat, cnc, coup, cwar, threat, pitf, harff, rf,
     file = "ushmm.tscs.models.RData")
