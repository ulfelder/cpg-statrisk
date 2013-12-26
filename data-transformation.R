# USHMM/CPG STATISTICAL RISK ASSESSMENT
# DATA TRANSFORMATION

# Jay Ulfelder
# ulfelder@gmail.com

# LOAD RAW DATA

# Clear workspace
rm(list=ls(all=TRUE))

# Load raw data file
data <- read.table("ushmm.raw.csv", sep = "\t", header = TRUE)

# Ethnic fractionalization categorized with post-2005 infilled to 2005 values
for (i in 1:length(data$sftgcode)) data$epr.elf[i] <- ifelse(data$year[i] >= 2006, data$epr.elf[i-1], data$epr.elf[i])
data$elfcat[data$epr.elf < quantile(data$epr.elf, 1/3, na.rm=T)] <- 1
data$elfcat[data$epr.elf >= quantile(data$epr.elf, 1/3, na.rm=T) & 
            data$epr.elf <= quantile(data$epr.elf, 2/3, na.rm=T)] <- 2
data$elfcat[data$epr.elf > quantile(data$epr.elf, 2/3, na.rm=T)] <- 3
data$elfcat[is.na(data$epr.elf)==T] <- 9
data$elfcat <- as.factor(data$elfcat)

# LAGGING
library(plm)
lags <- pdata.frame(data, index = c("country", "year"))

lags$age.1 <- lag(lags$age, 1)
lags$io.comnw.1 <- lag(lags$io.comnw, 1)
lags$io.franc.1 <- lag(lags$io.franc, 1)
lags$io.geneva.1 <- lag(lags$io.geneva, 1)
lags$io.wto.1 <- lag(lags$io.wto, 1)
lags$io.iccpr1.1 <- lag(lags$io.iccpr1, 1)
lags$pol.polity.1 <- lag(lags$pol.polity, 1)
lags$pol.polity2.1 <- lag(lags$pol.polity2, 1)
lags$pol.durable.1 <- lag(lags$pol.durable, 1)
lags$pol.parcomp.1 <- lag(lags$pol.parcomp, 1)
lags$pol.exrec.1 <- lag(lags$pol.exrec, 1)
lags$pol.exconst.1 <- lag(lags$pol.exconst, 1)
lags$pol.polcomp.1 <- lag(lags$pol.polcomp, 1)
lags$pol.democ.1 <- lag(lags$pol.democ, 1)
lags$uds.mean.1 <- lag(lags$uds.mean, 1)
lags$pri.incid.1 <- lag(lags$pri.incid, 1)
lags$pri.onset1.1 <- lag(lags$pri.onset1, 1)
lags$pri.onset2.1 <- lag(lags$pri.onset2, 1)
lags$pri.onset5.1 <- lag(lags$pri.onset5, 1)
lags$pri.maxint.1 <- lag(lags$pri.maxint, 1)
lags$pri.gov.1 <- lag(lags$pri.gov, 1)
lags$pri.terr.1 <- lag(lags$pri.terr, 1)
lags$pri.both.1 <- lag(lags$pri.both, 1)
lags$pri.sumconf.1 <- lag(lags$pri.sumconf, 1)
lags$wdi.popsize.1 <- lag(lags$wdi.popsize, 1)
lags$wdi.urban.1 <- lag(lags$wdi.urban, 1)
lags$wdi.gdppc.1 <- lag(lags$wdi.gdppc, 1)
lags$wdi.gdppcppp.1 <- lag(lags$wdi.gdppcppp, 1)
lags$wdi.gdppcgrow.1 <- lag(lags$wdi.gdppcgrow, 1)
lags$wdi.trade.1 <- lag(lags$wdi.trade, 1)
lags$wdi.energy.1 <- lag(lags$wdi.energy, 1)
lags$wdi.minerals.1 <- lag(lags$wdi.minerals, 1)
lags$wdi.forest.1 <- lag(lags$wdi.forest, 1)
lags$wdi.miltot.1 <- lag(lags$wdi.miltot, 1)
lags$wdi.milpct.1 <- lag(lags$wdi.milpct, 1)
lags$mac.nac.1 <- lag(lags$mac.nac, 1)
lags$mac.nreg.1 <- lag(lags$mac.nreg, 1)
lags$mac.regac.1 <- lag(lags$mac.regac, 1)
lags$mac.nrac.1 <- lag(lags$mac.nrac, 1)
lags$mac.ctot.1 <- lag(lags$mac.ctot, 1)
lags$epr.elf.1 <- lag(lags$epr.elf, 1)
lags$elfcat.1 <- lag(lags$elfcat, 1)
lags$epr.pelf.1 <- lag(lags$epr.pelf, 1)
lags$epr.celf.1 <- lag(lags$epr.celf, 1)
lags$epr.pcelf.1 <- lag(lags$epr.pcelf, 1)
lags$epr.ethrelevant.1 <- lag(lags$epr.ethrelevant, 1)
lags$epr.discpop.1 <- lag(lags$epr.discpop, 1)
lags$epr.pwrlpop.1 <- lag(lags$epr.pwrlpop, 1)
lags$epr.exclpop.1 <- lag(lags$epr.exclpop, 1)
lags$epr.rexclpop.1 <- lag(lags$epr.rexclpop, 1)
lags$epr.maxexclpop.1 <- lag(lags$epr.maxexclpop, 1)
lags$epr.maxegippop.1 <- lag(lags$epr.maxegippop, 1)
lags$epr.polrqnew.1 <- lag(lags$epr.polrqnew, 1)
lags$epr.egippolrqnew.1 <- lag(lags$epr.egippolrqnew, 1)
lags$epr.fegip.1 <- lag(lags$epr.fegip, 1)
lags$epr.fexcl.1 <- lag(lags$epr.elf, 1)
lags$epr.downgraded.1 <- lag(lags$epr.downgraded, 1)
lags$epr.dominant.1 <- lag(lags$epr.dominant, 1)
lags$epr.monop.1 <- lag(lags$epr.monop, 1)
lags$epr.powerless.1 <- lag(lags$epr.powerless, 1)
lags$epr.discrim.1 <- lag(lags$epr.discrim, 1)
lags$epr.jnrptr.1 <- lag(lags$epr.jnrptr, 1)
lags$epr.snrptr.1 <- lag(lags$epr.snrptr, 1)
lags$epr.olp.1 <- lag(lags$epr.olp, 1)
lags$epr.olps.1 <- lag(lags$epr.olps, 1)
lags$epr.pwrshare.1 <- lag(lags$epr.pwrshare, 1)
lags$mkl.ongoing.1 <- lag(lags$mkl.ongoing, 1)
lags$mkl.ever.1 <- lag(lags$mkl.ever, 1)
lags$cou.successful.1 <- lag(lags$cou.successful, 1)
lags$cou.successful.2 <- lag(lags$cou.successful, 2)
lags$cou.successful.3 <- lag(lags$cou.successful, 3)
lags$cou.successful.4 <- lag(lags$cou.successful, 4)
lags$cou.successful.5 <- lag(lags$cou.successful, 5)
lags$cou.failed.1 <- lag(lags$cou.failed, 1)
lags$cou.failed.2 <- lag(lags$cou.failed, 2)
lags$cou.failed.3 <- lag(lags$cou.failed, 3)
lags$cou.failed.4 <- lag(lags$cou.failed, 4)
lags$cou.failed.5 <- lag(lags$cou.failed, 5)
lags$cou.rumor.1 <- lag(lags$cou.rumor, 1)
lags$cou.plot.1 <- lag(lags$cou.plot, 1)
lags$cou.s.d.1 <- lag(lags$cou.s.d, 1)
lags$cou.f.d.1 <- lag(lags$cou.f.d, 1)
lags$cou.r.d.1 <- lag(lags$cou.r.d, 1)
lags$cou.p.d.1 <- lag(lags$cou.p.d, 1)
lags$nld.exec.1 <- lag(lags$nld.exec, 1)
lags$nld.leg.1 <- lag(lags$nld.leg, 1)
lags$nld.ca.1 <- lag(lags$nld.ca, 1)
lags$nld.doubtff.1 <- lag(lags$nld.doubtff, 1)
lags$nld.opphars.1 <- lag(lags$nld.opphars, 1)
lags$nld.inclost.1 <- lag(lags$nld.inclost, 1)
lags$nld.oppgain.1 <- lag(lags$nld.oppgain, 1)
lags$nld.protest.1 <- lag(lags$nld.protest, 1)
lags$nld.govviol.1 <- lag(lags$nld.govviol, 1)
lags$nld.oppallow.1 <- lag(lags$nld.oppallow, 1)
lags$nld.any.1 <- lag(lags$nld.any, 1)
lags$elceleth.1 <- lag(lags$elceleth, 1)
lags$elceliti.1 <- lag(lags$elceliti, 1)
lags$dispota4.1 <- lag(lags$dispota4, 1)
lags$cnsimr.1 <- lag(lags$cnsimr, 1)
lags$xxxcimr.1 <- lag(lags$xxxcimr, 1)
lags$sftpuhvl.1 <- lag(lags$sftpuhvl, 1)
lags$sftpuhv3.1 <- lag(lags$sftpuhv3, 1)
lags$osv.govt.1 <- lag(lags$osv.govt, 1)
lags$osv.oppt.1 <- lag(lags$osv.oppt, 1)
lags$pitf.on.1 <- lag(lags$pitf.on, 1)
lags$pitf.dur.max.1 <- lag(lags$pitf.dur.max, 1)
lags$pitf.dur.min.1 <- lag(lags$pitf.dur.min, 1)
lags$pitf.st.1 <- lag(lags$pitf.st, 1)
lags$areg.on.1 <- lag(lags$areg.on, 1)
lags$ewar.on.1 <- lag(lags$ewar.on, 1)
lags$rwar.on.1 <- lag(lags$rwar.on, 1)
lags$areg.dur.1 <- lag(lags$areg.dur, 1)
lags$ewar.dur.1 <- lag(lags$ewar.dur, 1)
lags$rwar.dur.1 <- lag(lags$rwar.dur, 1)

# Identify as data frame.
data <- as.data.frame(lags)

##### OTHER TRANSFORMATIONS I DO A LOT #####

data$year <- as.numeric(as.character(data$year))
data$postcw <- ifelse(data$year>=1991, 1, 0)

# New state indicator
data$newstate <- ifelse(data$age <= 1, 1, 0)

data$cou.tries5 <- data$cou.successful.1 + data$cou.successful.2 + data$cou.successful.3 + data$cou.successful.4 + data$cou.successful.5 +
                    data$cou.failed.1 + data$cou.failed.2 + data$cou.failed.3 + data$cou.failed.4 + data$cou.failed.5
data$cou.tries5d <- ifelse(data$cou.successful.1>0 | data$cou.successful.2>0 | data$cou.successful.3>0 | data$cou.successful.4>0 | data$cou.successful.5>0 |
                            data$cou.failed.1>0 | data$cou.failed.2>0 | data$cou.failed.3>0 | data$cou.failed.4>0 | data$cou.failed.5>0,
                            1, 0)

# Fearon & Laitin regime type (autocracy, anocracy, democracy)
data$polcat.1[data$pol.polity.1 >= -10 & data$pol.polity.1 < -5] <- 1 
data$polcat.1[data$pol.polity.1 >= -5 & data$pol.polity.1 <= 5] <- 2
data$polcat.1[data$pol.polity.1 > 5] <- 3
data$polcat.1[data$pol.polity.1 == -66 | data$pol.polity.1 == -77 | data$pol.polity.1 == -88 ] <- 7
data$polcat.1 <- as.factor(data$polcat.1)

# Harff autocracy indicator
data$autocracy.1 <- ifelse(data$pol.polity.1 <= 0 & data$pol.polity.1 >= -10, 1, 0)

# PITF AJPS regime types
data$pitfcat.1[data$pol.polity.1==-66 | data$pol.polity.1==-77 | data$pol.polity.1==-88] <- "other"
data$pitfcat.1[(data$pol.exrec.1 >= 1 & data$pol.exrec.1 <= 6) & (data$pol.parcomp.1 == 1 | data$pol.parcomp.1 == 2)] <- "A/F"
data$pitfcat.1[(data$pol.exrec.1 >= 1 & data$pol.exrec.1 <= 6) &
              (data$pol.parcomp.1 == 0 | data$pol.parcomp.1 == 3 | data$pol.parcomp.1 == 4 | data$pol.parcomp.1 == 5)] <- "A/P"
data$pitfcat.1[(data$pol.exrec.1 == 7 | data$pol.exrec.1 == 8) & (data$pol.parcomp.1 == 1 | data$pol.parcomp.1 == 2)] <- "A/P"
data$pitfcat.1[data$pol.parcomp.1 == 3 & (data$pol.exrec.1 == 7 | data$pol.exrec.1==8)] <- "D/fact"
data$pitfcat.1[data$pol.exrec.1 == 8 & (data$pol.parcomp.1 == 0 | data$pol.parcomp.1 == 4 )] <- "D/P"
data$pitfcat.1[data$pol.exrec.1 == 7 & (data$pol.parcomp.1 == 0 | data$pol.parcomp.1 == 4 | data$pol.parcomp.1 == 5)] <- "D/P"
data$pitfcat.1[data$pol.exrec.1 == 8 & data$pol.parcomp.1 == 5] <- "D/F"
data$pitfcat.1 <- as.factor(data$pitfcat.1)

# Other Polity vars made categorical
data$exrecc.1 <- ifelse(is.na(data$pol.exrec.1)==F & data$pol.exrec.1 >= 0, data$pol.exrec.1, NA)
data$exrecc.1[data$pol.polity.1 == -66 | data$pol.polity.1 == -77 | data$pol.polity.1 == -88] <- 99
data$exrecc.1 <- as.factor(data$exrecc.1)

data$polcompc.1 <- ifelse(is.na(data$pol.polcomp.1)==F & data$pol.polcomp.1 >= 0, data$pol.polcomp.1, NA)
data$polcompc.1[data$pol.polity.1 == -66 | data$pol.polity.1 == -77 | data$pol.polity.1 == -88] <- 99
data$polcompc.1 <- as.factor(data$polcompc.1)

data$exconstc.1 <- ifelse(is.na(data$pol.exconst.1)==F & data$pol.exconst.1 >= 0, data$pol.exconst.1, NA)
data$exconstc.1[data$pol.polity.1 == -66 | data$pol.polity.1 == -77 | data$pol.polity.1 == -88] <- 99
data$exconstc.1 <- as.factor(data$exconstc.1)

# Salient elite ethnicity (majority or minority)
data$elcelethc.1 <- ifelse(data$elceleth.1 > 0, 1, 0)

# PITF AJPS "bad neighborhood" indicator
data$badhood.1 <- ifelse(data$mac.nac.1 >= 4, 1, 0) 

# Coup attempt indicator
data$coupdv <- ifelse(data$cou.s.d==1 | data$cou.f.d==1, 1, 0)

# Executive constraints indicators for C&C, Hazlett models
data$xconccnc.1 <- ifelse(data$pol.exconst.1 >= 3, 1, 0)
data$xconchaz.1 <- ifelse(data$pol.exconst.1 >= 4, 1, 0)

# GDP per capita categorical measure
data$gdpcat.1[data$wdi.gdppc.1 < quantile(data$wdi.gdppc.1, 1/3, na.rm=T)] <- 1
data$gdpcat.1[data$wdi.gdppc.1 >= quantile(data$wdi.gdppc.1, 1/3, na.rm=T) & 
             data$wdi.gdppc.1 <= quantile(data$wdi.gdppc.1, 2/3, na.rm=T)] <- 2
data$gdpcat.1[data$wdi.gdppc.1 > quantile(data$wdi.gdppc.1, 2/3, na.rm=T)] <- 3
data$gdpcat.1[is.na(data$wdi.gdppc.1)==T] <- 9
data$gdpcat.1 <- as.factor(data$gdpcat.1)

# Trade openness indicator
data$tradecat.1[data$wdi.trade.1 < quantile(data$wdi.trade.1, 1/3, na.rm=T)] <- 1
data$tradecat.1[data$wdi.trade.1 >= quantile(data$wdi.trade.1, 1/3, na.rm=T) & 
             data$wdi.trade.1 <= quantile(data$wdi.trade.1, 2/3, na.rm=T)] <- 2
data$tradecat.1[data$wdi.trade.1 > quantile(data$wdi.trade.1, 2/3, na.rm=T)] <- 3
data$tradecat.1[is.na(data$wdi.trade.1)==T] <- 9
data$tradecat.1 <- as.factor(data$tradecat.1)

# Trade openness indicator, adjusted for time trend
data$traderes.1 <- residuals(lm(wdi.trade.1 ~ year, data = data, na.action = "na.exclude"))
data$traderesc.1[data$traderes.1 < quantile(data$traderes.1, 1/3, na.rm=T)] <- 1
data$traderesc.1[data$traderes.1 >= quantile(data$traderes.1, 1/3, na.rm=T) & 
             data$traderes.1 <= quantile(data$traderes.1, 2/3, na.rm=T)] <- 2
data$traderesc.1[data$traderes.1 > quantile(data$traderes.1, 2/3, na.rm=T)] <- 3
data$traderesc.1[is.na(data$traderes.1)==T] <- 9
data$traderesc.1 <- as.factor(data$traderesc.1)

# Population size indicator, adjusted for time trend
data$popres.1 <- residuals(lm(wdi.popsize.1 ~ year, data = data, na.action = "na.exclude"))
data$popcat.1[data$popres.1 < quantile(data$popres.1, 1/3, na.rm=T)] <- 1
data$popcat.1[data$popres.1 >= quantile(data$popres.1, 1/3, na.rm=T) & 
             data$popres.1 <= quantile(data$popres.1, 2/3, na.rm=T)] <- 2
data$popcat.1[data$popres.1 > quantile(data$popres.1, 2/3, na.rm=T)] <- 3
data$popcat.1[is.na(data$popres.1)==T] <- 9
data$popcat.1 <- as.factor(data$popcat.1)

# Natural-resource wealth
data$natrsc.1 <- data$wdi.energy.1 + data$wdi.minerals.1
data$rsccat.1[data$natrsc.1 < quantile(data$natrsc.1, 1/3, na.rm=T)] <- 1
data$rsccat.1[data$natrsc.1 >= quantile(data$natrsc.1, 1/3, na.rm=T) & 
             data$natrsc.1 <= quantile(data$natrsc.1, 2/3, na.rm=T)] <- 2
data$rsccat.1[data$natrsc.1 > quantile(data$natrsc.1, 2/3, na.rm=T)] <- 3
data$rsccat.1[is.na(data$natrsc.1)==T] <- 9
data$rsccat.1 <- as.factor(data$rsccat.1)

# Slow GDP growth
data$slowgrowth.1[data$wdi.gdppcgrow.1 < 2] <- 1
data$slowgrowth.1[data$wdi.gdppcgrow.1 >= 2] <- 0
data$slowgrowth.1[is.na(data$wdi.gdppcgrow.1)==T] <- 9
data$slowgrowth.1 <- as.factor(data$slowgrowth.1)

# Transform selected vars to appox normal distrib or just to transform.
data$regcon.1 <- ifelse(data$mac.nreg.1==0, data$mac.regac.1, data$mac.regac.1/data$mac.nreg.1)
data$civconc.1[data$mac.ctot.1==0] <- 0
data$civconc.1[data$mac.ctot.1 > 0] <- 1
data$civconc.1[is.na(data$mac.ctot.1)==T] <- NA
data$epr.inclpop.1 <- 1 - data$epr.exclpop.1   # Per Andreas Wimmer's recommendation
data$democ.1 <- ifelse(is.na(data$pol.democ.1) & data$pol.democ.1 >= 0, data$pol.democ.1, NA)
data$ageln.1 <- log1p(data$age)
data$poldurln.1 <- log1p(data$pol.durable.1)

data$elcelethc.1 <- ifelse(data$elceleth.1 >= 1, 1, 0)

# Colaresi & Carey's military measure
data$secstrength.1 <- data$wdi.miltot.1/log(data$wdi.popsize.1)
data$secstrc.1[data$secstrength.1 < quantile(data$secstrength.1, 1/3, na.rm=T)] <- 1
data$secstrc.1[data$secstrength.1 >= quantile(data$secstrength.1, 1/3, na.rm=T) & 
              data$secstrength.1 <= quantile(data$secstrength.1, 2/3, na.rm=T)] <- 2
data$secstrc.1[data$secstrength.1 > quantile(data$secstrength.1, 2/3, na.rm=T)] <- 3
data$secstrc.1[is.na(data$secstrength.1)==T] <- 9
data$secstrc.1 <- as.factor(data$secstrc.1)
data$secstrc3.1 <- ifelse(data$secstrc.1==3, 1, 0)

# Make sure categorical variables are read properly
data$popcat.1 <- as.factor(data$popcat.1)
data$polcat.1 <- as.factor(data$polcat.1)
data$rsccat.1 <- as.factor(data$rsccat.1)
data$gdpcat.1 <- as.factor(data$gdpcat.1)
data$elfcat.1 <- as.factor(data$elfcat.1)
data$traderesc.1 <- as.factor(data$traderesc.1)
data$pitfcat.1 <- as.factor(data$pitfcat.1)

# Create DVs for coup model
data$cou.try <- ifelse(data$cou.s.d == 1 | data$cou.f.d == 1, 1, ifelse(is.na(data$cou.s.d)==F, 0, NA) )

# LABELING AND SUMMARIZING USHMM DATA IN HMISC
#
# GENERAL NOTES
#
# Variables are in columns, country-year records in rows
# Prefixes (e.g., 'wdi.', 'pol.') identify data source
# Numeric suffixes identify lagged versions of variables with the same label (e.g.,
# cou.failed.1 is a 1-yr lagged version of cou.failed, cou.failed.2 is the 2-yr # lagged version, etc.)
#
# The binary dependent variable for mass-killing onset modeling is 'mkl.start'
#
# SOURCES
#
# WDI = World Bank's World Development Indicators
# MEPV = Center for Systemic Peace's Major Episodes of Political Violence
# PRIO = Peace Research Institute of Olso
# UDS = Unified Democracy Score
# Archigos 
# Polity
# U.S. Bureau of the Census, International Division
# EPR = Ethnic Power Relations
# NELDA
# COU = Center for Systemic Peace's List of Coups d'Etat
# PITF = Political Instability Task Force
# DIS = Center for Systemic Peace's Discrimination Data Set

#################################
# Add labels
#################################

library(Hmisc)
attach(data)

label(year) <- "Year"
label(sftgcode) <- "PITF country code"
label(ccode) <- "COW country code"
label(country) <- "Country name"
label(yrborn) <- "Country 'birth' year"
label(yrdied) <- "Country 'death' year"
label(colbrit) <- "Former British colony"
label(colfrnc) <- "Former French colony"
label(colespn) <- "Former Spanish colony"
label(colport) <- "Former Portuguese colony"
label(colbelg) <- "Former Belgian colony"
label(empotto) <- "Former part of Ottoman empire"
label(empruss) <- "Former part of Russian empire"
label(empaush) <- "Former part of Austro-Hungarian empire"
label(empussr) <- "Former part of USSR"
label(reg.eap) <- "US Dept State region: East Asia & Pacific"
label(reg.afr) <- "US Dept State region: Africa"
label(reg.eur) <- "US Dept State region: Europe"
label(reg.mna) <- "US Dept State region: Middle East & North Africa"
label(reg.sca) <- "US Dept State region: South & Central Asia"
label(reg.amr) <- "US Dept State region: Americas"
label(region) <- "US Dept State region (categorical)"
label(mkl.start) <- "Onset of mass killing per Valentino (yes/no) [DV for this analysis]"
label(mkl.end) <- "End of mass killing episode per Valentino (yes/no)"
label(mkl.ongoing) <- "Ongoing mass killing episode per Valentino (yes/no)"
label(mkl.ever) <- "Any mass killing episodes since WWII (yes/no)"
dat$mkl.type <- factor(dat$mkl.type, levels=c(0,1,2,3,4), labels=c('none','civil war','uprising','repression','other'))
label(mkl.type) <- "Type of ongoing mass killing per Ulfelder"
label(age) <-"country age"
label(wdi.popsize) <-"Population size, WDI"
label(wdi.urban) <-"Percent urban, WDI"
label(wdi.gdppcppp) <-"GDP per capita, PPP, WDI"
label(wdi.gdppc) <-"GDP per capita, WDI"
label(wdi.gdppcgrow) <-"Annual % change in GDP per capita, WDI"
label(wdi.trade) <-"Trade openness (imports + exports/GDP), WDI"
label(wdi.milspend) <-"Military spending as % of GNI, WDI"
label(wdi.miltot) <-"Military personnel, WDI"
label(wdi.milpct) <-"Military personnel as % of pop, WDI"
label(wdi.energy) <-"Energy depletion as % of GNI, WDI"
label(wdi.minerals) <-"Mineral depletion as a % of GNI, WDI"
label(wdi.forest) <-"Forest depletion as % of GNI, WDI"
label(wbcode) <-"World Bank country code"
label(io.eu) <-"European Union membership (0=no, 1=acceding, 2=member)"
label(io.nato) <-"NATO membership (0=no, 1=acceding, 2=member)"
label(io.natopfp) <-"NATO Partnership for Peace membership"
label(io.osce) <-"OSCE membership (0=no, 1=observer, 2=member)"
label(io.oecd) <-"OECD membership"
label(io.coe) <-"Council of Europe membership"
label(io.comnw) <-"Commonwealth membership"
label(io.franc) <-"Francophonie membership (0=no, 1=observer, 2=member)"
label(io.geneva) <-"Geneva Conventions signatory"
label(io.wto) <-"GATT signatory/WTO membership"
label(io.apec) <-"APEC membership"
label(io.asean) <-"ASEAN membership"
label(io.seato) <-"SEATO membership"
label(io.oas) <-"OAS membership"
label(io.mercosur) <-"Mercosur membership (0=no, 1=observer, 2=member)"
label(io.opec) <-"OPEC membership"
label(io.arablg) <-"Arab League membership"
label(io.oau) <-"OAU membership"
label(io.ecowas) <-"ECOWAS membership"
label(io.iccpr) <-"ICCPR signatory (0=no, 1=signed, 2=acceded)"
label(io.iccpr1) <-"ICCPR 1st Optional Protocol signatory"
label(io.achr) <-"ACHR signatory"
label(io.achpr) <- "ACHPR signatory"
label(io.icj) <- "ICJ signatory"
label(io.oic) <- "OIC membership"
label(pol.democ) <- "Polity democracy score"
label(pol.autoc) <- "Polity autocracy score"
label(pol.polity) <- "Polity score"
label(pol.polity2) <- "Polity 2 score"
label(pol.durable) <- "Polity durability"
label(pol.xrreg) <- "Polity regulation of executive recruitment"
label(pol.xrcomp) <- "Polity competitiveness of executive recruitment"
label(pol.xropen) <- "Polity openness of executive recruitment"
label(pol.xconst) <- "Polity executive constraints"
label(pol.parreg) <- "Polity regulation of participation"
label(pol.parcomp) <- "Polity competitiveness of participation"
label(pol.exrec) <- "Polity Executive Recruitment"
label(pol.exconst) <- "Polity Executive Constraints"
label(pol.polcomp) <- "Polity Political Competition"
label(uds.mean) <- "Unified Democracy Score: mean"
label(uds.sd) <- "Unified Democracy Score: std deviation"
label(uds.median) <- "Unified Democracy Score: median"
label(uds.pct025) <- "Unified Democracy Score: lower bound of 95% CI"
label(uds.pct0975) <- "Unified Democracy Score: upper bound of 95% CI"
label(pri.incid) <- "PRIO incidence of civil conflict"
label(pri.onset1) <- "PRIO onset of civil conflict, 1-yr gap"
label(pri.onset2) <- "PRIO onset of civil conflict, 2-yr gap"
label(pri.onset5) <- "PRIO onset of civil conflict, 5-yr gap"
label(pri.onset8) <- "PRIO onset of civil conflict, 8-yr gap"
label(pri.onset20) <- "PRIO onset of civil conflict, 20-yr gap"
label(pri.maxint) <- "PRIO maximum intensity of civil conflict"
label(pri.gov) <- "PRIO conflict over governance"
label(pri.terr) <- "PRIO conflict over territory"
label(pri.both) <- "PRIO conflict over governance & territory"
label(pri.sumconf) <- "PRIO sum of conflict intensities"
label(mac.ctote) <- "MEPV annual total magnitude of ethnic war & geno/politicide"
label(mac.ctotr) <- "MEPV annual total magnitude of revolutionary & colonial wars"
label(mac.ctott) <- "MEPV mac.ctote + mac.ctotr"
label(mac.iind) <- "MEPV annual magnitude of wars of independence"
label(mac.iviol) <- "MEPV score of episodes of international violence"
label(mac.iwar) <- "MEPV score of episodes of international war"
label(mac.cviol) <- "MEPV score of episodes of civil violence"
label(mac.cwar) <- "MEPV score of episodes of civil war"
label(mac.eviol) <- "MEPV score of episodes of ethnic violence"
label(mac.ewar) <- "MEPV score of episodes of ethnic war"
label(mac.itot) <- "MEPV mac.iviol + mac.iwar"
label(mac.ctot) <- "MEPV mac.cviol + mac.cwar"
label(mac.etot) <- "MEPV mac.eviol + mac.ewar"
label(mac.actot) <- "MEPV mac.ctot + mac.itot"
label(mac.bord) <- "MEPV number of bordering countries"
label(mac.regn) <- "MEPV code for geographic region"
label(mac.nreg) <- "MEPV number of regional countries"
label(mac.nciv) <- "MEPV # of bordering states w/major civil or ethnic conflict"
label(mac.nac) <- "MEPV # of bordering states w/any armed conflict"
label(mac.nint) <- "MEPV # of bordering states w/major int'l conflict"
label(mac.tciv) <- "MEPV sum of civil/ethnic conflicts in bordering states"
label(mac.totac) <- "MEPV sum of all armed conflicts of bordering states"
label(mac.tint) <- "MEPV sum of int'l conflicts of bordering states"
label(mac.cvlst) <- "MPEV list of bordering states w/civil or ethnic conflict"
label(mac.avlst) <- "MEPV list of bordering states w/any armed conflict"
label(mac.inlst) <- "MEPV list of bordering states w/int'l conflict"
label(mac.nrint) <- "MEPV # of regional states w/int'l conflict"
label(mac.nrciv) <- "MEPV # of regional states w/civil conflict"
label(mac.nrac) <- "MEPV # of regional states w/any armed conflict"
label(mac.rgint) <- "MEPV sum of int'l conflicts in regional states"
label(mac.rgciv) <- "MEPV sum of civil conflicts in regional states"
label(mac.regac) <- "MEPV sum of all armed conflicts in regional states"
label(arc.ex1n) <- "Archigos count of regular leader exits"
label(arc.ex2n) <- "Archigos count of leader deaths by natural causes"
label(arc.ex3n) <- "Archigos count of irregular leader exits"
label(arc.ex4n) <- "Archigos count of leaders deposed by foreign state"
label(arc.exn) <- "Archigos count of leader exits"
label(arc.ex3d) <- "Archigos indicator of any irregular leader exits"
label(epr.elf) <- "EPR ethno-linguistic fractionalization"
label(epr.pelf) <- "EPR fractionalization of excluded groups"
label(epr.celf) <- "EPR fractionalization of included groups"
label(epr.pcelf) <- "EPR fractionalization of included groups 2"
label(epr.discpop) <- "EPR discriminated population (% of tot)"
label(epr.pwrlpop) <- "EPR powerless population (% of tot)"
label(epr.exclpop) <- "EPR excluded population (% of tot)"
label(epr.maxexclpop) <- "EPR size of largest excluded group (% of pop)"
label(epr.maxegippop) <- "EPR size of largest included group (% of pop)"
label(epr.polrqnew) <- "EPR polarization (relative to ethnopol relevant pop)"
label(epr.egippolrqnew) "EPR polarization of included pop (relative of to ethnopol relevant pop)"
label(epr.fegip) <- "EPR included"
label(epr.fexcl) <- "EPR any excluded"
label(epr.downgraded) <- "EPR downgraded"
label(epr.dominant) <- "EPR dominant"
label(epr.monop) <- "EPR monopoly"
label(epr.powerless) <- "EPR powerless"
label(epr.discrim) <- "EPR discriminated"
label(epr.jnrptr) <- "EPR junior partner"
label(epr.snrptr) <- "EPR senior partner"
label(epr.olp) <- "EPR only local power"
label(epr.olps) <- "EPR only local power separatist"
label(epr.pwrshare) <- "EPR powersharing government"
label(epr.ethrelevant) <- "EPR indicator of political relevance of ethnicity"
label(epr.rexclpop) <- "EPR excluded pop as share of ethnopol relevant pop"
label(nld.exec) <- "NELDA any executive elections"
label(nld.leg) <- "NELDA any legislative elections"
label(nld.ca) <- "NELDA any constituent assembly elections"
label(nld.doubtff) <- "NELDA doubts about free/fairness of elections"
label(nld.opphars) <- "NELDA opposition harassed around elections"
label(nld.inclost) <- "NELDA incumbent lost elections"
label(nld.oppgain) <- "NELDA opposition made gains in elections"
label(nld.protest) <- "NELDA protests around elections"
label(nld.govviol) <- "NELDA govt violence around elections"
label(nld.oppallow) <- "NELDA opposition allowed in elections"
label(nld.any) <- "NELDA any elections"
label(cou.successful) <- "COU # of successful coups"
label(cou.failed) <- "COU # of failed coups"
label(cou.plot) <- "COU # of coup plots"
label(cou.rumor) <- "COU # of coup rumors"
label(cou.s.d) <- "COU any successful coups"
label(cou.f.d) <- "COU any failed coups"
label(cou.p.d) <- "COU any coup plots"
label(cou.r.d) <- "COU any coup rumors"
label(sftpeth) <- "PITF ethnic war indicator"
label(sftprev) <- "PITF revolutionary war indicator"
label(sftpreg) <- "PITF adverse regime change indicator"
label(sftpgen) <- "PITF genocide/politicide indicator"
label(cnsimr) <- "Census infant mortality rate"
label(xxxcimr) <- "Census infant mortality rate rel to annual global median"
label(sftpuhvl) <- "PITF upheaval score"
label(sftpuhv3) <- "PITF upheaval score w/o genocide/politicide"
dat$elceleth <- factor(dat$elceleth, levels=c(0,1,2), labels=c('not salient','salient/maj rule', 'salient/min rule'))
label(elceleth) <- "ELC salience of elite ethnicity"
label(elceliti) <- "ELC exclusionary elite ideology"
label(dispota4) <- "DIS % of pop subject to state-led discrimination"
label(cou.try) <- "Any coup attempts, successful or failed (binary)"
label(pitf.st) <- "Onset of PITF civil war or adverse regime change (binary)"
label(pitf.on) <- "Any PITF civil war or adverse regime change (binary)"
label(pitf.dur.min) <- "Duration of PITF instability episode (years, reset by onset)"
label(pitf.dur.max) <- "Duration of PITF instability episode (years, not reset by onset)"
detach(data) 

# Use Hmisc to get a quick sense of data structure and missingness
contents(data)

# WRITE IT OUT

# Going tab-delimited b/c there are some commas in text fields screwing up .csv format
write.table(data, file = "ushmm.cooked.csv", sep = "\t", quote = FALSE, row.names = FALSE)
