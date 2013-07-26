# USHMM/CPG STATISTICAL RISK ASSESSMENT
# DATA COMPILATION

# Jay Ulfelder
# ulfelder@gmail.com

# Clean up workspace
rm(list=ls(all=TRUE))

#Load required packages
library(foreign)
library(reshape)

# MASS KILLING EPISODES
# Source: Jay Ulfelder for USHMM
source("r/mass.killing.data.maker.R")
names(mkdv) <- c("country", "year", "yrborn", "yrdied", "age", "sftgcode", "ccode",
                 "mkl.start", "mkl.end", "mkl.ongoing", "mkl.type", "mkl.ever")

# GEOGRAPHIC REGIONS AND COLONIAL LEGACIES
# Source: Jay Ulfelder for USHMM
mkdv -> data
source("R/last_colonizer.R")
source("R/region_maker.R")
data -> df
rm(data, mkdv)

# WORLD BANK'S WORLD DEVELOPMENT INDICATORS
# Source: http://data.worldbank.org/data-catalog/world-development-indicators
source("R/wdi.extractor.R")
df <- merge(df, wdi, all.x = TRUE)
rm(wdi)

# IGOS & TREATY REGIMES
# Source: http://dx.doi.org/10.3886/ICPSR30541.v1
io <- read.csv("data/Ulfelder IO data 2010.csv", header = TRUE)
io <- subset(io, select=c(wbcode, pitfcode, year,
                          eu, nato, natopfp, osce, oecd, coe,
                          comnw, franc, geneva, gattwto, apec, asean, seato, oas,
                          mercosur, opec, arablg, oau, ecowas, iccpr, iccpr1, achr,
                          achpr, icj, oic) )
names(io) <- c("wbcode", "sftgcode", "year",
               "io.eu", "io.nato", "io.natopfp", "io.osce", "io.oecd", "io.coe",
               "io.comnw", "io.franc", "io.geneva", "io.wto", "io.apec", "io.asean", "io.seato", "io.oas",
               "io.mercosur", "io.opec", "io.arablg", "io.oau", "io.ecowas", "io.iccpr", "io.iccpr1", "io.achr",
               "io.achpr", "io.icj", "io.oic")
df <- merge(df, io, all.x = TRUE)
rm(io)

# PATTERNS OF POLITICAL AUTHORITY (POLITY IV)
# Source: http://www.systemicpeace.org/inscr/inscr.htm
polity <- read.delim("data/p4v2012.txt", header = TRUE)
polity <- subset(polity, select = c(scode, year,
                                    democ, autoc, polity, polity2, durable,
                                    xrreg, xrcomp, xropen, xconst, parreg, parcomp,
                                    exrec, exconst, polcomp, change, regtrans) )
names(polity) <- c("sftgcode", "year",
                   "pol.democ", "pol.autoc", "pol.polity", "pol.polity2", "pol.durable",
                   "pol.xrreg", "pol.xrcomp", "pol.xropen", "pol.xconst", "pol.parreg", "pol.parcomp", 
                   "pol.exrec", "pol.exconst", "pol.polcomp", "pol.change", "pol.regtrans")
polity$sftgcode <- as.character(polity$sftgcode)  # Match type for merging
df <- merge(df, polity, all.x = TRUE)
rm(polity)

# UCDP/PRIO ARMED CONFLICT DATA SET
# Source: http://www.pcr.uu.se/research/ucdp/datasets/onset_of_intrastate_armed_conflict/
ucdp <- read.csv("data/133280_onset2012csv.csv", header = TRUE)
ucdp -> data
data$gwno -> data$ccode
data$sftgcode <- "UNK"
source("R/COW_to_PITF_code.R")
data -> ucdp
ucdp$ccode <- NULL
names(ucdp) <- c("year", "gwno", "pri.incid", "pri.newcon", "pri.onset1", "pri.onset2", "pri.onset5",
                 "pri.onset8", "pri.onset20", "pri.maxint", "pri.gov", "pri.terr", "pri.both", "pri.sumconf",
                 "sftgcode")
df <- merge(df, ucdp, all.x = TRUE)
rm(ucdp)

# NATIONAL ELECTIONS ACROSS DEMOCRACY AND AUTOCRACY (NELDA)
# Source: http://hyde.research.yale.edu/nelda/#
source("R/nelda.converter.R")
names(nelda) <- c("sftgcode", "year",
                  "nld.exec", "nld.leg", "nld.ca", "nld.any",
                  "nld.oppallow", "nld.doubtff", "nld.opphars", "nld.inclost",
                  "nld.oppgain", "nld.protest", "nld.govviol")
df <- merge(df, nelda, all.x = TRUE)

# COUPS AND COUP ATTEMPTS
# Source: http://www.systemicpeace.org/inscr/inscr.htm
source("R/coup.converter.R")
df <- merge(df, coups, all.x = TRUE)

# POLITICAL TERROR SCALE
# Source: http://www.politicalterrorscale.org/
pts <- read.delim("data/PTS_2011_FINAL_10-19-2012.txt", header = TRUE)
names(pts) <- c("name", "cow", "ccode", "wbcode", "year", "pts.amnesty", "pts.state")
pts$name <- as.character(pts$name)
pts -> data
source("r/pitf_code_maker.R")
data -> pts
pts <- subset(pts, select = c(code, year, pts.amnesty, pts.state))
names(pts) <- c("sftgcode", "year", "pts.amnesty", "pts.state")
df <- merge(df, pts, all.x = TRUE)
rm(pts)

# ETHNIC POWER RELATIONS (EPR)
# Source: http://dvn.iq.harvard.edu/dvn/dv/epr (via http://www.epr.ucla.edu/)
epr <- read.delim("data/EPR_countryyear_v1.1.tab", header = TRUE)
epr$name <- as.character(epr$country)
epr$yearc <- epr$cowcode <- epr$stateabb <- epr$country <- NULL
epr -> data
source("r/pitf_code_maker.R")
data -> epr
epr$name <- NULL
names(epr) <- c("year",
                "epr.ethrelevant", "epr.groups", "epr.egipgrps", "epr.exclgrps",
                "epr.exclpop", "epr.egippop", "epr.ttlpop", "epr.discpop", "epr.pwrlpop",
                "epr.olppop", "epr.olpspop", "epr.jppop", "epr.sppop", "epr.dompop",
                "epr.monpop", "epr.maxexclpop", "epr.maxegippop", "epr.maxpop",
                "epr.minsnrptr", "epr.elf", "epr.pelf", "epr.celf", "epr.pcelf",
                "epr.polrqnew", "epr.poltrqnew", "epr.egiptpolrqnew", "epr.egippolrqnew",
                "epr.fegip", "epr.fexcl", "epr.dwegip", "epr.fmds", "epr.fmdcs", "epr.fscj", "epr.fmdcj",
                "epr.downgraded", "epr.dominant", "epr.monop", "epr.snrptr", "epr.jnrptr",
                "epr.powerless", "epr.discrim", "epr.olp", "epr.olps", "epr.pwrshare",
                "epr.nonpreg", "epr.rexclpop", "epr.lnexclpop", "epr.rexclpop2", "epr.rexclpop3",
                "epr.lrexclpop", "epr.legippolrqnew", "sftgcode")
df <- merge(df, epr, all.x = TRUE)

# MICHAEL ROSS'S OIL AND GAS DATA 
# Source: http://dvn.iq.harvard.edu/dvn/dv/mlross
oil <- read.delim("data/Ross Oil & Gas Data 1932-2011.tab", header = TRUE)
oil$name <- as.character(oil$cty_name)
oil$cty_name <- oil$id <- oil$eiacty <- NULL
oil -> data
source("r/pitf_code_maker.R")
data -> oil
oil$name <- NULL
names(oil) <- c("year", "oil.oilprod", "oil.oilprice2000", "oil.oilvalnom", "oil.oilval2000",
                "oil.oilval2009", "oil.gasprod", "oil.gasprice2000mboe", "oil.gasprice2000", "oil.gaspricenom",
                "oil.gasvalnom", "oil.gasval2000", "oil.gasval2009", "oil.gasvalnom", "oil.oilgasval2000",
                "oil.oilgasval2009", "oil.oilgasvalPOPnom", "oil.oilgasvalPOP2000", "oil.oilgasvalPOP2009", "oil.oilexp",
                "oil.netoilexp", "oil.netoilexpmt", "oil.netoilexpval", "oil.netoilexpvalPOP", "oil.gasexp",
                "oil.netgasexpbcf", "oil.netgasexpmboe", "oil.netgasexpval", "oil.netgasexpvalPOP", "oil.netgasexpvalPOP",
                "oil.pop", "oil.popmadd", "oil.sovrgn", "sftgcode")
df <- merge(df, oil, all.x = TRUE)

# UNIFIED DEMOCRACY SCORES
# Source: http://www.unified-democracy-scores.org/uds.html
uds <- read.csv("data/uds2008.csv", header = TRUE)
names(uds) <- c("year", "ccode", "uds.mean", "uds.sd", "uds.median", "uds.pct025", "uds.pct0975")
uds -> data
source("r/COW_to_PITF_code.R")
data -> uds
uds$ccode <- NULL  # Make sure merging matches on PITF code, not COW code
df <- merge(df, uds, all.x = TRUE)

# FREEDOM HOUSE
# Source: http://www.freedomhouse.org/report-types/freedom-world
source("r/fiw.converter.R")
fiw$name <- fiw$cowcode <- NULL
df <- merge(df, fiw, all.x = TRUE)

# PITF INSTABILITY EVENTS WITH DURATION
# source("r/pitf.event.data.maker.R") NOT WORKING
probsub <- read.csv("data/pitf.prob.csv", header = TRUE)
df <- merge(df, probsub, all.x = TRUE)

# CSP MAJOR ARMED CONFLICT
mac <- read.csv("data/mac.csv", header = TRUE)
names(mac) <- c('sftgcode', 'year', 'mac.ctote', 'mac.ctotr', 'mac.ctott', 'mac.iind', 'mac.iviol',
                'mac.iwar', 'mac.cviol', 'mac.cwar', 'mac.eviol', 'mac.ewar', 'mac.itot',
                'mac.ctot', 'mac.actot', 'mac.bord', 'mac.regn', 'mac.nreg', 'mac.nciv', 'mac.nac',
                'mac.nint', 'mac.tciv', 'mac.totac', 'mac.tint', 'mac.cvlst', 'mac.aclst', 'mac.inlst',
                'mac.nrint', 'mac.nrciv', 'mac.nrac', 'mac.rgint', 'mac.rgciv', 'mac.regac')
df <- merge(df, mac, all.x = TRUE)

# PITF INFANT MORTALITY AND UPHEAVAL
im <- read.csv("data/MAC VAR.csv", header = TRUE)
im <- subset(im, select=c(SFTGCODE, YEAR, CNSIMR, XXXCIMR, SFTPUHVL, SFTPUHV3))
names(im) <- c('sftgcode', 'year', 'cnsimr', 'xxxcimr', 'sftpuhvl', 'sftpuhv3')
df <- merge(df, im, all.x = TRUE)
rm(im)

# ELITE CHARACTERISTICS
elc <- read.csv("data/elc.csv", header = TRUE)
names(elc) <- c("sftgcode", "year", "elceleth", "elceliti")
df <- merge(df, elc, all.x = TRUE)

# DISCRIMINATION
dis <- read.csv("data/dis.csv", header = TRUE)
dis$dispota4 <- ifelse(dis$pdis1==4 | dis$edis1==4 | dis$pdis2==4 | dis$edis2==4 |
                          dis$pdis3==4 | dis$edis3==4 | dis$pdis4==4 | dis$edis4==4 |
                          dis$pdis5==4 | dis$edis5==4 | dis$pdis6==4 | dis$edis6==4 |
                          dis$pdis7==4 | dis$edis7==4 | dis$pdis8==4 | dis$edis8==4 |
                          dis$pdis9==4 | dis$edis9==4 | dis$pdis10==4 | dis$edis10==4 |
                          dis$pdis11==4 | dis$edis11==4 | dis$pdis12==4 | dis$edis12==4 |
                          dis$pdis13==4 | dis$edis13==4 | dis$pdis14==4 | dis$edis14==4 |
                          dis$pdis15==4 | dis$edis15==4 | dis$pdis16==4 | dis$edis16==4 |
                          dis$pdis17==4 | dis$edis17==4 | dis$pdis18==4 | dis$edis18==4 |
                          dis$pdis19==4 | dis$edis19==4 | dis$pdis20==4 | dis$edis20==4,
                          1, 0)
dis <- rename(dis, c(scode="sftgcode"))
dis <- subset(dis, select=c(sftgcode, year, dispota4) )
df <- merge(df, dis, all.x = TRUE)
df$dispota4[is.na(df$dispota4)== TRUE & df$year >= df$yrborn & df$year <= df$yrdied] <- 0
rm(dis)

# UCDP/PRIO ONE-SIDED VIOLENCE SUMMARIES
osv <- read.csv("data/ucdp_onesided_tscs.csv", header = TRUE)
df <- merge(df, subset(osv, select=c("sftgcode", "year", "govkill", "oppkill")), all.x = TRUE)
df$osv.govt <- ifelse(is.na(df$govkill) == TRUE & df$year>=1989, 0, df$govkill)
df$osv.oppt <- ifelse(is.na(df$oppkill) == TRUE & df$year>=1989, 0, df$oppkill)
df$govkill <- df$oppkill <- NULL

# OTHER SOURCES CONSIDERED BUT REJECTED
# Archigos (latest is 2006 and no plans for updates)
# Fearon ethnic fractionalization (overtaken by other sources)
# NAVCO (latest is 2006 and no plans for updates)
# CIRI (time series too short)

# ORDER BY COUNTRY THEN YEAR
df <- df[order(df$country, df$year),]

# WRITE IT OUT
# Writing to tab-delimited b/c some commas in text fields screw up .csv format
write.table(df, file = "outdata/ushmm.raw.csv", sep = "\t", quote = FALSE, row.names = FALSE)
