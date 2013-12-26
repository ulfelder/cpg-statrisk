# USHMM/CPG STATISTICAL RISK ASSESSMENT
# PRODUCE FORECASTS FOR 2013
#
# Jay Ulfelder
# ulfelder@gmail.com

# Clear workspace
rm(list=ls(all=TRUE))

# Load workspace with data and model objects created in 'estimation' script
load("ushmm.tscs.models.RData")

#####################################
# Get & Prepare 2012 Data
#####################################

# Select subset of variables from dat for 2010 or 2011 as available to cover slow-changing things
# Will need to check & update some by hand
datsub <- subset(dat, year == 2011, select = c(sftgcode, ccode, country, region,
                 popcat.1, xxxcimr.1, rsccat.1, gdpcat.1, postcw, secstrc3.1, elfcat.1, wdi.trade.1, traderesc.1,
                 polcat.1, pol.durable.1, xconccnc.1, pitfcat.1, autocracy.1,
                 cou.tries5d, dispota4.1, elceliti.1, elcelethc.1, mkl.ever.1, io.iccpr1.1, io.wto.1,
                 civconc.1, regcon.1, mac.nreg.1, mac.regac.1, ageln.1,
                 reg.eap, reg.afr, reg.eur, reg.mna, reg.sca, reg.amr) )

# Economic growth
# Source: IMF World Economic Outlook October 2012 http://www.imf.org/external/pubs/ft/weo/2012/02/pdf/text.pdf
datsub$slowgrowth.1 <- ifelse(datsub$sftgcode=="GER" | datsub$sftgcode=="FRN" | datsub$sftgcode=="SPN" | datsub$sftgcode=="ITA" | datsub$sftgcode=="BEL" |
                              datsub$sftgcode=="NTH" | datsub$sftgcode=="AUS" | datsub$sftgcode=="GRC" | datsub$sftgcode=="POR" | datsub$sftgcode=="FIN" |
                              datsub$sftgcode=="IRE" | datsub$sftgcode=="SLV" | datsub$sftgcode=="CYP" | datsub$sftgcode=="UK" | datsub$sftgcode=="CAN" |
                              datsub$sftgcode=="TAW" | datsub$sftgcode=="SWD" | datsub$sftgcode=="SWZ" | datsub$sftgcode=="CZR" | datsub$sftgcode=="DEN" |
                              datsub$sftgcode=="ALB" | datsub$sftgcode=="BOS" | datsub$sftgcode=="BUL" | datsub$sftgcode=="RUM" | datsub$sftgcode=="CRO" |
                              datsub$sftgcode=="HUN" | datsub$sftgcode=="MAC" | datsub$sftgcode=="MNG" | datsub$sftgcode=="SRB" | datsub$sftgcode=="KYR" |
                              datsub$sftgcode=="ARG" | datsub$sftgcode=="BRA" | datsub$sftgcode=="SAL" | datsub$sftgcode=="JAM" | datsub$sftgcode=="PAR" |
                              datsub$sftgcode=="TRI" | datsub$sftgcode=="IRN" | datsub$sftgcode=="SUD" | datsub$sftgcode=="SYR" | datsub$sftgcode=="YEM" |
                              datsub$sftgcode=="GAM" | datsub$sftgcode=="GNB" | datsub$sftgcode=="MAG" | datsub$sftgcode=="MLI" | datsub$sftgcode=="SSD" |
                              datsub$sftgcode=="SWA",
                              1, 0 )

# ICCPR 1st Optional Protocol
# Source: UN http://treaties.un.org/Pages/ViewDetails.aspx?src=TREATY&mtdsg_no=IV-5&chapter=4&lang=en
datsub$io.iccpr1.1[datsub$sftgcode=="TUN"] <- 1 
datsub$io.iccpr1.1[datsub$sftgcode=="USA"] <- 0 
datsub$io.iccpr1.1[datsub$sftgcode=="SSD"] <- 0 
datsub$io.iccpr1.1[datsub$sftgcode=="MNG"] <- 0 
datsub$io.iccpr1.1[datsub$sftgcode=="SWA"] <- 0 

# Prior mass killings (mkl.ever.1)
datsub$mkl.ever.1[datsub$sftgcode=="SSD"] <- 0  # Fill in missing

# Trade openness infills
# Source: older WDI or CIA World Factbook
datsub$wdi.trade.1[datsub$sftgcode=="BAH"] <- 171
datsub$wdi.trade.1[datsub$sftgcode=="BHU"] <- 137
datsub$wdi.trade.1[datsub$sftgcode=="BFO"] <- 38
datsub$wdi.trade.1[datsub$sftgcode=="BUI"] <- 58
datsub$wdi.trade.1[datsub$sftgcode=="CEN"] <- 37
datsub$wdi.trade.1[datsub$sftgcode=="COM"] <- 63
datsub$wdi.trade.1[datsub$sftgcode=="CON"] <- 96
datsub$wdi.trade.1[datsub$sftgcode=="DJI"] <- 134
datsub$wdi.trade.1[datsub$sftgcode=="EQG"] <- 128
datsub$wdi.trade.1[datsub$sftgcode=="ERI"] <- 25
datsub$wdi.trade.1[datsub$sftgcode=="GER"] <- 92
datsub$wdi.trade.1[datsub$sftgcode=="GNB"] <- 81
datsub$wdi.trade.1[datsub$sftgcode=="GUY"] <- 204
datsub$wdi.trade.1[datsub$sftgcode=="IRN"] <- 18
datsub$wdi.trade.1[datsub$sftgcode=="IRQ"] <- 92
datsub$wdi.trade.1[datsub$sftgcode=="KUW"] <- 84
datsub$wdi.trade.1[datsub$sftgcode=="LBR"] <- 90
datsub$wdi.trade.1[datsub$sftgcode=="LIB"] <- 95
datsub$wdi.trade.1[datsub$sftgcode=="MAG"] <- 82
datsub$wdi.trade.1[datsub$sftgcode=="MLI"] <- 62
datsub$wdi.trade.1[datsub$sftgcode=="MNG"] <- 44
datsub$wdi.trade.1[datsub$sftgcode=="NAM"] <- 61
datsub$wdi.trade.1[datsub$sftgcode=="NIR"] <- 30
datsub$wdi.trade.1[datsub$sftgcode=="PRK"] <- 15
datsub$wdi.trade.1[datsub$sftgcode=="OMA"] <- 94
datsub$wdi.trade.1[datsub$sftgcode=="QAT"] <- 78
datsub$wdi.trade.1[datsub$sftgcode=="RWA"] <- 41
datsub$wdi.trade.1[datsub$sftgcode=="SRB"] <- 39
datsub$wdi.trade.1[datsub$sftgcode=="SOM"] <- 31
datsub$wdi.trade.1[datsub$sftgcode=="SSD"] <- 
datsub$wdi.trade.1[datsub$sftgcode=="TAW"] <- 67 
datsub$wdi.trade.1[datsub$sftgcode=="ETM"] <- 7
datsub$wdi.trade.1[datsub$sftgcode=="TOG"] <- 103
datsub$wdi.trade.1[datsub$sftgcode=="TRI"] <- 103
datsub$wdi.trade.1[datsub$sftgcode=="USA"] <- 25
datsub$wdi.trade.1[datsub$sftgcode=="VIE"] <- 65
datsub$wdi.trade.1[datsub$sftgcode=="YEM"] <- 29  
datsub$wdi.trade.1[datsub$sftgcode=="MYA"] <- 18
datsub$wdi.trade.1[datsub$sftgcode=="GAB"] <- 95
datsub$wdi.trade.1[datsub$sftgcode=="IVO"] <- 85

# Infant mortality
# Source: CIA World Factbook
datsub$xxxcimr.1[datsub$sftgcode=="USA"] <- 0.27  # 6/1,000, same as Croatia's
datsub$xxxcimr.1[datsub$sftgcode=="MNG"] <- 0.3  # Using SRB's bc no sep est
datsub$xxxcimr.1[datsub$sftgcode=="SSD"] <- 3.36  # 72/1,000, ~same as Liberia's

# Recent coup activity
# Source: Center for Systemic Peace http://www.systemicpeace.org/inscr/CSPCoupsCodebook2011.pdf
datsub$cou.tries5d <- ifelse(datsub$sftgcode=="CHA" | datsub$sftgcode=="ETM" | datsub$sftgcode=="SUD" | datsub$sftgcode=="MAA" | datsub$sftgcode=="GNB" |
                             datsub$sftgcode=="GUI" | datsub$sftgcode=="EQG" | datsub$sftgcode=="LES" | datsub$sftgcode=="GRG" | datsub$sftgcode=="NIR" |
                             datsub$sftgcode=="ECU" | datsub$sftgcode=="MAG" | datsub$sftgcode=="ZAI" |
                             datsub$sftgcode=="MLI",
                             1, 0 )

### POLITY UPDATES ###

# Update timer across board before adjusting by case
datsub$pol.durable.1 <- ifelse(datsub$polcat.1==7, 0, datsub$pol.durable.1 + 2)

#Bahrain
datsub$pol.durable.1[datsub$sftgcode=="BAH"] <- 1

#Germany [missing in original, prob bc of code-matching problem]
datsub$pol.durable.1[datsub$sftgcode=="GER"] <- 22
datsub$polcat.1[datsub$sftgcode=="GER"] <- 3
datsub$autocracy.1[datsub$sftgcode=="GER"] <- 0
datsub$xconccnc.1[datsub$sftgcode=="GER"] <- 1
datsub$pitfcat.1[datsub$sftgcode=="GER"] <- "D/F"

# Ethiopia: just wrong
datsub$pol.durable.1[datsub$sftgcode=="ETH"] <- 17
datsub$polcat.1[datsub$sftgcode=="ETH"] <- 2
datsub$autocracy.1[datsub$sftgcode=="ETH"] <- 1
datsub$xconccnc.1[datsub$sftgcode=="ETH"] <- 1
datsub$pitfcat.1[datsub$sftgcode=="ETH"] <- "A/P"

# Georgia: no change bc Polity had as partial dem since 1992 [!]

# Ivory Coast: change in 2011 per Polity
datsub$pol.durable.1[datsub$sftgcode=="IVO"] <- 1
datsub$polcat.1[datsub$sftgcode=="IVO"] <- 2
datsub$autocracy.1[datsub$sftgcode=="IVO"] <- 0
datsub$xconccnc.1[datsub$sftgcode=="IVO"] <- 1
datsub$pitfcat.1[datsub$sftgcode=="IVO"] <- "D/fact"

# Libya: collapse in 2011, still there in 2012
datsub$pol.durable.1[datsub$sftgcode=="LIB"] <- 0
datsub$polcat.1[datsub$sftgcode=="LIB"] <- 7
datsub$autocracy.1[datsub$sftgcode=="LIB"] <- 0
datsub$xconccnc.1[datsub$sftgcode=="LIB"] <- 0
datsub$pitfcat.1[datsub$sftgcode=="LIB"] <- "other"

# Yemen: collapse in 2011, still there in 2012
datsub$pol.durable.1[datsub$sftgcode=="YEM"] <- 0
datsub$polcat.1[datsub$sftgcode=="YEM"] <- 7
datsub$autocracy.1[datsub$sftgcode=="YEM"] <- 0
datsub$xconccnc.1[datsub$sftgcode=="YEM"] <- 0
datsub$pitfcat.1[datsub$sftgcode=="YEM"] <- "other"

# Mali: collapse in 2012
datsub$pol.durable.1[datsub$sftgcode=="MLI"] <- 0
datsub$polcat.1[datsub$sftgcode=="MLI"] <- 7
datsub$autocracy.1[datsub$sftgcode=="MLI"] <- 0
datsub$xconccnc.1[datsub$sftgcode=="MLI"] <- 0
datsub$pitfcat.1[datsub$sftgcode=="MLI"] <- "other"

# Guinea-Bissau: collapse in 2012
datsub$pol.durable.1[datsub$sftgcode=="GNB"] <- 0
datsub$polcat.1[datsub$sftgcode=="GNB"] <- 7
datsub$autocracy.1[datsub$sftgcode=="GNB"] <- 0
datsub$xconccnc.1[datsub$sftgcode=="GNB"] <- 0
datsub$pitfcat.1[datsub$sftgcode=="GNB"] <- "other"

# Hungary: backslide in 2012
datsub$pol.durable.1[datsub$sftgcode=="HUN"] <- 0
datsub$polcat.1[datsub$sftgcode=="HUN"] <- 2
datsub$autocracy.1[datsub$sftgcode=="HUN"] <- 1
datsub$xconccnc.1[datsub$sftgcode=="HUN"] <- 1
datsub$pitfcat.1[datsub$sftgcode=="HUN"] <- "A/P"

# Tunisia: democratization in 2012
datsub$pol.durable.1[datsub$sftgcode=="TUN"] <- 0
datsub$polcat.1[datsub$sftgcode=="TUN"] <- 2
datsub$autocracy.1[datsub$sftgcode=="TUN"] <- 0
datsub$xconccnc.1[datsub$sftgcode=="TUN"] <- 1
datsub$pitfcat.1[datsub$sftgcode=="TUN"] <- "D/fact"

# Egypt: transitional in 2012
datsub$pol.durable.1[datsub$sftgcode=="EGY"] <- 0
datsub$polcat.1[datsub$sftgcode=="EGY"] <- 7
datsub$autocracy.1[datsub$sftgcode=="EGY"] <- 0
datsub$xconccnc.1[datsub$sftgcode=="EGY"] <- 0
datsub$pitfcat.1[datsub$sftgcode=="EGY"] <- "other"

# Haiti: uncollapse in 2012
datsub$pol.durable.1[datsub$sftgcode=="HAI"] <- 0
datsub$polcat.1[datsub$sftgcode=="HAI"] <- 2
datsub$autocracy.1[datsub$sftgcode=="HAI"] <- 0
datsub$xconccnc.1[datsub$sftgcode=="HAI"] <- 1
datsub$pitfcat.1[datsub$sftgcode=="HAI"] <- "D/P"

# Honduras: wrong after 2009 coup
datsub$pol.durable.1[datsub$sftgcode=="HON"] <- 3
datsub$polcat.1[datsub$sftgcode=="HON"] <- 2
datsub$autocracy.1[datsub$sftgcode=="HON"] <- 1
datsub$xconccnc.1[datsub$sftgcode=="HON"] <- 1
datsub$pitfcat.1[datsub$sftgcode=="HON"] <- "A/P"

# Madagascar: changed in 2011
datsub$pol.durable.1[datsub$sftgcode=="MAG"] <- 1
datsub$polcat.1[datsub$sftgcode=="MAG"] <- 2
datsub$autocracy.1[datsub$sftgcode=="MAG"] <- 1
datsub$xconccnc.1[datsub$sftgcode=="MAG"] <- 1
datsub$pitfcat.1[datsub$sftgcode=="MAG"] <- "A/P"

# Montenegro: fix missing
datsub$pol.durable.1[datsub$sftgcode=="MNG"] <- 6
datsub$polcat.1[datsub$sftgcode=="MNG"] <- 3
datsub$autocracy.1[datsub$sftgcode=="MNG"] <- 0
datsub$xconccnc.1[datsub$sftgcode=="MNG"] <- 1
datsub$pitfcat.1[datsub$sftgcode=="MNG"] <- "D/F"

# Myanmar: change in 2011
datsub$pol.durable.1[datsub$sftgcode=="MYA"] <- 1
datsub$polcat.1[datsub$sftgcode=="MYA"] <- 2
datsub$autocracy.1[datsub$sftgcode=="MYA"] <- 1
datsub$xconccnc.1[datsub$sftgcode=="MYA"] <- 1
datsub$pitfcat.1[datsub$sftgcode=="MYA"] <- "A/P"

# Niger: democratization in 2011
datsub$pol.durable.1[datsub$sftgcode=="NIR"] <- 1
datsub$polcat.1[datsub$sftgcode=="NIR"] <- 3
datsub$autocracy.1[datsub$sftgcode=="NIR"] <- 0
datsub$xconccnc.1[datsub$sftgcode=="NIR"] <- 1
datsub$pitfcat.1[datsub$sftgcode=="NIR"] <- "D/P"

# Nicaragua: just wrong
datsub$pol.durable.1[datsub$sftgcode=="NIC"] <- 1
datsub$polcat.1[datsub$sftgcode=="NIC"] <- 2
datsub$autocracy.1[datsub$sftgcode=="NIC"] <- 1
datsub$xconccnc.1[datsub$sftgcode=="NIC"] <- 1
datsub$pitfcat.1[datsub$sftgcode=="NIC"] <- "A/P"

# Serbia: fix missing
datsub$pol.durable.1[datsub$sftgcode=="SRB"] <- 6
datsub$polcat.1[datsub$sftgcode=="SRB"] <- 3
datsub$autocracy.1[datsub$sftgcode=="SRB"] <- 0
datsub$xconccnc.1[datsub$sftgcode=="SRB"] <- 1
datsub$pitfcat.1[datsub$sftgcode=="SRB"] <- "D/P"

# Russia: just wrong
datsub$pol.durable.1[datsub$sftgcode=="RUS"] <- 12
datsub$polcat.1[datsub$sftgcode=="RUS"] <- 2
datsub$autocracy.1[datsub$sftgcode=="RUS"] <- 1
datsub$xconccnc.1[datsub$sftgcode=="RUS"] <- 0
datsub$pitfcat.1[datsub$sftgcode=="RUS"] <- "A/P"

# Sri Lanka: backsliding in 2011
datsub$pol.durable.1[datsub$sftgcode=="SRI"] <- 1
datsub$polcat.1[datsub$sftgcode=="SRI"] <- 2
datsub$autocracy.1[datsub$sftgcode=="SRI"] <- 1
datsub$xconccnc.1[datsub$sftgcode=="SRI"] <- 1
datsub$pitfcat.1[datsub$sftgcode=="SRI"] <- "A/P"

# South Sudan: fix missing
datsub$pol.durable.1[datsub$sftgcode=="SSD"] <- 1
datsub$polcat.1[datsub$sftgcode=="SSD"] <- 2
datsub$autocracy.1[datsub$sftgcode=="SSD"] <- 1
datsub$xconccnc.1[datsub$sftgcode=="SSD"] <- 1
datsub$pitfcat.1[datsub$sftgcode=="SSD"] <- "A/P"

# Syria: collapse in 2012
datsub$pol.durable.1[datsub$sftgcode=="SYR"] <- 0
datsub$polcat.1[datsub$sftgcode=="SYR"] <- 7
datsub$autocracy.1[datsub$sftgcode=="SYR"] <- 0
datsub$xconccnc.1[datsub$sftgcode=="SYR"] <- 0
datsub$pitfcat.1[datsub$sftgcode=="SYR"] <- "other"

# Thailand: change in 2011
datsub$pol.durable.1[datsub$sftgcode=="THI"] <- 1
datsub$polcat.1[datsub$sftgcode=="THI"] <- 3
datsub$autocracy.1[datsub$sftgcode=="THI"] <- 0
datsub$xconccnc.1[datsub$sftgcode=="THI"] <- 0
datsub$pitfcat.1[datsub$sftgcode=="THI"] <- "D/fact"

# Turkey: exit from factionalism in 2011 per Polity
datsub$pitfcat.1[datsub$sftgcode=="TUR"] <- "D/P"

# U.K.: fix missing
datsub$pol.durable.1[datsub$sftgcode=="UK"] <- 132
datsub$polcat.1[datsub$sftgcode=="UK"] <- 3
datsub$autocracy.1[datsub$sftgcode=="UK"] <- 0
datsub$xconccnc.1[datsub$sftgcode=="UK"] <- 1
datsub$pitfcat.1[datsub$sftgcode=="UK"] <- "D/F"

# Ukraine: backslide in 2012
datsub$pol.durable.1[datsub$sftgcode=="UKR"] <- 0
datsub$polcat.1[datsub$sftgcode=="UKR"] <- 2
datsub$autocracy.1[datsub$sftgcode=="UKR"] <- 1
datsub$xconccnc.1[datsub$sftgcode=="UKR"] <- 1
datsub$pitfcat.1[datsub$sftgcode=="UKR"] <- "A/P"

# Zimbabwe: just wrong; no signif change in 2009
datsub$pol.durable.1[datsub$sftgcode=="ZIM"] <- 13
datsub$polcat.1[datsub$sftgcode=="ZIM"] <- 2
datsub$autocracy.1[datsub$sftgcode=="ZIM"] <- 1
datsub$xconccnc.1[datsub$sftgcode=="ZIM"] <- 1
datsub$pitfcat.1[datsub$sftgcode=="ZIM"] <- "A/P"

### DISCRIMINATION, ETHNICITY, AND IDEOLOGY UPDATES
datsub$dispota4.1[datsub$sftgcode=="BAH"] <- 1  # Active repression of Shiites
datsub$elceliti.1[datsub$sftgcode=="EGY"] <- 0  # End of anti-Islamist
datsub$dispota4.1[datsub$sftgcode=="ETH"] <- 1  # Active repression of Oromo
datsub$elcelethc.1[datsub$sftgcode=="GRG"] <- 0  # Correcting error
datsub$elceliti.1[datsub$sftgcode=="GER"] <- 0  # Filling in missing
datsub$elcelethc.1[datsub$sftgcode=="GER"] <- 0  # Filling in missing
datsub$elceliti.1[datsub$sftgcode=="LIB"] <- 0  # End of anti-Islamist
datsub$elceliti.1[datsub$sftgcode=="MNG"] <- 0  # Filling in missing
datsub$elcelethc.1[datsub$sftgcode=="MNG"] <- 0  # Filling in missing
datsub$elceliti.1[datsub$sftgcode=="SRB"] <- 0  # Filling in missing
datsub$elcelethc.1[datsub$sftgcode=="SRB"] <- 1  # Filling in missing
datsub$dispota4.1[datsub$sftgcode=="SSD"] <- 0  # Filling in missing
datsub$elcelethc.1[datsub$sftgcode=="SSD"] <- 0  # Filling in missing
datsub$elceliti.1[datsub$sftgcode=="SSD"] <- 0  # Filling in missing
datsub$elceliti.1[datsub$sftgcode=="SRI"] <- 0  # End of anti-Tamil by defeat
datsub$dispota4.1[datsub$sftgcode=="SYR"] <- 1  # Active repression of non-Alawites
datsub$elceliti.1[datsub$sftgcode=="TUN"] <- 0  # End of anti-Islamist
datsub$elceliti.1[datsub$sftgcode=="UK"] <- 0  # Filling in missing
datsub$elcelethc.1[datsub$sftgcode=="UK"] <- 0  # Filling in missing
datsub$elceliti.1[datsub$sftgcode=="USA"] <- 0  # Filling in missing
datsub$elcelethc.1[datsub$sftgcode=="USA"] <- 0  # Filling in missing

### CONFLICT INDICATORS ###
datsub$civconc.1[datsub$sftgcode=="AFG"] <- 1  # Gotta call this a civil conflict, too
# Bahrain: not enough deaths (yet)
# Burundi: not enough deaths (yet)
datsub$civconc.1[datsub$sftgcode=="CHA"] <- 0  # Seems to have ended (for now)
datsub$civconc.1[datsub$sftgcode=="IRQ"] <- 1  # Many hundreds killed since U.S. withdrawal
datsub$civconc.1[datsub$sftgcode=="IVO"] <- 1  # Attacks in 2012 indicate conflict still ongoing
datsub$civconc.1[datsub$sftgcode=="KEN"] <- 1  # May not quite be over 500-death line, but awfully close
datsub$civconc.1[datsub$sftgcode=="KYR"] <- 0  # Nothing ongoing at this level
# Lebanon: not lethal enough yet http://en.wikipedia.org/wiki/2011%E2%80%93present_conflict_in_Lebanon#Deaths_and_injuries
datsub$civconc.1[datsub$sftgcode=="LIB"] <- 1  # Civil war & its aftermath
datsub$civconc.1[datsub$sftgcode=="MLI"] <- 1  # Civil war & aftermath
datsub$civconc.1[datsub$sftgcode=="MNG"] <- 0  # Fill in missing
datsub$civconc.1[datsub$sftgcode=="SSD"] <- 1  # Repeated clashes between tribal militias kill several hundreds since 2011
datsub$civconc.1[datsub$sftgcode=="SYR"] <- 1  # Ongoing civil war\
# Tajikistan: not violent enough
datsub$civconc.1[datsub$sftgcode=="USA"] <- 0  # Fill in missing

### FIX UPHEAVAL

uhvl <- read.csv('C:/Documents and Settings/Jay/My Documents/jay data/sftpuhvl.csv', header=T)
uhvl$SFTGNAME <- NULL
names(uhvl) <- c('sftgcode', 'year', 'sftpuhvl.1')
uhvl2011 <- subset(uhvl, year==2011, select=c('sftgcode', 'sftpuhvl.1'))
rownames(datsub) <- rownames(uhvl2011) <- NULL
datsub$sftgcode <- as.character(datsub$sftgcode)
uhvl2011$sftgcode <- as.character(substr(uhvl2011$sftgcode,1,3))
datsub <- merge(datsub, uhvl2011, by="sftgcode", all=T)
datsub$sftpuhvl.1[datsub$sftgcode=="ALG"] <- 19
datsub$sftpuhvl.1[datsub$sftgcode=="ANG"] <- 19
datsub$sftpuhvl.1[datsub$sftgcode=="AZE"] <- 0
datsub$sftpuhvl.1[datsub$sftgcode=="BUI"] <- 27
datsub$sftpuhvl.1[datsub$sftgcode=="CAR"] <- 22
datsub$sftpuhvl.1[datsub$sftgcode=="CHN"] <- 2
datsub$sftpuhvl.1[datsub$sftgcode=="COM"] <- 1
datsub$sftpuhvl.1[datsub$sftgcode=="EGY"] <- 4
datsub$sftpuhvl.1[datsub$sftgcode=="ETI"] <- 16
datsub$sftpuhvl.1[datsub$sftgcode=="GNB"] <- 13
datsub$sftpuhvl.1[datsub$sftgcode=="IRQ"] <- 44
datsub$sftpuhvl.1[datsub$sftgcode=="IVO"] <- 36
datsub$sftpuhvl.1[datsub$sftgcode=="LBR"] <- 4
datsub$sftpuhvl.1[datsub$sftgcode=="LIB"] <- 8
datsub$sftpuhvl.1[datsub$sftgcode=="MEX"] <- 20
datsub$sftpuhvl.1[datsub$sftgcode=="MLI"] <- 4
datsub$sftpuhvl.1[datsub$sftgcode=="MYA"] <- 35
datsub$sftpuhvl.1[datsub$sftgcode=="NEP"] <- 33
datsub$sftpuhvl.1[datsub$sftgcode=="NIG"] <- 14
datsub$sftpuhvl.1[datsub$sftgcode=="PAK"] <- 29
datsub$sftpuhvl.1[datsub$sftgcode=="PER"] <- 0
datsub$sftpuhvl.1[datsub$sftgcode=="PNG"] <- 1
datsub$sftpuhvl.1[datsub$sftgcode=="RUS"] <- 28
datsub$sftpuhvl.1[datsub$sftgcode=="RWA"] <- 6
datsub$sftpuhvl.1[datsub$sftgcode=="SEN"] <- 4
datsub$sftpuhvl.1[datsub$sftgcode=="SIE"] <- 17
datsub$sftpuhvl.1[datsub$sftgcode=="SRI"] <- 38
datsub$sftpuhvl.1[datsub$sftgcode=="SUD"] <- 53
datsub$sftpuhvl.1[datsub$sftgcode=="SYR"] <- 8
datsub$sftpuhvl.1[datsub$sftgcode=="TAJ"] <- 3
datsub$sftpuhvl.1[datsub$sftgcode=="THI"] <- 20
datsub$sftpuhvl.1[datsub$sftgcode=="TUR"] <- 34
datsub$sftpuhvl.1[datsub$sftgcode=="UGA"] <- 18
datsub$sftpuhvl.1[datsub$sftgcode=="YEM"] <- 20
datsub$sftpuhvl.1[datsub$sftgcode=="ZAI"] <- 53
datsub$sftpuhvl.1[datsub$sftgcode=="USA"] <- 0
datsub$sftpuhvl.1[datsub$sftgcode=="UK"] <- 0
datsub$sftpuhvl.1[datsub$sftgcode=="MNG"] <- 0

# Update regional conflict measure by hand (best guesses)
datsub$mac.regac.1[datsub$sftgcode=="USA"] <- 2  # Mexico
datsub$mac.regac.1[datsub$sftgcode=="SSD"] <- 16 # Same as Sudan
datsub$mac.regac.1[datsub$sftgcode=="MNG"] <- 5  # Same as Serbia
datsub$mac.nreg.1[datsub$sftgcode=="USA"] <- 26  # Same as Mexico
datsub$mac.nreg.1[datsub$sftgcode=="SSD"] <- 15  # Same as Sudan
datsub$mac.nreg.1[datsub$sftgcode=="MNG"] <- 40  # Same as Serbia

datsub$mac.regac.1 <- ifelse(datsub$sftgcode=="BEN" | datsub$sftgcode=="BFO" | datsub$sftgcode=="CAO" | datsub$sftgcode=="CEN" | datsub$sftgcode=="CHA" |
                             datsub$sftgcode=="CON" | datsub$sftgcode=="EQG" | datsub$sftgcode=="GAB" | datsub$sftgcode=="GAM" | datsub$sftgcode=="GHA" |
                             datsub$sftgcode=="GNB" | datsub$sftgcode=="GUI" | datsub$sftgcode=="IVO" | datsub$sftgcode=="LBR" | datsub$sftgcode=="MAA" |
                             datsub$sftgcode=="MLI" | datsub$sftgcode=="NIG" | datsub$sftgcode=="NIR" | datsub$sftgcode=="SEN" | datsub$sftgcode=="SIE" |
                             datsub$sftgcode=="TOG" | datsub$sftgcode=="ZAI" | datsub$sftgcode=="BEN",
                             datsub$mac.regac.1 + 5, datsub$mac.regac.1)
datsub$mac.regac.1 <- ifelse(datsub$sftgcode=="ALG" | datsub$sftgcode=="DJI" | datsub$sftgcode=="EGY" | datsub$sftgcode=="ERI" | datsub$sftgcode=="ETI" |
                             datsub$sftgcode=="LIB" | datsub$sftgcode=="MOR" | datsub$sftgcode=="TUN",
                             datsub$mac.regac.1 + 3, datsub$mac.regac.1)
datsub$mac.regac.1 <- ifelse(datsub$sftgcode=="BUI" | datsub$sftgcode=="KEN" | datsub$sftgcode=="RWA" | datsub$sftgcode=="TAZ" | datsub$sftgcode=="UGA" |
                             datsub$sftgcode=="SUD" | datsub$sftgcode=="SSD",
                             datsub$mac.regac.1 + 3, datsub$mac.regac.1)
datsub$mac.regac.1 <- ifelse(datsub$sftgcode=="ARM" | datsub$sftgcode=="AZE" | datsub$sftgcode=="BAH" | datsub$sftgcode=="CYP" | datsub$sftgcode=="GRG" |
                             datsub$sftgcode=="IRQ" | datsub$sftgcode=="ISR" | datsub$sftgcode=="JOR" | datsub$sftgcode=="KUW" | datsub$sftgcode=="LEB" |
                             datsub$sftgcode=="OMA" | datsub$sftgcode=="QAT" | datsub$sftgcode=="SAU" | datsub$sftgcode=="SYR" | datsub$sftgcode=="TUR" |
                             datsub$sftgcode=="UAE" | datsub$sftgcode=="YEM",     
                             datsub$mac.regac.1 + 2, datsub$mac.regac.1)
datsub$mac.regac.1 <- ifelse(datsub$sftgcode=="CAM" | datsub$sftgcode=="CHN" | datsub$sftgcode=="ETM" | datsub$sftgcode=="INS" | datsub$sftgcode=="JPN" |
                             datsub$sftgcode=="LAO" | datsub$sftgcode=="MAL" | datsub$sftgcode=="MON" | datsub$sftgcode=="MYA" | datsub$sftgcode=="PHI" |
                             datsub$sftgcode=="PNG" | datsub$sftgcode=="PRK" | datsub$sftgcode=="ROK" | datsub$sftgcode=="SIN" | datsub$sftgcode=="TAW" |
                             datsub$sftgcode=="THI" | datsub$sftgcode=="VIE",
                             datsub$mac.regac.1 + 1, datsub$mac.regac.1)

datsub$regcon.1 <- NULL
datsub$regcon.1 <- ifelse(datsub$mac.nreg.1==0, datsub$mac.regac.1, datsub$mac.regac.1/datsub$mac.nreg.1)

# Ethnic fractionalization missing
datsub$elfcat.1[datsub$sftgcode=="SSD"] <- 2  # Best guess; can't locate authoritative source 

# PITF Problem Set, per http://www.systemicpeace.org/inscr/PITF%20Consolidated%20Case%20List2012.pdf
pitfsub <- subset(dat, year == 2012,
     select=c(sftgcode, pitf.on, pitf.dur.min, ewar.dur, rwar.dur, areg.dur))
names(pitfsub) <- c("sftgcode", "pitf.on.1", "pitf.dur.min.1", "ewar.dur.1", "rwar.dur.1", "areg.dur.1")
datsub <- merge(datsub, pitfsub)

# Change PITF's Israel ethnic war coding to capture shift to interstate after Oslo Accords 
datsub$pitf.dur.min.1[datsub$sftgcode=="ISR"] <- 0
datsub$pitf.on.1[datsub$sftgcode=="ISR"] <- 0
datsub$ewar.dur.1[datsub$sftgcode=="ISR"] <- 0

# Ongoing mass killings
datsub$mkl.ongoing.1 <- ifelse(datsub$sftgcode=="SUD" |
                               datsub$sftgcode=="MYA" |
                               datsub$sftgcode=="SYR" |
                               datsub$sftgcode=="ZAI" |
                               datsub$sftgcode=="PRK",
                               1, 0)

# Country age
datsub$ageln.1 <- log(exp(datsub$ageln.1) + 1)

# WTO membership
datsub$io.wto.1[datsub$sftgcode=="MNG"] <- 1
datsub$io.wto.1[datsub$sftgcode=="RUS"] <- 1
datsub$io.wto.1[datsub$sftgcode=="SRB"] <- 0
datsub$io.wto.1[datsub$sftgcode=="SSD"] <- 0
datsub$io.wto.1[datsub$sftgcode=="USA"] <- 1

# Polity duration under other label for RF
datsub$poldurln.1 <- NULL
datsub$poldurln.1 <- log1p(datsub$pol.durable.1)

#####################################
# Generate Forecasts
#####################################

datsub$cwar.p <- predict(cwar, newdata = datsub, type = "response")
datsub$coup.p <- predict(coup, newdata = datsub, type = "response")
datsub$threat.p <- predict(threat, newdata = datsub, type = "response")
datsub$cnc.p <- predict(cnc, newdata = datsub, type = "response")
datsub$pitf.p <- predict(pitf, newdata = datsub, type = "response")
datsub$pitf.p.2 <- ifelse(datsub$pitf.on.1==1, 1, datsub$pitf.p)
datsub$harff.cond.p <- predict(harff, newdata = datsub, type = "response")
datsub$harff.p <- datsub$pitf.p.2 * datsub$harff.cond.p
library(randomForest)
datsub$rf.p <- predict(rf, newdata = datsub, type = "prob", na.action = "na.exclude")[,2]
datsub$mean.p <- (datsub$threat.p + datsub$cnc.p + datsub$harff.p + datsub$rf.p)/4

datsub <- datsub[order(-datsub$mean.p),] 

# Write out file as .csv for future use
write.csv(datsub, "cpg.stat.risk.2013.csv", quote = FALSE, row.names = FALSE)

#########################################
# Charts and Maps
#########################################

# Dot plot of Top 30 for 2013
library(Hmisc)
datsub <- datsub[order(-datsub$mean.p),]
png(file="cpg.stat.risk.2013.top30.png",
     width=350, height=600, bg='white')
par(mai=c(0.5,0.25,0.25,0.25))
dotchart2(datsub$threat.p[1:30], labels=datsub$country[1:30],
     sort = FALSE, lines = TRUE, lwd=0.5, lty=1, lcolor = "gray", dotsize=1, pch=16, col="gray",
     cex.labels=1, xlim=c(0,0.25), xaxis = FALSE)
box(col="white")
axis(1, col="gray", tick = FALSE)
dotchart2(datsub$cnc.p[1:30], sort=FALSE, lines=FALSE, dotsize=1, pch=16, col="gray",
     cex.labels=0.75, xlim=c(0,0.25), add=TRUE )
dotchart2(datsub$harff.p[1:30], sort=FALSE, lines=FALSE, dotsize=1, pch=16, col="gray",
     cex.labels=0.75, xlim=c(0,0.25), add=TRUE )
dotchart2(datsub$rf.p[1:30], sort=FALSE, lines=FALSE, dotsize=1, pch=16, col="gray",
     cex.labels=0.75, xlim=c(0,0.25), add=TRUE )
dotchart2(datsub$mean.p[1:30], sort=FALSE, lines=FALSE, dotsize=1, pch=16, col="red",
     cex.labels=0.75, xlim=c(0,0.25), add=TRUE )
dev.off()

# HEAT MAP

# Load required package.
library(rworldmap)

# Prepare country names to match ones used by mapping package
datsub$country <- as.character(datsub$country)
datsub$country <- replace(datsub$country, datsub$country=="Ivory Coast", "Cote d'Ivoire")
datsub$country <- replace(datsub$country, datsub$country=="Congo-Brazzaville", "Congo")
datsub$country <- replace(datsub$country, datsub$country=="Congo-Kinshasa", "Democratic Republic of the Congo")
datsub$country <- replace(datsub$country, datsub$country=="Congo-Brazzaville", "Congo")
datsub$country <- replace(datsub$country, datsub$country=="Iran", "Iran (Islamic Republic of)")
datsub$country <- replace(datsub$country, datsub$country=="Macedonia", "The former Yugoslav Republic of Macedonia")
datsub$country <- replace(datsub$country, datsub$country=="Laos", "Lao People's Democratic Republic")
datsub$country <- replace(datsub$country, datsub$country=="Moldova", "Republic of Moldova")
datsub$country <- replace(datsub$country, datsub$country=="Vietnam", "Viet Nam")
datsub$country <- replace(datsub$country, datsub$country=="Syria", "Syrian Arab Republic")
datsub$country <- replace(datsub$country, datsub$country=="Tanzania", "United Republic of Tanzania")
datsub$country <- replace(datsub$country, datsub$country=="North Korea", "Korea, Democratic People's Republic of")
datsub$country <- replace(datsub$country, datsub$country=="South Korea", "Korea, Republic of")
datsub$country <- replace(datsub$country, datsub$country=="Timor Leste", "Timor-Leste")
datsub$country <- replace(datsub$country, datsub$country=="Myanmar", "Burma")

# Create 5-level cat var based on predicted probability
datsub$status <- NULL
datsub$status[datsub$mean.p < 0.01] <- 0
datsub$status[datsub$mean.p >= 0.01 & datsub$mean.p < 0.02] <- 1
datsub$status[datsub$mean.p >= 0.02 & datsub$mean.p < 0.03] <- 2
datsub$status[datsub$mean.p >= 0.03 & datsub$mean.p < 0.04] <- 3
datsub$status[datsub$mean.p >= 0.04 & datsub$mean.p < 0.05] <- 4
datsub$status[datsub$mean.p >= 0.05] <- 5
datsub$status[is.na(datsub$mean.p)==T] <- NA
datsub$status[datsub$sftgcode=="SYR"] <- 9
datsub$status[datsub$sftgcode=="SUD"] <- 9
datsub$status[datsub$sftgcode=="MYA"] <- 9
datsub$status[datsub$sftgcode=="PRK"] <- 9
datsub$status[datsub$sftgcode=="ZAI"] <- 9
datsub$status <- factor(datsub$status, levels=c(9,5,4,3,2,1,0),
     labels=c("ongoing", "5%-", "4-5%", "3-4%", "2-3%", "1-2%", "<1%"))

# Make a map
png(file="cpg.stat.risk.2013.map.png",
    width=6.75, height=3.25, units='in', bg='white', res=150 )
par(mai = c(0, 0, 0, 0), xaxs = "i", yaxs = "i")
mapCountryData(joinCountryData2Map(datsub, joinCode = "NAME", nameJoinColumn = "country"),
               nameColumnToPlot="status", numCats = 6, catMethod="categorical",
               colourPalette=c("red2", "gray20", "gray35", "gray50", "gray65", "gray80", "gray95"), oceanCol = "azure2",
               addLegend = "FALSE",
               mapTitle = NULL )
mtext("mapped with rworldmap", line=-1, side=1, adj=1, cex=0.6)
dev.off()
