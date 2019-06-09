library(lubridate)
library(dplyr)

yahoo <- read.csv("C:/Users/scotg/Downloads/transactions (4).csv")

yahoo$Date	               <- mdy(yahoo$Date) %>% as.Date()
yahoo$Original.Description <- yahoo$Original.Description %>% as.character() 
yahoo$Description          <- yahoo$Description %>% as.character()
yahoo$Category             <- yahoo$Category %>% as.character()
 
what.are.you <- strsplit(yahoo$Original.Description,"\\s+")

df <- data.frame(matrix(unlist(what.are.you),               #unwind the list of strings split by spaces
                        nrow=length(unlist(what.are.you)),  #how many strings?
                        byrow=1),
                        stringsAsFactors=FALSE) %>%         #to character not factor
                        table() %>%                         #count frequency of string
                        as.data.frame()                     #you know what it is

df <- df[order(df$Freq,decreasing = TRUE) & df$Freq >= 3,]

what.are.you1 <- strsplit(yahoo$Description,"\\s+")

df1 <- data.frame(matrix(unlist(what.are.you1),               #unwind the list of strings split by spaces
                        nrow=length(unlist(what.are.you1)),  #how many strings?
                        byrow=1),
                 stringsAsFactors=FALSE) %>%         #to character not factor
  table() %>%                         #count frequency of string
  as.data.frame()                     #you know what it is

df1 <- df1[order(df1$Freq,decreasing = TRUE) & df1$Freq >= 3,]

df2 <- rbind(df,df1)
df2 <- df2[order(df2$Freq,decreasing = TRUE),]

j <- 1
for(j in 1:length(df2$.)){
  dummy.str <- df2$.[[j]] %>% as.character()
  dummy.dta1 <- yahoo[grep(dummy.str,yahoo$Description),]
  dummy.dta2 <- yahoo[grep(dummy.str,yahoo$Original.Description),]
  dummy.dta  <- rbind(dummy.dta1,dummy.dta2)
  dummy.df <- table(dummy.dta$Category) %>% as.data.frame()
  dummy.df <- dummy.df[order(dummy.df$Freq,decreasing = TRUE),]
}


yahoo$year.trans <- year(yahoo$Date)
yahoo$month.trans <- month(yahoo$Date)
yahoo$date.char <- Sys.Date()
year(yahoo$date.char) <- yahoo$year.trans
month(yahoo$date.char) <- yahoo$month.trans
yahoo <- yahoo[yahoo$year.trans == 2019,]

credits <- yahoo[casefold(yahoo$Transaction.Type,upper = FALSE) == 'credit',]

cred.map <- read.csv("C:/Users/scotg/OneDrive/Documents/Cred.Map.v1.csv")
cred.map$Glas.Account.Map <- cred.map$Glas.Account.Map %>% as.character()
cred.map[is.na(cred.map)] <- 'No'
names(cred.map)[1] <- 'Account.Name'
credits <- merge(credits,cred.map, by = 'Account.Name')
cred.summ <- credits %>% group_by(Glas.Account.Map,Category,date.char) %>% summarise(Inflows = sum(Amount))
cred.summ <- cred.summ[year(cred.summ$date.char) == 2019,]

cred.summ1 <- cred.summ[,c('Glas.Account.Map','Category','date.char','Inflows')]
cred.summ2 <- tidyr::spread(cred.summ1,key = date.char,value =Inflows)
cred.summ2[is.na(cred.summ2)] <- 0
write.csv(cred.summ2,"C:/Users/scotg/OneDrive/Documents/Dollars_Through.csv")

#### STARTING CF OUTS ####

lnch.strings <- c('YUMM','ZONA','DUTCH BROS','SQ','KURE','ROASTING','FOOD CART','HOUSE OF TERIYAKI','SUBWAY','LUC LAC','potbelly','pharaoh','MARKET@WORK','EGGY POCKET','water avenue','starbucks','Duck House','coffe','JIMMY JOHNS')
healthcare.strings <- c('zoom','walgreens','kadera','robben','ohsu','Debit Card Purchase')
grocery.strings <- c('new seasons','world foods','ANNUAL ','fred meyer','costco whse','target','fred-meyer','WHOLEFDS','COSTCO BY',"TRADER JOE")
parking.strings <- c('parking','uber','lyft','mgmt co','trimet')
util.strings <- c('lowe','gardener','Portland General','tonys garden','DOLLAR SHAVE CLUB','centurylink','simplisafe','playstation','netflix','prime video','waste mgmt','nw natural gas','REGISTERWEBSITE','UTILITY')
invest.strings <- c('sumday','fidelity','INVESCO','VIX','tax')
albee.strings <- c('WAG!','WAGWALKING','PETS ',' VET ')
carstff.strings <- c('EXXONMOBIL','CHEVRON','ALL DAY TOWING','AUTO CARE','COSTCO GAS','BAXTER AUTO','FAST LANE OIL')
clothes.strings <- c('nike','ann taylor','NORDSTROM','SHIRT','ZAPPOS.COM','SEPHORA','MAIN STREET CLEAN','ETSY')
transfer.strings <- c('sheraton','epay','transfer','moneyline','citi card','auto pay','montessori','amex')
fang.strings <- c('CUPERTINO','INFINITELOOP','amzn','AMAZON.COM','ITUNES.COM','GOOGLE','AplPay','APPLE PAY')
golf.strings <- c('GIVV','coinbase','golf','heron lakes','national car','TG LEAGUE','finnegan','powell')
outtown.strings <- c('westlake','hoxton','smugglers','rialto','nice nails','mac','dosha','spa','ulta','LUCKY LABRADOR','Portland Farmer','PUNCH BOWL SOCIAL')

debits        <- yahoo[!(casefold(yahoo$Transaction.Type,upper = FALSE) == 'credit'),]
debits        <- debits[debits$year.trans == 2019,]
debits.cnt    <- nrow(debits)
debits$Row.Id <- seq.int(nrow(debits)) 

debits$Original.Description <- debits$Original.Description %>% as.character() 

#HC Exhibit

hc.costs <- list()
row.count <- 0 
for(j in healthcare.strings){
hc.costs[[j]] <- debits[grep(casefold(j,upper = FALSE),casefold(debits$Original.Description,upper = FALSE)),]
row.count <- 0 + nrow(hc.costs[[j]]) + row.count
}

for(j in healthcare.strings){
  if(j == healthcare.strings[1]){
    hc.df <- hc.costs[[j]]
  }  
  else{
    hc.df <- rbind(hc.df,hc.costs[[j]])
  }
  }
hc.df$Major.Cat <- "HealthCare"
hc.df.grp <- hc.df %>% group_by(Major.Cat,date.char) %>% summarise(Amt.Total = sum(Amount))

debits <- debits[!(debits$Row.Id %in% hc.df$Row.Id),] 

#Grocery Exhibit

grcy.costs <- list()
row.count <- 0 
for(j in grocery.strings){
  grcy.costs[[j]] <- debits[grep(casefold(j,upper=FALSE),casefold(debits$Original.Description,upper = FALSE)),]
  row.count <- 0 + nrow(grcy.costs[[j]]) + row.count
}

for(j in grocery.strings){
  if(j == grocery.strings[1]){
    grcy.df <- grcy.costs[[j]]
  }  
  else{
    grcy.df <- rbind(grcy.df,grcy.costs[[j]])
  }
}
grcy.df$Major.Cat <- "Grocery"
grcy.df.grp <- grcy.df %>% group_by(Major.Cat,date.char) %>% summarise(Amt.Total = sum(Amount))

debits <- debits[!(debits$Row.Id %in% grcy.df$Row.Id),]

#Parking Commuter Exhibit

prk.costs <- list()
row.count <- 0 
for(j in parking.strings){
  prk.costs[[j]] <- debits[grep(j,casefold(debits$Original.Description,upper = FALSE)),]
  row.count <- 0 + nrow(prk.costs[[j]]) + row.count
}

for(j in parking.strings){
  if(j == parking.strings[1]){
    prk.df <- prk.costs[[j]]
  }  
  else{
    prk.df <- rbind(prk.df,prk.costs[[j]])
  }
}
prk.df$Major.Cat <- "Parking"
prk.df.grp <- prk.df %>% group_by(Major.Cat,date.char) %>% summarise(Amt.Total = sum(Amount))

debits <- debits[!(debits$Row.Id %in% prk.df$Row.Id),]

#Utilities 

util.costs <- list()
row.count <- 0 
for(j in util.strings){
  util.costs[[j]] <- debits[grep(casefold(j,upper = FALSE),casefold(debits$Original.Description,upper = FALSE)),]
  row.count <- 0 + nrow(util.costs[[j]]) + row.count
}

for(j in util.strings){
  if(j == util.strings[1]){
    util.df <- util.costs[[j]]
  }  
  else{
    util.df <- rbind(util.df,util.costs[[j]])
  }
}
util.df$Major.Cat <- "utilities"
util.df.grp <- util.df %>% group_by(Major.Cat,date.char) %>% summarise(Amt.Total = sum(Amount))

debits <- debits[!(debits$Row.Id %in% util.df$Row.Id),]

#lunch 

lnch.costs <- list()
row.count <- 0 
for(j in lnch.strings){
  lnch.costs[[j]] <- debits[grep(casefold(j,upper=FALSE),casefold(debits$Original.Description,upper = FALSE)),]
  row.count <- 0 + nrow(lnch.costs[[j]]) + row.count
}

for(j in lnch.strings){
  if(j == lnch.strings[1]){
    lnch.df <- lnch.costs[[j]]
  }  
  else{
    lnch.df <- rbind(lnch.df,lnch.costs[[j]])
  }
}
lnch.df$Major.Cat <- "lunch"
lnch.df.grp <- lnch.df %>% group_by(Major.Cat,date.char) %>% summarise(Amt.Total = sum(Amount))

debits <- debits[!(debits$Row.Id %in% lnch.df$Row.Id),]

#investment 

invest.costs <- list()
row.count <- 0 
for(j in invest.strings){
  invest.costs[[j]] <- debits[grep(casefold(j,upper=FALSE),casefold(debits$Original.Description,upper = FALSE)),]
  row.count <- 0 + nrow(invest.costs[[j]]) + row.count
}

for(j in invest.strings){
  if(j == invest.strings[1]){
    invest.df <- invest.costs[[j]]
  }  
  else{
    invest.df <- rbind(invest.df,invest.costs[[j]])
  }
}
invest.df$Major.Cat <- "invest"
invest.df.grp <- invest.df %>% group_by(Major.Cat,date.char) %>% summarise(Amt.Total = sum(Amount))

debits <- debits[!(debits$Row.Id %in% invest.df$Row.Id),]

#car 

carstff.costs <- list()
row.count <- 0 
for(j in carstff.strings){
  carstff.costs[[j]] <- debits[grep(casefold(j,upper=FALSE),casefold(debits$Original.Description,upper = FALSE)),]
  row.count <- 0 + nrow(carstff.costs[[j]]) + row.count
}

for(j in carstff.strings){
  if(j == carstff.strings[1]){
    carstff.df <- carstff.costs[[j]]
  }  
  else{
    carstff.df <- rbind(carstff.df,carstff.costs[[j]])
  }
}
carstff.df$Major.Cat <- "car"
carstff.df.grp <- carstff.df %>% group_by(Major.Cat,date.char) %>% summarise(Amt.Total = sum(Amount))

debits <- debits[!(debits$Row.Id %in% carstff.df$Row.Id),]

#golf 

golf.costs <- list()
row.count <- 0 
for(j in golf.strings){
  golf.costs[[j]] <- debits[grep(casefold(j,upper=FALSE),casefold(debits$Original.Description,upper = FALSE)),]
  row.count <- 0 + nrow(golf.costs[[j]]) + row.count
}

for(j in golf.strings){
  if(j == golf.strings[1]){
    golf.df <- golf.costs[[j]]
  }  
  else{
    golf.df <- rbind(golf.df,golf.costs[[j]])
  }
}
golf.df$Major.Cat <- "golf"
golf.df.grp <- golf.df %>% group_by(Major.Cat,date.char) %>% summarise(Amt.Total = sum(Amount))

debits <- debits[!(debits$Row.Id %in% golf.df$Row.Id),]

#albee 

albee.costs <- list()
row.count <- 0 
for(j in albee.strings){
  albee.costs[[j]] <- debits[grep(casefold(j,upper=FALSE),casefold(debits$Original.Description,upper = FALSE)),]
  row.count <- 0 + nrow(albee.costs[[j]]) + row.count
}

for(j in albee.strings){
  if(j == albee.strings[1]){
    albee.df <- albee.costs[[j]]
  }  
  else{
    albee.df <- rbind(albee.df,albee.costs[[j]])
  }
}
albee.df$Major.Cat <- "albee"
albee.df.grp <- albee.df %>% group_by(Major.Cat,date.char) %>% summarise(Amt.Total = sum(Amount))

debits <- debits[!(debits$Row.Id %in% albee.df$Row.Id),]

#fang


fang.costs <- list()
row.count <- 0 
for(j in fang.strings){
  fang.costs[[j]] <- debits[grep(casefold(j,upper=FALSE),casefold(debits$Original.Description,upper = FALSE)),]
  row.count <- 0 + nrow(fang.costs[[j]]) + row.count
}

for(j in fang.strings){
  if(j == fang.strings[1]){
    fang.df <- fang.costs[[j]]
  }  
  else{
    fang.df <- rbind(fang.df,fang.costs[[j]])
  }
}
fang.df$Major.Cat <- "fang"
fang.df.grp <- fang.df %>% group_by(Major.Cat,date.char) %>% summarise(Amt.Total = sum(Amount))


debits <- debits[!(debits$Row.Id %in% fang.df$Row.Id),]

#transfer 

transfer.costs <- list()
row.count <- 0 
for(j in transfer.strings){
  transfer.costs[[j]] <- debits[grep(casefold(j,upper=FALSE),casefold(debits$Original.Description,upper = FALSE)),]
  row.count <- 0 + nrow(transfer.costs[[j]]) + row.count
}

for(j in transfer.strings){
  if(j == transfer.strings[1]){
    transfer.df <- transfer.costs[[j]]
  }  
  else{
    transfer.df <- rbind(transfer.df,transfer.costs[[j]])
  }
}
transfer.df$Major.Cat <- "transfer"
transfer.df.grp <- transfer.df %>% group_by(Major.Cat,date.char) %>% summarise(Amt.Total = sum(Amount))

debits <- debits[!(debits$Row.Id %in% transfer.df$Row.Id),]
#clothes 

clothes.costs <- list()
row.count <- 0 
for(j in clothes.strings){
  clothes.costs[[j]] <- debits[grep(casefold(j,upper=FALSE),casefold(debits$Original.Description,upper = FALSE)),]
  row.count <- 0 + nrow(clothes.costs[[j]]) + row.count
}

for(j in clothes.strings){
  if(j == clothes.strings[1]){
    clothes.df <- clothes.costs[[j]]
  }  
  else{
    clothes.df <- rbind(clothes.df,clothes.costs[[j]])
  }
}
clothes.df$Major.Cat <- "clothes"
clothes.df.grp <- clothes.df %>% group_by(Major.Cat,date.char) %>% summarise(Amt.Total = sum(Amount))


debits <- debits[!(debits$Row.Id %in% clothes.df$Row.Id),]

#outtown 

outtown.costs <- list()
row.count <- 0 
for(j in outtown.strings){
  outtown.costs[[j]] <- debits[grep(casefold(j,upper=FALSE),casefold(debits$Original.Description,upper = FALSE)),]
  row.count <- 0 + nrow(outtown.costs[[j]]) + row.count
}

for(j in outtown.strings){
  if(j == outtown.strings[1]){
    outtown.df <- outtown.costs[[j]]
  }  
  else{
    outtown.df <- rbind(outtown.df,outtown.costs[[j]])
  }
}
outtown.df$Major.Cat <- "outtown"
outtown.df.grp <- outtown.df %>% group_by(Major.Cat,date.char) %>% summarise(Amt.Total = sum(Amount))


debits <- debits[!(debits$Row.Id %in% outtown.df$Row.Id),]

other.df <- debits
other.df$Major.Cat <- "other"
other.df.grp <- other.df %>% group_by(Major.Cat,date.char) %>% summarise(Amt.Total = sum(Amount))



hist(debits$Amount)
debits[debits$Amount > 20,]

what.are.you <- strsplit(debits$Original.Description,"\\s+")
what.are.you <- data.frame(matrix(unlist(what.are.you),               #unwind the list of strings split by spaces
                        nrow=length(unlist(what.are.you)),  #how many strings?
                        byrow=1),
                 stringsAsFactors=FALSE) %>%         #to character not factor
  table() %>%                         #count frequency of string
  as.data.frame()                     #you know what it is
what.are.you <- what.are.you[order(what.are.you$Freq,decreasing = TRUE),]


final.summary <- rbind(albee.df.grp,
                       carstff.df.grp,
                       fang.df.grp,
                       golf.df.grp,
                       grcy.df.grp,
                       hc.df.grp,
                       invest.df.grp,
                       lnch.df.grp,
                       prk.df.grp,
                       util.df.grp,
                       other.df.grp,
                       transfer.df.grp,
                       outtown.df.grp,
                       clothes.df.grp)
  
final.summary <- tidyr::spread(final.summary,key = date.char, value = Amt.Total)  
write.csv(final.summary,"C:/Users/scotg/Downloads/final_Summary.csv")  




