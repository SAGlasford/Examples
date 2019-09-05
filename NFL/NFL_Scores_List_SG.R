rm(list=ls())

library(lubridate)
library(ggplot2)
library(smooth)
library(Mcomp)
library(zoo)


source("C:/Users/scotg/OneDrive/GitHub/Examples/NFL/fx_listMassage.R")
if(!exists("result.list")){
source("C:/Users/scotg/OneDrive/GitHub/Examples/NFL/NFL_Scores.R")
counter <- 0
result.list <- list()
for(loop.urls in all.websites){
aaa <- dummy.html.node.read(url = loop.urls)    #pulls all the tables from a URL
bbb <- NCol.Dim.Freq(node.table = aaa)          #gets counts of col table dimensions
ccc <- NCol.Dim.Id(node.table = aaa)            #gets col count (V1) by ID

for(v in 1:length(bbb$.)){

counter <- counter + 1
ccc.sub <- ccc[ccc$V1 == bbb$.[v],]

  
ddd <- Table.Data.Type.Map(node.table = aaa,column.cnt = bbb$.[v]) #type combinations
ddd <- cbind(ddd,ccc.sub)
eee <- Table.Data.Type.ID(node.table = aaa,column.cnt = bbb$.[v])     #creates ID
fff <- Table.Data.Type.Final1(aa = ddd,bb= eee) #creates ID.List 
dummy.df.list <- list()
for(j in fff$ID){ggg <- fff[fff$ID == j,c('ID.Row')] %>% as.array()
            hhh <- aaa[ggg]
            dummy.list <- list()
              for(q in 1:length(hhh)){
                dummy.spot      <- hhh[[q]] %>% html_table() %>% as.data.frame()
                dummy.list[[q]] <- dummy.spot}
            dummy.df.list[[j]] <- bind_rows(dummy.list)
}
result.list[[counter]] <- dummy.df.list
}
}
}

unit_scrape <- Col_Bind_All(result.list)
table_root_str <- "CCCC_"

row_list <- vector()
col_list <- vector()

for(xx in 1:length(unit_scrape)){
  row_list[xx] <- nrow(unit_scrape[[xx]]) %>% as.numeric()
  col_list[xx] <- ncol(unit_scrape[[xx]]) %>% as.numeric()
  out_list <- cbind(row_list,col_list) %>% as.data.frame()
}

dummy_name_list <- list()
for(zz in 1:length(unique(out_list$col_list))){
  dummy_name_specs    <- unique(out_list$col_list) %>% as.vector()
  dummy_name_list[[zz]] <- paste0(table_root_str,dummy_name_specs[zz])
}

dummy_data_list <- list()
for(zz in 1:length(unique(out_list$col_list))){
  dummy_data_list[[zz]] <- purrr::keep(unit_scrape,out_list$col_list == dummy_name_specs[zz])
  assign(dummy_name_list[[zz]],bind_rows(dummy_data_list[[zz]]))
}

go_wide_by_nn <- function(nn,data_to_fatten){
  dummy.set             <- 0:(nn-1)
  dummy.set2            <- 1:nn
  dummy.list            <- list()
  for(qq in 1:length(dummy.set)){
    dummy.names.list    <- rownames(data_to_fatten) %>% as.numeric()
    dummy.names.list    <- dummy.names.list[dummy.names.list %% nn == dummy.set[qq]]
    dummy.list[[qq]]    <- data_to_fatten[dummy.names.list,]
  }
  return(bind_cols(dummy.list))
}

NFL.Scores <- go_wide_by_nn(nn = 3,CCCC_6)

names(NFL.Scores) <- c('Home_Team',
                       'Home_Team_Score',
                       'OT_Flag',
                       'RecYds',
                       'RecYds_Lead',
                       'Lead_Rec_Yards',
                       'Date',
                       'Date_Ex',
                       'Date_Exx',
                       'PassYds',
                       'PassYds_Lead',
                       'Lead_Pass_Yards',
                       'Away_Team',
                       'Away_Team_Score',
                       'Final_Flag',
                       'RushYds',
                       'RushYds_Lead',
                       'Lead_Rush_Yards')

NFL.Scores <- NFL.Scores[,c('Home_Team',
                            'Home_Team_Score',
                            'OT_Flag',
                            'RecYds',
                            'RecYds_Lead',
                            'Lead_Rec_Yards',
                            'Date',
                            'PassYds',
                            'PassYds_Lead',
                            'Lead_Pass_Yards',
                            'Away_Team',
                            'Away_Team_Score',
                            'RushYds',
                            'RushYds_Lead',
                            'Lead_Rush_Yards')]

NFL.Scores$Month   <- month(mdy(NFL.Scores$Date))
NFL.Scores$Year    <- year(mdy(NFL.Scores$Date))
NFL.Scores$Weekday <- weekdays(mdy(NFL.Scores$Date))

NFL.Teams     <- unique(NFL.Scores$Home_Team)
Schedule.Cube <- list()
cnt <- 0
for(jj in NFL.Teams){
  cnt <- cnt + 1
  dummy.df <- NFL.Scores[NFL.Scores$Home_Team == jj | NFL.Scores$Away_Team == jj,]  
  dummy.df$Report.Team <- jj
  Schedule.Cube[[cnt]] <- dummy.df 
}
NFL.Scores.Cube             <- bind_rows(Schedule.Cube)
NFL.Scores.Cube$Week        <- format(as.Date(mdy(NFL.Scores.Cube$Date)-7),"%W") %>% as.numeric()
week.seq <- unique(NFL.Scores.Cube[,c("Year","Week")]) %>% group_by(Year) %>% summarize(Start = min(Week) %>% as.numeric(),End = max(Week)%>% as.numeric(),Length = End - Start)
years.id <- unique(week.seq$Year)
NFL.Scores.Cube <- merge(NFL.Scores.Cube,week.seq,by= 'Year')
NFL.Scores.Cube$Week <- NFL.Scores.Cube$Week - NFL.Scores.Cube$Start + 1 
NFL.Scores.Cube$HA          <- ifelse(NFL.Scores.Cube$Home_Team == NFL.Scores.Cube$Report.Team, "HOME","AWAY")
NFL.Scores.Cube$WINS        <- ifelse(NFL.Scores.Cube$Home_Team_Score >= NFL.Scores.Cube$Away_Team_Score & NFL.Scores.Cube$HA == "HOME" | NFL.Scores.Cube$Away_Team_Score >= NFL.Scores.Cube$Home_Team_Score & NFL.Scores.Cube$HA == "AWAY",1,0)
NFL.Scores.Cube$TIES        <- ifelse(NFL.Scores.Cube$Home_Team_Score == NFL.Scores.Cube$Away_Team_Score,1,0)
NFL.Scores.Cube$LOSS        <- 1 - NFL.Scores.Cube$WINS - NFL.Scores.Cube$TIES
NFL.Scores.Cube$MARGIN      <- ifelse(NFL.Scores.Cube$WINS == 1,1,-1) *abs(as.integer(NFL.Scores.Cube$Home_Team_Score) - as.integer(NFL.Scores.Cube$Away_Team_Score)) %>% as.numeric()
NFL.Scores.Cube$MARGINSQ    <- ifelse(NFL.Scores.Cube$WINS == 1,1,-1) *(as.integer(NFL.Scores.Cube$Home_Team_Score) - as.integer(NFL.Scores.Cube$Away_Team_Score))^2 %>% as.numeric()
NFL.Scores.Cube$Date        <- mdy(NFL.Scores.Cube$Date) %>% as.Date()
NFL.Scores.Cube$Report.Team <- gsub("St. Louis Rams","Los Angeles Rams",NFL.Scores.Cube$Report.Team)
NFL.Scores.Cube$Report.Team <- gsub("San Diego Chargers","Los Angeles Chargers",NFL.Scores.Cube$Report.Team)




DIV.Table <- read.csv("C:/Users/scotg/OneDrive/GitHub/Teams_NFL.csv")
NFL.Scores.Cube <- merge(NFL.Scores.Cube,DIV.Table,by = "Report.Team",all.x = TRUE)

kk <- unique(NFL.Scores.Cube$Report.Team)[1]

pdf("C:/Users/scotg/OneDrive/Desktop/12MoAvg.pdf")
for(kk in unique(NFL.Scores.Cube$Report.Team)){
color.lkp <- merge(kk,DIV.Table,by.x = "x",by.y = "Report.Team")["NFL.Color"] %>% data.frame(stringsAsFactors = TRUE)
color.lkp <- color.lkp$NFL.Color[1] %>% as.character()
color.df <- colorr::nfl.colors(color.lkp)

test.df  <- NFL.Scores.Cube[NFL.Scores.Cube$Report.Team == kk,c("Date","MARGIN")] %>% as.data.frame()
test.df  <- test.df[order(test.df$Date),]
temp.zoo <- zoo(test.df$MARGIN,test.df$Date)
m.av     <- rollmean(temp.zoo, 12,fill = list(NA, NULL, NA))
m.av     <- m.av[format(index(m.av),'%b') %in% c('Sep','Oct','Nov','Dec','Jan')]

home.df   <- NFL.Scores.Cube[NFL.Scores.Cube$Report.Team == kk & NFL.Scores.Cube$HA == "HOME",c("Date","MARGIN")] %>% as.data.frame()
home.df   <- home.df[order(home.df$Date),]
home.zoo  <- zoo(home.df$MARGIN,home.df$Date)
home.av   <- rollmean(home.zoo, 12,fill = list(NA, NULL, NA))
home.av   <- home.av[format(index(home.av),'%b') %in% c('Sep','Oct','Nov','Dec','Jan')]

away.df   <- NFL.Scores.Cube[NFL.Scores.Cube$Report.Team == kk & NFL.Scores.Cube$HA == "AWAY",c("Date","MARGIN")] %>% as.data.frame()
away.df   <- away.df[order(away.df$Date),]
away.zoo  <- zoo(away.df$MARGIN,away.df$Date)
away.av   <- rollmean(away.zoo, 12,fill = list(NA, NULL, NA))
away.av   <- away.av[format(index(away.av),'%b') %in% c('Sep','Oct','Nov','Dec','Jan')]

color.df <- gsub("#02244","#000080",color.df)

ex1 <- ggplot() + 
  geom_line(data = fortify.zoo(m.av),aes(x = Index,y = m.av, color = color.df[2]),size = 1.25) +
  geom_line(data = fortify.zoo(home.av),aes(x = Index,y = home.av, color = color.df[1])) +
  geom_line(data = fortify.zoo(away.av),aes(x = Index,y = away.av, color = color.df[3])) +
  scale_color_manual(name = "Moving Averages",labels = c("Away Spread","Home Spread","Total"),values = matrix(color.df)[,1]) +
  theme_minimal() +
  geom_point(data = fortify.zoo(temp.zoo),aes(x = Index,y = temp.zoo), color = color.df[1]) +
  ggtitle(kk) 
print(ex1)
}
dev.off()




plot(sma(test.ts,h =16))
