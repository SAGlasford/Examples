
library(lubridate)
library(stringr)

source("C:/Users/scotg/OneDrive/Documents/GitHub/Examples/NFL/fx_listMassage.R")

if(!exists("result.list")){
source("C:/Users/scotg/OneDrive/Documents/GitHub/Examples/NFL/NFL_Scores.R")
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
NFL.Scores$Year   <- year(mdy(NFL.Scores$Date))
NFL.Scores$Weekday <- weekdays(mdy(NFL.Scores$Date))
NFL.Scores$Date.No <- mdy(NFL.Scores$Date) %>% as.numeric()
NFL.Scores$Home.Team.Net.Points <- as.numeric(NFL.Scores$Home_Team_Score) - as.numeric(NFL.Scores$Away_Team_Score)
NFL.Scores$Away.Team.Net.Points <- -NFL.Scores$Home.Team.Net.Points

SeasonList <- list()
for(jjj in 1:length(years.to.study)){
  dummy.name <- paste0("Season_",jjj)
  dummy.date.1 <- mdy("06-01-2018")
  year(dummy.date.1) <- years.to.study[jjj] %>% as.numeric()
  dummy.date.2 <- mdy("05-31-2019")
  year(dummy.date.2) <- years.to.study[jjj] %>% as.numeric() +1
  dummy.df <- NFL.Scores[mdy(NFL.Scores$Date) >= dummy.date.1 & mdy(NFL.Scores$Date) <= dummy.date.2,]
  dummy.df$Season <- years.to.study[jjj]
  SeasonList[[jjj]] <- dummy.df
}
NFL1 <- bind_rows(SeasonList)
NFL1 <- NFL1[order(NFL1$Date),]
season.min.dates <-  NFL1 %>% group_by(Season) %>% summarise(OpenDay = max(Date))

NFL1 <- merge(NFL1,season.min.dates,by = "Season",all.x = TRUE)
NFL1$Day.Diff <- mdy(NFL1$Date) - mdy(NFL1$OpenDay) 
add.func.dates <-  NFL1 %>% group_by(Season) %>% summarise(OpenDay = min(Day.Diff))
NFL1 <- merge(NFL1,add.func.dates,by = "Season",all.x = TRUE)
NFL1$Day.Diff.1 <- NFL1$Day.Diff - NFL1$OpenDay.y
NFL1$Day.Diff.1 <- NFL1$Day.Diff.1 %>% as.integer()
NFL1$Week <-    floor(NFL1$Day.Diff.1/7) +1

GamesPlayed.Week <- NFL1 %>% group_by(Season,Week) %>% summarise(NoOfGames = n())
GamesPlayed.Year <- NFL1 %>% group_by(Season) %>% summarise(NoOfGames = n())



head()



