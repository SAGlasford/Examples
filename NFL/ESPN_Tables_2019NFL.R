library(rvest)
library(httr)

seasonlength <- seq(1,17)

create.websites.htm <- function(sequence.array = 17,year = 2018){
  library(dplyr)
  dummy.list <- list()
  for(i in 1:sequence.array){
    dummy.list[[i]] <-paste0("https://www.espn.com/nfl/schedule/_/week/",i)
    }
  dummy.list <- dummy.list %>% as.data.frame()
  dummy.list <- t(dummy.list)
  rownames(dummy.list) <- c()
  return(dummy.list)
}


dummy.html.node.read <- function(url = "https://www.pro-football-reference.com/years/2018/week_1.htm",node.type = "table"){
  dummy.list      <- list()
  dummy.page.read <- url %>% read_html()
  dummy.tbls      <- html_nodes(dummy.page.read, node.type)
  return(dummy.tbls)}

Table.Data.Type.Map <- function(node.table = dummy.html.node.read(),column.cnt = 3){
  dummy.char.list <- list()
  for(q in 1:length(node.table)){
    if(node.table[[q]] %>% html_table(fill = TRUE) %>% ncol() == column.cnt){
      dummy.list[[q]] <- node.table[[q]] %>% html_table(fill = TRUE) %>% as.data.frame()
      dummy.char.list[[q]] <- sapply(dummy.list[[q]], class) %>% t() %>% as.data.frame()
    }
  }
  dummy.char.list.df        <- bind_rows(dummy.char.list) %>% as.data.frame()
  return(dummy.char.list.df)
}

NCol.Dim.Id <- function(node.table = dummy.html.node.read()){
  dummy.tbl.ncols <- list()
  for(q in 1:length(node.table)){dummy.tbl.ncols[[q]] <- node.table[[q]] %>% html_table(fill = TRUE) %>% ncol()}
  dummy.tbl.ncols.1 <- dummy.tbl.ncols %>% as.data.frame() %>% t()
  row.names(dummy.tbl.ncols.1) <- c()
  dummy.tbl.ncols.1 <- dummy.tbl.ncols.1 %>% as.data.frame()
  dummy.tbl.ncols.1$ID.Row         <- 1:nrow(dummy.tbl.ncols.1)
  return(dummy.tbl.ncols.1)
}


Get.Nodes <- function(x,Week = 1,Year = 2019){
All.Nodes <- list()
for(jj in 1:length(dummy.html.node.read(x))){
  All.Nodes[[jj]] <- dummy.html.node.read(x)[jj] %>% html_table(fill = TRUE) %>% as.data.frame()  
}
dummy.out      <- bind_rows(All.Nodes)
dummy.out$Week <- Week
dummy.out$Year <- Year

return(dummy.out)
}

countSpaces <- function(s) { sapply(gregexpr(" ", s), function(p) { sum(p>=0) } ) }

Loop.List <- create.websites.htm(sequence.array = max(seasonlength))
outlist <- list()
cnt <- 0
for(kk in Loop.List){
  cnt <- cnt +1
  outlist[[cnt]] <- Get.Nodes(x = kk,Week = cnt,Year = 2019)
}
final.out         <- unique(bind_rows(outlist))
final.out$matchup <- ifelse(countSpaces(final.out$matchup) > 1,sub(" ","_",final.out$matchup),sub(" "," ",final.out$matchup))
final.out$Var.2   <- ifelse(countSpaces(final.out$Var.2) > 1,sub(" ","_",final.out$Var.2),sub(" "," ",final.out$Var.2))

final.out <- tidyr::separate(final.out,matchup,into =c("Away_Team","Away_Team_Code"),sep = " ")
final.out <- tidyr::separate(final.out,Var.2,into =c("Home_Team","Home_Team_Code"),sep = " ")
final.out <- final.out[!is.na(final.out$Home_Team),]
unique(final.out$Home_Team_Code)

yy <- dummy.html.node.read(url = "https://betiq.teamrankings.com/nfl/betting-trends/by-closing-line/win-loss-records/?season-filter=since_2015") %>% html_table(fill = TRUE) %>% as.data.frame() 
yy <- yy[2:nrow(yy),]
yy$All.Teams.3 <- as.numeric(gsub("%",'',yy$All.Teams.3))/100
yy$All.Teams <- as.numeric(yy$All.Teams)


library(ggplot2)
ggplot(data = yy) + geom_point(aes(x = All.Teams,y = All.Teams.3)) + geom_smooth(aes(x = All.Teams,y = All.Teams.3))

model1 <- 


yy <- bind_rows(yy)

Table.Data.Type.Map(yy,column.cnt = NCol.Dim.Id(yy)$V1[1])
