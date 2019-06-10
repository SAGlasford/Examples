library(dplyr)
library(rvest)
years.to.study <- seq(2010,2018,by =1)

####### Universe of Websites ###################
##### Define create.websites.htm ###############
### Creates Data Frame of Websites to Scrape ###

create.websites.htm <- function(seasonlength = 17,year = 2018){
  dummy.list <- list()
  for(i in 1:seasonlength){
  dummy.list[[i]] <-paste0("https://www.pro-football-reference.com/years/",year,"/week_",i,".htm")}
  dummy.list <- dummy.list %>% as.data.frame()
  dummy.list <- t(dummy.list)
  rownames(dummy.list) <- c()
  return(dummy.list)
}

###
#####

first.set    <- create.websites.htm()
for(j in years.to.study){
  first.set  <- rbind(first.set,create.websites.htm(17,j))}
all.websites <- unique(first.set) 

################################################
########## END: Universe of Websites ###########
################################################


################################################
####### Read Tables ############################
##### Create Harvest.HTTM.Table ################

url = "https://www.pro-football-reference.com/years/2018/week_1.htm"
dummy.list      <- list()
dummy.page.read <- url %>% read_html()
dummy.tbls      <- html_nodes(dummy.page.read, "table")

dummy.html.node.read <- function(url = "https://www.pro-football-reference.com/years/2018/week_1.htm",node.type = "table"){
  dummy.list      <- list()
  dummy.page.read <- url %>% read_html()
  dummy.tbls      <- html_nodes(dummy.page.read, node.type)
  return(dummy.tbls)}
NCol.Dim.Freq <- function(node.table = dummy.html.node.read()){
  dummy.tbl.ncols <- list()
  for(q in 1:length(node.table)){dummy.tbl.ncols[[q]] <- node.table[[q]] %>% html_table() %>% ncol()}
  dummy.tbl.ncols.1 <- dummy.tbl.ncols %>% as.data.frame() %>% t() %>% table() %>% as.data.frame()
  return(dummy.tbl.ncols.1)
  }
NCol.Dim.Id <- function(node.table = dummy.html.node.read()){
  dummy.tbl.ncols <- list()
  for(q in 1:length(node.table)){dummy.tbl.ncols[[q]] <- node.table[[q]] %>% html_table() %>% ncol()}
  dummy.tbl.ncols.1 <- dummy.tbl.ncols %>% as.data.frame() %>% t()
  row.names(dummy.tbl.ncols.1) <- c()
  dummy.tbl.ncols.1 <- dummy.tbl.ncols.1 %>% as.data.frame()
  dummy.tbl.ncols.1$ID.Row         <- 1:nrow(dummy.tbl.ncols.1)
  return(dummy.tbl.ncols.1)
}

Table.Data.Type.Map <- function(node.table = dummy.html.node.read(),column.cnt = 3){
  dummy.char.list <- list()
  for(q in 1:length(node.table)){
    if(node.table[[q]] %>% html_table() %>% ncol() == column.cnt){
      dummy.list[[q]] <- node.table[[q]] %>% html_table() %>% as.data.frame()
      dummy.char.list[[q]] <- sapply(dummy.list[[q]], class) %>% t() %>% as.data.frame()
    }
  }
  dummy.char.list.df        <- bind_rows(dummy.char.list) %>% as.data.frame()
  return(dummy.char.list.df)
}


Table.Data.Type.ID <- function(node.table = dummy.html.node.read(),column.cnt = 3){
dummy.char.list <- list()
for(q in 1:length(node.table)){
  if(node.table[[q]] %>% html_table() %>% ncol() == column.cnt){
    dummy.list[[q]] <- node.table[[q]] %>% html_table() %>% as.data.frame()
    dummy.char.list[[q]] <- sapply(dummy.list[[q]], class) %>% t() %>% as.data.frame()
  }
}
dummy.char.list.df        <- bind_rows(dummy.char.list)
dummy.char.list.df.tbl    <- table(dummy.char.list.df) %>% as.data.frame()
dummy.char.list.df.tbl$ID <- 1:nrow(dummy.char.list.df.tbl)
return(dummy.char.list.df.tbl)
}

Table.Data.Type.Final1 <- function(aa = Table.Data.Type.ID(),bb = Table.Data.Type.Map()){
  bb$ID.List <- 1:nrow(bb)
  ee         <- merge(aa,bb) 
  return(ee)}


