


Harvest.HTTM.Table <- function(url = "https://www.pro-football-reference.com/years/2018/week_1.htm"){
  
  dummy.html.node.read(url)
  table.col.counts <- NCol.Dim.Freq()
  for(qq in table.col.counts){}
  
  for(q in tbls){
    if(q %>% html_table() %>% ncol() == 3){dummy.list[q] <- q %>% html_table() %>% as.data.frame() %>% mutate_all(as.character())}
  }
  dummy.df <- bind_rows(dummy.list, .id = "column_label")  
  return(dummy.df)}

Harvest.HTTM.Table()

for(k in all.websites[1]){
  page.read <- k %>% read_html()
  tbls <- html_nodes(page.read, "table")
  for(q in tbls){
    q %>% html_table() %>% as.data.frame()
  }
}
