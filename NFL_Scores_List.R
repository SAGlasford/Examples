
source("C:/Users/scotg/OneDrive/Documents/NFL_Scores.R")

counter <- 0
result.list <- list()
for(loop.urls in all.websites){
aaa <- dummy.html.node.read(url = loop.urls)   #pulls all the tables from a URL
bbb <- NCol.Dim.Freq(node.table = aaa)          #gets counts of col table dimensions
ccc <- NCol.Dim.Id(node.table = aaa)            #gets col count (V1) by ID

for(v in 1:1:length(bbb$.)){

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
