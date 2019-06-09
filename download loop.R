library(httr)
library(dplyr)
pages <- list()
fin.page <- list()
for(j in 1:99){
for(i in 1:99){
  pages[[i]] <- paste0("https://www.porncomixonline.net/pics/2016/12/",formatC(i,width = 2,format = "d",flag = "0"),"-",j,".jpg")
  pages[[i]]
  
  }
fin.page[[j]] <- pages[[i]] %>% as.data.frame()
}
xml2::download_html(pages[[i]])
tryCatch({xml2::download_html(pages[[i]])},error=function(e){cat("ERROR :",conditionMessage(e), "\n")})

download.file(pages[[i]],download.file(pages[[i]],destfile = paste0("file:///C:/Users/scotg/Downloads/"),method = "libcurl"),method = "internal")

library(rvest)

install.packages("rvest")

for (i in 1:10) {
  tryCatch({
    print(i)
    if (i==7) stop("Urgh, the iphone is in the blender !")
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}
