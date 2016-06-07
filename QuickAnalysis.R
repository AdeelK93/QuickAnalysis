library(stringi)
library(stats)
suppressMessages(library(plyr))
suppressMessages(library(dplyr))
library(reshape2)
library(readr)
library(readxl)
library(colorspace)
suppressMessages(library(plotly))

stdnames <- c("Voltage","Current","Amp.Hours","Total.Time","Step.Time","Step","Cycle")

import <- function(fileschosen) { #can import both csv and excel files
  if(is.null(fileschosen)) return(NULL)
  files <- mapply(import.single,fileschosen$name,fileschosen$datapath,SIMPLIFY = F)
  files <- rbind.fill(files[!sapply(files, is.null)])
  filenames <- tryCatch(parsebatteryname(unique(files$Filename)),
                        error=function(e){
                          Filename <- unique(files$Filename)
                          Battery.ID <- gsub(".xls?x*","",Filename)
                          Battery.ID <- gsub(".csv","",Battery.ID)
                          Battery.ID <- LCSrepeater(gsub("(Test_)[[:digit:]]{10,}(_)[A-E][[:digit:]]{1,2} ",
                                                         "",Battery.ID))
                          data.frame(Filename,Battery.ID,Type=Battery.ID)
                        })
  files <- suppressMessages(left_join(files,filenames))
  files$Filename <- as.factor(files$Filename)
  files
}

import.single <- function(filename,filepath) { #worker function
  filext <- tail(strsplit(filename,".",T)[[1]],n=1)
  filepath.new <- paste0(filepath,".",filext)
  file.copy(filepath,filepath.new)
  if(length(grep("csv",filext,T))){
    imported <- suppressWarnings(read_csv(filepath.new,col_names = (as.character(1:25))))
  } else if(length(grep("xls",filext,T))){
    if(length(excel_sheets(filepath.new))>1){ #multiple sheets
      return(bigXL(filepath.new))
    } else imported <- suppressWarnings(read_excel(filepath.new,col_names = F))
  } else return(NULL)
  file.remove(filepath.new)
  
  mincol <- min(50,nrow(imported))
  charcount <- suppressWarnings(apply(imported[1:mincol,],1,function(x) sum(is.na(x==as.numeric(x))-is.na(x))))
  datacount <- suppressWarnings(apply(imported[1:mincol,],1,function(x) sum(!is.na(x))))
  headrow <- which.min(abs(charcount-median(datacount)))
  colnames(imported) <- make.names(imported[headrow,])
  imported <- imported[(headrow+1):nrow(imported),]
  imported <- imported[,colSums(is.na(imported))<(nrow(imported)/3)] #remove empty columns
  imported <- imported[rowSums(is.na(imported))<(ncol(imported)/3),] #remove empty rows
  imported <- mutate_each(imported,funs(cleantime)) #remove bad time symbols
  imported <- type_convert(imported)
  imported$Filename <- filename
  imported
}

bigXL <- function(xl){
  xlfile <- xl
  sheets.orig <- excel_sheets(xl)
  if("Channel_Chart" %in% sheets.orig){ #arbin data, has weird xml structure
    sheetnames <- sheets.orig[(grep("Channel",sheets.orig)[grep("Channel",sheets.orig)!=2])]
    sheets.orig <- (grep("Channel",sheets.orig)[grep("Channel",sheets.orig)!=2])-1
    xl <- lapply(sheets.orig,read_excel,path=xl,col_names = F)
  } else{
    xl <- lapply(sheets.orig,read_excel,path=xl,col_names = F)
    sheetnames <- excel_sheets(xlfile)[sapply(xl,ncol)==max(sapply(xl,ncol))]
    xl <- xl[sapply(xl,ncol)==max(sapply(xl,ncol))] #pick out the files with the most columns
  }
  
  bigXL.single <- function(imported,filename){
    mincol <- min(50,nrow(imported))
    charcount <- suppressWarnings(apply(imported[1:50,],1,function(x) sum(is.na(x==as.numeric(x))-is.na(x))))
    datacount <- suppressWarnings(apply(imported[1:50,],1,function(x) sum(!is.na(x))))
    headrow <- which.min(abs(charcount-median(datacount)))
    colnames(imported) <- make.names(imported[headrow,])
    imported <- imported[(headrow+1):nrow(imported),]
    imported$Filename <- filename
    imported <- imported[,colSums(is.na(imported))<(nrow(imported)/3)] #remove empty columns
    imported <- imported[rowSums(is.na(imported))<(ncol(imported)/3),] %>% #remove empty rows
      mutate_each(funs(cleantime)) %>% #remove bad time symbols
      type_convert()
  }
  file.remove(xlfile)
  rbind.fill(mapply(bigXL.single,xl,sheetnames,SIMPLIFY = F))
}

dygraph.cast <- function(data, x,y,xlab=NULL,ylab=NULL,color=NULL,coloravg,group=NULL) {
  if(is.null(data)) return(NULL)
  if("Battery.ID" %in% colnames(data)){
    casted <- dcast(data, as.formula(paste(x,"~Battery.ID")),mean,value.var=y)
    lines <- colorline(data)
    parsed <- mapply(function(x,y) paste0("%>% dySeries('",x,"',strokePattern=1:",y,")"),
                     lines$Battery.ID,lines$Line,SIMPLIFY=F)
    parsed <- paste(parsed, collapse = " ")
    graph <- dygraph(casted, xlab=xlab,ylab=ylab,group=group) %>%
      dyLegend(width = 500,labelsDiv="legendDivID") %>% dyRangeSelector() %>%
      dyHighlight(highlightSeriesOpts = list(strokeWidth = 2,strokeBorderWidth=1)) %>%
      dyOptions(connectSeparatedPoints=T,colors=color) %>% dyCSS("dy.css")
    graph <- eval(parse(text=paste("graph",parsed)))
  } else{ #avg/sem
    casted <- dcast(data, as.formula(paste(x,"~Type")),mean,value.var=y)
    casted.sem <- dcast(data, as.formula(paste(x,"~Type")),mean,value.var=paste0(y,".SEM"))
    casted.lwr <- casted-casted.sem
    colnames(casted.lwr) <- paste0(colnames(casted),".lwr")
    casted.upr <- casted+casted.sem
    colnames(casted.upr) <- paste0(colnames(casted),".upr")
    cnames <- sapply(colnames(casted)[2:ncol(casted)],
                     function(x) paste0(x,c(".lwr","",".upr"),collapse="','"))
    cnames.orig <- c(colnames(casted),colnames(casted.lwr)[2:ncol(casted)],
                     colnames(casted.upr)[2:ncol(casted)]) #this is some shiny bug
    parsed <- sapply(cnames,function(x) paste0("%>% dySeries(c('",x,"'))"))
    parsed <- paste(parsed, collapse = " ")
    casted <- cbind(casted,casted.lwr[,2:ncol(casted)],casted.upr[,2:ncol(casted)])
    colnames(casted) <- cnames.orig
    graph <- dygraph(casted, xlab=xlab,ylab=ylab,group=group) %>%
      dyLegend(width = 500,labelsDiv="legendDivID") %>% dyRangeSelector() %>%
      dyHighlight(highlightSeriesOpts = list(strokeWidth = 2,strokeBorderWidth=1)) %>%
      dyOptions(connectSeparatedPoints=T,colors=coloravg) %>% dyCSS("dy.css")
    graph <- eval(parse(text=paste("graph",parsed)))
  }
  graph
}

#reduce cycling data by extracting a single data point of interest per cycle
#syntax is: when y=yval, what is x? ie when voltage=6, what is time?
#NOTE: Should make more efficient by checking if x=y
reducer <- function(dataset, newcolumn = "V", y, yval, x) {
  if(yval=="max") func <- max
  else if(yval=="min") func <- min
  else {
    yval<-as.numeric(yval) #looking for a numerical value
    func<-function(x){
      if(yval<min(x) || yval>max(x)) return(NA) #value does not exist
      x[which.min(abs(yval-x))]
    }
  }
  newdata <-
    melt(tapply( #melting a tapply looks messy but it works, and it's quite fast
      unlist(dataset[, y]), #4 times faster than aggregate
      list(dataset$Cycle, dataset$Battery.ID),
      func
    ))
  data.length <- nrow(newdata)
  colnames(newdata) <- c("Cycle", "Battery.ID", y)
  #this left join followed by a select allows for values to be matched up
  newdata <- suppressMessages(select_(left_join(newdata, dataset),
                                      "Cycle", "Battery.ID", "Type", x))
  colnames(newdata) <- c("Cycle", "Battery.ID", "Type", newcolumn)
  if(data.length!=nrow(newdata)) print(paste((nrow(newdata)-data.length),"ties found"))
  newdata
}

se <- function(x) sd(x)/sqrt(length(x))

#this is a very fast and efficent way of calculating avg/sem for all variables
#x should be set to the common x variable (commonly cycle or time)
avgsem <- function(dataset, x = "Cycle") {
  if(!all("Type" %in% colnames(dataset),"Battery.ID" %in% colnames(dataset))) {
    stop("Could not find the Type and Battery.ID columns. Normalize names first.")
  }
  newdata <- as.data.frame(dataset[,!colnames(dataset) %in% c("Battery.ID", x, "Type"), drop=F])
  cnames <- colnames(newdata)
  avg <- aggregate(newdata[, 1:length(cnames)],
                   by = list(dataset$Type, unlist(dataset[, x])), mean)
  colnames(avg) <- c("Type", x, cnames)
  sem <- aggregate(newdata[, 1:length(cnames)],
                   by = list(dataset$Type, unlist(dataset[, x])), se)
  colnames(sem) <- c("Type", x, paste0(cnames, ".SEM"))
  suppressMessages(left_join(avg, sem))
}

#makes a 1D bar graph, by battery type
#will default to boxplot on data without avg/sem data
bargraph <- function(dataset, y, ylab){
  if("Battery.ID" %in% colnames(dataset)){ #boxplot with fivenum
    gg <- ggplot(na.omit(dataset), aes_string(x = "Type", y = y, fill = "Type")) +
      geom_boxplot() + scale_y_continuous() +
      xlab("") + ylab(ylab) + theme(legend.position = "none")
  } else if("Stage" %in% colnames(dataset)){ #avg+sem as stacked bar chart
    gg <- ggplot(dataset, aes(Type, Avg, fill=Stage, ymin=ystart, ymax=yend)) +
      geom_bar(stat = "identity") + geom_errorbar(size = 1, width = .1) +
      xlab("") + ylab(ylab)
  } else if("Type" %in% colnames(dataset)){ #avg+sem as bar chart
    avg <- y
    sem <- paste0(y,".SEM")
    gg <- ggplot(dataset, aes_string(x = "Type", y = avg, fill = "Type")) +
      geom_bar(stat = "identity") +
      geom_errorbar(aes_string(ymin=sprintf("%s - %s",avg,sem),
                               ymax=sprintf("%s + %s",avg,sem)), size = 1, width = .1) +
      xlab("") + ylab(ylab) + theme(legend.position = "none")
  } else return(NULL)
  if(length(unique(dataset$Cycle))>1){ #going to add x axis facets
    gg <- gg + facet_grid(.~Cycle) 
  }
  return(gg)
}

#different linetypes for different batteries within a type
colorline <- function(dataset) {
  dataset <- dataset[!duplicated(dataset$Battery.ID),c("Type","Battery.ID")]
  dataset <- arrange(dataset, Battery.ID)
  spl <- split(dataset$Battery.ID,dataset$Type)
  dataset$Line <- unsplit(sapply(spl,seq_along),dataset$Type)
  dataset
}

#makes a 2D line graph, by battery type
#will detect if data requires ribbons or not
linegraph <- function(dataset, x, y) {
  if("Battery.ID" %in% colnames(dataset)){ #simple line plot
    lines <- colorline(dataset)
    gg <- ggplot(dataset,aes_string(x,y,color="Battery.ID",linetype="Battery.ID"))+geom_line()+
      scale_linetype_manual(values=as.numeric(lines$Line),labels=as.vector(lines$Battery.ID),name="")
  } else if("Type" %in% colnames(dataset)){ #avg+sem with geom_ribbon
    avg <- y
    sem <- paste0(y,".SEM")
    gg <- ggplot(dataset, aes_string(x,avg,color = "Type",fill = "Type")) +
      geom_line()+geom_ribbon(aes_string(ymin=sprintf("%s - %s",avg,sem),
                                         ymax=sprintf("%s + %s",avg,sem)),alpha = .3, color = NA, show.legend = F)
  } else return(NULL)
  return(gg)
}

cleantime <- function(x){
  if(is.numeric(x)) return(x)
  if(!length(grep(":",x[5]))) return(x) #check if there are : in the 5th line
  x <- gsub("d","",x)
  x <- gsub(" ","",x)
  x <- gsub("\"","",x)
  x <- gsub("=","",x)
  toSeconds(x)
}

toSeconds <- function(x){
  if (!is.character(x)) stop("x must be a character string of the form H:M:S")
  if (length(x)<=0)return(x)
  unlist(lapply(x, function(i){
    i <- as.numeric(strsplit(i,':',fixed=TRUE)[[1]])
    if (length(i) == 3) i[1]*3600 + i[2]*60 + i[3]
    else if (length(i) == 2) i[1]*60 + i[2]
    else if (length(i) == 1) i[1]
  }))  
}

parsebatteryname <- function(Filename) { #sort filename into ID and Type
  Battery.ID <- gsub(".xls?x*","",Filename)
  Battery.ID <- gsub(".csv","",Battery.ID)
  Battery.ID <- gsub("_"," ",Battery.ID)
  Battery.ID <- gsub("-"," ",Battery.ID)
  Battery.ID <- LCSrepeater(gsub("[[:digit:]]{10,} [A-E][[:digit:]]{1,2} ",
                                 "",trimws(Battery.ID)))
  Battery.ID <- gsub("Test ","",Battery.ID)
  Type <- adist(Battery.ID)
  rownames(Type) <- Battery.ID
  Type <- cutree(hclust(as.dist(Type)),h=1)
  parsed <- data.frame(Filename,Battery.ID,Type=as.factor(Type))
  typenames <- unlist(by(Battery.ID,Type,function(x) LCS(x,T)[1]))
  if(sum(sapply(typenames,is.na))) {
    typenames <- unlist(by(Battery.ID,Type,function(x) LCS(gsub("[^[:alpha:]]","",x),T)[1]))
  }
  levels(parsed$Type) <- typenames
  parsed
}

LCSrepeater <- function(strings, pattern=F) { #repeats lcs until there is a match
  ni <- 0
  x <- character(0)
  while(!length(x) & ni<(min(nchar(as.character(strings)))-2)){
    ni <- ni + 1
    x <- LCS(strings,T,ni)
  }
  LCS(strings,pattern,ni)
}

LCS <- function(strings, pattern=F, dampener=1) { #longest common substring
  types <- length(strings)
  if(types==1) return(strings)
  trafos <- drop(attr(adist(strings, counts=TRUE), "trafos"))
  if(min(nchar(as.character(strings)))<=2) {
    poss.pat <- do.call(rbind,stri_locate_all_regex(trafos,"M+"))
  } else {
    poss.pat <- do.call(rbind,stri_locate_all_regex(trafos, paste(c("M{2,",
                                                                    min(nchar(as.character(strings)))-dampener,"}"),collapse="")))
  }
  poss.pat <- unique(poss.pat)
  pat <- mapply(function(x,y) stri_sub(strings,x,y),poss.pat[,1],poss.pat[,2])
  pat <- names(table(pat)[table(pat)==types])
  if(min(nchar(as.character(strings)))>1) pat <- pat[nchar(pat)>1]
  if(pattern) return(pat)
  uniques <- unname(strings)
  if(!length(pat)) return(uniques)
  for (n in 1:length(pat)) {
    uniques <- gsub(pat[n],"",uniques)
  }
  uniques
}

#Optimize Levenshtein distance to find best fuzzy string match
agrep.best <-function(pattern, vect) {
  vect[which.min(adist(pattern,vect,ignore.case = T, fixed = F))]
}

#calculates the recharge factor per cycle
calculaterechargefactor <- function(dataset, capname="Amp.Hours") {
  dataset$Recharge.Factor <- suppressWarnings(unlist(ave(dataset[capname],
                                                         list(dataset$Battery.ID, dataset$Cycle),
                                                         FUN = function(x) -x / min(x))[capname]))
  dataset
}

#determines what the protocol might be
Protocol <- function(dataset) {
  if(is.null(dataset)) return(NULL)
  Step <- data.frame(Step=unique(dataset$Step))
  
  Mode <- sapply(as.character(by(dataset$Amp.Hours,dataset$Step,
                                 function(x) sign(sum(range(x))))),
                 switch,"0"="Rest","-1"="Discharge","1"="Charge",NA)
  Voltage <- vector(length = nrow(Step))
  Current <- vector(length = nrow(Step))
  Capacity <- vector(length = nrow(Step))
  for (n in 1:nrow(Step)) {
    d <- dataset[dataset$Step==Step[n,],]
    if(Mode[n]=="Rest"){
      Voltage[n]<-median(d$Voltage)
      Current[n]<-median(d$Current)
      Capacity[n]<-0
    } else if(Mode[n]=="Charge"){
      if(floor(max(d$Voltage))>quantile(d$Voltage,.75)){
        Voltage[n]<-floor(max(d$Voltage)) #probably formation data
      } else Voltage[n]<-quantile(d$Voltage,.75)
      Current[n]<-max(d$Current)
      Capacity[n]<-max(d$Amp.Hours)
    } else if(Mode[n]=="Discharge"){
      Voltage[n]<-min(d$Voltage)
      Current[n]<-min(d$Current)
      Capacity[n]<-min(d$Amp.Hours)
    } else{ #something has gone wrong
      Voltage[n]<-median(d$Voltage)
      Current[n]<-median(d$Current)
      Capacity[n]<-NA
    }
  }
  Minutes<-as.numeric(by(dataset$StepTimeMin,dataset$Step,max))
  Instances<-as.numeric(by(dataset$Cycle,dataset$Step,
                           function(x) length(unique(x))))
  cbind(Step,Mode,Voltage,Current,Capacity,Minutes,Instances)
}

theme_pub <- function (base_size = 12, base_family = "") {
  #from areshenk_blog
  theme_grey(base_size = base_size, 
             base_family = base_family) %+replace% 
    theme(# Set text size
      plot.title = element_text(size = 18),
      axis.title.x = element_text(size = 16),
      axis.title.y = element_text(size = 16, 
                                  angle = 90),
      axis.text.x = element_text(size = 14),
      axis.text.y = element_text(size = 14),
      strip.text.x = element_text(size = 15),
      strip.text.y = element_text(size = 15,
                                  angle = -90),
      # Legend text
      legend.title = element_text(size = 15),
      legend.text = element_text(size = 15),
      # Configure lines and axes
      axis.ticks.x = element_line(colour = "black"), 
      axis.ticks.y = element_line(colour = "black"), 
      # Plot background
      panel.background = element_rect(fill = "white"),
      panel.grid.major = element_line(colour = "grey83", 
                                      size = 0.2), 
      panel.grid.minor = element_line(colour = "grey88", 
                                      size = 0.5), 
      # Facet labels        
      legend.key = element_rect(colour = "grey80"), 
      strip.background = element_rect(fill = "grey80", 
                                      colour = "grey50", 
                                      size = 0.2))
}