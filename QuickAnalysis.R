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
  #multithreaded import function to read all files
  files <- mapply(import.single,fileschosen$name,fileschosen$datapath,SIMPLIFY = F)
  #merge all files that did not throw an exception
  files <- rbind.fill(files[!sapply(files, is.null)])
  #extract type and ID information from file name
  filenames <- tryCatch(parsebatteryname(unique(files$Filename)),
                        error=function(e){
                          #ok that didn't work, try something else
                          Filename <- unique(files$Filename)
                          Battery.ID <- gsub(".xls?x*","",Filename)
                          Battery.ID <- gsub(".csv","",Battery.ID)
                          Battery.ID <- LCSrepeater(gsub("(Test_)[[:digit:]]{10,}(_)[A-E][[:digit:]]{1,2} ",
                                                         "",Battery.ID))
                          data.frame(Filename,Battery.ID,Type=Battery.ID)
                        })
  #attach type and ID data to structure
  files <- suppressMessages(left_join(files,filenames))
  #store as factor to reduce memory footprint
  files$Filename <- as.factor(files$Filename)
  files
}

import.single <- function(filename,filepath) { #worker function
  #find the file extension
  filext <- tail(strsplit(filename,".",T)[[1]],n=1)
  filepath.new <- paste0(filepath,".",filext)
  #add file extension to temporary file to smooth out import
  file.copy(filepath,filepath.new)
  if(length(grep("csv",filext,T))){
    imported <- suppressWarnings(read_csv(filepath.new,col_names = (as.character(1:25))))
  } else if(length(grep("xls",filext,T))){
    if(length(excel_sheets(filepath.new))>1){ #multiple sheets
      return(bigXL(filepath.new))
    } else imported <- suppressWarnings(read_excel(filepath.new,col_names = F))
  } else return(NULL)
  file.remove(filepath.new) #remove temporary files
  
  #begin removing unnecessary information from files
  #for performance reasons, this approach fails if there is no data for the first 50 rows
  mincol <- min(50,nrow(imported))
  #create vector describing text data across rows
  charcount <- suppressWarnings(apply(imported[1:mincol,],1,function(x) sum(is.na(x==as.numeric(x))-is.na(x))))
  #create vector describing numeric data across rows
  datacount <- suppressWarnings(apply(imported[1:mincol,],1,function(x) sum(!is.na(x))))
  #the header is the best match between the text and numeric data
  headrow <- which.min(abs(charcount-median(datacount)))
  #remove foreign characters from column names
  cnames <- make.names(trimws(imported[headrow,]))
  if(!sum(duplicated(cnames[cnames!="NA."]))) { #no duplicated column names
    colnames(imported) <- cnames
    imported <- imported[(headrow+1):nrow(imported),]
  }
  imported <- imported[,colSums(is.na(imported))<(nrow(imported)/3)] #remove empty columns
  imported <- imported[rowSums(is.na(imported))<(ncol(imported)/3),] %>% #remove empty rows
    mutate_each(funs(cleantime)) %>% #remove bad time symbols
    type_convert()
  imported$Filename <- filename
  imported
}

bigXL <- function(xl){ #worker function for big excel files
  xlfile <- xl
  sheets.orig <- excel_sheets(xl)
  if("Channel_Chart" %in% sheets.orig){ #arbin data, has weird xml structure
    sheetnames <- sheets.orig[(grep("Channel(?!_Chart)",sheets.orig,perl=T))]
    sheets.orig <- grep("Channel(?!_Chart)",sheets.orig,perl=T)-1
    xl <- lapply(sheets.orig,read_excel,path=xl,col_names = F)
  } else {
    xl <- lapply(sheets.orig,read_excel,path=xl,col_names = F)
    sheetnames <- excel_sheets(xlfile)[sapply(xl,ncol)==max(sapply(xl,ncol))]
    xl <- xl[sapply(xl,ncol)==max(sapply(xl,ncol))] #pick out the files with the most columns
  }
  
  bigXL.single <- function(imported,filename){ #worker worker function for big excel files
    #most of this is identical to the original import.single function, two turtles up
    mincol <- min(50,nrow(imported))
    charcount <- suppressWarnings(apply(imported[1:50,],1,function(x) sum(is.na(x==as.numeric(x))-is.na(x))))
    datacount <- suppressWarnings(apply(imported[1:50,],1,function(x) sum(!is.na(x))))
    headrow <- which.min(abs(charcount-median(datacount)))
    cnames <- make.names(trimws(imported[headrow,]))
    if(!sum(duplicated(cnames))) { #no duplicated column names
      colnames(imported) <- cnames
      imported <- imported[(headrow+1):nrow(imported),]
    }
    imported$Filename <- filename
    imported <- imported[,colSums(is.na(imported))<(nrow(imported)/3)] #remove empty columns
    imported[rowSums(is.na(imported))<(ncol(imported)/3),] %>% #remove empty rows
      mutate_each(funs(cleantime)) %>% #remove bad time symbols
      type_convert()
  }
  file.remove(xlfile) #remove temporary files
  #return a merged data frame
  rbind.fill(mapply(bigXL.single,xl,sheetnames,SIMPLIFY = F))
}

#turn long-form data frame into a time series
#graphs the resultant time series with error bars, if applicable
dygraph.cast <- function(data, x,y,xlab=NULL,ylab=NULL,color=NULL,coloravg,group=NULL) {
  if(is.null(data)) return(NULL)
  if("Battery.ID" %in% colnames(data)){ #regular
    casted <- dcast(data, as.formula(paste(x,"~Battery.ID")),mean,value.var=y)
    lines <- colorline(data) #figure out line pattern
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
  newdata <- suppressMessages(select_(left_join(newdata, dataset[,c("Battery.ID","Type")]),
                                      "Cycle", "Battery.ID", "Type", x))
  newdata <- unique(newdata[ ,1:4] )
  colnames(newdata) <- c("Cycle", "Battery.ID", "Type", newcolumn)
  if(data.length!=nrow(newdata)) print(paste((nrow(newdata)-data.length),"ties found"))
  newdata
}

se <- function(x) sd(x)/sqrt(length(x))
diff0 <- function(x) c(0,diff(x))

#this is a very fast and efficent way of calculating avg/sem for all variables
#x should be set to the common x variable (commonly cycle or time)
avgsem <- function(dataset, x = "Cycle") {
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
  if(length(unique(dataset$Cycle))>1){ #going to add x-axis facets
    gg <- gg + facet_grid(.~Cycle) 
  }
  return(gg)
}

#different linetypes for different batteries within a type
colorline <- function(dataset) {
  dataset <- dataset[!duplicated(dataset$Battery.ID),c("Type","Battery.ID")] %>%
    arrange(Battery.ID)
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
    sem <- paste0(y,".SEM")
    gg <- ggplot(dataset, aes_string(x,y,color = "Type",fill = "Type")) +
      geom_line()+geom_ribbon(aes_string(ymin=sprintf("%s - %s",y,sem),
                                         ymax=sprintf("%s + %s",y,sem)),alpha = .3, color = NA, show.legend = F)
  } else return(NULL)
  return(gg)
}

#first wave of timestamp checking
cleantime <- function(x){
  if(is.numeric(x)) return(x)
  if("POSIXct" %in% class(x)) { #this *should* be impossible
    x <- as.numeric(x)
    return(x-min(x))
  }
  if(!length(grep(":",x[5]))) return(x) #check if there are : in the 5th line
  x <- gsub("d","",x)
  x <- gsub(" ","",x)
  x <- gsub("\"","",x)
  x <- gsub("=","",x)
  toSeconds(x)
}

#turn text string into seconds
toSeconds <- function(x) {
  if (!is.character(x)) stop("x must be a character string of the form H:M:S")
  if (length(x)<=0)return(x)
  unlist(lapply(x, function(i){
    i <- as.numeric(strsplit(i,':',fixed=TRUE)[[1]])
    if (length(i) == 3) i[1]*3600 + i[2]*60 + i[3]
    else if (length(i) == 2) i[1]*60 + i[2]
    else if (length(i) == 1) i[1]
  }))  
}

#jagged to monotonic time series
as.monotonic <- function(x) {
  y <- diff0(x)
  y[y<0] <- min(y[y>0])
  cumsum(y) + 1 #or should it be min(x)?
}

#sort filename into ID and Type
parsebatteryname <- function(Filename) {
  Battery.ID <- gsub(".xls?x*","",Filename,ignore.case=T) #remove file extensions
  Battery.ID <- gsub(".csv","",Battery.ID,ignore.case=T)
  Battery.ID <- gsub(".txt","",Battery.ID,ignore.case=T)
  Battery.ID <- gsub("_"," ",Battery.ID,ignore.case=T) #normalize spacers
  Battery.ID <- gsub("-"," ",Battery.ID)
  #remove padding from IDs
  Battery.ID <- LCSrepeater(gsub("[[:digit:]]{10,} [A-E][[:digit:]]{1,2} ",
                                 "",trimws(Battery.ID)))
  Battery.ID <- sort(gsub("Test ","",Battery.ID))
  #calculate dissimilarity matrix for types
  Type <- adist(sort(Battery.ID))
  #cut dissimilarity matrix to a height of 1 mutation (maybe should be 2?)
  Type <- cutree(hclust(as.dist(Type)),h=1)
  #ensure that there is no mismatch between factors and numbers
  Battery.ID <- suppressWarnings(vapply(Battery.ID,function(x){
    if(!is.na(as.numeric(x))) paste0("Bat",x)
    else x
  },"x"))
  parsed <- data.frame(Filename,Battery.ID,Type=as.factor(Type))
  typenames <- unlist(by(Battery.ID,Type,function(x) LCS(x,T)[1]))
  #try different things to find type names
  if(sum(sapply(typenames,is.na))) {
    typenames <- unlist(by(Battery.ID,Type,function(x) LCS(gsub("[^[:alpha:]]","",x),T)[1])) 
  }
  if(sum(sapply(typenames,is.na))) typenames <- Battery.ID
  levels(parsed$Type) <- typenames
  parsed
}

#repeats LCS algorithm until there is a match or we run out of iterations
LCSrepeater <- function(strings, pattern=F) {
  ni <- 0 #this looks wrong but it's right
  x <- character(0)
  while(!length(x) & ni<(min(nchar(as.character(strings)))-2)){
    ni <- ni + 1
    x <- LCS(strings,T,ni)
  }
  LCS(strings,pattern,ni)
}

#my attempt at solving the k-common longest common subsequence problem
#rather than finding something elegant, we will use the shotgun approach:
#blast strings into bits until something happens
#this won't win awards, but it is surprisingly effective
LCS <- function(strings, pattern=F, dampener=1) { 
  types <- length(strings)
  if(types==1) return(strings) #can't find a pattern if k=1
  trafos <- drop(attr(adist(strings, counts=TRUE), "trafos"))
  if(min(nchar(as.character(strings)))<=2) {
    poss.pat <- do.call(rbind,stri_locate_all_regex(trafos,"M+"))
  } else {
    poss.pat <- do.call(rbind,
                        stri_locate_all_regex(trafos, paste(c("M{2,",
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