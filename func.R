##### Function to automatically load packages i use regularly ######
.First <- function(x){
  library(MASS)
  library(tidyverse)
  library(magrittr)
  library(Hmisc)
  library(psych)
  library(jtools)
  library(readr)
  library(lubridate)
  library(parallel)
  library(mirt)
  library(here)
}

####mean.n function####
#similar to mean.n in SPSS
#can generate scale means that are a little more lenient with NAs
#for example if you have a 15-item scale and you have a participant that completed only 14
#of those items, rowMeans() command would yield an NA for that observation
#the "n" in this function should be the number of NAs you will allow
#df is a dataframe of the items in the scale
#Example: mean.n(df,3) will generate a vector that is the mean of each row in the data frame as long as
#there are no more than 3 NAs

mean.n<-function(df,n){
  means <- apply(as.matrix(df), 1, mean, na.rm = TRUE)
  notvalid <- apply(as.matrix(df), 1, function(df) sum(is.na(df)))
  ifelse(notvalid <= n, means, NA)
}

#### Search Function ####
#found this function online
#example:varlist(data,pattern="hsns",exclude="5|oj")
#above example identifies all variable names that include 'hsns' but
#excludes those that have a 5 or "oj". Pipe key "|" is the "or" operator
varlist <- function (df=NULL,type=c("numeric","factor","character"), pattern=NULL, exclude=NULL, ignore.case=TRUE) {
  vars <- character(0)
  if (any(type %in% "numeric")) {
    vars <- c(vars,names(df)[sapply(df,is.numeric)])
  }
  if (any(type %in% "factor")) {
    vars <- c(vars,names(df)[sapply(df,is.factor)])
  }
  if (any(type %in% "character")) {
    vars <- c(vars,names(df)[sapply(df,is.character)])
  }
  if(!is.null(exclude)){
    if(ignore.case==TRUE){
      list<-vars[(!vars %in% vars[grepl(vars,pattern=exclude,ignore.case=TRUE)]) & grepl(vars,pattern=pattern, ignore.case=TRUE)]
    }
    else{
      list<-vars[(!vars %in% vars[grepl(vars,pattern=exclude,ignore.case=FALSE)]) & grepl(vars,pattern=pattern, ignore.case=FALSE)]
    }
  }
  if(is.null(exclude)){
    if(ignore.case==TRUE){
      list<-vars[(!vars %in% exclude) & grepl(vars,pattern=pattern, ignore.case=TRUE)]
    }
    else{
      list<-vars[(!vars %in% exclude) & grepl(vars,pattern=pattern, ignore.case=FALSE)]
    }
  }
  list
}

#More examples of how varlist function works, df = dataframe object:
#can also look up data on the grepl function, all of that should work
## All variable starting with cred:
# varlist(df,pattern="^cred")

### All variables labeled neo followed by any number (e.g., 'neo1','neo2', etc.):
# varlist(df,pattern="neo[1-9]")

## All numeric variables:
# varlist(df,type="numeric")

## All factor variable except variable gb and variables starting with c:
# varlist(df,type="factor",exclude=c("gb|^c"))

## Can use in conjunction with sapply:
# sapply(df[,varlist(df,type="numeric",pattern="credit")], summary)

search <- function(x = ''){
  varlist(df, pattern = x)
}

#####cor function####
#the skeleton of this function I got from a different student here. It gives flagged correlations.
#I modified a little so if you find it doesn't work for one purpose or another it is probably my fault.
#for the most part you can ignore this function because it just feeds into my r_table() function that is below.

cor_table<- function(x,vars=NULL,with=NULL,flag=TRUE,strict=FALSE,round = 2){
  require(Hmisc,warn.conflicts=TRUE)
  data<-as.matrix(x)
  Rmat <- rcorr(data)
  RmatP <- Rmat$P
  ComRmat <- round(Rmat$r, 5)
  originalRmat<-Rmat$r
  ComRPmat <- round(Rmat$P,3)
  ComRnmat<- Rmat$n
  
  if(round == 2){
    numformat <- function(val) { sub("^(-?)0.", "\\1.", sprintf("%.2f", val)) } #remove leading zero function
  }
  
  if(round == 3){
    numformat <- function(val) { sub("^(-?)0.", "\\1.", sprintf("%.3f", val)) } #remove leading zero function
  }
  
  if(round == 4){
    numformat <- function(val) { sub("^(-?)0.", "\\1.", sprintf("%.4f", val)) } #remove leading zero function
  }
  
  if(round == 1){
    numformat <- function(val) { sub("^(-?)0.", "\\1.", sprintf("%.f", val)) } #remove leading zero function
  }
  
  ComRmat<-matrix(numformat(ComRmat),ncol=ncol(ComRmat),nrow=nrow(ComRmat),dimnames = list(rownames(ComRmat),colnames(ComRmat)))
  
  #this for statement changes the NAs in the p-value table to 1's
  #if else statements are in the form of: ifelse(test, yes, no)
  for(i in 1:nrow(ComRmat)) {
    for (g in 1:ncol(ComRmat)){
      ifelse(is.na(ComRPmat[i,g]), ComRPmat[i,g] <- 1, ComRPmat[i,g] <- ComRPmat[i,g])
    }
  }
  
  ComRmatFin <- matrix(nrow=nrow(Rmat$r), ncol=ncol(Rmat$r)) #makes an empty matrix of correct size
  
  if(flag==TRUE){
    if(strict==FALSE){
      for(i in 1:nrow(ComRmat)) {
        for (g in 1:ncol(ComRmat)){
          ifelse(ComRPmat[i,g] <= .01, ComRmatFin[i,g] <- paste(ComRmat[i,g], '**',sep=""),
                 ifelse(ComRPmat[i,g] <= .05, ComRmatFin[i,g] <- paste(ComRmat[i,g], '*',sep=""),
                        ifelse(ComRPmat[i,g] <= .1, ComRmatFin[i,g] <- paste(ComRmat[i,g], "'",sep=""),
                               ifelse(i==g,ComRmatFin[i,g]<-paste('n=',ComRnmat[i,g],sep=""),
                                      ComRmatFin[i,g] <- ComRmat[i,g]))))
        }
      }
    }
    if(strict==TRUE){
      for(i in 1:nrow(ComRmat)) {
        for (g in 1:ncol(ComRmat)){
          ifelse(ComRPmat[i,g] <= .01, ComRmatFin[i,g] <- paste(ComRmat[i,g], '*',sep=""),
                 ifelse(i==g,ComRmatFin[i,g]<-paste('n=',ComRnmat[i,g],sep=""),
                        ComRmatFin[i,g] <- ComRmat[i,g]))
        }
      }
    }
  }
  
  if(flag==FALSE){
    
    for(i in 1:nrow(ComRmat)) {
      for (g in 1:ncol(ComRmat)){
        ComRmatFin[i,g] <- ComRmat[i,g]
      }
    }
  }
  
  ComRmatFin <- as.data.frame(ComRmatFin,stringsAsFactors=FALSE)
  
  ComRPmat<-as.data.frame(ComRPmat)
  ComRnmat<-as.data.frame(ComRnmat)
  originalRmat<-as.data.frame(originalRmat)
  
  names(ComRmat)<-colnames(ComRmat)
  row.names(ComRmat)<-row.names(ComRmat)
  
  names(originalRmat)<-colnames(ComRmat)
  row.names(originalRmat)<-row.names(ComRmat)
  
  names(ComRmatFin) <- colnames(ComRmat)
  row.names(ComRmatFin) <- row.names(ComRmat)
  
  names(ComRPmat) <- colnames(ComRmat)
  row.names(ComRPmat) <- row.names(ComRmat)
  
  names(ComRnmat) <- colnames(ComRmat)
  row.names(ComRnmat) <- row.names(ComRmat)
  
  if(!is.null(vars)&is.null(with)){
    ComRmatFin<-t(ComRmatFin[vars,colnames(ComRmat)[!colnames(ComRmat)%in%vars],drop=FALSE])
    ComRPmat<-t(ComRPmat[vars,colnames(ComRmat[!colnames(ComRmat)%in%vars]),drop=FALSE])
    ComRnmat<-t(ComRnmat[vars,colnames(ComRmat[!colnames(ComRmat)%in%vars]),drop=FALSE])
    originalRmat<-t(originalRmat[vars,colnames(ComRmat[!colnames(ComRmat)%in%vars]),drop=FALSE])
    ComRmat<-t(ComRmat[vars,colnames(ComRmat[!colnames(ComRmat)%in%vars]),drop=FALSE])
  }
  if(is.null(vars)&!is.null(with)){
    ComRmatFin<-ComRmatFin[colnames(ComRmat)[!colnames(ComRmat) %in% with],with,drop=FALSE]
    ComRPmat<-ComRPmat[colnames(ComRmat)[!colnames(ComRmat) %in% with],with,drop=FALSE]
    ComRnmat<-ComRnmat[colnames(ComRmat)[!colnames(ComRmat) %in% with],with,drop=FALSE]
    originalRmat<-originalRmat[colnames(ComRmat)[!colnames(ComRmat) %in% with],with,drop=FALSE]
    ComRmat<-ComRmat[colnames(ComRmat)[!colnames(ComRmat) %in% with],with,drop=FALSE]
    
  }
  if(!is.null(vars)&!is.null(with)){
    ComRmatFin<-ComRmatFin[vars,with,drop=FALSE]
    ComRPmat<-ComRPmat[vars,with,drop=FALSE]
    ComRnmat<-ComRnmat[vars,with,drop=FALSE]
    originalRmat<-originalRmat[vars,with,drop=FALSE]
    ComRmat<-ComRmat[vars,with,drop=FALSE]
    
  }
  list(r_table=ComRmatFin,cors=originalRmat,p_table=ComRPmat,n_table=ComRnmat)
  
}


#### Package Coding for r_table function ####
#this is the function I use for correlations, it does what the cor_table function does but it is coded like a
#package so it can present a little cleaner output, although I rarely use it for that purpose.

# x has to be a dataframe or matrix (something that can be coerced to matrix)
# Example for use: r_table(test) - includes flags by default
# the diagonal gives the N for each variable

#the "with" command I use for an abbreviated table
#it works the same way as 'with' in the syntax of SPSS if you have ever used that
#basically, if you just want correlate the neo domains with all of your outcomes,
#r_table(df,with=c("n","e","o","a","c")) would give you a table with 5 columns showing the correlations
#of the NEO domains with your outcomes, but not any of the correlations of the neo domains with each other.
# Example: r_table(df,with=c("n","e","o","a","c")) - or - r_table(df, with = names(df[,1:5])) -
# or - r_table(df, with = varlist(df,pattern="hsns"))

# can't really remember how I was intending to use the "vars" command,
# just tried it and it wasn't really what I was expecting
# so I might have fucked that part of the code up

# flag you can use false if you want to remove the flags e.g. r_table(test,with=c("a","b"),flag=FALSE)
# can also just call the original cor_table output:
# r_table(df, with = c('var1','var2','var3'))$cors
# that is actually the way I typically use this function and the way I recommend using it if you are
# going to just be pasting into excel as I typtically am.


r_table<- function(x, #data frame that can be coerced to a matrix
                   vars=NULL, #select the variables to be used
                   with=NULL, #the variables that will appear across the top
                   flag=TRUE, #flag significance
                   strict=FALSE, #If TRUE, it will use .01 cutoff for significance
                   round = 2,...) #how many decimal places to round to when printing
  UseMethod("r_table")

r_table.default<-function(x, #data frame that can be coerced to a matrix
                          vars=NULL, #select the variables to be used
                          with=NULL, #the variables that will appear across the top
                          flag=TRUE, #flag significance
                          strict=FALSE, #If TRUE, it will use .01 cutoff for significance
                          round = 2,...) #how many decimal places to round to
{
  r_table<-cor_table(x,vars=vars,with=with,flag=flag,strict=strict,round=round)
  class(r_table)<-"r_table"
  r_table
}

print.r_table<-function(x,...){         #removed p-values table from print.r_table
  cat("Correlation Table:\n\n")
  print(x$r_table)
  cat("\nIf strict = FALSE: .01 **; .05 *\nIf strict = TRUE: .01 *\n")
}

summary.r_table<-function(x,...){
  cat("Correlation Table:\n\n")
  print(x$r_table)
  cat("\nIf strict = FALSE: .01 **; .05 *\nIf strict = TRUE: .01 *\n")
}

#### Number Format ####
#don't use this one much anymore, found it online, it can be used to remove leading zeros when printing tables
#but it turns them into character vectors so it can lead to problems, I think I have only ever used it within
#the context of the r_table function.
numformat <- function(val) { sub("^(-?)0.", "\\1.", sprintf("%.2f", val)) } #remove leading zero

#### NAs per row ####
#quick function that I load into all of my projects were identifying the number of NAs in a row
numNAs<-function(x){
  sum(is.na(x))
}

##### Copy dataframe to Excel through clipboard #######
write.excel <- function(x,row.names=FALSE,col.names=TRUE,...) {
  write.table(x,"clipboard-10000",sep="\t",row.names=row.names,col.names=col.names,...)
}

#if dataframe is df:
#write.excel(df)
#then ctrl + v into excel spreadsheet

##### Copy data from Excel to R through clipboard #######
read.excel <- function(header=TRUE,...) {
  read.table("clipboard",sep="\t",header=header,...)
}

#after copying to clipboard
# df <- read.excel()

##### calculate size of correlation necessary to reach particular level of significance ######

r.crit <- function(tails = 2, p = .01, n){
  t.crit <- qt(1-(p/tails),n-2)
  r.crit <- sqrt(((t.crit)^2)/((n-2)+((t.crit)^2)))
  return(r.crit)
}

###### remove entirely missing rows from a dataframe ######

remove.missing.rows <- function(df){
  all.missing.rows <- which(apply(df,1,numNAs) == length(df))
  cat('removed ', length(all.missing.rows), ' observations: ', all.missing.rows, "\n")
  if(length(all.missing.rows) == 0){
    new.df <- df
    cat('returned original dataframe', '\n')
  }
  else{
    new.df <- df[-c(all.missing.rows),]
    cat('returned new dataframe', '\n')
  }
  list(df = new.df, old.df = df, missing.rows = all.missing.rows)
}

#### trim leading and trailing spaces from character vectors ######
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

###### read a .csv downloaded from qualtrics ###########
#found the base of this function online

read.qualtrics.csv <- function(filename) {
  require(readr)
  n <- read.csv(filename, nrows = 1)
  dat <- read_csv(filename, col_names = FALSE, skip = 3)
  names(dat) <- names(n)
  for(i in seq_along(dat)) {
    attr(dat[,i], "question") <- n[1,i]
  }
  dat <- select(dat, -c(ResponseId,
                        RecipientLastName,
                        RecipientFirstName,
                        RecipientEmail,
                        ExternalReference,
                        IPAddress,
                        Status,
                        LocationLatitude,
                        LocationLongitude,
                        DistributionChannel,
                        UserLanguage))
  dat <- rename(dat, duration = Duration..in.seconds., WorkerId = dem, code = Random.ID)
  dat$WorkerId <- trim(dat$WorkerId)
  dat
}

###### read mturk .csv ###########
read.mturk.csv <- function(filename) {
  require(readr)
  dat <- read_csv(filename)
  dat <- select(dat, c(AssignmentId, WorkerId, AcceptTime, SubmitTime, WorkTimeInSeconds, Answer.surveycode, Approve, Reject))
  dat$WorkerId <- trim(dat$WorkerId)
  dat$AcceptTime <- strptime(dat$AcceptTime,format = '%a %b %d %T', tz = 'America/Los_Angeles')
  dat$SubmitTime <- strptime(dat$SubmitTime,format = '%a %b %d %T', tz = 'America/Los_Angeles')
  dat <- rename(dat, duration = WorkTimeInSeconds)
  dat <- rename(dat, code = Answer.surveycode)
  dat
}

### Compute CFI from fa() output #####
#retreived from https://gist.github.com/tonosan/cb7581f3459ae7c4217a
#formula seems to match that provided by David A. Kenny on his website
fa.CFI<-function(x){
  nombre<-paste(x,"CFI",sep = ".")
  nombre<-
    ((x$null.chisq-x$null.dof)-(x$STATISTIC-x$dof))/(x$null.chisq-x$null.dof)
  return(nombre)
}

### IRT M2 function ####
m2.stats <- function(model, factors, itemtype){
  scores<-fscores(model,method='EAP',full.scores=TRUE,scores.only=TRUE) #EAP estimation method for the scores
  fulldataframe<-imputeMissing(model,scores) #just imputing  the data one time
  fmodel<-mirt(fulldataframe,factors,itemtype, technical = list(removeEmptyRows = TRUE)) #save imputed dataset
  m2<-M2(fmodel) #save M2 statistics
  return(m2)
}

###### remove entirely missing rows from a dataframe ######
remove.missing.rows <- function(df){
  all.missing.rows <- which(apply(df,1,numNAs) == length(df))
  cat('removed ', length(all.missing.rows), ' observations: ', all.missing.rows, "\n")
  if(length(all.missing.rows) == 0){
    new.df <- df
    cat('returned original dataframe', '\n')
  }
  else{
    new.df <- df[-c(all.missing.rows),]
    cat('returned new dataframe', '\n')
  }
  list(df = new.df, old.df = df, missing.rows = all.missing.rows)
}

#### Paste IRT results into Modfit program - needs to be modified ####
# I think we need to divide by the constant

paste.modfit <- function(data, model.results){
  cat('\n','Recoding data','\n')
  min <- min(data, na.rm = T)
  if(min != 0){
    data <- data - min
    cat('\n','Response values must start at 0','\n')
    cat('\n','Subtracted ',min,' from data','\n')
    cat('\n','Current minimum','\n')
    print(apply(data, 2, min))
    cat('\n','Current maximum','\n')
    print(apply(data, 2, max))
  }
  if(numNAs(data)>0){
    all.missing.rows <- which(apply(data,1,numNAs) == length(data))
    if(length(all.missing.rows) > 0){
      data <- remove.missing.rows(data)
      data <- data$df
    }
    cat('\n','Recoded ',numNAs(data), ' missing data points to 9','\n')
    data[is.na(data)] <- 9
  }
  cat('\n','Number of items: ', length(data),'\n')
  cat('\n','Number of persons: ', nrow(data),'\n')
  coefs <- coef(model.results, simplify=TRUE, IRTpars = T)
  write.excel(coefs$items, row.names = F, col.names = F)
  cat("\n","\bPaste item parameters","\n")
  cat("\n","\bEnter 1 when complete","\n")
  continue <- scan(n=1, what = numeric(0), quiet = T)
  if(continue == 1){
    write.excel(data, row.names = F, col.names = F)
    cat("\n","\bPaste item responses","\n")
  }
}

#critical.t <- function(){
#  cat("\n","\bEnter Alpha Level","\n")
#  alpha<-scan(n=1,what = double(0),quiet=T)
#  cat("\n","\b1 Tailed or 2 Tailed:\nEnter either 1 or 2","\n")
#  tt <- scan(n=1,what = double(0),quiet=T)
#  cat("\n","\bEnter Number of Observations","\n")
#  n <- scan(n=1,what = double(0),quiet=T)
#  cat("\n\nCritical Value =",qt(1-(alpha/tt), n-2), "\n")
#}