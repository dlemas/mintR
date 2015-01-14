# This script is intended to generate simulated data from MintR package.

# **************************************************************************** #
# ***************                Directory Variables           *************** #
# **************************************************************************** #

# Working directory
# setwd("C:/Users/lemasd/Dropbox/DL_MC/Finance/Mint.com_Tracking")
# list.files()
  
# **************************************************************************** #
# ***************                Input Data                    *************** #
# **************************************************************************** #
  
mint=read.csv(file="mint.testing.csv",as.is=T, header=T,dec='.',na.string="")
mint_cats=read.csv(file="mint_cats.csv",as.is=T, header=T,dec='.',na.string="")
mint.test=read.csv(file="mint_descriptions.csv",sep=",", header=T)
mint.budget=read.csv(file="mint_budget.csv",sep=",", header=T)

# **************************************************************************** #
# ***************                Library                       *************** #
# **************************************************************************** #
  #install.packages("lubridate")
    library(lubridate)
  
  #install.packages("plyr")
  library("plyr")  

# **************************************************************************** #
# ***************          Create Simulated mint.com data          *********** #
# **************************************************************************** #

names(mint)
names(mint_cats)
mint.budget$Category=tolower(as.character(mint.budget$Category))

unique(mint$Date)

# Create mint.com simulated data with 2300 transactions
dim(mint)

# Random distribution of transaction dates
#-----------------------------------------
# set start date and end dates to sample between
day.start <- "2012/01/11"
day.end <- "2013/07/13"

# define a random date/time selection function
rand.day.time <- function(day.start,day.end,size) {
  dayseq <- seq.Date(as.Date(day.start),as.Date(day.end),by="day")
  dayselect <- sample(dayseq,size,replace=TRUE)
  hourselect <- sample(1:24,size,replace=TRUE)
  minselect <- sample(0:59,size,replace=TRUE)
  as.POSIXlt(paste(dayselect, hourselect,":",minselect,sep="") )
}

Date=as.character(rand.day.time(day.start, day.end, 2300))
str(Date)

# Transaction Descriptions
#------------------------
description.list=as.character(mint.test$Description)
Description=sample(description.list,2300, replace=T)

# Create data.frame
data=as.data.frame(cbind(Date,Description))

# Original Description
#---------------------
data$Original.Description=NA

# Amount(s)
#----------
data$Amount="NA"
  
# Categories
#----------
category.list=as.character(names(mint_cats))
data$Category=as.character(sample(category.list, 2300, replace=T))
head(data)

# Transaction
#-----------
data$Transaction="NA"

# Transaction.Type
data$Transaction.Type="NA"

# Account.Name
#-------------
data$Account.Name="NA"

# Labels
#------
data$Labels="NA"

# Notes
#------
data$Notes="NA"

# Reformat data.frame
#===================
str(data)
data$Description=as.character(data$Description)


# Define the Category Amounts
#=============================

  # Matching
  #----------
    # list of categories in data
      cats.data=unique(data$Category);length(cats.data)
    # list of categories from budget
      cats.budget=as.character(mint.budget$Category);length(cats.budget)
    # Metching
      intersect(cats.data,cats.budget)
    # Not-Matching

      # Loop thru Categories/Budget
          # Create Index
            col_index=as.character(mint.budget$Category);col_index
            col_budget=as.numeric(mint.budget$Budget);col_budget
            MyIndex=length(col_index)

      # Start the Loop
        for (i in 1:MyIndex){
            cat.len=length(which(data$Category==col_index[i]))
            data$Amount=ifelse(data$Category==col_index[i],rnorm((cat.len), mean=col_budget[i], sd=col_budget[i]*0.2), data$Amount)
                            } # End Loop
    # Rounding # *** START HERE *** Deal with missing values
    data$Amount=round(data$Amount, digits=2)



# **************************************************************************** #
# ***************     Dummy Code Mint.com Output with Categories   *********** #
# **************************************************************************** #
names(mint_cats)
head(mint)
    
mint.cats=function(df, mint_cats){
              # Create index for loops
              index=df$Category
              groups=(names(mint_cats))
              myIndex<-length(groups) 
  
              # Start the Loop
              for (i in 1:myIndex)
                  {
                      # create indexes
                      col=groups[i]
                      head(index)
                      list=mint_cats[,col]
                      df[,col]=0
                      df[,col][which(df$Category%in%list)]=0+i    
                      df[,col]      
                  }  # End loop
              
              # Create overall variable
              cols.add=names(mint_cats)
              # Parameters for rowSum
              cols.add.length=length(cols.add)
              first.col=cols.add[1]
              last.col=cols.add[cols.add.length]
              # rowSums columns in df
              which.first.col=which(names(df)==first.col)
              which.last.col=which(names(df)==last.col)
              # create variable
              df$overall=rowSums(df[which.first.col:which.last.col])
              # Create factored variable
              df$overall=ifelse(df$overall==0,"uncatagorized",df$overall)
              df$overall=as.factor(df$overall)
              
            
              # Loop through to change factor levels to group names
              groups=(names(mint_cats))
              myIndex<-length(groups) 
              for (i in 1:myIndex)
              {
                levels(df$overall)[levels(df$overall)==i] <- groups[i]  
              }
                            
              # Few more modifications
              df$date=as.Date(df$Date,"%m/%d/%Y")
              
              # Dates
              #------
              # Create index for loops
              index=df$Date
              myIndex<-length(index) 
              
              # Start the Loop
              for (i in 1:myIndex)
              {
                
                # Time variables
                A=df$Date[i]
                model=unlist(strsplit(A,split="/"))
                
                df$month[i]=model[1]
                df$day[i]=model[2]
                df$year[i]=as.numeric(model[3])
              }
              # Structure variables
              df$month=as.numeric(df$month)
              df$day=as.numeric(df$day)
              df$year=as.numeric(df$year)
              
              return(df)
  } # End function
  
# Complete code for sum variable. dynamic and can be included inside the above loop. single function would prep data.
  
df2=mint.cats(mint,mint_cats)
names(df2)
str(df2)
  df2$overall
  df2$month

  
# **************************************************************************** #
# ***************                TIME DATA.FRAMES              *************** #
# **************************************************************************** #  
  # NOW Time variables
    now=as.Date(today, format="%d%b%y")
    now.split=as.character(now)
    now.s=unlist(strsplit(now.split,split="-"))
  # month of now
    now.month=as.numeric(now.s[2])
  # day of now
    now.day=as.numeric(now.s[3])
  # year of now
    now.year=as.numeric(now.s[1])
  # month_year
    mint$month_yr=paste(mint$year,mint$month,sep="_")
  # sort data.frame by date (recent --> past)
    mint<- mint[order(mint$date) , ]
    head(mint)
    str(mint)
  
# one week ago 
  week_ago_date=now-7
  mint.week_ago=subset(mint, date>week_ago_date)
  
# one month ago
  month_ago=now-months(1)
  mint.month_ago.1=subset(mint, date>month_ago)
  mint.month_ago=subset(mint.month_ago.1, month<now.month)
  range(mint.month_ago$month)
  
# last month
  month.last=now.month-1
  year.before=now.year-1    
  now.year.final=ifelse(now.month==1, year.before , now.year )
  mint.last_month.1=subset(mint,month==month.last & year==now.year.final)
  mint.last_month=subset(mint.last_month.1,month<now.month)
  range(mint.last_month$month)
  
# last three months
  three_mo.last=month.last-2
  mint.three_month.1=subset(mint,month>=three_mo.last & year==now.year.final)
  mint.three_month=subset(mint.three_month.1,month<now.month)
  range(mint.three_month$month)
  
# last six months
  six_mo.last=month.last-5
  mint.six_month.1=subset(mint,month>=six_mo.last & year==now.year.final)
  mint.six_month=subset(mint.six_month.1,month<now.month)
  range(mint.six_month$month)
  
# **************************************************************************** #
# ***************                ANALYSIS FUNCTIONS            *************** #
# **************************************************************************** #

  
# check data.frame features
  names(mint)
  head(mint)
  str(mint)
  levels(mint$overall)
  
# month totals for ALL data  
  totals=ddply(mint,~month_yr+month+year,summarise,total=sum(Amount))
  totals.s<- totals[order(totals$year, totals$month) , ]
  totals.s
  plot(totals.s$month,totals.s$total)
  str(mint)

# month totals BY catagory 
  totals=ddply(mint,~month_yr+month+year,summarise,total=sum(Amount))
  mint$overall=as.factor(mint$overall)
  total.cats=cbind(tapply(mint$Amount,list(mint$overall,mint$year), sum))
  total.cats
  names(mint)
  
# Year totals within a catagory (auto)
  mint.auto=subset(mint,auto>0)
  str(mint.auto)
  mint.auto$Category=as.factor(mint.auto$Category)
  mint.auto.month=cbind(tapply(mint.auto$Amount,list(mint.auto$Category,mint.auto$year),sum))
  mint.auto.month
  str(mint)
  
  # Function that totals category by year
    # pick cat: list possible cats below
      cats=as.vector(unique(mint$overall))
    # Run the function
      year.cat=function(cat){ #start function
          as.factor(cat)
          mint.cat=subset(mint,mint[cat]>0)
          mint.cat$Category=as.factor(mint.cat$Category)
          str(mint.cat)
          mint.cat.yr=cbind(tapply(mint.cat$Amount,list(mint.cat$Category,mint.cat$year),sum))
        return(mint.cat.yr1)
        }

  
  
    # pick data.frame: mint.last_month, mint.month_ago, mint.six_month, mint.three_month, mint.week_ago
      month_yr.cat=function(cat,df){ #start function
          as.factor(cat)
          mint.cat.month_yr=subset(df,df[cat]>0)
          mint.cat.month_yr$Category=as.factor(mint.cat.month_yr$Category)
          mint.cat.yr1=cbind(tapply(mint.cat.month_yr$Amount,list(mint.cat.month_yr$Category,mint.cat.month_yr$month_yr),sum))
        return(mint.cat.yr1)
        }
A=as.data.frame(month_yr.cat(cats[13],mint.six_month))
sort=A[,order(names(A))]
  
  
  
  