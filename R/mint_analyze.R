# This script is intended to analyze data from Mint.com

# **************************************************************************** #
# ***************                Directory Variables           *************** #
# **************************************************************************** #

# Set working directory
  setwd("C:/Users/lemasd/Dropbox/DL_MC/Finance/Mint.com_Tracking/2013/")
  list.files()
# Clear the slate #
  rm(list=ls())
  
# **************************************************************************** #
# ***************                Input Data                    *************** #
# **************************************************************************** #
  
mint=read.csv(file="ALL_Transactions.csv",as.is=T, header=T,dec='.',na.string="")
mint_cats=read.csv(file="mint_cats.csv",as.is=T, header=T,dec='.',na.string="")

# **************************************************************************** #
# ***************                Library                       *************** #
# **************************************************************************** #
  #install.packages("lubridate")
    library(lubridate)
  
  #install.packages("plyr")
  library("plyr")  
  
# **************************************************************************** #
# ***************                Time Stamp                    *************** #
# **************************************************************************** #
  
now=Sys.Date()
today=format(now, format="%d%b%y")
  
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
  
  
  
  