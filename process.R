require(utils)
require(lattice)
require(dplyr)
require(reshape2)
require(RColorBrewer)

# setup
tickers = "SPY|TLT" # regex pattern for grep
f = "/Users/mrb/Desktop/OPRA_AMEX_20140624.txt"

# read the data file into a data frame
df=read.table(f,header=TRUE,sep=',',stringsAsFactors=FALSE)
colnames(df) <- c("Name","ID","Date","Open","High","Low","Close","Volume","OI")

# select just the tickers of interest before exploding frame
df = df[grep(tickers,df$ID),]

# split the description into useful columns
df = transform(df, 
               XP = colsplit(Name, 
                        pattern = ' ', 
                        names = c('Ticker','Expiration','Strike','Option')))
colnames(df) = gsub("XP\\.","",colnames(df))

# transform the expiration date string into real date
df = df %.% 
  mutate(ExpDate=as.Date(Expiration,"%d-%B-%Y")) %.% 
  select(-Name)

# plots
xyplot(Close~Strike|Ticker,
       data=df,
       scales=list(relation="free"))
xyplot(Close~Strike|Ticker+Option,data=df,
       scales=list(relation="free"),
       group=ExpDate,
       auto.key=list(columns=5))

# just one ticker
ticker = "SPY"
xyplot(Close~Strike|ExpDate,
       data=df,
       subset=Ticker==ticker,
       main=paste(ticker,"Expirations"),
       group=Option,
       auto.key=list(columns=2)
       )

xyplot(OI~Strike|ExpDate,
       data=df,
       subset=Ticker==ticker,
       main=paste(ticker,"Open Interest by Expiration"),
       group=Option,
       ylab="Open Interest",
       auto.key=list(columns=2),
       panel=function(x,...){
         panel.xyplot(x,...)
         panel.grid(-1,-1)
       }
)

# extract prices
dfp = filter(df,Ticker==ticker)

# find the available strikes by expiration date
strikes = dfp %.% group_by(ExpDate) %.% summarise(paste(unique(Strike),collapse=' '))

# build a data frame of collar prices
collars = NULL
apply(strikes,1,function(d){
  expiration = d[1]
  slist = strsplit(as.character(d[2]),' ')
  sapply(slist[[1]],function(sc){
    s = as.numeric(sc)
    dfpc = filter(dfp,ExpDate==expiration,Option=='Call',Strike==s)
    dfpp = filter(dfp,ExpDate==expiration,Option=='Put',Strike==s)
    
    price = NA
    if ( nrow(dfpc) == nrow(dfpp)) {
      price = dfpp$Close - dfpc$Close
    }
    
    collars <<- rbind(collars,
                    data.frame(ExpDate=as.Date(expiration),
                               Strike=s, 
                               Price=price,
                               row.names=NULL,
                               stringsAsFactors=FALSE)
                    )    
  })
  invisible()
})

# plot a particular strike
atm = 195

xyplot(Price~ExpDate,
       data=collars,
       group=Strike,
       auto.key=FALSE,
       main=paste(ticker,"Collar Prices"),
       xlab="Expiration Date",
       ylab="Price ($)",
       panel=function(x,...){
         panel.xyplot(x,...)
         panel.grid(-1,-1,...)
       }
       )

bwplot(~Price|ExpDate,
          data=collars,
          auto.key=FALSE,
          main=paste(ticker,"Collar Prices"),
          xlab="Price ($)"
)

stripplot(~Price|ExpDate,
       data=collars,
       group=Strike,
       auto.key=FALSE,
       main=paste(ticker,"Collar Prices"),
       xlab="Price ($)"
)

xyplot(Price~ExpDate,
       data=collars,
       subset=Strike==atm,
       auto.key=FALSE,
       main=paste(ticker,"Collar Prices: ATM",atm),
       xlab="Expiration Date",
       ylab="Price ($)",
       panel=function(x,y,...){
         panel.xyplot(x,y,...)
         panel.grid(-1,-1,...)
         panel.rug(x,y,...)
       })



