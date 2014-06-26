require(utils)
require(lattice)
require(dplyr)
require(reshape2)

# setup
tickers = "SPY|TLT" # regex pattern for grep
paths = "/Users/mrb/Desktop/Options/*.txt"
files = Sys.glob(paths)

# read the data file into a data frame
df=do.call(rbind,lapply(files,function(f){
  dff=read.table(f,header=TRUE,sep=',',stringsAsFactors=FALSE)
  colnames(dff) <- c("Name","ID","Date","Open","High","Low","Close","Volume","OI")
  dff
}))

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
       group=Date,
       scales=list(relation="free"),
       auto.key=list(columns=min(5,length(unique(df$Date)))))

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

xyplot(Close~Strike|Option,
       data=df,
       subset=Ticker==ticker,
       main=paste(ticker,"Trading Days"),
       group=Date,
       auto.key=list(columns=min(5,length(unique(df$Date))))
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
strikes = dfp %.% 
  group_by(ExpDate,Date) %.% 
  summarise(paste(unique(Strike),collapse=' '))

# build a data frame of collar prices
collars = NULL
apply(strikes,1,function(d){
  expiration = d[1]
  date = d[2]
  slist = strsplit(as.character(d[3]),' ')
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
                                 Date=as.Date(date,format="%Y%m%d"),
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

xyplot(Price~ExpDate|Date,
       data=collars,
       group=Strike,
       auto.key=FALSE,
       main=paste(ticker,"Collar Prices by Trade Date"),
       xlab="Expiration",
       ylab="Price ($)",
       panel=function(x,...){
         panel.xyplot(x,...)
         panel.grid(-1,-1,...)
       }
)

bwplot(Date~Price|ExpDate,
       data=collars,
       auto.key=FALSE,
       main=paste(ticker,"Collar Prices by Expiration"),
       xlab="Price ($)"
)

stripplot(Date~Price|ExpDate,
          data=collars,
          group=Strike,
          auto.key=FALSE,
          main=paste(ticker,"Collar Prices by Expiration"),
          xlab="Price ($)"
)

xyplot(Price~ExpDate,
       data=collars,
       group=Date,
       subset=Strike==atm,
       auto.key=list(columns=min(5,length(unique(collars$Date)))),
       main=paste(ticker,"Collar Prices: ATM",atm),
       xlab="Expiration Date",
       ylab="Price ($)",
       panel=function(x,y,...){
         panel.xyplot(x,y,...)
         panel.grid(-1,-1,...)
         panel.rug(x,y,...)
       })

# time series
xyplot(Price~Date,
       data=collars,
       group=ExpDate,
       subset=Strike==atm,
       type='b',
       auto.key=list(columns=5),
       main=paste(ticker,"Collar Prices by Trading Date")
       )

collars$ExpDate = as.numeric(format(collars$ExpDate,
                                    format="%Y%m%d"))

# at this point make it wide by expdate
# TODO...

collars.xts = xts(collars[,c('ExpDate','Strike','Price')],
                  order.by=collars$Date)

# TODO not working, want Price by Date with ExpDate as group
xyplot(collars.xts[which(collars.xts$Strike==atm),c('Price')],          #group=collars.xts$ExpDate,
          #subset=collars.xts$Strike==atm,
          group=collars.xts$ExpDate,
          type='b')
