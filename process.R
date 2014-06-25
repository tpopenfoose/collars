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
#pchar = 5:(5+length(unique(df$ExpDate)))
#col <- lattice.theme$superpose.symbol$col
xyplot(Close~Strike|Ticker,data=df,scales=list(relation="free"))
xyplot(Close~Strike|Ticker+Option,data=df,
       scales=list(relation="free"),
       group=ExpDate,
       auto.key=list(columns=5))

#trellis.par.set(theme = col.whitebg())
#par.line <- trellis.par.get("superpose.line")
#par.symb <- trellis.par.get("superpose.symbol")
# n <- seq(length(unique(df$ExpDate)))
# par.line 
# par.symb <- n
# col <- brewer.pal(length(n),"YlOrRd")

# xyplot(Close~Strike|Ticker+Option,data=df,
#        scales=list(relation="free"),
#        group=ExpDate,
#                key=list(text=list(as.character(unique(df$ExpDate))), 
#                         space='top',
#                         border=FALSE,
#                         cex.title=1.2,
#                         title="Expiration",
#                         # size=7,
#                         points=list(pch=par.symb$pch[n],
#                                     col=par.line$col[n]
#                                     #lty=par.line$lty[n],
#                                     #type='b'),
#                         ),
#                         columns=5),
#        pch=par.symb$pch[n],
#        type='p')


# just one ticker
xyplot(Close~Strike|ExpDate,
       data=df,
       subset=Ticker=="SPY",
       main="SPY Expiration",
       group=Option,
       auto.key=list(columns=2)
       )

xyplot(OI~Strike|ExpDate,
       data=df,
       subset=Ticker=="SPY",
       main="SPY Open Interest by Expiration",
       group=Option,
       auto.key=list(columns=2)
)

# just SPY
dfspy = filter(df,Ticker=="SPY")

# find the available strikes by expiration date
strikes = dfspy %.% group_by(ExpDate) %.% summarise(paste(unique(Strike),collapse=' '))

# build a data frame of collar prices
collars = NULL
apply(strikes,1,function(d){
  expiration = d[1]
  slist = strsplit(as.character(d[2]),' ')
  sapply(slist[[1]],function(sc){
    s = as.numeric(sc)
    dfspyc = filter(dfspy,ExpDate==expiration,Option=='Call',Strike==s)
    dfspyp = filter(dfspy,ExpDate==expiration,Option=='Put',Strike==s)
    
    price = NA
    if ( nrow(dfspyc) == nrow(dfspyp)) {
      price = dfspyp$Close - dfspyc$Close
    }
    
    print(paste("binding",expiration,s,price))
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

xyplot(Price~ExpDate,data=collars,group=Strike,auto.key=FALSE,main="SPY Collar Prices")

xyplot(Price~ExpDate,data=collars,subset=Strike==atm,auto.key=FALSE,
       main=paste("SPY Collar Prices: ATM",atm),
       panel=function(x,y,...){
         panel.xyplot(x,y,...)
         panel.rug(x,y,...)
       })



