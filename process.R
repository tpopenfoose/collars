require(utils)
require(lattice)
require(dplyr) # group_by, summarise, filter, select and arrange

# setup
tickers = c("SPY")

# read the data file into a data frame
f = "/Users/mrb/Desktop/OPRA_AMEX_20140624.txt"
df=read.table(f,header=TRUE,sep=',',stringsAsFactors=FALSE) #,row.names=2)
colnames(df) <- c("Name","ID","Date","Open","High","Low","Close","Volume","OI")

# terms = split(df$Name," ")

# split the contract specification into column terms
df = df %.% 
  mutate(Ticker=strsplit(Name," ")[[1]][1]) %.%
  mutate(Expiration=strsplit(Name," ")[[1]][2]) %.% 
  mutate(ExpDate=as.Date(Expiration,"%d-%B-%Y")) %.% 
  mutate(Strike=strsplit(Name," ")[[1]][3]) %.% 
  mutate(Option=strsplit(Name," ")[[1]][4])
df = select(df,-Name) %.%
  filter(Ticker %in% tickers)


