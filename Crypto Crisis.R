setwd("C:/Users/acher/crypto/")

exchange_list = c("Bitfinex","Bitstamp","Coinbase","Exmo","FTX","HitBTC","Kraken","OKcoin")
exchange_list_post = c("Bitfinex","Bitstamp","Coinbase","Exmo","HitBTC","Kraken","OKcoin")

#note: FTX stop in 11/12 and ETH is missing most of june and may
#kraken is in USDT for ETH only

coin = "BTC" #"ETH" #"BTC"
sd1 ="7/1/2022" #"11/13/2022"
sd2 ="11/12/2022"

##############functions###############################################################

#Read in coin metrics files
coiner<-function(coin,sd1,sd2){
  df=read.csv(paste(coin,'.csv',sep=''))
  names(df)[names(df) == "time"] <- "date"
  df$date <- as.Date(df$date)
  start=which(df$date==sd1)
  stop=which(df$date==sd2)
  df=df[start:stop,]
  df
}

#Output binary vector with 1 for jump day, 0 otherwise
jump_calc <- function(dfraw){
  dfmat=data.matrix(dfraw, rownames.force = NA)
  dfmat=t(dfmat)
  dfmat=log(dfmat)
  dfdiff=diff(dfmat)
  delta=1/nrow(dfmat)
  alpha=matrix(0, (nrow(dfmat)-1), ncol(dfmat))
  for (j in 1: ncol(dfmat)){for (i in 1: (nrow(dfmat)-1)){if (abs(dfdiff[i,j]) <= sqrt(delta)){alpha[i,j]=abs((dfdiff[i,j]))^2} else {alpha[i,j]=0}}} 
  alph_fin=5*sqrt(colSums(alpha)) 
  omega=0.47 
  BPD = colSums((abs(dfdiff))^4) 
  kfreq = (nrow(dfmat)+1)/2 
  data_10 = matrix(0, kfreq, ncol(dfmat)) 
  for (j in 1: ncol(dfmat)){for (i in 1:kfreq){data_10[i,j] = dfmat[(i-1)*2+1,j]}} 
  BPK = colSums((abs(diff(data_10)))^4) 
  SPK = BPK/BPD 
  trun_4 = matrix(0,nrow(dfmat)-1,ncol(dfmat)) 
  for (j in 1: ncol(dfmat)){for (i in 1:(nrow(dfmat)-1)){ 
    if (abs(dfdiff[i,j]) <= alph_fin[j]*delta^omega){trun_4[i,j] = abs((dfdiff[i,j]))^4}
    else {trun_4[i,j] = 0}}} 
  mp = pi^(-0.5)*4*gamma(5/2) 
  AP = (delta^(-1)/mp)*colSums(trun_4) 
  trun_8 = matrix(0, (nrow(dfmat)-1), ncol(dfmat)) 
  for (j in 1: ncol(dfmat)){for (i in 1: (nrow(dfmat)-1)){ 
    if (abs(dfdiff[i,j]) <= alph_fin[j]*delta^omega){trun_8[i,j] = abs((dfdiff[i,j]))^8} 
    else {trun_8[i,j] = 0}}} 
  mp_8 = pi^(-0.5)*16*gamma(9/2) 
  AP_8 = (delta^(-3)/mp_8)*colSums(trun_8) 
  Var = (delta* AP_8*160)/(3*AP^2) 
  ASJ = (2 - SPK)/sqrt(Var) 
  normASJ=pnorm(ASJ)
  jumpday = ifelse(normASJ > 0.95, 1, 0)
  jumpday[is.na(jumpday)] <- 0
  jumpday
}

#Read in exchange files and sample at interval
ex_read <- function(exchange,coin,interval,start_date,stop_date){
  raw=read.csv(file=paste0("raw/",exchange,"_",coin,"Full_Minute.csv"),header=TRUE)
  start=which(raw$Day==start_date)
  stop=which(raw$Day==stop_date)
  raw=raw[start:stop,]
  raw <- raw[, -c(1)]
  df = raw[, seq(1, ncol(raw), interval)]
  df
}

#realized volatility calculation
RV_calc <- function (raw){
  mat=data.matrix(raw, rownames.force = NA) 
  mat=t(mat)
  mat=log(mat)
  #calculate RV
  dif=diff(mat)
  RV=colSums((dif)^2)
  
  #vol_df=data.frame(RV)
  RV
} 

#compute number of jump days over a given interval
jump_days <- function (exchange,coin,interval,start_date,stop_date){
  dfraw=ex_read(exchange,coin,interval,start_date,stop_date)
  dfmat=data.matrix(dfraw, rownames.force = NA)
  dfmat=t(dfmat)
  dfmat=log(dfmat)
  dfdiff=diff(dfmat)
  delta=1/nrow(dfmat)
  alpha=matrix(0, (nrow(dfmat)-1), ncol(dfmat))
  for (j in 1: ncol(dfmat)){for (i in 1: (nrow(dfmat)-1)){if (abs(dfdiff[i,j]) <= sqrt(delta)){alpha[i,j]=abs((dfdiff[i,j]))^2} else {alpha[i,j]=0}}} 
  alph_fin=5*sqrt(colSums(alpha)) 
  omega=0.47 
  BPD = colSums((abs(dfdiff))^4) 
  kfreq = (nrow(dfmat)+1)/2 
  data_10 = matrix(0, kfreq, ncol(dfmat)) 
  for (j in 1: ncol(dfmat)){for (i in 1:kfreq){data_10[i,j] = dfmat[(i-1)*2+1,j]}} 
  BPK = colSums((abs(diff(data_10)))^4) 
  SPK = BPK/BPD 
  trun_4 = matrix(0,nrow(dfmat)-1,ncol(dfmat)) 
  for (j in 1: ncol(dfmat)){for (i in 1:(nrow(dfmat)-1)){ 
    if (abs(dfdiff[i,j]) <= alph_fin[j]*delta^omega){trun_4[i,j] = abs((dfdiff[i,j]))^4}
    else {trun_4[i,j] = 0}}} 
  mp = pi^(-0.5)*4*gamma(5/2) 
  AP = (delta^(-1)/mp)*colSums(trun_4) 
  trun_8 = matrix(0, (nrow(dfmat)-1), ncol(dfmat)) 
  for (j in 1: ncol(dfmat)){for (i in 1: (nrow(dfmat)-1)){ 
    if (abs(dfdiff[i,j]) <= alph_fin[j]*delta^omega){trun_8[i,j] = abs((dfdiff[i,j]))^8} 
    else {trun_8[i,j] = 0}}} 
  mp_8 = pi^(-0.5)*16*gamma(9/2) 
  AP_8 = (delta^(-3)/mp_8)*colSums(trun_8) 
  Var = (delta* AP_8*160)/(3*AP^2) 
  ASJ = (2 - SPK)/sqrt(Var) 
  normASJ=pnorm(ASJ)
  jumpday = ifelse(normASJ > 0.95, 1, 0)
  jumpday[is.na(jumpday)] <- 0
  sum(jumpday/length(jumpday))
}

#MedRV calculation
MedRV_calc <- function (raw){
  mat=data.matrix(raw, rownames.force = NA) 
  mat=t(mat)
  mat=log(mat)
  #calculate MedRV
  dif=diff(mat)
  dif11=dif[1:(nrow(mat)-3),] 
  dif22=dif[2:(nrow(mat)-2),] 
  dif33=dif[3:(nrow(mat)-1),] 
  medvec=matrix(0, (nrow(mat)-3), ncol(mat)) 
  for (j in 1:ncol(mat)){for (i in 1:(nrow(mat)-3)){medvec[i,j]=(median(c(abs(dif11[i,j]),abs(dif22[i,j]),abs(dif33[i,j]))))^2}} 
  MedRV=(pi/(6-4*sqrt(3)+pi))*(nrow(mat)/(nrow(mat)-2))*colSums(medvec)
  MedRV
} 

#Jump Table Create
table_create <-function(exch_list,intervals){
  df <- data.frame(matrix(ncol = length(intervals), nrow = length(exch_list)))
  col1 <- paste0(intervals[1]," min")
  col2 <- paste0(intervals[2]," min")
  col3 <- paste0(intervals[3]," min")
  col = do.call(c, list(col1,col2,col3))
  col = as.character(col)
  colnames(df) <- col
  rownames(df) <- exch_list
  df
}

##############Create RV###############################################################

sd1 ="7/1/2022" #"11/13/2022"
sd2 ="11/12/2022"
Day <- seq(as.Date("2022-07-01"), as.Date("2022-11-12"), by = "day")
Vol_list =as.data.frame(Day)

coin = "ETH"

for(exchange in exchange_list){
  raw=ex_read(exchange,coin,5,sd1,sd2)
  RV=data.frame(RV_calc(raw))
  Vol_list[exchange] <- RV
}

View(Vol_list)

#write.csv(Vol_list,paste0("crisis/",coin,"5_min_rv.csv"), row.names = TRUE)


##############Create ASJ Table###############################################################

library(scales)
coin = "ETH"
sd1 ="7/1/2022" 
sd2 ="11/12/2022"

sd1post ="11/13/2022" #"11/13/2022"
sd2post ="6/31/2023"


#Exchanges and Sample Intervals
intervals = c(1,5,10)


#Create the Empty Table
df = table_create(exchange_list,intervals)

coin = "ETH"

#Populate the Empty Table
for (i in 1:length(exchange_list)){
  for (j in 1:length(intervals)){
    df[i,j] <- percent(jump_days(exchange_list[i],coin,intervals[j],sd1,sd2),accuracy = 0.01)
  }
}

View(df)

#Export the Table
#write.csv(df,paste0(coin,"ASJ_table.csv"), row.names = TRUE)

##############Regressions###############################################################
library(stargazer)

sd1 ="7/1/2022" #7/1/2022
sd2 ="11/12/2022"

coin = "ETH"

sd11 ="2022-07-01" #07-01
sd22 ="2022-11-12"

btc = coiner('btc',sd11,sd22)
eth = coiner('eth',sd11,sd22)

df <- data.frame(matrix(nrow = nrow(btc), ncol = 0))

df$ethftx = MedRV_calc(ex_read("FTX","ETH",5,sd1,sd2))
df$btcftx = MedRV_calc(ex_read("FTX","BTC",5,sd1,sd2))
df$ethftxj= jump_calc(ex_read("FTX","ETH",5,sd1,sd2))
df$btcftxj = jump_calc(ex_read("FTX","BTC",5,sd1,sd2))
df$ethBitfinex = RV_calc(ex_read("Bitfinex","ETH",5,sd1,sd2))
df$btcBitfinex = RV_calc(ex_read("Bitfinex","BTC",5,sd1,sd2))
df$ethBitstamp = RV_calc(ex_read("Bitstamp","ETH",5,sd1,sd2))
df$btcBitstamp = RV_calc(ex_read("Bitstamp","BTC",5,sd1,sd2))
df$ethCoinbase = RV_calc(ex_read("Coinbase","ETH",5,sd1,sd2))
df$btcCoinbase = RV_calc(ex_read("Coinbase","BTC",5,sd1,sd2))
df$ethExmo = RV_calc(ex_read("Exmo","ETH",5,sd1,sd2))
df$btcExmo = RV_calc(ex_read("Exmo","BTC",5,sd1,sd2))
df$ethHitBTC = RV_calc(ex_read("HitBTC","ETH",5,sd1,sd2))
df$btcHitBTC = RV_calc(ex_read("HitBTC","BTC",5,sd1,sd2))
df$ethKraken = RV_calc(ex_read("Kraken","ETH",5,sd1,sd2))
df$btcKraken = RV_calc(ex_read("Kraken","BTC",5,sd1,sd2))
df$ethOKcoin = RV_calc(ex_read("OKcoin","ETH",5,sd1,sd2))
df$btcOKcoin = RV_calc(ex_read("OKcoin","BTC",5,sd1,sd2))

df$btcsupply = btc$SplyAct1d
df$ethsupply = eth$SplyAct1d


stargazer(
  df,
  type = "latex", # Output LaTeX code
  title = "Summary Statistics for Cryptocurrency Metrics",
  label = "tab:summary_stats",
  summary = TRUE, # Generate summary statistics
  digits = 4,     # Number of decimal places
  covariate.labels = c(
    "ETH FTX MedRV", "BTC FTX MedRV", 
    "ETH FTX Jump Days", "BTC FTX Jump Days",
    "ETH Bitfinex RV", "BTC Bitfinex RV",
    "ETH Bitstamp RV", "BTC Bitstamp RV",
    "ETH Coinbase RV", "BTC Coinbase RV",
    "ETH Exmo RV", "BTC Exmo RV",
    "ETH HitBTC RV", "BTC HitBTC RV",
    "ETH Kraken RV", "BTC Kraken RV",
    "ETH OKcoin RV", "BTC OKcoin RV",
    "BTC Supply", "ETH Supply"
  ),
  align = TRUE,
  no.space = TRUE,
  float = TRUE
)

modelb1 = lm(btcBitfinex ~ btcftx+btcftxj+btcsupply,df)
modelb2 = lm(btcBitstamp ~ btcftx+btcftxj+btcsupply,df)
modelb3 = lm(btcCoinbase ~ btcftx+btcftxj+btcsupply,df)
modelb4 = lm(btcExmo ~ btcftx+btcftxj+btcsupply,df)
modelb5 = lm(btcHitBTC ~ btcftx+btcftxj+btcsupply,df)
modelb6 = lm(btcKraken ~ btcftx+btcftxj+btcsupply,df)
modelb7 = lm(btcOKcoin ~ btcftx+btcftxj+btcsupply,df)


stargazer(modelb1, modelb2, modelb3, modelb4, modelb5, modelb6, modelb7, 
          type = "latex",
          no.space = TRUE,
          omit.stat = c("f", "ser"),
          dep.var.labels = c("Bitfinex", "Bitstamp", "Coinbase", "Exmo", 
                            "HitBTC", "Kraken", "Okcoin"),
          dep.var.caption = "Dependent variable: RV of BTC on each exchange",
          covariate.labels = c("FTX MedRV", "FTX Jump Days", "Active Supply"),
          title = "Regression Results for BTC Realized Volatility Across Exchanges")

modele1 = lm(ethBitfinex ~ ethftx+ethftxj+ethsupply,df)
modele2 = lm(ethBitstamp ~ ethftx+ethftxj+ethsupply,df)
modele3 = lm(ethCoinbase ~ ethftx+ethftxj+ethsupply,df)
modele4 = lm(ethExmo ~ ethftx+ethftxj+ethsupply,df)
modele5 = lm(ethHitBTC ~ ethftx+ethftxj+ethsupply,df)
modele6 = lm(ethKraken ~ ethftx+ethftxj+ethsupply,df)
modele7 = lm(ethOKcoin ~ ethftx+ethftxj+ethsupply,df)

stargazer(modele1, modele2, modele3, modele4, modele5, modele6, modele7, 
          type = "latex",
          no.space = TRUE,
          omit.stat = c("f", "ser"),
          dep.var.labels = c("Bitfinex", "Bitstamp", "Coinbase", "Exmo", 
                             "HitBTC", "Kraken", "Okcoin"),
          dep.var.caption = "Dependent variable: RV of ETH on each exchange",
          covariate.labels = c("FTX MedRV", "FTX Jump Days", "Active Supply"),
          title = "Regression Results for ETH Realized Volatility Across Exchanges")
