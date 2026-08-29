library(readr);library(dplyr);library(forecast);library(Rlibeemd);library(lightgbm)
set.seed(1);od=file.path("C:/Users/18904/Github/huashu-cup","解答","第一题")
d=read_csv(file.path("C:/Users/18904/Github/huashu-cup","解答","数据预处理","逐时图形处理器需求_切分.csv"),locale=locale(encoding="UTF-8"),show_col_types=FALSE)
trn=d%>%filter(到达小时<=2375);tst=d%>%filter(到达小时>=2376);y=trn$图形处理器总需求;ytest=tst$图形处理器总需求;h=nrow(tst)
metric=function(yhat)c(RMSE=sqrt(mean((yhat-ytest)^2)),MAE=mean(abs(yhat-ytest)),MAPE=mean(abs((yhat-ytest)/ytest))*100)
me=metric(as.numeric(forecast(ets(ts(y,frequency=24)),h=h)$mean));ma=metric(as.numeric(forecast(auto.arima(ts(y,frequency=24)),h=h)$mean))
dec=ceemdan(y);k=ncol(dec);trend=dec[,k];fluct=rowSums(dec[,-k,drop=FALSE])
f1=numeric(h);for(j in 1:k)f1=f1+as.numeric(forecast(auto.arima(dec[,j]),h=h)$mean)
p=6;nx=length(fluct);X=matrix(NA,nx-p,p);for(l in 1:p)X[,l]=fluct[(p+1):nx-l];Y=fluct[(p+1):nx]
mod=lgb.train(params=list(objective="regression",metric="rmse",learning_rate=.05),data=lgb.Dataset(data=X,label=Y),nrounds=200,verbose=-1)
last=tail(fluct,p);ff=numeric(h);for(s in 1:h){ff[s]=predict(mod,matrix(last,1));last=c(last[-1],ff[s])}
f2=as.numeric(forecast(auto.arima(trend),h=h)$mean)+ff
res=data.frame(模型=c("ETS","ARIMA","分解+ARIMA","分解+LightGBM"),RMSE=round(c(me[1],ma[1],metric(f1)[1],metric(f2)[1]),1),MAE=round(c(me[2],ma[2],metric(f1)[2],metric(f2)[2]),1),MAPE=round(c(me[3],ma[3],metric(f1)[3],metric(f2)[3]),2))
write_csv(res,file.path(od,"预测模型对比_含改进.csv"))
write_csv(data.frame(到达小时=tst$到达小时,真实值=ytest,ETS=as.numeric(forecast(ets(ts(y,frequency=24)),h=h)$mean),ARIMA=as.numeric(forecast(auto.arima(ts(y,frequency=24)),h=h)$mean),分解ARIMA=f1,分解LightGBM=f2),file.path(od,"预测结果_测试集_含改进.csv"))
print(res)
