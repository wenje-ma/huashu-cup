library(readr);library(dplyr);library(forecast)
set.seed(1);od=file.path("C:/Users/18904/Github/huashu-cup","解答","第一题")
d=read_csv(file.path("C:/Users/18904/Github/huashu-cup","解答","数据预处理","逐时图形处理器需求_切分.csv"),locale=locale(encoding="UTF-8"),show_col_types=FALSE)
y1=d$图形处理器总需求[d$到达小时<=2351];yval=d$图形处理器总需求[d$到达小时>=2352&d$到达小时<=2375];ytest=d$图形处理器总需求[d$到达小时>=2376];h=length(ytest);hv=length(yval)
metric=function(yhat)c(RMSE=sqrt(mean((yhat-ytest)^2)),MAE=mean(abs(yhat-ytest)),MAPE=mean(abs((yhat-ytest)/ytest))*100)
f1v=as.numeric(forecast(ets(ts(y1,frequency=24)),h=hv)$mean);f2v=as.numeric(forecast(auto.arima(ts(y1,frequency=24)),h=hv)$mean)
grid=seq(0,1,.01);wopt=grid[which.min(sapply(grid,function(w)sqrt(mean((w*f1v+(1-w)*f2v-yval)^2))))]
ytr=d$图形处理器总需求[d$到达小时<=2375];f1t=as.numeric(forecast(ets(ts(ytr,frequency=24)),h=h)$mean);f2t=as.numeric(forecast(auto.arima(ts(ytr,frequency=24)),h=h)$mean)
fe=.5*f1t+.5*f2t;fo=wopt*f1t+(1-wopt)*f2t
res=data.frame(模型=c("ETS","ARIMA","等权组合","最优权重组合"),RMSE=round(c(metric(f1t)[1],metric(f2t)[1],metric(fe)[1],metric(fo)[1]),1),MAE=round(c(metric(f1t)[2],metric(f2t)[2],metric(fe)[2],metric(fo)[2]),1),MAPE=round(c(metric(f1t)[3],metric(f2t)[3],metric(fe)[3],metric(fo)[3]),2))
write_csv(res,file.path(od,"预测模型对比_组合预测.csv"))
write_csv(data.frame(到达小时=d$到达小时[d$到达小时>=2376],真实值=ytest,ETS=f1t,ARIMA=f2t,等权组合=fe,最优组合=fo),file.path(od,"预测结果_测试集_组合预测.csv"))
print(res);cat("w(ETS)=",round(wopt,3)," w(ARIMA)=",round(1-wopt,3),"\n")
