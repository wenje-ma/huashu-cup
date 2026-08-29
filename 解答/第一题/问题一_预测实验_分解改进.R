# ===== 问题一② 改进预测：分解(CEEMDAN) + ARIMA/LightGBM + 重构 =====
# 思路：借鉴文献"VMD 分解-预测-重构"框架（R 中以 CEEMDAN 实现信号分解）
#       趋势项用 ARIMA/ETS，波动项用 LightGBM，随机高频项用 ARIMA（替代文献中的 TCN）
library(readr);library(dplyr);library(forecast);library(Rlibeemd);library(lightgbm)
set.seed(1)
od=file.path("C:/Users/18904/Github/huashu-cup","解答","第一题")
pt=file.path("C:/Users/18904/Github/huashu-cup","解答","数据预处理","逐时图形处理器需求_切分.csv")
d=read_csv(pt,locale=locale(encoding="UTF-8"),show_col_types=FALSE)

# 训练=0-2375，测试=2376-2399
trn=d %>% filter(到达小时<=2375); tst=d %>% filter(到达小时>=2376)
y=trn$图形处理器总需求; ytest=tst$图形处理器总需求; h=nrow(tst)

metric=function(yhat)c(RMSE=sqrt(mean((yhat-ytest)^2)),
                      MAE=mean(abs(yhat-ytest)),
                      MAPE=mean(abs((yhat-ytest)/ytest))*100)

# ---- 基线 ----
m_ets=metric(as.numeric(forecast(ets(ts(y,frequency=24)),h=h)$mean))
m_arima=metric(as.numeric(forecast(auto.arima(ts(y,frequency=24)),h=h)$mean))

# ---- 分解 ----
dec=ceemdan(y)                        # 列=分量(高频→低频)，行=时间；列和≈y
k=ncol(dec)
trend=dec[,k]                          # 末列=低频趋势/残差
fluct=rowSums(dec[,-k,drop=FALSE])     # 其余=波动项
cat("分解分量数 k =",k," 重构误差 =",max(abs(rowSums(dec)-y)),"\n")

# ---- 方法1：分解 + 逐分量 ARIMA 预测 + 重构 ----
f1=numeric(h)
for(j in 1:k){
  f1=f1+as.numeric(forecast(auto.arima(dec[,j]),h=h)$mean)
}
m_decarima=metric(f1)

# ---- 方法2：趋势项 ARIMA + 波动项 LightGBM（递归多步）+ 重构 ----
p=6                                   # 滞后阶数
nx=length(fluct)
X=matrix(NA,nx-p,p)
for(l in 1:p) X[,l]=fluct[(p+1):nx-l]  # 滞后特征
Y=fluct[(p+1):nx]                     # 目标
dt=lgb.Dataset(X,Y)
mod=lgb.train(list(objective="regression",metric="rmse",learning_rate=.05),dt,nrounds=200,verbose=-1)
last=tail(fluct,p); ff=numeric(h)
for(s in 1:h){
  ff[s]=lgb.predict(mod,matrix(last,nrow=1))
  last=c(last[-1],ff[s])              # 滚动更新
}
ftrend=as.numeric(forecast(auto.arima(trend),h=h)$mean)
f2=ftrend+ff
m_declgbm=metric(f2)

# ---- 输出 ----
res=data.frame(模型=c("ETS","ARIMA","分解+ARIMA","分解+LightGBM"),
               RMSE=round(c(m_ets[1],m_arima[1],m_decarima[1],m_declgbm[1]),1),
               MAE =round(c(m_ets[2],m_arima[2],m_decarima[2],m_declgbm[2]),1),
               MAPE=round(c(m_ets[3],m_arima[3],m_decarima[3],m_declgbm[3]),2))
write_csv(res,file.path(od,"预测模型对比_含改进.csv"))

pred=data.frame(到达小时=tst$到达小时,真实值=ytest,
                ETS=as.numeric(forecast(ets(ts(y,frequency=24)),h=h)$mean),
                ARIMA=as.numeric(forecast(auto.arima(ts(y,frequency=24)),h=h)$mean),
                分解ARIMA=f1,分解LightGBM=f2)
write_csv(pred,file.path(od,"预测结果_测试集_含改进.csv"))
print(res)
cat("输出目录:",od,"\n")
