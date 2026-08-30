library(readr);library(dplyr)
set.seed(1)
od=file.path("C:/Users/18904/Github/huashu-cup","解答","5-灵敏度分析");ap=file.path("C:/Users/18904/Github/huashu-cup","题目","附件数据")
rt=read_csv(file.path(ap,"region_time_data_region_time_data.csv"),locale=locale(encoding="UTF-8"),show_col_types=FALSE)
st=read_csv(file.path(ap,"storage_information_storage_information.csv"),locale=locale(encoding="UTF-8"),show_col_types=FALSE)
pue=c(区域A=1.35,区域B=1.35,区域C=1.38,区域D=1.28,区域E=1.25,区域F=1.27)
R=st$区域编号;TT=2407
price=ci=matrix(0,TT,6);colnames(price)=colnames(ci)=R
for(j in seq_along(R)){s=rt[rt$区域编号==R[j],];price[,j]=s$购电价格;ci[,j]=s$碳强度}
stor=function(net,price,E,Smin,S0,Pc,Pd,ec,ed,Smax){win=24;tg=as.numeric(stats::filter(net,rep(1/win,win),sides=2));tg[is.na(tg)]=net[is.na(tg)]
 band=sd(net)*.2;qp=quantile(price,c(.35,.65));soc=S0;n=length(net);pg=ds=ch=numeric(n)
 for(t in 1:n){if(net[t]<0){sc=min(Pc,-net[t],(E-soc)/ec);soc=soc+ec*sc;pg[t]=0}
  else if(net[t]>tg[t]+band&&price[t]>qp[1]&&soc>Smin){ds[t]=min(Pd,net[t]-tg[t],(soc-Smin)*ed);soc=soc-ds[t]/ed;pg[t]=net[t]-ds[t]}
  else if(net[t]<tg[t]-band&&price[t]<qp[2]&&soc<E){gc=min(Pc,tg[t]-net[t],(E-soc)/ec);soc=soc+ec*gc;pg[t]=net[t]+gc}
  else{pg[t]=net[t]}}
 if(soc<S0)for(t in n:1){if(soc>=S0)break;gc=min(Pc,(S0-soc)/ec,(E-soc)/ec);if(gc<=0)next;soc=soc+ec*gc;pg[t]=pg[t]+gc}
 pg}
sc=c(.5,1,1.5,2);res=data.frame()
for(k in sc){cost=carbon=0;peak=0;vari=0
 for(r in R){q=rt[rt$区域编号==r,];s=st[st$区域编号==r,]
  fac=(q$基准人工智能信息技术负荷+q$非人工智能信息技术负荷)*pue[r];net=fac-q$直接消纳新能源
  pg=stor(net,price[,r],s$储能容量*k,s$最小荷电状态*k,s$初始荷电状态*k,s$最大充电功率,s$最大放电功率,s$充电效率,s$放电效率,s$外送上限)
  cost=cost+sum(pg*price[,r]);carbon=carbon+sum(pg*ci[,r]);peak=max(peak,max(pg));vari=vari+sd(pg)}
 res=rbind(res,data.frame(容量倍数=k,购电成本万元=round(cost/1e4,1),碳排放吨=round(carbon,0),峰值净购电_MW=round(peak,1),负荷波动=round(vari,2)))}
write_csv(res,file.path(od,"灵敏度_储能容量.csv"));print(res)
cat("输出目录:",od,"\n")
