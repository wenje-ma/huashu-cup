# 灵敏度A：碳价对碳感知调度成本/碳排放的影响
# 复用贪心碳感知调度(空间迁移+时移)，碳价梯度循环；口径与问题2一致(net=基准净购电+AI增量购电)
library(readr);library(dplyr)
set.seed(1)
od=file.path("C:/Users/18904/Github/huashu-cup","解答","5-灵敏度分析")
bp=file.path("C:/Users/18904/Github/huashu-cup","解答","0-数据预处理")
ap=file.path("C:/Users/18904/Github/huashu-cup","题目","附件数据")
d=read_csv(file.path(bp,"任务轨迹_清洗后.csv"),locale=locale(encoding="UTF-8"),show_col_types=FALSE)
rt=read_csv(file.path(ap,"region_time_data_region_time_data.csv"),locale=locale(encoding="UTF-8"),show_col_types=FALSE)
lat=read_csv(file.path(ap,"network_latency_时延矩阵.csv"),locale=locale(encoding="UTF-8"),show_col_types=FALSE)
lm=as.matrix(lat[,-1]);rownames(lm)=lat[[1]];colnames(lm)=names(lat)[-1]
cap=c(区域A=630,区域B=585,区域C=540,区域D=1472,区域E=1012,区域F=966)
pue=c(区域A=1.35,区域B=1.35,区域C=1.38,区域D=1.28,区域E=1.25,区域F=1.27)
pw=c(人工智能训练任务=.16,批量推理任务=.1,实时推理任务=.08)
R=names(cap);TT=2407
price=ci=net0=matrix(0,TT,6);colnames(price)=colnames(ci)=colnames(net0)=R
for(j in seq_along(R)){s=rt[rt$区域编号==R[j],];price[,j]=s$购电价格;ci[,j]=s$碳强度;net0[,j]=s$净购电功率}
d$L=ceiling(d$连续执行时长_小时);d$P=d$图形处理器需求量*pw[d$任务类型]
sched=function(cp,W=48,K=5){
 ecm=price+cp*ci;cvm=apply(ecm,2,function(v)c(0,cumsum(v)))
 gocc=matrix(0,TT,6);colnames(gocc)=R;aip=gocc
 ord=c(which(d$任务类型=="实时推理任务"),which(d$任务类型!="实时推理任务"))
 for(i in ord){
  g=as.numeric(d[i,"图形处理器需求量"]);P=d$P[i];L=d$L[i];at=d$到达小时[i];typ=d$任务类型[i];src=d$来源区域[i]
  if(typ=="实时推理任务"){reg=src;s=at;e=as.numeric(d[i,"最晚完成小时"])}
  else{
   dl=as.numeric(d[i,"最晚完成小时"]);cand=names(which(lm[src,]<=as.numeric(d[i,"最大网络时延"])))
   best=NULL
   for(r in cand){
    hi=min(at+W,dl-L);if(hi<at)next
    s0=at:hi;wc=P*pue[r]*(cvm[s0+L+1,r]-cvm[s0+1,r])+(lm[src,r]-5)
    for(k in order(wc)[1:min(K,length(s0))]){s=s0[k]
     if(max(gocc[(s+1):(s+L),r])+g<=cap[r]){if(is.null(best)||wc[k]<best$w)best=list(r=r,s=s,w=wc[k]);break}}
   }
   if(is.null(best))next;reg=best$r;s=best$s;e=s+L
  }
  gocc[(s+1):e,reg]=gocc[(s+1):e,reg]+g;aip[(s+1):e,reg]=aip[(s+1):e,reg]+P
 }
 net=net0+aip*pue
 c(碳价=cp,购电成本万元=round(sum(net*price)/1e4,1),碳排放吨=round(sum(net*ci),0))
}
cps=c(0,100,300,500,700)
res=as.data.frame(t(sapply(cps,function(cp)sched(cp))))
write_csv(res,file.path(od,"灵敏度_碳价.csv"));print(res)
cat("输出目录:",od,"\n")
