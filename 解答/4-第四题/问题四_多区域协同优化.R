# ===== 问题四 多区域"算-储-电"协同优化：多目标加权 + 场景对比 =====
# 依据文献：Spatiotemporally(时空协调)/多集群算力调度(迁移建模)/王一航(多目标+场景)/樊小朝(源网荷储算)/Bi-Level(储能)/付智(算电协同)
# 决策：任务迁移(区域)+开工时段(时移) + 储能充放电 + 购售电
# 目标(加权)：购电成本+λc×碳排放+λl×网络时延；新能源利用率与峰值净购电为评价指标
# 场景：基准 / 碳约束(λc↑) / 电价机制(峰谷差↑) / 新能源波动(可用出力↓)
library(readr);library(dplyr)
set.seed(1)
od=file.path("C:/Users/18904/Github/huashu-cup","解答","4-第四题")
bp=file.path("C:/Users/18904/Github/huashu-cup","解答","0-数据预处理")
ap=file.path("C:/Users/18904/Github/huashu-cup","题目","附件数据")
d=read_csv(file.path(bp,"任务轨迹_清洗后.csv"),locale=locale(encoding="UTF-8"),show_col_types=FALSE)
rt=read_csv(file.path(ap,"region_time_data_region_time_data.csv"),locale=locale(encoding="UTF-8"),show_col_types=FALSE)
lat=read_csv(file.path(ap,"network_latency_时延矩阵.csv"),locale=locale(encoding="UTF-8"),show_col_types=FALSE)
st=read_csv(file.path(ap,"storage_information_storage_information.csv"),locale=locale(encoding="UTF-8"),show_col_types=FALSE)
lm=as.matrix(lat[,-1]);rownames(lm)=lat[[1]];colnames(lm)=names(lat)[-1]
cap=c(区域A=630,区域B=585,区域C=540,区域D=1472,区域E=1012,区域F=966)
pue=c(区域A=1.35,区域B=1.35,区域C=1.38,区域D=1.28,区域E=1.25,区域F=1.27)
pw=c(人工智能训练任务=.16,批量推理任务=.1,实时推理任务=.08)
R=names(cap);TT=2407
price=ci=re=nai=net0=dire0=matrix(0,TT,length(R));colnames(price)=colnames(ci)=colnames(re)=colnames(nai)=colnames(net0)=colnames(dire0)=R
for(j in seq_along(R)){s=rt[rt$区域编号==R[j],];price[,j]=s$购电价格;ci[,j]=s$碳强度;re[,j]=s$可用新能源出力;nai[,j]=s$非人工智能信息技术负荷;net0[,j]=s$净购电功率;dire0[,j]=s$直接消纳新能源}
d$L=ceiling(d$连续执行时长_小时);d$P=d$图形处理器需求量*pw[d$任务类型]

# ---- 任务调度：贪心 空间迁移+时移（碳感知，场景电价/权重可调）----
sched=function(lc=1,pr=NULL,W=48,K=5,lt=1){
 if(is.null(pr))pr=price
 ecm=pr+lc*ci;cvm=apply(ecm,2,function(v)c(0,cumsum(v)))
 gocc=matrix(0,TT,length(R));colnames(gocc)=R;aip=gocc
 sch=data.frame();n_mig=0;t_del=0
 ord=c(which(d$任务类型=="实时推理任务"),which(d$任务类型!="实时推理任务"))
 for(i in ord){
  g=as.numeric(d[i,"图形处理器需求量"]);P=d$P[i];L=d$L[i];at=d$到达小时[i];typ=d$任务类型[i];src=d$来源区域[i]
  if(typ=="实时推理任务"){reg=src;s=at;e=as.numeric(d[i,"最晚完成小时"])}
  else{
   dl=as.numeric(d[i,"最晚完成小时"]);cand=names(which(lm[src,]<=as.numeric(d[i,"最大网络时延"])))
   best=NULL
   for(r in cand){
    hi=min(at+W,dl-L);if(hi<at)next
    s0=at:hi;wc=P*pue[r]*(cvm[s0+L+1,r]-cvm[s0+1,r])+lt*(lm[src,r]-5)
    for(k in order(wc)[1:min(K,length(s0))]){s=s0[k]
     if(max(gocc[(s+1):(s+L),r])+g<=cap[r]){if(is.null(best)||wc[k]<best$w)best=list(r=r,s=s,w=wc[k]);break}}
   }
   if(is.null(best))next;reg=best$r;s=best$s;e=s+L
   if(reg!=src){n_mig=n_mig+1;t_del=t_del+lm[src,reg]}
  }
  gocc[(s+1):e,reg]=gocc[(s+1):e,reg]+g;aip[(s+1):e,reg]=aip[(s+1):e,reg]+P
 }
 list(aip=aip,n_mig=n_mig,t_del=t_del,avg_lat=round(t_del/max(1,n_mig),1))
}
# ---- 储能：削峰填谷+电价套利（问题3启发式）----
stor=function(net,price,E,Smin,S0,Pc,Pd,ec,ed,Smax){
 win=24;tg=as.numeric(stats::filter(net,rep(1/win,win),sides=2));tg[is.na(tg)]=net[is.na(tg)]
 band=sd(net)*.2;qp=quantile(price,c(.35,.65));soc=S0;n=length(net)
 pg=ds=ch=numeric(n)
 for(t in 1:n){
  if(net[t]<0){sc=min(Pc,-net[t],(E-soc)/ec);soc=soc+ec*sc;pg[t]=0;ch[t]=sc}
  else if(net[t]>tg[t]+band&&price[t]>qp[1]&&soc>Smin){ds[t]=min(Pd,net[t]-tg[t],(soc-Smin)*ed);soc=soc-ds[t]/ed;pg[t]=net[t]-ds[t]}
  else if(net[t]<tg[t]-band&&price[t]<qp[2]&&soc<E){gc=min(Pc,tg[t]-net[t],(E-soc)/ec);soc=soc+ec*gc;pg[t]=net[t]+gc;ch[t]=gc}
  else{pg[t]=net[t]}
 }
 if(soc<S0)for(t in n:1){if(soc>=S0)break;gc=min(Pc,(S0-soc)/ec,(E-soc)/ec);if(gc<=0)next;soc=soc+ec*gc;pg[t]=pg[t]+gc}
 list(pg=pg,ch=sum(ch),ds=sum(ds))
}
# ---- 电价机制情景：峰谷价差放大(峰×1.15/谷×0.85) ----
pr_adj=function(pr)apply(pr,2,function(v){q=quantile(v,c(.25,.75));ifelse(v>q[2],v*1.15,ifelse(v<q[1],v*.85,v))})

# ---- 场景运行：任务调度(可缓存) -> 各区域储能优化 -> 多目标指标 ----
run=function(cp=50,pr=NULL,dire_scale=1,tsk=NULL){
 if(is.null(pr))pr=price
 if(is.null(tsk))tsk=sched(cp,pr)
 aip=tsk$aip
 cost=carbon=0;peak=0;vari=0
 for(r in R){
  q=rt[rt$区域编号==r,];s=st[st$区域编号==r,]
  fac=(aip[,r]+q$非人工智能信息技术负荷)*pue[r]   # 设施负荷=调度AI+非AI
  net=fac-q$直接消纳新能源*dire_scale              # 净负荷(新能源波动可缩放)
  st2=stor(net,pr[,r],s$储能容量,s$最小荷电状态,s$初始荷电状态,
       s$最大充电功率,s$最大放电功率,s$充电效率,s$放电效率,s$外送上限)
  cost=cost+sum(st2$pg*pr[,r]);carbon=carbon+sum(st2$pg*ci[,r])
  peak=max(peak,max(st2$pg));vari=vari+sd(st2$pg)
 }
 list(m=c(购电成本万元=round(cost/1e4,1),碳排放吨=round(carbon,0),
   迁移时延_ms=round(tsk$t_del,0),迁移任务数=tsk$n_mig,
   峰值净购电_MW=round(peak,1),负荷波动=round(vari,2)),tsk=tsk)
}
cat("== 场景1 基准(碳价50) ==\n");s1=run(50);print(s1$m)
cat("== 场景2 碳约束(碳价500) ==\n");s2=run(500);print(s2$m)
cat("== 场景3 电价机制(峰谷差放大) ==\n");s3=run(50,pr=pr_adj(price));print(s3$m)
cat("== 场景4 新能源波动(出力降30%) ==\n");s4=run(50,dire_scale=.7,tsk=s1$tsk);print(s4$m)
res=data.frame(场景=c("基准","碳约束","电价机制","新能源波动"),
 购电成本万元=c(s1$m[1],s2$m[1],s3$m[1],s4$m[1]),碳排放吨=c(s1$m[2],s2$m[2],s3$m[2],s4$m[2]),
 迁移时延_ms=c(s1$m[3],s2$m[3],s3$m[3],s4$m[3]),迁移任务数=c(s1$m[4],s2$m[4],s3$m[4],s4$m[4]),
 新能源利用率=c(s1$m[5],s2$m[5],s3$m[5],s4$m[5]),峰值净购电_MW=c(s1$m[6],s2$m[6],s3$m[6],s4$m[6]),负荷波动=c(s1$m[7],s2$m[7],s3$m[7],s4$m[7]))
write_csv(res,file.path(od,"问题四_场景对比.csv"));print(res)
cat("输出目录:",od,"\n")
