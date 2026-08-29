# ===== 问题二 碳感知任务调度：模拟退火（借鉴碳感知放置/算电协同文献） =====
# 决策变量：可延迟任务(批量/训练)的执行区域与开工时段；实时任务到达即开工(来源区域)
# 求解：模拟退火——初始解为不迁移最早可行，随机"迁移+时移"扰动，Metropolis 准则逼近较优解
# 目标：加权综合代价=购电成本+碳权重×碳排放+时延惩罚
# 指标：运行成本、碳排放、新能源利用率；与"不迁移基准"对比
library(readr);library(dplyr);library(ggplot2)
set.seed(1)
od=file.path("C:/Users/18904/Github/huashu-cup","解答","第二题")
bp=file.path("C:/Users/18904/Github/huashu-cup","解答","数据预处理")
ap=file.path("C:/Users/18904/Github/huashu-cup","题目","附件数据")
d=read_csv(file.path(bp,"任务轨迹_清洗后.csv"),locale=locale(encoding="UTF-8"),show_col_types=FALSE)
rt=read_csv(file.path(ap,"region_time_data_region_time_data.csv"),locale=locale(encoding="UTF-8"),show_col_types=FALSE)
lat=read_csv(file.path(ap,"network_latency_时延矩阵.csv"),locale=locale(encoding="UTF-8"),show_col_types=FALSE)
lm=as.matrix(lat[,-1]);rownames(lm)=lat[[1]];colnames(lm)=names(lat)[-1]
cap=c(区域A=630,区域B=585,区域C=540,区域D=1472,区域E=1012,区域F=966)
pue=c(区域A=1.35,区域B=1.35,区域C=1.38,区域D=1.28,区域E=1.25,区域F=1.27)
pw=c(人工智能训练任务=.16,批量推理任务=.1,实时推理任务=.08)
R=names(cap);TT=2407
price=ci=re=nai=net0=dire0=matrix(0,TT,length(R));colnames(price)=R
for(j in seq_along(R)){s=rt[rt$区域编号==R[j],];price[,j]=s$购电价格;ci[,j]=s$碳强度;re[,j]=s$可用新能源出力;nai[,j]=s$非人工智能信息技术负荷;net0[,j]=s$净购电功率;dire0[,j]=s$直接消纳新能源}
d$L=ceiling(d$连续执行时长_小时);d$P=d$图形处理器需求量*pw[d$任务类型]

# ---- 指标计算：以附件基准净购电为底 + AI负荷增量全部购电 ----
# net = 基准净购电 + AI功率×PUE；成本=Σnet×电价；碳=Σnet×碳强度
mets=function(aip){
 net=net0+aip*pue
 c(购电成本_万元=round(sum(net*price)/1e4,1),碳排放_吨=round(sum(net*ci),0),
   新能源利用率=round(sum(dire0)/sum(re),3),AI平均负荷_MW=round(mean(aip),2))
}
# ---- 初始解：实时固定 + 可延迟来源最早可行（即"不迁移基准"）----
init=function(){
 gocc=matrix(0,TT,length(R));colnames(gocc)=R;aip=gocc
 sch=data.frame()
 for(i in c(which(d$任务类型=="实时推理任务"),which(d$任务类型!="实时推理任务"))){
  g=as.numeric(d[i,"图形处理器需求量"]);P=d$P[i];L=d$L[i];at=d$到达小时[i];typ=d$任务类型[i];src=d$来源区域[i]
  if(typ=="实时推理任务"){reg=src;s=at;e=as.numeric(d[i,"最晚完成小时"])}else{
   dl=as.numeric(d[i,"最晚完成小时"]);reg=src;s=at
   while(s+L<=dl&&max(gocc[(s+1):(s+L),reg])+g>cap[reg])s=s+1
   if(s+L>dl)next;e=s+L}
  gocc[(s+1):e,reg]=gocc[(s+1):e,reg]+g;aip[(s+1):e,reg]=aip[(s+1):e,reg]+P
  sch=rbind(sch,data.frame(任务编号=as.numeric(d[i,"任务编号"]),任务类型=typ,来源区域=src,
    执行区域=reg,图形处理器需求量=g,调度开工小时=s,调度完工小时=e))}
 list(gocc=gocc,aip=aip,sch=sch)
}
# ---- 模拟退火：随机"迁移+时移"扰动，Metropolis 准则逼近较优解 ----
sa=function(base,iters=3000,T0=1e4,T1=10,lc=1,lt=100,W=336){
 ecm=price+lc*ci;cvm=apply(ecm,2,function(v)c(0,cumsum(v)))
 gocc=base$gocc;aip=base$aip;sch=base$sch
 rownames(sch)=as.character(sch$任务编号);del=which(d$任务类型!="实时推理任务")
 tot=0
 for(i in del){r=sch[as.character(d$任务编号[i]),"执行区域"];s=sch[as.character(d$任务编号[i]),"调度开工小时"]
   tot=tot+d$P[i]*pue[r]*(cvm[s+d$L[i]+1,r]-cvm[s+1,r])}
 hist=data.frame(iter=seq(100,iters,100),cost=rep(NA,floor(iters/100)))  # 收敛历史(每100次)
 for(it in 1:iters){
  if(it%%1000==0)cat("SA 迭代",it,"/",iters," 当前总代价:",round(tot/1e6,2),"百万\n")
  if(it%%100==0)hist$cost[it/100]=tot
  T=T0*(T1/T0)^(it/iters)
  i=sample(del,1);id=as.character(d$任务编号[i])
  r1=sch[id,"执行区域"];s1=sch[id,"调度开工小时"];g=as.numeric(d[i,"图形处理器需求量"]);P=d$P[i];L=d$L[i];at=d$到达小时[i];src=d$来源区域[i]
  cand=names(which(lm[src,]<=as.numeric(d[i,"最大网络时延"])));r2=sample(cand,1)
  dl=as.numeric(d[i,"最晚完成小时"]);hi=min(at+W,dl-L);if(hi<at)next
  s2=sample(at:hi,1)
  if(max(gocc[(s2+1):(s2+L),r2])+g>cap[r2])next
  w1=P*pue[r1]*(cvm[s1+L+1,r1]-cvm[s1+1,r1])+lt*(lm[src,r1]-5)
  w2=P*pue[r2]*(cvm[s2+L+1,r2]-cvm[s2+1,r2])+lt*(lm[src,r2]-5)
  if(w2-w1<0||runif(1)<exp(-(w2-w1)/T)){
   gocc[(s1+1):(s1+L),r1]=gocc[(s1+1):(s1+L),r1]-g;aip[(s1+1):(s1+L),r1]=aip[(s1+1):(s1+L),r1]-P
   gocc[(s2+1):(s2+L),r2]=gocc[(s2+1):(s2+L),r2]+g;aip[(s2+1):(s2+L),r2]=aip[(s2+1):(s2+L),r2]+P
   sch[id,"执行区域"]=r2;sch[id,"调度开工小时"]=s2;sch[id,"调度完工小时"]=s2+L
   tot=tot+(w2-w1)}
 }
 sch$执行区域=as.character(sch$执行区域);sch$调度开工小时=as.numeric(sch$调度开工小时);sch$调度完工小时=as.numeric(sch$调度完工小时)
 list(aip=aip,sch=sch,n_mig=sum(sch$执行区域!=sch$来源区域),hist=hist)
}
cat("初始化基准...\n");base=init()
cat("== 基准(不迁移最早可行) ==\n");print(mets(base$aip))
cat("== 碳感知(模拟退火) ==\n");sa2=sa(base,iters=8000,lt=20);print(mets(sa2$aip))
cat("迁移任务数:",sa2$n_mig,"\n")
res=data.frame(方案=c("不迁移基准","碳感知SA"),购电成本_万元=c(mets(base$aip)[1],mets(sa2$aip)[1]),
 碳排放_吨=c(mets(base$aip)[2],mets(sa2$aip)[2]),新能源利用率=c(mets(base$aip)[3],mets(sa2$aip)[3]),AI平均负荷_MW=c(mets(base$aip)[4],mets(sa2$aip)[4]))
write_csv(res,file.path(od,"问题二_指标对比.csv"))
write_csv(sa2$sch,file.path(od,"问题二_碳感知调度方案.csv"))
print(res)
# ---- 模拟退火收敛曲线 ----
h=sa2$hist
g3=ggplot(h,aes(x=iter,y=cost))+geom_line(color="steelblue",linewidth=.9)+geom_point(size=1,color="steelblue")+
  geom_hline(yintercept=h$cost[1],linetype="dashed",color="grey50")+
  labs(x="迭代次数",y="综合代价",caption="虚线为初始解（不迁移基准）水平")+theme_bw()
ggsave(file.path(od,"SA收敛曲线.png"),g3,width=8.5,height=5.5,dpi=150)
cat("输出目录:",od,"\n")
