library(readr);library(dplyr);library(tidyr);library(ggplot2)
set.seed(1);base="C:/Users/18904/Github/huashu-cup";od=file.path(base,"解答","1-第一题")
d=read_csv(file.path(base,"解答","0-数据预处理","任务轨迹_清洗后.csv"),locale=locale(encoding="UTF-8"),show_col_types=FALSE)
cap=c(区域A=630,区域B=585,区域C=540,区域D=1472,区域E=1012,区域F=966)
tasks=d%>%filter(到达小时>=2376)%>%arrange(到达小时,任务编号)
lat=read_csv(file.path(base,"题目","附件数据","network_latency_时延矩阵.csv"),locale=locale(encoding="UTF-8"),show_col_types=FALSE)
lm=as.matrix(lat[,-1]);rownames(lm)=lat[[1]];colnames(lm)=names(lat)[-1]
occ=matrix(0,2406,6);colnames(occ)=names(cap);sch=data.frame()
for(i in 1:nrow(tasks)){g=as.numeric(tasks[i,"图形处理器需求量"]);src=as.character(tasks[i,"来源区域"]);at=as.numeric(tasks[i,"到达小时"]);typ=as.character(tasks[i,"任务类型"])
 if(typ=="实时推理任务"){s=at;e=as.numeric(tasks[i,"最晚完成小时"]);cand=names(which(lm[src,]<=as.numeric(tasks[i,"最大网络时延"])))
  if(occ[s+1,src]+g<=cap[src])reg=src else{free=sapply(cand,function(r)if(occ[s+1,r]+g<=cap[r])occ[s+1,r]else Inf);reg=cand[which.min(free)];if(occ[s+1,reg]+g>cap[reg])next}}
 else{L=ceiling(as.numeric(tasks[i,"连续执行时长_小时"]));dl=as.numeric(tasks[i,"最晚完成小时"]);reg=src;s=at
  while(s+L<=dl){if(max(occ[(s+1):(s+L),reg])+g<=cap[reg])break;s=s+1};if(s+L>dl)next;e=s+L}
 occ[(s+1):e,reg]=occ[(s+1):e,reg]+g
 sch=rbind(sch,data.frame(任务编号=as.numeric(tasks[i,"任务编号"]),任务类型=typ,来源区域=src,执行区域=reg,图形处理器需求量=g,连续执行时长_小时=as.numeric(tasks[i,"连续执行时长_小时"]),调度开工小时=s,调度完工小时=e))}
write_csv(sch,file.path(od,"基础调度方案_最后24小时.csv"))
hr=2376:2405;util=data.frame(小时=hr);for(r in names(cap))util[[r]]=occ[hr+1,r]/cap[r]
write_csv(util,file.path(od,"区域GPU利用率_逐时.csv"))
print(t(sapply(names(cap),function(r)c(均值24=round(mean(util[[r]][hr<=2399]),3),峰值24=round(max(util[[r]][hr<=2399]),3)))))
print(t(sapply(names(cap),function(r)c(均值30=round(mean(util[[r]]),3),峰值30=round(max(util[[r]]),3)))))
gp=sch%>%mutate(开始=调度开工小时,结束=调度完工小时)%>%arrange(执行区域,开始,任务编号)%>%mutate(序号=row_number())
g1=ggplot(gp,aes(x=开始,xend=结束,y=序号,yend=序号,color=任务类型))+geom_segment(linewidth=2.6)+facet_wrap(~执行区域,scales="free_y",ncol=2)+scale_x_continuous(breaks=seq(2376,2406,6),limits=c(2376,2406))+scale_color_manual(values=c("实时推理任务"="#E64B35","批量推理任务"="#4DBBD5","人工智能训练任务"="#00A087"))+labs(x="小时",y="任务序号",color="任务类型")+theme_bw()+theme(legend.position="bottom")
ggsave(file.path(od,"甘特图_基础调度.png"),g1,width=13,height=12,dpi=150)
ul=pivot_longer(util,cols=all_of(names(cap)),names_to="区域",values_to="利用率")
g2=ggplot(ul,aes(x=小时,y=利用率,color=区域))+geom_line(linewidth=.8)+geom_point(size=1)+geom_hline(yintercept=1,linetype="dashed",color="grey40")+scale_x_continuous(breaks=seq(2376,2406,6))+labs(x="小时",y="GPU利用率")+theme_bw()+theme(legend.position="bottom")
ggsave(file.path(od,"GPU利用率_区域.png"),g2,width=12,height=6.5,dpi=150)
