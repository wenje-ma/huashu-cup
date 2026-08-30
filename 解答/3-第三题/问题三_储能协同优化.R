library(readr);library(dplyr);library(ggplot2)
od=file.path("C:/Users/18904/Github/huashu-cup","解答","3-第三题");ap=file.path("C:/Users/18904/Github/huashu-cup","题目","附件数据")
st=read_csv(file.path(ap,"storage_information_storage_information.csv"),locale=locale(encoding="UTF-8"),show_col_types=FALSE)
rt=read_csv(file.path(ap,"region_time_data_region_time_data.csv"),locale=locale(encoding="UTF-8"),show_col_types=FALSE)
pue=c(区域A=1.35,区域B=1.35,区域C=1.38,区域D=1.28,区域E=1.25,区域F=1.27)
R=st$区域编号;TT=2407
base=function(net,sell,Smax){pg=pmax(net,0);rem=pmax(-net,0);ex=pmin(Smax,rem);cur=pmax(rem-ex,0)
 list(m=c(成本=sum(pg*price)-sum(ex*sell),碳=sum(pg*ci),峰值=max(pg-ex),波动=sd(pg-ex)),nc=pg-ex)}
stor=function(net,price,sell,ci,E,Smin,S0,Pc,Pd,ec,ed,Smax){win=24;tg=as.numeric(stats::filter(net,rep(1/win,win),sides=2));tg[is.na(tg)]=net[is.na(tg)]
 band=sd(net)*.2;qp=quantile(price,c(.35,.65));soc=S0;n=length(net);pg=ds=ch=ex=cur=socc=numeric(n)
 for(t in 1:n){if(net[t]<0){sc=min(Pc,-net[t],(E-soc)/ec);soc=soc+ec*sc;rem=-net[t]-sc
   ex[t]=min(Smax,rem);cur[t]=max(0,rem-ex[t]);ch[t]=sc;pg[t]=0}
  else if(net[t]>tg[t]+band&&price[t]>qp[1]&&soc>Smin){ds[t]=min(Pd,net[t]-tg[t],(soc-Smin)*ed);soc=soc-ds[t]/ed;pg[t]=net[t]-ds[t]}
  else if(net[t]<tg[t]-band&&price[t]<qp[2]&&soc<E){gc=min(Pc,tg[t]-net[t],(E-soc)/ec);soc=soc+ec*gc;pg[t]=net[t]+gc;ch[t]=gc}
  else{pg[t]=net[t]};socc[t]=soc}
 if(soc<S0)for(t in n:1){if(soc>=S0)break;gc=min(Pc,(S0-soc)/ec,(E-soc)/ec);if(gc<=0)next
   soc=soc+ec*gc;ch[t]=ch[t]+gc;pg[t]=pg[t]+gc}
 list(m=c(成本=sum(pg*price)-sum(ex*sell),碳=sum(pg*ci),峰值=max(pg-ex),波动=sd(pg-ex),终SOC=round(soc,1)),ch=sum(ch),ds=sum(ds),nc=pg-ex,socc=socc)}
out=data.frame();alldat=data.frame()
for(r in R){s=st[st$区域编号==r,];q=rt[rt$区域编号==r,]
 fac=(q$基准人工智能信息技术负荷+q$非人工智能信息技术负荷)*pue[r]
 net=fac-q$直接消纳新能源;price=q$购电价格;sell=q$售电价格;ci=q$碳强度
 b=base(net,sell,s$外送上限)
 st2=stor(net,price,sell,ci,s$储能容量,s$最小荷电状态,s$初始荷电状态,s$最大充电功率,s$最大放电功率,s$充电效率,s$放电效率,s$外送上限)
 cat("区域",r," 充电量=",round(st2$ch,0)," 放电量=",round(st2$ds,0)," 终SOC=",st2$m[5],"\n")
 out=rbind(out,data.frame(区域=r,方案="无储能",成本万元=round(b$m[1]/1e4,1),碳排放=round(b$m[2],0),峰值净购电=round(b$m[3],1),负荷波动=round(b$m[4],2)))
 out=rbind(out,data.frame(区域=r,方案="有储能",成本万元=round(st2$m[1]/1e4,1),碳排放=round(st2$m[2],0),峰值净购电=round(st2$m[3],1),负荷波动=round(st2$m[4],2)))
 alldat=rbind(alldat,data.frame(区域=r,小时=0:2406,方案="无储能",净购电=b$nc),data.frame(区域=r,小时=0:2406,方案="有储能",净购电=st2$nc))}
print(out)
write_csv(out,file.path(od,"问题三_指标对比.csv"))
g2=ggplot(alldat[alldat$小时<=336,],aes(x=小时,y=净购电,color=方案,linetype=方案))+geom_line(linewidth=.6)+scale_color_manual(values=c("无储能"="grey45","有储能"="#2166AC"))+scale_linetype_manual(values=c("无储能"="dashed","有储能"="solid"))+facet_wrap(~区域,ncol=2,scales="fixed")+labs(x="小时(第0–336小时)",y="净购电功率(MW)",color="方案",linetype="方案")+theme_bw()+theme(legend.position="bottom")
ggsave(file.path(od,"问题三_净购电对比_局部.png"),g2,width=13,height=9,dpi=150)
cat("输出目录:",od,"\n")
