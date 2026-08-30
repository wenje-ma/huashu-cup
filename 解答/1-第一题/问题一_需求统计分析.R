<<<<<<< HEAD:解答/1-第一题/问题一_需求统计分析.R
# 问题一：图形处理器需求统计分析（表1/表2 统计 + 图1 可视化）
# 输出：GPU需求统计_分类型.csv（表1）、GPU需求统计_分区域.csv（表2）
#       图 1(a) 需求分布_箱线图.png（表1可视化）、图 1(b) 需求分布_堆叠柱状图.png（表2可视化）
# 输入：题目/附件数据/workload_trace_Sheet1.csv
library(readr);library(dplyr);library(ggplot2)
set.seed(1)
base = "C:/Users/18904/Github/huashu-cup"
od = file.path(base,"解答","1-第一题")
d = read_csv(file.path(base,"题目","附件数据","workload_trace_Sheet1.csv"),
             locale=locale(encoding="UTF-8"), show_col_types=FALSE)
d = d %>% mutate(gpu_h = `图形处理器需求量` * `连续执行时长` / 60,
  `任务类型` = factor(`任务类型`, levels=c("实时推理任务","批量推理任务","人工智能训练任务")),
  `来源区域` = factor(`来源区域`, levels=c("区域A","区域B","区域C","区域D","区域E","区域F")))

# ---- 表1：按任务类型 ----
tab1 = d %>% group_by(`任务类型`) %>% summarise(
  任务数=n(), 需求量均值=round(mean(`图形处理器需求量`),1),
  中位数=median(`图形处理器需求量`), 最大值=max(`图形处理器需求量`),
  图形处理器小时总量=round(sum(gpu_h),0))
write_csv(tab1, file.path(od,"GPU需求统计_分类型.csv")); print(tab1)

# ---- 表2：按区域 ----
tab2 = d %>% group_by(`来源区域`) %>% summarise(
  任务数=n(), 需求量均值=round(mean(`图形处理器需求量`),1),
  图形处理器小时总量=round(sum(gpu_h),0))
write_csv(tab2, file.path(od,"GPU需求统计_分区域.csv")); print(tab2)
=======
# 问题一：图形处理器需求统计分析绘图
# 输出论文图 1 的两张子图：
#   图 1(a) 需求分布_箱线图.png    —— 按任务类型的图形处理器需求量箱线图（表1的可视化）
#   图 1(b) 需求分布_堆叠柱状图.png —— 各区域各任务类型图形处理器-小时构成（表2的可视化）
# 输入：题目/附件数据/workload_trace_Sheet1.csv
library(readr);library(dplyr);library(ggplot2)
set.seed(1)
base = "C:/Users/21981/OneDrive/文档/GitHub/huashu-cup"
od = file.path(base,"解答","第一题")
d = read_csv(file.path(base,"题目","附件数据","workload_trace_Sheet1.csv"),
             locale=locale(encoding="UTF-8"), show_col_types=FALSE)
d = d %>% mutate(
  gpu_h = `图形处理器需求量` * `连续执行时长` / 60,
  `任务类型` = factor(`任务类型`, levels=c("实时推理任务","批量推理任务","人工智能训练任务")),
  `来源区域` = factor(`来源区域`, levels=c("区域A","区域B","区域C","区域D","区域E","区域F"))
)
cols = c("实时推理任务"="#E64B35","批量推理任务"="#4DBBD5","人工智能训练任务"="#00A087")

# ---------- 图 1(a)：按任务类型的图形处理器需求量箱线图 ----------
g1 = ggplot(d, aes(x=`任务类型`, y=`图形处理器需求量`, fill=`任务类型`)) +
  geom_boxplot(outlier.size=.5, outlier.alpha=.35, alpha=.9) +
  scale_fill_manual(values=cols) +
  labs(x=NULL, y="图形处理器需求量（等效GPU）") +
  theme_bw() + theme(legend.position="none")
ggsave(file.path(od,"需求分布_箱线图.png"), g1, width=9, height=6, dpi=150)

# ---------- 图 1(b)：各区域各任务类型图形处理器-小时构成（堆叠柱状图） ----------
agg = d %>% group_by(`来源区域`,`任务类型`) %>% summarise(gpu_h=sum(gpu_h), .groups="drop")
g2 = ggplot(agg, aes(x=`来源区域`, y=gpu_h, fill=`任务类型`)) +
  geom_col(position="stack", width=.7) +
  scale_fill_manual(values=cols) +
  labs(x="区域", y="图形处理器-小时总量", fill="任务类型") +
  theme_bw() + theme(legend.position="bottom")
ggsave(file.path(od,"需求分布_堆叠柱状图.png"), g2, width=9, height=6, dpi=150)

cat("done\n")
>>>>>>> li:解答/第一题/问题一_需求统计分析.R
