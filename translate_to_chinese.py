# -*- coding: utf-8 -*-
"""
将 huashu-cup 题目目录下的英文内容统一替换为中文。

处理范围：
  1. 题目/附件数据/ 下全部 14 个 CSV（表头 + 枚举取值 + 说明文字）
  2. 题目/附件1.md
  3. 题目/C题.md

替换策略：
  - 数据表头：按"字段 -> 规范中文名"精确映射（来源于附件1数据字典）
  - 枚举取值：整格精确匹配替换（区域、任务类型、时延等级等）
  - 说明文字/公式：按"长词优先 + 词边界"的顺序替换标识符、缩写与单位
  - 保留：xlsx/docx 扩展名、LaTeX 命令(\text \sum \times)、数学变量(r/t/i)、希腊字母(ηc/ηd)、C题编号
"""
import os
import re

BASE = r"C:\Users\18904\Github\huashu-cup"
DATA_DIR = os.path.join(BASE, "题目", "附件数据")

# ============ 1. 枚举取值整格精确替换（数据单元格） ============
VALUE_MAP = {
    "RegionA": "区域A", "RegionB": "区域B", "RegionC": "区域C",
    "RegionD": "区域D", "RegionE": "区域E", "RegionF": "区域F",
    "AITraining": "人工智能训练任务",
    "BatchInference": "批量推理任务",
    "RealTimeInference": "实时推理任务",
    "High": "高", "Medium": "中", "Low": "低",
    "NonPreemptive": "不可抢占",
    "Local": "本地", "Regional": "区域内",
    "InterRegional": "区域间", "LongDistance": "远距离",
    "Valley": "谷", "Flat": "平", "Peak": "峰",
    "Very High": "很高", "Medium-Low": "中低",
    "Main_0_2399": "主时域0_2399",
    "Closure_2400_2406": "收尾时域2400_2406",
    "SOC_MWh is end-of-hour; InitialSOC_MWh is before Hour 0":
        "荷电状态为时段末状态；初始荷电状态为第0小时运行前状态",
    "From\\To": "来源\\目标",
}

# ============ 2. 字段名 -> 规范中文名（数据表头 / 字段名列 / 文档标识符） ============
FIELD_MAP = {
    # GPU_information
    "Region": "区域编号",
    "RegionRole": "区域角色",
    "Total_GPU": "图形处理器总容量",
    "Max_IT_Power_MW": "信息技术侧最大功率",
    "PUE": "能源使用效率",
    "Max_Facility_Power_MW": "设施侧最大功率",
    "Reserved_GPU_Ratio": "图形处理器预留比例",
    "Available_GPU": "可调度图形处理器容量",
    "Max_Workload_GPUh_per_h": "每小时最大图形处理器时能力",
    "CapacityLevel": "容量等级",
    "Remarks": "备注",
    # workload_trace
    "TaskID": "任务编号",
    "TaskType": "任务类型",
    "ArrivalHour": "到达小时",
    "EarliestStartHour": "最早开工小时",
    "GPU_Demand": "图形处理器需求量",
    "EstimatedDuration_min": "连续执行时长",
    "ExecutionMode": "执行模式",
    "SourceRegion": "来源区域",
    "MaxLatency_ms": "最大网络时延",
    "LatestFinishHour": "最晚完成小时",
    "DelaySensitivity": "延时敏感性",
    # network_latency
    "FromRegion": "源区域",
    "ToRegion": "目标区域",
    "NetworkLatency_ms": "网络时延",
    "LatencyClass": "时延等级",
    # power_mapping
    "GPU_Power_MW_per_EquivalentGPU": "每等效图形处理器功率（兆瓦）",
    # region_time_data
    "Hour": "调度时段编号",
    "PricePeriod": "电价时段",
    "ElectricityPrice_CNY_per_MWh": "购电价格",
    "SellPrice_CNY_per_MWh": "售电价格",
    "CarbonIntensity_tCO2_per_MWh": "碳强度",
    "AITrainingPower_MW": "人工智能训练任务功率",
    "GPU_Utilization_Percent": "图形处理器利用率",
    "AvailableRenewable_MW": "可用新能源出力",
    "UsedRenewable_MW": "直接消纳新能源",
    "RenewableCharge_MW": "新能源充电功率",
    "Curtailment_MW": "弃风弃光功率",
    "IT_Load_MW": "信息技术侧负荷",
    "Total_Load_MW": "设施侧总负荷",
    "GridPurchase_MW": "电网购电功率",
    "GridCharge_MW": "电网充电功率",
    "GridSell_MW": "外送售电功率",
    "NetGridImport_MW": "净购电功率",
    "CarbonEmission_tCO2": "碳排放量",
    "DemandResponseLevel": "需求响应等级",
    "SOC_MWh": "储能荷电状态",
    "ChargePower_MW": "储能充电功率",
    "DischargePower_MW": "储能放电功率",
    "Baseline_AI_IT_Load_MW": "基准人工智能信息技术负荷",
    "NonAI_IT_Load_MW": "非人工智能信息技术负荷",
    "DataPeriod": "数据时段",
    # storage_information
    "StorageCapacity_MWh": "储能容量",
    "MinSOC_MWh": "最小荷电状态",
    "InitialSOC_MWh": "初始荷电状态",
    "MaxChargePower_MW": "最大充电功率",
    "MaxDischargePower_MW": "最大放电功率",
    "ChargeEfficiency": "充电效率",
    "DischargeEfficiency": "放电效率",
    "SellLimit_MW": "外送上限",
    "MaxGridImport_MW": "最大购电功率",
    "MaxGridExport_MW": "最大外送功率",
    "SOC_State_Convention": "荷电状态时点口径",
    # 公式中出现的无单位标识符
    "AI_IT_Load": "人工智能信息技术负荷",
    "NonAI_IT_Load": "非人工智能信息技术负荷",
    "IT_Load": "信息技术负荷",
    "Total_Load": "设施总负荷",
    "GridPurchase": "电网购电",
    "AvailableRenewable": "可用新能源",
    "DischargePower": "放电功率",
    "ChargePower": "充电功率",
    "GridSell": "外送售电",
    "Curtailment": "弃风弃光",
    "CarbonEmission": "碳排放",
    "CarbonIntensity": "碳强度",
    "ElectricityPrice": "购电价格",
    "SellPrice": "售电价格",
    "Overlap": "重叠",
    "GPU_Power": "图形处理器功率",
    "IT_Power": "信息技术功率",
    "EstimatedDuration": "连续执行时长",
}

# 文件名引用翻译
FILE_MAP = {
    "storage_information.xlsx": "储能信息.xlsx",
    "region_time_data.xlsx": "区域时间数据.xlsx",
    "workload_trace.xlsx": "工作负载轨迹.xlsx",
    "network_latency.xlsx": "网络时延.xlsx",
    "power_mapping.xlsx": "功率映射.xlsx",
    "GPU_information.xlsx": "图形处理器信息.xlsx",
}

# 缩写/单位翻译（词边界）
ABBR_MAP = {
    "GPUh": "图形处理器时",
    "GPU": "图形处理器",
    "AI": "人工智能",
    "IT": "信息技术",
    "PUE": "能源使用效率",
    "SOC": "荷电状态",
    "SLA": "服务等级协议",
    "MWh": "兆瓦时",
    "MW": "兆瓦",
    "ms": "毫秒",
    "min": "分钟",
    "tCO2": "吨二氧化碳",
    "CNY": "人民币",
    "kWh": "千瓦时",
    "hour": "小时",
    "h": "小时",
}


# ============ CSV 行解析/组装（保留引号与换行风格） ============
def parse_csv_line(line):
    """解析一行 CSV，返回 (字段列表, 是否被引号包裹列表)。"""
    fields, quoted = [], []
    cur, in_q = [], False
    i, n = 0, len(line)
    while i < n:
        c = line[i]
        if in_q:
            if c == '"':
                if i + 1 < n and line[i + 1] == '"':
                    cur.append('"'); i += 2; continue
                in_q = False; i += 1; continue
            cur.append(c); i += 1; continue
        if c == '"':
            in_q = True; i += 1; continue
        if c == ',':
            fields.append(''.join(cur)); quoted.append(False); cur = []; i += 1; continue
        cur.append(c); i += 1
    fields.append(''.join(cur)); quoted.append(in_q)
    return fields, quoted


def join_csv_line(fields, quoted):
    """组装 CSV 行，保留原引号风格。"""
    out = []
    for f, q in zip(fields, quoted):
        if q or (',' in f) or ('"' in f) or ('\n' in f) or ('\r' in f):
            out.append('"' + f.replace('"', '""') + '"')
        else:
            out.append(f)
    return ','.join(out)


# ============ 文本翻译引擎 ============
def _compile_pairs():
    """构造有序替换对（长词优先）。返回 list[(编译后regex, 替换串)]。"""
    pairs = []

    # 1) 文件名（字面量）
    for k in sorted(FILE_MAP, key=len, reverse=True):
        pairs.append((re.compile(re.escape(k)), FILE_MAP[k]))

    # 2) LaTeX 转义下划线形式：AI\_IT\_Load
    latex_variants = {}
    for k, v in FIELD_MAP.items():
        if '_' in k:
            latex_variants[k.replace('_', '\\_')] = v
    for k in sorted(latex_variants, key=len, reverse=True):
        pairs.append((re.compile(re.escape(k)), latex_variants[k]))

    # 3) 字段标识符（长词优先，词边界）
    id_bound = lambda s: r'(?<![A-Za-z0-9])' + re.escape(s) + r'(?![A-Za-z0-9])'
    for k in sorted(FIELD_MAP, key=len, reverse=True):
        pairs.append((re.compile(id_bound(k)), FIELD_MAP[k]))

    # 4) 枚举取值（长词优先，词边界）
    enum_order = sorted(VALUE_MAP, key=len, reverse=True)
    for k in enum_order:
        if re.search(r'[A-Za-z]', k):
            pairs.append((re.compile(id_bound(k)), VALUE_MAP[k]))

    # 5) 区域名 RegionA..F 单独处理（已在 VALUE_MAP，但词边界策略不同）
    #    VALUE_MAP 已覆盖，跳过。

    # 6) 缩写/单位（词边界，允许左侧为数字，如 1h -> 1小时）
    unit_bound = lambda s: r'(?<![A-Za-z])' + re.escape(s) + r'(?![A-Za-z])'
    abbr_order = sorted(ABBR_MAP, key=len, reverse=True)
    for k in abbr_order:
        pairs.append((re.compile(unit_bound(k)), ABBR_MAP[k]))

    return pairs


TEXT_PAIRS = _compile_pairs()


def translate_text(text):
    """对文本执行全部有序替换，并清理中文之间的多余空格。"""
    for rx, repl in TEXT_PAIRS:
        text = rx.sub(repl, text)
    # 清理中文与中文之间的空格（注意：不能用 \s，会匹配换行符导致跨行合并）
    text = re.sub(r'(?<=[\u3400-\u9fff\uF900-\uFAFF])[ \t]+(?=[\u3400-\u9fff\uF900-\uFAFF])', '', text)
    text = re.sub(r' {2,}', ' ', text)
    return text


def translate_cell(cell, use_text=True):
    """单元格翻译：先整格精确匹配，再走文本替换。"""
    if cell in VALUE_MAP:
        return VALUE_MAP[cell]
    if use_text:
        return translate_text(cell)
    return cell


# ============ 文件处理 ============
def process_csv(path, header_map, apply_text_to_values=True):
    """处理一个 CSV：表头用 header_map（找不到则走文本替换），数值走 translate_cell。"""
    with open(path, 'r', encoding='utf-8-sig', newline='') as f:
        raw = f.read()
    lines = raw.splitlines(keepends=True)
    out = []
    for idx, line in enumerate(lines):
        ending = '\r\n' if line.endswith('\r\n') else ('\n' if line.endswith('\n') else '')
        content = line[:len(line) - len(ending)]
        fields, quoted = parse_csv_line(content)
        if idx == 0:
            fields = [header_map.get(f, translate_text(f)) for f in fields]
        else:
            fields = [translate_cell(f, apply_text_to_values) for f in fields]
        out.append(join_csv_line(fields, quoted) + ending)
    with open(path, 'w', encoding='utf-8-sig', newline='') as f:
        f.write(''.join(out))
    print('  [OK]', os.path.basename(path), '->', len(lines), 'lines')


def process_md(path):
    with open(path, 'r', encoding='utf-8', newline='') as f:
        text = f.read()
    text = translate_text(text)
    with open(path, 'w', encoding='utf-8', newline='') as f:
        f.write(text)
    print('  [OK]', os.path.basename(path))


# ============ 各文件表头映射 ============
GPU_HEADER = {k: FIELD_MAP[k] for k in [
    "Region", "RegionRole", "Total_GPU", "Max_IT_Power_MW", "PUE",
    "Max_Facility_Power_MW", "Reserved_GPU_Ratio", "Available_GPU",
    "Max_Workload_GPUh_per_h", "CapacityLevel", "Remarks"]}

LATENCY_HEADER = {k: FIELD_MAP[k] for k in
                  ["FromRegion", "ToRegion", "NetworkLatency_ms", "LatencyClass"]}

POWER_HEADER = {k: FIELD_MAP[k] for k in
                ["TaskType", "GPU_Power_MW_per_EquivalentGPU", "Remarks"]}

STORAGE_HEADER = {k: FIELD_MAP[k] for k in [
    "Region", "StorageCapacity_MWh", "MinSOC_MWh", "InitialSOC_MWh",
    "MaxChargePower_MW", "MaxDischargePower_MW", "ChargeEfficiency",
    "DischargeEfficiency", "SellLimit_MW", "Remarks", "MaxGridImport_MW",
    "MaxGridExport_MW", "SOC_State_Convention"]}

TRACE_HEADER = {k: FIELD_MAP[k] for k in [
    "TaskID", "TaskType", "ArrivalHour", "GPU_Demand", "EstimatedDuration_min",
    "DelaySensitivity", "SourceRegion", "MaxLatency_ms", "LatestFinishHour",
    "EarliestStartHour", "ExecutionMode"]}

REGION_HEADER = {k: FIELD_MAP[k] for k in [
    "Hour", "Region", "PricePeriod", "ElectricityPrice_CNY_per_MWh",
    "SellPrice_CNY_per_MWh", "CarbonIntensity_tCO2_per_MWh",
    "AITrainingPower_MW", "GPU_Utilization_Percent", "AvailableRenewable_MW",
    "UsedRenewable_MW", "RenewableCharge_MW", "Curtailment_MW", "IT_Load_MW",
    "Total_Load_MW", "GridPurchase_MW", "GridCharge_MW", "GridSell_MW",
    "NetGridImport_MW", "CarbonEmission_tCO2", "DemandResponseLevel",
    "SOC_MWh", "ChargePower_MW", "DischargePower_MW",
    "Baseline_AI_IT_Load_MW", "NonAI_IT_Load_MW", "DataPeriod"]}


# 字段说明类文件：将"字段名"列映射为规范中文名，"中文名称"列用文本翻译
def process_field_desc(path, field_col=0, name_col=1):
    with open(path, 'r', encoding='utf-8-sig', newline='') as f:
        raw = f.read()
    lines = raw.splitlines(keepends=True)
    out = []
    for idx, line in enumerate(lines):
        ending = '\r\n' if line.endswith('\r\n') else ('\n' if line.endswith('\n') else '')
        content = line[:len(line) - len(ending)]
        fields, quoted = parse_csv_line(content)
        if idx == 0:
            fields = [translate_text(f) for f in fields]
        else:
            for ci, val in enumerate(fields):
                if ci == field_col:
                    # 字段名：优先用规范中文名
                    fields[ci] = FIELD_MAP.get(val.strip(), translate_text(val))
                elif name_col is not None and ci == name_col:
                    # 中文名称：文本翻译（描述性）
                    fields[ci] = translate_text(val)
                else:
                    fields[ci] = translate_text(val)
        out.append(join_csv_line(fields, quoted) + ending)
    with open(path, 'w', encoding='utf-8-sig', newline='') as f:
        f.write(''.join(out))
    print('  [OK]', os.path.basename(path))


def main():
    print('== 处理 附件数据 CSV ==')
    process_csv(os.path.join(DATA_DIR, 'GPU_information_GPU中心基础情况.csv'), GPU_HEADER)
    process_csv(os.path.join(DATA_DIR, 'network_latency_network_latency.csv'), LATENCY_HEADER)
    process_csv(os.path.join(DATA_DIR, 'network_latency_时延矩阵.csv'), {'From\\To': '来源\\目标'})
    process_csv(os.path.join(DATA_DIR, 'power_mapping_任务功率映射.csv'), POWER_HEADER)
    process_csv(os.path.join(DATA_DIR, 'storage_information_storage_information.csv'), STORAGE_HEADER)
    # 大数据文件：数值/枚举整格替换（不做逐格文本替换，保持速度与安全）
    process_csv(os.path.join(DATA_DIR, 'workload_trace_Sheet1.csv'), TRACE_HEADER, apply_text_to_values=False)
    process_csv(os.path.join(DATA_DIR, 'region_time_data_region_time_data.csv'), REGION_HEADER, apply_text_to_values=False)
    # 说明类文件
    process_field_desc(os.path.join(DATA_DIR, 'GPU_information_字段说明.csv'))
    process_field_desc(os.path.join(DATA_DIR, 'network_latency_字段说明.csv'))
    process_field_desc(os.path.join(DATA_DIR, 'network_latency_模型说明.csv'))
    process_field_desc(os.path.join(DATA_DIR, 'power_mapping_计算口径.csv'))
    process_field_desc(os.path.join(DATA_DIR, 'region_time_data_字段说明.csv'))
    process_field_desc(os.path.join(DATA_DIR, 'storage_information_字段说明.csv'))
    process_field_desc(os.path.join(DATA_DIR, 'workload_trace_字段说明.csv'))

    print('== 处理 文档 ==')
    process_md(os.path.join(BASE, '题目', '附件1.md'))
    process_md(os.path.join(BASE, '题目', 'C题.md'))
    print('完成。')


if __name__ == '__main__':
    main()
