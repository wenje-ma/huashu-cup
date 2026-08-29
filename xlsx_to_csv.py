import os
import pandas as pd

src_dir = r"C:\Users\18904\Github\huashu-cup\题目\附件数据"   # xlsx 所在目录
dst_dir = r"C:\Users\18904\Github\huashu-cup\题目\附件数据"     # csv 输出目录
os.makedirs(dst_dir, exist_ok=True)

for fname in os.listdir(src_dir):
    if not fname.lower().endswith((".xlsx", ".xls")):
        continue
    xlsx_path = os.path.join(src_dir, fname)
    base = os.path.splitext(fname)[0]

    # 读取所有 sheet
    xls = pd.ExcelFile(xlsx_path)
    for sheet in xls.sheet_names:
        df = pd.read_excel(xlsx_path, sheet_name=sheet)
        # 单 sheet 直接用原文件名；多 sheet 追加 sheet 名
        out_name = f"{base}.csv" if len(xls.sheet_names) == 1 else f"{base}_{sheet}.csv"
        df.to_csv(os.path.join(dst_dir, out_name), index=False, encoding="utf-8-sig")
        print(f"已转换: {out_name}  ({len(df)} 行)")
