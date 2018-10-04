# Parexel period

## 2015年前使用SAS时的部分积累，内容主要为工具宏

ADM：本宏用于将读入数据集按照输入参数做相应规整。包括：字符型字段长度规约；字段类型相互转化、定长，更新属性。

TF_Fmt：本宏专注数据集的format功能描述。1. 可从数据集中按照id字段和对应的char字段提取维表；2. 可遍历某个字段全部值的length，按实值最大length对format做规约；3. 对指定字段做类型转换，设置新字段的format。

TF_Report：仿照默克公司table宏效果，编写画表格宏。通过自定义规则宏变量，对输入数据集做格式化表格输出。

Sharing：Parexel时期所做的SAS technology sharing。
