
# 安装所需包（仅需运行一次）
# install.packages(c("ggplot2", "dplyr", "readr"))

library(ggplot2)
library(dplyr)
library(readr)

# 读取数据：CSV 文件须包含 group 和 time 两列
# group 示例：WT、OE1、OE5、OE12
# time：每株植物的开花时间（天）
data <- read_csv("flowering_time.csv", show_col_types = FALSE)

# 设置组别顺序，并检查数据列
group_order <- c("WT", "OE1", "OE5", "OE12")

if (!all(c("group", "time") %in% names(data))) {
  stop("数据文件必须包含 group 和 time 两列。")
}

data <- data %>%
  mutate(
    group = factor(group, levels = group_order),
    time = as.numeric(time)
  ) %>%
  filter(!is.na(group), !is.na(time))

# 计算各组均值和标准误（SE）
summary_data <- data %>%
  group_by(group) %>%
  summarise(
    mean = mean(time),
    se = sd(time) / sqrt(n()),
    .groups = "drop"
  )

# 按示例图添加显著性星号；如需调整，可修改此处
stars <- data.frame(
  group = factor(c("OE1", "OE5", "OE12"), levels = group_order),
  label = "*"
)

# 为星号设置高度，留出误差线和数据点上方的空间
star_positions <- summary_data %>%
  filter(group %in% stars$group) %>%
  left_join(stars, by = "group") %>%
  mutate(y = mean + se + 3)

# 绘图
p <- ggplot(summary_data, aes(x = group, y = mean, fill = group)) +
  geom_col(width = 0.55, color = NA) +
  geom_errorbar(
    aes(ymin = mean - se, ymax = mean + se),
    width = 0.16,
    linewidth = 0.6
  ) +
  geom_jitter(
    data = data,
    aes(x = group, y = time),
    inherit.aes = FALSE,
    width = 0.09,
    height = 0,
    size = 2.1,
    color = "black"
  ) +
  geom_text(
    data = star_positions,
    aes(x = group, y = y, label = label),
    inherit.aes = FALSE,
    size = 5,
    vjust = 0
  ) +
  scale_fill_manual(
    values = c(
      "WT" = "#F4C84A",
      "OE1" = "#5594C7",
      "OE5" = "#D97D7D",
      "OE12" = "#078F88"
    )
  ) +
  scale_y_continuous(
    limits = c(0, 60),
    breaks = seq(0, 60, 10),
    expand = expansion(mult = c(0, 0.02))
  ) +
  labs(x = NULL, y = "Flowering time (d)") +
  theme_classic(base_size = 12) +
  theme(
    legend.position = "none",
    axis.title.y = element_text(size = 12),
    axis.text = element_text(color = "black", size = 11),
    axis.line = element_line(color = "black", linewidth = 0.6),
    axis.ticks = element_line(color = "black", linewidth = 0.6)
  )

print(p)

# 保存图片
ggsave(
  "flowering_time_plot.png",
  plot = p,
  width = 5,
  height = 3.5,
  dpi = 300
)
