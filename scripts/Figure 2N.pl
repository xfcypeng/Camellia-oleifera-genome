
library(ggplot2)
library(dplyr)

# 原始数据
dat <- data.frame(
  Construct = rep(c("PERK", "AFH", "BRR2a"), each = 10),
  Condition = rep(rep(c("Empty", "Treatment"), each = 5), times = 3),
  Value = c(
    # PERK
    1.2515, 1.2639, 1.8912, 1.2731, 1.6432,
    2.5231, 2.6278, 3.4314, 2.5867, 3.1969,
    # AFH
    1.6119, 1.3479, 1.6102, 1.2311, 1.2811,
    3.5639, 3.4671, 3.2793, 3.1881, 2.9127,
    # BRR2a
    1.4265, 1.2138, 1.2691, 1.2382, 1.5471,
    3.8123, 3.0959, 3.1876, 3.3992, 3.9241
  )
)

# 按每个 Construct 的 Empty 均值归一化
dat <- dat %>%
  group_by(Construct) %>%
  mutate(
    Empty_mean = mean(Value[Condition == "Empty"]),
    Normalized = Value / Empty_mean
  ) %>%
  ungroup() %>%
  mutate(
    Construct = factor(Construct, levels = c("PERK", "AFH", "BRR2a")),
    Condition = factor(Condition, levels = c("Empty", "Treatment"))
  )

# 计算均值和 SEM
summary_dat <- dat %>%
  group_by(Construct, Condition) %>%
  summarise(
    Mean = mean(Normalized),
    SEM = sd(Normalized) / sqrt(n()),
    .groups = "drop"
  )

# 绘图：柱形、SEM 误差棒和轻微水平错开的单个数据点
p <- ggplot(summary_dat, aes(x = Condition, y = Mean, fill = Condition)) +
  geom_col(
    width = 0.55,
    color = "black",
    linewidth = 0.6
  ) +
  geom_errorbar(
    aes(ymin = Mean - SEM, ymax = Mean + SEM),
    width = 0.16,
    linewidth = 0.7
  ) +
  geom_point(
    data = dat,
    aes(x = Condition, y = Normalized),
    inherit.aes = FALSE,
    position = position_jitter(width = 0.045, height = 0, seed = 123),
    shape = 16,
    size = 2.5,
    color = "black"
  ) +
  facet_wrap(~ Construct, nrow = 1) +
  scale_fill_manual(
    values = c("Empty" = "#88A6D8", "Treatment" = "#5BC0A5"),
    labels = c("Empty" = "Empty", "Treatment" = "Treatment")
  ) +
  scale_x_discrete(labels = c("Empty" = "Empty", "Treatment" = NULL)) +
  scale_y_continuous(
    breaks = 0:3,
    expand = expansion(mult = c(0, 0.04))
  ) +
  coord_cartesian(ylim = c(0, 3.1)) +
  labs(
    x = NULL,
    y = "Normalized LUC/REN"
  ) +
  theme_classic(base_size = 15) +
  theme(
    legend.position = "none",
    strip.background = element_blank(),
    strip.text = element_text(size = 16, face = "plain"),
    axis.title.y = element_text(size = 15),
    axis.text = element_text(size = 13, color = "black"),
    axis.line = element_line(color = "black", linewidth = 0.7),
    axis.ticks = element_line(color = "black", linewidth = 0.7),
    panel.spacing = grid::unit(1.2, "lines")
  )

# 显示图形
print(p)

# 保存为高分辨率图片
ggsave(
  filename = "normalized_LUC_REN_scatter.png",
  plot = p,
  width = 10,
  height = 5,
  dpi = 300
)
