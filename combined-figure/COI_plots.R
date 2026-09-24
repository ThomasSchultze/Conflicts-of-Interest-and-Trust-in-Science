## COI and Trust in Science - Combined Plots


# 0. load libraries and data ----------------------------------------------

# Run from the repository root, e.g. Rscript combined-figure/COI_plots.R.

# custom colour palettes
BAM_primary = c('#00457D', '#FFD300', '#97BF0D', '#E6444F')

library(dplyr)
library(ggplot2)
library(gridExtra)


# read individual data sets
df1 = read.csv("combined-figure/data/plot_data_Exp1.csv") 
df2 = read.csv("combined-figure/data/plot_data_Exp2.csv") 
df3 = read.csv("combined-figure/data/plot_data_Exp3.csv") 
df4 = read.csv("combined-figure/data/plot_data_Exp4.csv") 
df5 = read.csv("combined-figure/data/plot_data_Exp5.csv")


# combine data sets
df = data.frame(Experiment = c(
  rep("Exp 1", nrow(df1)),
  rep("Exp 2", nrow(df2)),
  rep("Exp 3", nrow(df3)),
  rep("Exp 4", nrow(df4)),
  rep("Exp 5", nrow(df5))),
  rbind(df1, df2, df3, df4, df5))


# 1. prepare data for plotting --------------------------------------------

# transform independent variables to factors for analyses
df = df %>% mutate(COI = factor(COI, levels = c('no COI', 'COI', 'external COI')),
                               result = factor(result, levels = c('pro', 'con'),
                                               labels = c("pos", "neg")))


# create a single variable coding experimental conditions
df = df %>% mutate(
  condition = factor(paste(COI, result, sep = " - "),
                     levels = c("no COI - pos", "no COI - neg", 
                                "COI - pos", "COI - neg",
                                "external COI - pos", "external COI - neg")))

# z-standardisze trust measures to have them all on the same scale
df = df %>% group_by(Experiment, measure) %>%
  mutate(z_trust = scale(trust)) %>% 
  filter(measure%in% c("study", "authors", "replicability")) %>%
  mutate(measure = factor(measure, levels = c("study", "authors", "replicability")))


# 2. create plot ----------------------------------------------------------





p1 = ggplot(data = df %>% filter(Experiment == "Exp 1"), 
            aes(x = measure, y = z_trust, fill = COI, shape = result)) +
  geom_errorbar(stat = "summary", fun.data = mean_cl_normal,
                position = position_dodge(width = 0.5), width = 0.2) +
  geom_point(stat = "summary", fun = mean, size = 2.5,
             position = position_dodge(width = 0.5)) +
  scale_shape_manual(values = c(21, 23)) +
  scale_fill_manual(values = BAM_primary[1:3]) +
  theme_classic() +
    coord_cartesian(ylim = c(-1,1)) +
  theme(legend.position = "none",
        plot.title=element_text(size=12, face = "bold")) +
  ylab("z-standardised trust") +
  xlab("trust in") +
  ggtitle("Exp. 1 (drug scenario, lay sample)")

p2 = ggplot(data = df %>% filter(Experiment == "Exp 2"), 
            aes(x = measure, y = z_trust, fill = COI, shape = result)) +
  geom_errorbar(stat = "summary", fun.data = mean_cl_normal,
                position = position_dodge(width = 0.5), width = 0.2) +
  geom_point(stat = "summary", fun = mean, size = 2.5,
             position = position_dodge(width = 0.5)) +
  scale_shape_manual(values = c(21, 23)) +
  scale_fill_manual(values = BAM_primary[1:3]) +
  theme_classic() +
  coord_cartesian(ylim = c(-1,1)) +
  theme(legend.position = "none",
        plot.title=element_text(size=12, face = "bold")) +
  ylab("z-standardised trust") +
  xlab("trust in") +
  ggtitle("Exp. 2 (sewage treatment scenario, lay sample)")

p3 = ggplot(data = df %>% filter(Experiment == "Exp 3"), 
            aes(x = measure, y = z_trust, fill = COI, shape = result)) +
  geom_errorbar(stat = "summary", fun.data = mean_cl_normal,
                position = position_dodge(width = 0.5), width = 0.2) +
  geom_point(stat = "summary", fun = mean, size = 2.5,
             position = position_dodge(width = 0.5)) +
  scale_shape_manual(values = c(21, 23)) +
  scale_fill_manual(values = BAM_primary[1:3]) +
  theme_classic() +
  coord_cartesian(ylim = c(-1,1)) +
  theme(legend.position = "none",
        plot.title=element_text(size=12, face = "bold")) +
  ylab("z-standardised trust") +
  xlab("trust in") +
  ggtitle("Exp. 3 (drug scenario, expert sample)")


p4 = ggplot(data = df %>% filter(Experiment == "Exp 4"), 
            aes(x = measure, y = z_trust, fill = COI, shape = result)) +
  geom_errorbar(stat = "summary", fun.data = mean_cl_normal,
                position = position_dodge(width = 0.5), width = 0.2) +
  geom_point(stat = "summary", fun = mean, size = 2.5,
             position = position_dodge(width = 0.5)) +
  scale_shape_manual(values = c(21, 23)) +
  scale_fill_manual(values = BAM_primary[1:3]) +
  theme_classic() +
  coord_cartesian(ylim = c(-1,1)) +
  theme(legend.position = "none",
        plot.title=element_text(size=12, face = "bold")) +
  ylab("z-standardised trust") +
  xlab("trust in") +
  ggtitle("Exp. 4 (drug scenario, lay sample, salient costs)")

p5 = ggplot(data = df %>% filter(Experiment == "Exp 5"), 
                 aes(x = measure, y = z_trust, fill = COI, shape = result)) +
  geom_errorbar(stat = "summary", fun.data = mean_cl_normal,
                position = position_dodge(width = 0.5), width = 0.2) +
  geom_point(stat = "summary", fun = mean, size = 2.5,
             position = position_dodge(width = 0.5)) +
  scale_shape_manual(values = c(21, 23)) +
  scale_fill_manual(values = BAM_primary[1:3]) +
  theme_classic() +
  coord_cartesian(ylim = c(-1,1)) +
  ylab("z-standardised trust") +
  xlab("trust in") +
  labs(title = "Exp. 5 (drug scenario, lay sample, external COI)")  +
  theme(legend.position = "none",
        plot.title=element_text(size=12, face = "bold"))


# plot for the legend
p_legend = ggplot(data = df %>% filter(measure == "study"),
                  aes(x = Experiment, y = trust, fill = condition, shape = condition)) +
  geom_errorbar(stat = "summary", fun.data = mean_cl_normal,
                position = position_dodge(width = 0.4), width = 0.1) +
  geom_point(stat = "summary", fun = mean, size = 2.5,
             position = position_dodge(width = 0.4)) +
  scale_shape_manual(values = c(21, 23, 21, 23, 21, 23)) +
  scale_fill_manual(values = BAM_primary[c(1,1,2,2,3,3)]) +
  theme_classic(base_size = 12) +
  ylim(1,7) +
  ylab("trust in the study (1 to 7)") +
  theme(legend.position = "bottom")


fig1 = grid.arrange(p1, p2, p3, p4, p5, ggpubr::get_legend(p_legend),
                    ncol = 2)


dir.create("figures", showWarnings = FALSE)
ggsave("figures/Fig1.png", fig1, width = 9, height = 6, units = "in", dpi = 600)


 
