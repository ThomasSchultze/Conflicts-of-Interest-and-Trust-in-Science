### COI and Trust in Science - Study 2


# 0. load libraries and data ----------------------------------------------

# Run from the repository root, e.g. Rscript experiments/experiment-2/COI_Exp2.R.

library(BayesFactor)
library(dplyr)
library(yarrr)
library(HDInterval)

data_COI = read.csv('experiments/experiment-2/data_COI_Exp2.csv')

# transform independent variables to factors for analyses
data_COI = data_COI %>% mutate(COI = factor(COI, levels = c('no COI', 'COI')),
                               result = factor(result, levels = c('pro', 'con'),
                                               labels = c('pos', 'neg')))


# 1. Inspect attention check and exclude participants who failed it -------

# check how many participants failed the attention check
table(data_COI$AC)

# remove cases with incorrect attention checks
data_COI = data_COI %>% filter(AC == 'correct')


# 2. analyses -------------------------------------------------------------

# 2.0 custom functions ----------------------------------------------------

print_t_test = function(freq_t, Bayes_t, one_tailed = F){
  # this function requires a frequentist and a Bayesian t-test as arguments
  # it uses information from these tests to create a copy-pastable report
  # of the t-test including:
  # - t-value and degrees of freedom
  # - the Bayes factor
  # - posterior estimate of the effect size d and the 95% HDI
  
  ## extract the relevant data
  
  # t-value rounded to 2 decimals
  t_value = round(freq_t$statistic, 2)
  
  # degrees of freedom (complete since we assume equal variances)
  df = freq_t$parameter
  
  # Bayes factor, rounded to two decimals
  BF = ifelse(one_tailed == F,
              round(exp(Bayes_t@bayesFactor$bf), 2),
              round(exp(Bayes_t@bayesFactor$bf[2]), 2))
              
  
  # sample the posterior of the effect size for the Bayesian t-test
  set.seed(1234) # for reproducible posterior sampling
  if(one_tailed == F){
    post_d = posterior(Bayes_t, iterations = 50000)
  }
  else{
    post_d = posterior(Bayes_t[2], iterations = 50000)
  }
  
  # estimate of the posterior effect size d, rounded to two decimals
  d_est = round(mean(post_d[,4]),2)
  
  # lower bound of the 95% HDI, rounded to two decimals
  d_lower = round(hdi(post_d[,4], ci=.95)[1], 2)
  
  # upper bound of the 95% HDI, rounded to two decimals
  d_upper = round(hdi(post_d[,4], ci=.95)[2], 2)
  
  
  # create a character string containing all the relevant info on the t-test
  report = paste("t(", df, ") = ", format(t_value, nsmall = 2), 
                 ", BF = ", BF, 
                 ", d = ", format(d_est, nsmall = 2),
                 ", 95% HDI [", format(d_lower, nsmall = 2),
                 "; ", format(d_upper, nsmall = 2), "]", sep = "")
  
  return(report)
  
}


# 2.1 Trust in the study --------------------------------------------------

# 2.1.1 Bayesian ANOVA ------------------------------------------------------

# set seed for reproducible results
set.seed(1234)

# run the Bayesian ANOVA
aov1 = anovaBF(trust_study ~ COI * result, data = na.omit(data_COI), progress = F)

# BF for the interaction effect
exp(aov1@bayesFactor$bf)[4]/exp(aov1@bayesFactor$bf)[3]


# 2.1.2 simple effects ---------------------------------------------

# 2.1.2.1 simple effect of RES when there is no COI ----------------

# frequentist t-test of the simple effect to derive t-statistic and degrees of freedom from
freq_se_1a = t.test(data_COI$trust_study[data_COI$COI == 'no COI' & data_COI$result == 'neg'],
          data_COI$trust_study[data_COI$COI == 'no COI' & data_COI$result == 'pos'],
          var.equal = T)

# Bayesian t-test
bayes_se_1a = ttestBF(data_COI$trust_study[data_COI$COI == 'no COI' & data_COI$result == 'neg'],
                      data_COI$trust_study[data_COI$COI == 'no COI' & data_COI$result == 'pos'],
                      rscale = 0.5)

# display the results
print_t_test(freq_t = freq_se_1a, Bayes_t = bayes_se_1a, one_tailed = F)


# 2.1.2.2 simple effect of RES when there is a COI ----------------

# frequentist t-test of the simple effect to derive t-statistic and degrees of freedom from
freq_se_1b = t.test(data_COI$trust_study[data_COI$COI == 'COI' & data_COI$result == 'neg'],
                    data_COI$trust_study[data_COI$COI == 'COI' & data_COI$result == 'pos'],
                    var.equal = T)

# Bayesian t-test
bayes_se_1b = ttestBF(data_COI$trust_study[data_COI$COI == 'COI' & data_COI$result == 'neg'],
                      data_COI$trust_study[data_COI$COI == 'COI' & data_COI$result == 'pos'],
                      rscale = 0.5, nullInterval = c(-Inf, 0))

# display the results
print_t_test(freq_t = freq_se_1b, Bayes_t = bayes_se_1b, one_tailed = T)



# 2.1.2.3 simple effect of COI when results favour the new method ----------------

# frequentist t-test of the simple effect to derive t-statistic and degrees of freedom from
freq_se_1c = t.test(data_COI$trust_study[data_COI$COI == 'no COI' & data_COI$result == 'pos'],
                    data_COI$trust_study[data_COI$COI == 'COI' & data_COI$result == 'pos'],
                    var.equal = T)

# Bayesian t-test
bayes_se_1c = ttestBF(data_COI$trust_study[data_COI$COI == 'no COI' & data_COI$result == 'pos'],
                      data_COI$trust_study[data_COI$COI == 'COI' & data_COI$result == 'pos'],
                      rscale = 0.5, nullInterval = c(-Inf, 0))

# display the results
print_t_test(freq_t = freq_se_1c, Bayes_t = bayes_se_1c, one_tailed = T)


# 2.1.2.4 simple effect of COI when results speak against the new method ----------------

# frequentist t-test of the simple effect to derive t-statistic and degrees of freedom from
freq_se_1d = t.test(data_COI$trust_study[data_COI$COI == 'COI' & data_COI$result == 'neg'],
                    data_COI$trust_study[data_COI$COI == 'no COI' & data_COI$result == 'neg'],
                    var.equal = T)

# Bayesian t-test
bayes_se_1d = ttestBF(data_COI$trust_study[data_COI$COI == 'COI' & data_COI$result == 'neg'],
                      data_COI$trust_study[data_COI$COI == 'no COI' & data_COI$result == 'neg'],
                      rscale = 0.5, nullInterval = c(-Inf, 0))

# display the results
print_t_test(freq_t = freq_se_1d, Bayes_t = bayes_se_1d, one_tailed = T)

## As per the preregistration, we now run a two-tailed Bayesian t-test to 
## infer whether the evidence in favour of the Null hypothesis is due to 
## a) absence of a difference or b) a difference in the opposite direction

# Bayesian t-test
bayes_se_1d.2 = ttestBF(data_COI$trust_study[data_COI$COI == 'COI' & data_COI$result == 'neg'],
                      data_COI$trust_study[data_COI$COI == 'no COI' & data_COI$result == 'neg'],
                      rscale = 0.5)

# display the results
print_t_test(freq_t = freq_se_1d, Bayes_t = bayes_se_1d.2)

# The follow-up test indicates evidence of the absence of a difference, so
# we adopt this as the best representation of the data and report the results
# of this test.


# 2.1.3 plot --------------------------------------------------------------

plot_study = function(){
  
  # custom QUB colour palette
  QUB_palette = c("#00A1E1", "#F18903", "#AC004D")
  
  # set figure margins
  par(mar = c(2.5,2.5,1,1))
  
  # call the base plot
  pirateplot(data = data_COI, trust_study ~ result * COI, theme = 1, inf.method = 'hdi',
             pal = QUB_palette[1:2], gl = 0,  xaxt = 'n', yaxt = 'n', bty = 'L', inf.lwd = 0.5,
             xlab = '', ylab = '', bean.lwd = 0.5, point.cex = 0.5, gl.lwd = 1, ylim = c(0, 10))
  
  # configure axes and labels
  mtext('conflict of interest', 1, font = 2, line = 1, cex = 0.9)
  axis(1, cex.axis = 0.75, padj = -2, at = c(1.5, 4.5), 
       labels = c("no conflict", "conflict"), tcl = -0.25)
  mtext('trust in the study', 2, font = 2, line = 1.25, cex = 0.9)
  axis(2, cex.axis = 0.75, at = seq(1,7,1), padj = 1.5, tcl = -0.25)
  
  # add legend
  legend('topright', legend = c('results positive', 'results negative'), 
         fill = QUB_palette[1:2], bty = 'n')
  
  # add info on simple effect of COI for positive results
  lines(x = c(1, 4), y = c(7.5, 7.5))
  lines(x = c(1, 1), y = c(7.4, 7.5))
  lines(x = c(4, 4), y = c(7.4, 7.5))
  text(paste("BF = ", format(round(exp(bayes_se_1c@bayesFactor$bf[2]), 2), nsmall = 2), sep = ""),
       x = 2.5, y = 7.7, cex = 0.75)
  
  # add info on simple effect of COI for negative results
  lines(x = c(2, 5), y = c(8.5, 8.5))
  lines(x = c(2, 2), y = c(8.4, 8.5))
  lines(x = c(5, 5), y = c(8.4, 8.5))
  text(paste("BF = ", format(round(exp(bayes_se_1d@bayesFactor$bf[2]), 2), nsmall = 2), 
             " (two-tailed BF = ", format(round(exp(bayes_se_1d.2@bayesFactor$bf), 2), nsmall = 2),
             ")", sep = ""),
       x = 3.5, y = 8.7, cex = 0.75)
  
  # add info on simple effect of results when there is no COI
  lines(x = c(1, 2), y = c(0.5, 0.5))
  lines(x = c(1, 1), y = c(0.5, 0.6))
  lines(x = c(2, 2), y = c(0.5, 0.6))
  text(paste("BF = ", format(round(exp(bayes_se_1a@bayesFactor$bf), 2), nsmall = 2), sep = ""),
       x = 1.5, y = 0.3, cex = 0.75)
  
  # add info on simple effect of results when there is no COI
  lines(x = c(4, 5), y = c(0.5, 0.5))
  lines(x = c(4, 4), y = c(0.5, 0.6))
  lines(x = c(5, 5), y = c(0.5, 0.6))
  text(paste("BF = ", format(round(exp(bayes_se_1b@bayesFactor$bf[2]), 2), nsmall = 2), sep = ""),
       x = 4.5, y = 0.3, cex = 0.75)
 
}

plot_study()


# 2.2 Trust in the authors --------------------------------------------------

# 2.2.1 Bayesian ANOVA ------------------------------------------------------

# set seed for reproducible results
set.seed(1234)

# run the Bayesian ANOVA
aov2 = anovaBF(trust_authors ~ COI * result, data = na.omit(data_COI), progress = F)

# BF for the interaction effect
exp(aov2@bayesFactor$bf)[4]/exp(aov2@bayesFactor$bf)[3]


# 2.2.2 simple effects ---------------------------------------------

# 2.2.2.1 simple effect of RES when there is no COI ----------------

# frequentist t-test of the simple effect to derive t-statistic and degrees of freedom from
freq_se_2a = t.test(data_COI$trust_authors[data_COI$COI == 'no COI' & data_COI$result == 'neg'],
                    data_COI$trust_authors[data_COI$COI == 'no COI' & data_COI$result == 'pos'],
                    var.equal = T)

# Bayesian t-test
bayes_se_2a = ttestBF(data_COI$trust_authors[data_COI$COI == 'no COI' & data_COI$result == 'neg'],
                      data_COI$trust_authors[data_COI$COI == 'no COI' & data_COI$result == 'pos'],
                      rscale = 0.5)

# display the results
print_t_test(freq_t = freq_se_2a, Bayes_t = bayes_se_2a)


# 2.2.2.2 simple effect of RES when there is a COI ----------------

# frequentist t-test of the simple effect to derive t-statistic and degrees of freedom from
freq_se_2b = t.test(data_COI$trust_authors[data_COI$COI == 'COI' & data_COI$result == 'neg'],
                    data_COI$trust_authors[data_COI$COI == 'COI' & data_COI$result == 'pos'],
                    var.equal = T)

# Bayesian t-test
bayes_se_2b = ttestBF(data_COI$trust_authors[data_COI$COI == 'COI' & data_COI$result == 'neg'],
                      data_COI$trust_authors[data_COI$COI == 'COI' & data_COI$result == 'pos'],
                      rscale = 0.5, nullInterval = c(-Inf, 0))

# display the results
print_t_test(freq_t = freq_se_2b, Bayes_t = bayes_se_2b, one_tailed = T)


# 2.2.2.3 simple effect of COI when results favour the new method ----------------

# frequentist t-test of the simple effect to derive t-statistic and degrees of freedom from
freq_se_2c = t.test(data_COI$trust_authors[data_COI$COI == 'no COI' & data_COI$result == 'pos'],
                    data_COI$trust_authors[data_COI$COI == 'COI' & data_COI$result == 'pos'],
                    var.equal = T)

# Bayesian t-test
bayes_se_2c = ttestBF(data_COI$trust_authors[data_COI$COI == 'no COI' & data_COI$result == 'pos'],
                      data_COI$trust_authors[data_COI$COI == 'COI' & data_COI$result == 'pos'],
                      rscale = 0.5, nullInterval = c(-Inf, 0))

# display the results
print_t_test(freq_t = freq_se_2c, Bayes_t = bayes_se_2c, one_tailed = T)


# 2.2.2.4 simple effect of COI when results speak against the new method ----------------

# frequentist t-test of the simple effect to derive t-statistic and degrees of freedom from
freq_se_2d = t.test(data_COI$trust_authors[data_COI$COI == 'COI' & data_COI$result == 'neg'],
                    data_COI$trust_authors[data_COI$COI == 'no COI' & data_COI$result == 'neg'],
                    var.equal = T)

# Bayesian t-test
bayes_se_2d = ttestBF(data_COI$trust_authors[data_COI$COI == 'COI' & data_COI$result == 'neg'],
                      data_COI$trust_authors[data_COI$COI == 'no COI' & data_COI$result == 'neg'],
                      rscale = 0.5, nullInterval = c(-Inf, 0))

# display the results
print_t_test(freq_t = freq_se_2d, Bayes_t = bayes_se_2d, one_tailed = T)


## As per the preregistration, we now run a two-tailed Bayesian t-test to 
## infer whether the evidence in favour of the Null hypothesis is due to 
## a) absence of a difference or b) a difference in the opposite direction

# Bayesian t-test
bayes_se_2d.2 = ttestBF(data_COI$trust_authors[data_COI$COI == 'COI' & data_COI$result == 'neg'],
                        data_COI$trust_authors[data_COI$COI == 'no COI' & data_COI$result == 'neg'],
                        rscale = 0.5)

# display the results
print_t_test(freq_t = freq_se_2d, Bayes_t = bayes_se_2d.2)

# The follow-up test indicates evidence of the absence of a difference, so
# we adopt this as the best representation of the data and report the results
# of this test.


# 2.2.3 plot --------------------------------------------------------------

# 2.1.3 plot --------------------------------------------------------------

plot_authors = function(){
  
  # custom QUB colour palette
  QUB_palette = c("#00A1E1", "#F18903", "#AC004D")
  
  # set figure margins
  par(mar = c(2.5,2.5,1,1))
  
  # call the base plot
  pirateplot(data = data_COI, trust_study ~ result * COI, theme = 1, inf.method = 'hdi',
             pal = QUB_palette[1:2], gl = 0,  xaxt = 'n', yaxt = 'n', bty = 'L', inf.lwd = 0.5,
             xlab = '', ylab = '', bean.lwd = 0.5, point.cex = 0.5, gl.lwd = 1, ylim = c(0, 10))
  
  # configure axes and labels
  mtext('conflict of interest', 1, font = 2, line = 1, cex = 0.9)
  axis(1, cex.axis = 0.75, padj = -2, at = c(1.5, 4.5), 
       labels = c("no conflict", "conflict"), tcl = -0.25)
  mtext('trust in the authors', 2, font = 2, line = 1.25, cex = 0.9)
  axis(2, cex.axis = 0.75, at = seq(1,7,1), padj = 1.5, tcl = -0.25)
  
  # add legend
  legend('topright', legend = c('results positive', 'results negative'), 
         fill = QUB_palette[1:2], bty = 'n')
  
  # add info on simple effect of COI for positive results
  lines(x = c(1, 4), y = c(7.5, 7.5))
  lines(x = c(1, 1), y = c(7.4, 7.5))
  lines(x = c(4, 4), y = c(7.4, 7.5))
  text(paste("BF = ", format(round(exp(bayes_se_2c@bayesFactor$bf[2]), 2), nsmall = 2), sep = ""),
       x = 2.5, y = 7.7, cex = 0.75)
  
  # add info on simple effect of COI for negative results
  lines(x = c(2, 5), y = c(8.5, 8.5))
  lines(x = c(2, 2), y = c(8.4, 8.5))
  lines(x = c(5, 5), y = c(8.4, 8.5))
  text(paste("BF = ", format(round(exp(bayes_se_2d@bayesFactor$bf[2]), 2), nsmall = 2), 
             " (two-tailed BF = ", format(round(exp(bayes_se_2d.2@bayesFactor$bf), 2), nsmall = 2),
             ")", sep = ""),
       x = 3.5, y = 8.7, cex = 0.75)
  
  # add info on simple effect of results when there is no COI
  lines(x = c(1, 2), y = c(0.5, 0.5))
  lines(x = c(1, 1), y = c(0.5, 0.6))
  lines(x = c(2, 2), y = c(0.5, 0.6))
  text(paste("BF = ", format(round(exp(bayes_se_2a@bayesFactor$bf), 2), nsmall = 2), sep = ""),
       x = 1.5, y = 0.3, cex = 0.75)
  
  # add info on simple effect of results when there is no COI
  lines(x = c(4, 5), y = c(0.5, 0.5))
  lines(x = c(4, 4), y = c(0.5, 0.6))
  lines(x = c(5, 5), y = c(0.5, 0.6))
  text(paste("BF = ", format(round(exp(bayes_se_2b@bayesFactor$bf[2]), 2), nsmall = 2), sep = ""),
       x = 4.5, y = 0.3, cex = 0.75)
  
}

plot_authors()


# 2.3 Trust in the journal --------------------------------------------------

# 2.3.1 Bayesian ANOVA ------------------------------------------------------

# set seed for reproducible results
set.seed(1234)

# run the Bayesian ANOVA
aov3 = anovaBF(trust_journal ~ COI * result, data = na.omit(data_COI), progress = F)

# BF for the interaction effect
exp(aov3@bayesFactor$bf)[4]/exp(aov3@bayesFactor$bf)[3]


# 2.3.2 simple effects ---------------------------------------------

# 2.3.2.1 simple effect of RES when there is no COI ----------------

# frequentist t-test of the simple effect to derive t-statistic and degrees of freedom from
freq_se_3a = t.test(data_COI$trust_journal[data_COI$COI == 'no COI' & data_COI$result == 'neg'],
                    data_COI$trust_journal[data_COI$COI == 'no COI' & data_COI$result == 'pos'],
                    var.equal = T)

# Bayesian t-test
bayes_se_3a = ttestBF(data_COI$trust_journal[data_COI$COI == 'no COI' & data_COI$result == 'neg'],
                      data_COI$trust_journal[data_COI$COI == 'no COI' & data_COI$result == 'pos'],
                      rscale = 0.5)

# display the results
print_t_test(freq_t = freq_se_3a, Bayes_t = bayes_se_3a)


# 2.3.2.2 simple effect of RES when there is a COI ----------------

# frequentist t-test of the simple effect to derive t-statistic and degrees of freedom from
freq_se_3b = t.test(data_COI$trust_journal[data_COI$COI == 'COI' & data_COI$result == 'neg'],
                    data_COI$trust_journal[data_COI$COI == 'COI' & data_COI$result == 'pos'],
                    var.equal = T)

# Bayesian t-test
bayes_se_3b = ttestBF(data_COI$trust_journal[data_COI$COI == 'COI' & data_COI$result == 'neg'],
                      data_COI$trust_journal[data_COI$COI == 'COI' & data_COI$result == 'pos'],
                      rscale = 0.5, nullInterval = c(-Inf, 0))

# display the results
print_t_test(freq_t = freq_se_3b, Bayes_t = bayes_se_3b, one_tailed = T)


# 2.3.2.3 simple effect of COI when results favour the new method ----------------

# frequentist t-test of the simple effect to derive t-statistic and degrees of freedom from
freq_se_3c = t.test(data_COI$trust_journal[data_COI$COI == 'no COI' & data_COI$result == 'pos'],
                    data_COI$trust_journal[data_COI$COI == 'COI' & data_COI$result == 'pos'],
                    var.equal = T)

# Bayesian t-test
bayes_se_3c = ttestBF(data_COI$trust_journal[data_COI$COI == 'no COI' & data_COI$result == 'pos'],
                      data_COI$trust_journal[data_COI$COI == 'COI' & data_COI$result == 'pos'],
                      rscale = 0.5, nullInterval = c(-Inf, 0))

# display the results
print_t_test(freq_t = freq_se_3c, Bayes_t = bayes_se_3c, one_tailed = T)


# 2.3.2.4 simple effect of COI when results speak against the new method ----------------

# frequentist t-test of the simple effect to derive t-statistic and degrees of freedom from
freq_se_3d = t.test(data_COI$trust_journal[data_COI$COI == 'COI' & data_COI$result == 'neg'],
                    data_COI$trust_journal[data_COI$COI == 'no COI' & data_COI$result == 'neg'],
                    var.equal = T)

# Bayesian t-test
bayes_se_3d = ttestBF(data_COI$trust_journal[data_COI$COI == 'COI' & data_COI$result == 'neg'],
                      data_COI$trust_journal[data_COI$COI == 'no COI' & data_COI$result == 'neg'],
                      rscale = 0.5, nullInterval = c(-Inf, 0))

# display the results
print_t_test(freq_t = freq_se_3d, Bayes_t = bayes_se_3d, one_tailed = T)

## As per the preregistration, we now run a two-tailed Bayesian t-test to 
## infer whether the evidence in favour of the Null hypothesis is due to 
## a) absence of a difference or b) a difference in the opposite direction

# Bayesian t-test
bayes_se_3d.2 = ttestBF(data_COI$trust_journal[data_COI$COI == 'COI' & data_COI$result == 'neg'],
                        data_COI$trust_journal[data_COI$COI == 'no COI' & data_COI$result == 'neg'],
                        rscale = 0.5)

# display the results
print_t_test(freq_t = freq_se_3d, Bayes_t = bayes_se_3d.2)

# The follow-up test indicates evidence of the absence of a difference, so
# we adopt this as the best representation of the data and report the results
# of this test.


# 2.3.3 plot --------------------------------------------------------------

plot_journal = function(){
  
  # custom QUB colour palette
  QUB_palette = c("#00A1E1", "#F18903", "#AC004D")
  
  # set figure margins
  par(mar = c(2.5,2.5,1,1))
  
  # call the base plot
  pirateplot(data = data_COI, trust_journal ~ result * COI, theme = 1, inf.method = 'hdi',
             pal = QUB_palette[1:2], gl = 0,  xaxt = 'n', yaxt = 'n', bty = 'L', inf.lwd = 0.5,
             xlab = '', ylab = '', bean.lwd = 0.5, point.cex = 0.5, gl.lwd = 1, ylim = c(0, 10))
  
  # configure axes and labels
  mtext('conflict of interest', 1, font = 2, line = 1, cex = 0.9)
  axis(1, cex.axis = 0.75, padj = -2, at = c(1.5, 4.5), 
       labels = c("no conflict", "conflict"), tcl = -0.25)
  mtext('trust in the journal', 2, font = 2, line = 1.25, cex = 0.9)
  axis(2, cex.axis = 0.75, at = seq(1,7,1), padj = 1.5, tcl = -0.25)
  
  # add legend
  legend('topright', legend = c('results positive', 'results negative'), 
         fill = QUB_palette[1:2], bty = 'n')
  
  # add info on simple effect of COI for positive results
  lines(x = c(1, 4), y = c(7.5, 7.5))
  lines(x = c(1, 1), y = c(7.4, 7.5))
  lines(x = c(4, 4), y = c(7.4, 7.5))
  text(paste("BF = ", format(round(exp(bayes_se_3c@bayesFactor$bf[2]), 2), nsmall = 2), sep = ""),
       x = 2.5, y = 7.7, cex = 0.75)
  
  # add info on simple effect of COI for negative results
  lines(x = c(2, 5), y = c(8.5, 8.5))
  lines(x = c(2, 2), y = c(8.4, 8.5))
  lines(x = c(5, 5), y = c(8.4, 8.5))
  text(paste("BF = ", format(round(exp(bayes_se_3d@bayesFactor$bf[2]), 2), nsmall = 2), 
             " (two-tailed BF = ", format(round(exp(bayes_se_3d.2@bayesFactor$bf), 2), nsmall = 2),
             ")", sep = ""),
       x = 3.5, y = 8.7, cex = 0.75)
  
  # add info on simple effect of results when there is no COI
  lines(x = c(1, 2), y = c(0.5, 0.5))
  lines(x = c(1, 1), y = c(0.5, 0.6))
  lines(x = c(2, 2), y = c(0.5, 0.6))
  text(paste("BF = ", format(round(exp(bayes_se_3a@bayesFactor$bf), 2), nsmall = 2), sep = ""),
       x = 1.5, y = 0.3, cex = 0.75)
  
  # add info on simple effect of results when there is no COI
  lines(x = c(4, 5), y = c(0.5, 0.5))
  lines(x = c(4, 4), y = c(0.5, 0.6))
  lines(x = c(5, 5), y = c(0.5, 0.6))
  text(paste("BF = ", format(round(exp(bayes_se_3b@bayesFactor$bf[2]), 2), nsmall = 2), sep = ""),
       x = 4.5, y = 0.3, cex = 0.75)
  
}

plot_journal()


# 2.4 Trust in the company that developed the new method ----------------------

# 2.4.1 Bayesian ANOVA ------------------------------------------------------

# set seed for reproducible results
set.seed(1234)

# run the Bayesian ANOVA
aov4 = anovaBF(trust_company ~ COI * result, data = na.omit(data_COI), progress = F)

# BF for the interaction effect
exp(aov4@bayesFactor$bf)[4]/exp(aov4@bayesFactor$bf)[3]


# 2.4.2 simple effects ---------------------------------------------

# 2.4.2.1 simple effect of RES when there is no COI ----------------

# frequentist t-test of the simple effect to derive t-statistic and degrees of freedom from
freq_se_4a = t.test(data_COI$trust_company[data_COI$COI == 'no COI' & data_COI$result == 'neg'],
                    data_COI$trust_company[data_COI$COI == 'no COI' & data_COI$result == 'pos'],
                    var.equal = T)

# Bayesian t-test
bayes_se_4a = ttestBF(data_COI$trust_company[data_COI$COI == 'no COI' & data_COI$result == 'neg'],
                      data_COI$trust_company[data_COI$COI == 'no COI' & data_COI$result == 'pos'],
                      rscale = 0.5)

# display the results
print_t_test(freq_t = freq_se_4a, Bayes_t = bayes_se_4a)


# 2.4.2.2 simple effect of RES when there is a COI ----------------

# frequentist t-test of the simple effect to derive t-statistic and degrees of freedom from
freq_se_4b = t.test(data_COI$trust_company[data_COI$COI == 'COI' & data_COI$result == 'neg'],
                    data_COI$trust_company[data_COI$COI == 'COI' & data_COI$result == 'pos'],
                    var.equal = T)

# Bayesian t-test
bayes_se_4b = ttestBF(data_COI$trust_company[data_COI$COI == 'COI' & data_COI$result == 'neg'],
                      data_COI$trust_company[data_COI$COI == 'COI' & data_COI$result == 'pos'],
                      rscale = 0.5, nullInterval = c(-Inf, 0))

# display the results
print_t_test(freq_t = freq_se_4b, Bayes_t = bayes_se_4b, one_tailed = T)


# 2.4.2.3 simple effect of COI when results favour the new method ----------------

# frequentist t-test of the simple effect to derive t-statistic and degrees of freedom from
freq_se_4c = t.test(data_COI$trust_company[data_COI$COI == 'no COI' & data_COI$result == 'pos'],
                    data_COI$trust_company[data_COI$COI == 'COI' & data_COI$result == 'pos'],
                    var.equal = T)

# Bayesian t-test
bayes_se_4c = ttestBF(data_COI$trust_company[data_COI$COI == 'no COI' & data_COI$result == 'pos'],
                      data_COI$trust_company[data_COI$COI == 'COI' & data_COI$result == 'pos'],
                      rscale = 0.5, nullInterval = c(-Inf, 0))

# display the results
print_t_test(freq_t = freq_se_4c, Bayes_t = bayes_se_4c, one_tailed = T)


# 2.4.2.4 simple effect of COI when results speak against the new method ----------------

# frequentist t-test of the simple effect to derive t-statistic and degrees of freedom from
freq_se_4d = t.test(data_COI$trust_company[data_COI$COI == 'COI' & data_COI$result == 'neg'],
                    data_COI$trust_company[data_COI$COI == 'no COI' & data_COI$result == 'neg'],
                    var.equal = T)

# Bayesian t-test
bayes_se_4d = ttestBF(data_COI$trust_company[data_COI$COI == 'COI' & data_COI$result == 'neg'],
                      data_COI$trust_company[data_COI$COI == 'no COI' & data_COI$result == 'neg'],
                      rscale = 0.5, nullInterval = c(-Inf, 0))

# display the results
print_t_test(freq_t = freq_se_4d, Bayes_t = bayes_se_4d, one_tailed = T)

## As per the preregistration, we now run a two-tailed Bayesian t-test to 
## infer whether the evidence in favour of the Null hypothesis is due to 
## a) absence of a difference or b) a difference in the opposite direction

# Bayesian t-test
bayes_se_4d.2 = ttestBF(data_COI$trust_company[data_COI$COI == 'COI' & data_COI$result == 'neg'],
                        data_COI$trust_company[data_COI$COI == 'no COI' & data_COI$result == 'neg'],
                        rscale = 0.5)

# display the results
print_t_test(freq_t = freq_se_4d, Bayes_t = bayes_se_4d.2)

# The follow-up test indicates evidence of the absence of a difference, so
# we adopt this as the best representation of the data and report the results
# of this test.


# 2.4.3 plot --------------------------------------------------------------

plot_company = function(){
  
  # custom QUB colour palette
  QUB_palette = c("#00A1E1", "#F18903", "#AC004D")
  
  # set figure margins
  par(mar = c(2.5,2.5,1,1))
  
  # call the base plot
  pirateplot(data = data_COI, trust_company ~ result * COI, theme = 1, inf.method = 'hdi',
             pal = QUB_palette[1:2], gl = 0,  xaxt = 'n', yaxt = 'n', bty = 'L', inf.lwd = 0.5,
             xlab = '', ylab = '', bean.lwd = 0.5, point.cex = 0.5, gl.lwd = 1, ylim = c(0, 10))
  
  # configure axes and labels
  mtext('conflict of interest', 1, font = 2, line = 1, cex = 0.9)
  axis(1, cex.axis = 0.75, padj = -2, at = c(1.5, 4.5), 
       labels = c("no conflict", "conflict"), tcl = -0.25)
  mtext('trust in the company', 2, font = 2, line = 1.25, cex = 0.9)
  axis(2, cex.axis = 0.75, at = seq(1,7,1), padj = 1.5, tcl = -0.25)
  
  # add legend
  legend('topright', legend = c('results positive', 'results negative'), 
         fill = QUB_palette[1:2], bty = 'n')
  
  # add info on simple effect of COI for positive results
  lines(x = c(1, 4), y = c(7.5, 7.5))
  lines(x = c(1, 1), y = c(7.4, 7.5))
  lines(x = c(4, 4), y = c(7.4, 7.5))
  text(paste("BF = ", format(round(exp(bayes_se_4c@bayesFactor$bf[2]), 2), nsmall = 2), sep = ""),
       x = 2.5, y = 7.7, cex = 0.75)
  
  # add info on simple effect of COI for negative results
  lines(x = c(2, 5), y = c(8.5, 8.5))
  lines(x = c(2, 2), y = c(8.4, 8.5))
  lines(x = c(5, 5), y = c(8.4, 8.5))
  text(paste("BF = ", format(round(exp(bayes_se_4d@bayesFactor$bf[2]), 2), nsmall = 2), 
             " (two-tailed BF = ", format(round(exp(bayes_se_4d.2@bayesFactor$bf), 2), nsmall = 2),
             ")", sep = ""),
       x = 3.5, y = 8.7, cex = 0.75)
  
  # add info on simple effect of results when there is no COI
  lines(x = c(1, 2), y = c(0.5, 0.5))
  lines(x = c(1, 1), y = c(0.5, 0.6))
  lines(x = c(2, 2), y = c(0.5, 0.6))
  text(paste("BF = ", format(round(exp(bayes_se_4a@bayesFactor$bf), 2), nsmall = 2), sep = ""),
       x = 1.5, y = 0.3, cex = 0.75)
  
  # add info on simple effect of results when there is no COI
  lines(x = c(4, 5), y = c(0.5, 0.5))
  lines(x = c(4, 4), y = c(0.5, 0.6))
  lines(x = c(5, 5), y = c(0.5, 0.6))
  text(paste("BF = ", format(round(exp(bayes_se_4b@bayesFactor$bf[2]), 2), nsmall = 2), sep = ""),
       x = 4.5, y = 0.3, cex = 0.75)
  
}

plot_company()


# 2.5 estimated replicability ---------------------------------------------

# 2.5.1 Bayesian ANOVA ------------------------------------------------------

# set seed for reproducible results
set.seed(1234)

# run the Bayesian ANOVA
aov5 = anovaBF(est_replicability ~ COI * result, data = na.omit(data_COI), progress = F)

# BF for the interaction effect
exp(aov5@bayesFactor$bf)[4]/exp(aov5@bayesFactor$bf)[3]


# 2.5.2 simple effects ---------------------------------------------

# 2.5.2.1 simple effect of RES when there is no COI ----------------

# frequentist t-test of the simple effect to derive t-statistic and degrees of freedom from
freq_se_5a = t.test(data_COI$est_replicability[data_COI$COI == 'no COI' & data_COI$result == 'neg'],
                    data_COI$est_replicability[data_COI$COI == 'no COI' & data_COI$result == 'pos'],
                    var.equal = T)

# Bayesian t-test
bayes_se_5a = ttestBF(data_COI$est_replicability[data_COI$COI == 'no COI' & data_COI$result == 'neg'],
                      data_COI$est_replicability[data_COI$COI == 'no COI' & data_COI$result == 'pos'],
                      rscale = 0.5)

# display the results
print_t_test(freq_t = freq_se_5a, Bayes_t = bayes_se_5a)


# 2.5.2.2 simple effect of RES when there is a COI ----------------

# frequentist t-test of the simple effect to derive t-statistic and degrees of freedom from
freq_se_5b = t.test(data_COI$est_replicability[data_COI$COI == 'COI' & data_COI$result == 'neg'],
                    data_COI$est_replicability[data_COI$COI == 'COI' & data_COI$result == 'pos'],
                    var.equal = T)

# Bayesian t-test
bayes_se_5b = ttestBF(data_COI$est_replicability[data_COI$COI == 'COI' & data_COI$result == 'neg'],
                      data_COI$est_replicability[data_COI$COI == 'COI' & data_COI$result == 'pos'],
                      rscale = 0.5, nullInterval = c(-Inf, 0))

# display the results
print_t_test(freq_t = freq_se_5b, Bayes_t = bayes_se_5b, one_tailed = T)


# 2.5.2.3 simple effect of COI when results favour the new method ----------------

# frequentist t-test of the simple effect to derive t-statistic and degrees of freedom from
freq_se_5c = t.test(data_COI$est_replicability[data_COI$COI == 'no COI' & data_COI$result == 'pos'],
                    data_COI$est_replicability[data_COI$COI == 'COI' & data_COI$result == 'pos'],
                    var.equal = T)

# Bayesian t-test
bayes_se_5c = ttestBF(data_COI$est_replicability[data_COI$COI == 'no COI' & data_COI$result == 'pos'],
                      data_COI$est_replicability[data_COI$COI == 'COI' & data_COI$result == 'pos'],
                      rscale = 0.5, nullInterval = c(-Inf, 0))

# display the results
print_t_test(freq_t = freq_se_5c, Bayes_t = bayes_se_5c, one_tailed = T)


# 2.5.2.4 simple effect of COI when results speak against the new method ----------------

# frequentist t-test of the simple effect to derive t-statistic and degrees of freedom from
freq_se_5d = t.test(data_COI$est_replicability[data_COI$COI == 'COI' & data_COI$result == 'neg'],
                    data_COI$est_replicability[data_COI$COI == 'no COI' & data_COI$result == 'neg'],
                    var.equal = T)

# Bayesian t-test
bayes_se_5d = ttestBF(data_COI$est_replicability[data_COI$COI == 'COI' & data_COI$result == 'neg'],
                      data_COI$est_replicability[data_COI$COI == 'no COI' & data_COI$result == 'neg'],
                      rscale = 0.5, nullInterval = c(-Inf, 0))

# display the results
print_t_test(freq_t = freq_se_5d, Bayes_t = bayes_se_5d, one_tailed = T)

## As per the preregistration, we now run a two-tailed Bayesian t-test to 
## infer whether the evidence in favour of the Null hypothesis is due to 
## a) absence of a difference or b) a difference in the opposite direction

# Bayesian t-test
bayes_se_5d.2 = ttestBF(data_COI$est_replicability[data_COI$COI == 'COI' & data_COI$result == 'neg'],
                        data_COI$est_replicability[data_COI$COI == 'no COI' & data_COI$result == 'neg'],
                        rscale = 0.5)

# display the results
print_t_test(freq_t = freq_se_5d, Bayes_t = bayes_se_5d.2)

# The follow-up test indicates evidence of the absence of a difference, so
# we adopt this as the best representation of the data and report the results
# of this test.


# 2.5.3 plot --------------------------------------------------------------

plot_replicability = function(){
  
  # custom QUB colour palette
  QUB_palette = c("#00A1E1", "#F18903", "#AC004D")
  
  # set figure margins
  par(mar = c(2.5,2.5,1,1))
  
  # call the base plot
  pirateplot(data = data_COI, est_replicability ~ result * COI, theme = 1, inf.method = 'hdi',
             pal = QUB_palette[1:2], gl = 0,  xaxt = 'n', yaxt = 'n', bty = 'L', inf.lwd = 0.5,
             xlab = '', ylab = '', bean.lwd = 0.5, point.cex = 0.5, gl.lwd = 1, ylim = c(-10, 140))
  
  # configure axes and labels
  mtext('conflict of interest', 1, font = 2, line = 1, cex = 0.9)
  axis(1, cex.axis = 0.75, padj = -2, at = c(1.5, 4.5), 
       labels = c("no conflict", "conflict"), tcl = -0.25)
  mtext('estimated replicability', 2, font = 2, line = 1.25, cex = 0.9)
  axis(2, cex.axis = 0.75, at = seq(0,100,25), padj = 1.5, tcl = -0.25)
  
  # add legend
  legend('topright', legend = c('results positive', 'results negative'), 
         fill = QUB_palette[1:2], bty = 'n')
  
  # add info on simple effect of COI for positive results
  lines(x = c(1, 4), y = c(107.5, 107.5))
  lines(x = c(1, 1), y = c(105, 107.5))
  lines(x = c(4, 4), y = c(105, 107.5))
  text(paste("BF = ", format(round(exp(bayes_se_5c@bayesFactor$bf[2]), 2), nsmall = 2), sep = ""),
       x = 2.5, y = 110, cex = 0.75)
  
  # add info on simple effect of COI for negative results
  lines(x = c(2, 5), y = c(117.5, 117.5))
  lines(x = c(2, 2), y = c(115, 117.5))
  lines(x = c(5, 5), y = c(115, 117.5))
  text(paste("BF = ", format(round(exp(bayes_se_5d@bayesFactor$bf[2]), 2), nsmall = 2), 
             " (two-tailed BF = ", format(round(exp(bayes_se_5d.2@bayesFactor$bf), 2), nsmall = 2),
             ")", sep = ""), x = 3.5, y = 120, cex = 0.75)
  
  # add info on simple effect of results when there is no COI
  lines(x = c(1, 2), y = c(-7.5, -7.5))
  lines(x = c(1, 1), y = c(-5, -7.5))
  lines(x = c(2, 2), y = c(-5, -7.5))
  text(paste("BF = ", format(round(exp(bayes_se_5a@bayesFactor$bf), 2), nsmall = 2), sep = ""),
       x = 1.5, y = -10, cex = 0.75)
  
  # add info on simple effect of results when there is no COI
  lines(x = c(4, 5), y = c(-7.5, -7.5))
  lines(x = c(4, 4), y = c(-5, -7.5))
  lines(x = c(5, 5), y = c(-5, -7.5))
  text(paste("BF = ", format(round(exp(bayes_se_5b@bayesFactor$bf[2]), 2), nsmall = 2), sep = ""),
       x = 4.5, y = -10, cex = 0.75)
  
}

plot_replicability()
