### COI and Trust in Science - Study 5
### Data Wrangling


# 0. load libraries and data ----------------------------------------------

# Run from the repository root, e.g. Rscript experiments/experiment-5/COI_Exp5.R.

library(BayesFactor)
library(dplyr)
library(HDInterval)
library(bain)
library(ggplot2)
library(gridExtra)
library(tidyr)

data_COI = read.csv('experiments/experiment-5/data_COI_Exp5.csv')


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
  
  # post_d = ifelse(one_tailed == F,
  #                 posterior(Bayes_t, iterations = 50000),
  #                 posterior(Bayes_t[2], iterations = 50000))
  
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



# 2.1 Trust in the Study --------------------------------------------------

# 2.1.1 BAIN Test ---------------------------------------------------------

# create two sets of informative hypotheses representing the two predictions

bain_hypotheses = 
  "condneg_noCOI = condneg_COI = condneg_extCOI = condpos_noCOI > condpos_extCOI = condpos_COI;
   condneg_extCOI > condpos_noCOI = condneg_noCOI = condneg_COI = condpos_extCOI > condpos_COI"

# run the BAIN analysis
lm_study = lm(trust_study ~ cond-1, data = data_COI)
fit_study = bain(lm_study, bain_hypotheses) 

# extract BFs for the two hypotheses against the unconstrained model
# and add the BF for the unconstrained model as 1
BFs_study = c(fit_study$fit$BF.u[1:(length(fit_study$hypotheses))], 1)
#extract posterior probabilities including unconstrained model
PPs_study = fit_study$fit$PMPb[1:(length(fit_study$hypotheses)+1)]

# inspect the Bayes factor and compute BF comparison for the two hypotheses
BFs_study

BFs_study[1]/BFs_study[2]


# inspect the posterior probabilities
PPs_study



# 2.1.2 plot --------------------------------------------------------------

# trust by condition
p_study = ggplot(data = data_COI, aes(x = COI, y = trust_study, fill = result)) +
  geom_violin() +
  geom_point(stat = "summary", fun = mean, position = position_dodge(width = 0.9)) +
  geom_errorbar(stat = "summary", fun.data = mean_cl_normal, position = position_dodge(width = 0.9)) +
  theme_classic() +
  scale_fill_manual(values = c("#00A1E1", "#F18903"),
                    labels = c("positive result", "negative result")) +
  scale_y_continuous(limits = c(0,8), breaks = 1:7, name = "trust in the study") +
  theme(legend.position = "inside", legend.position.inside = c(0.85, 0.85),
        legend.title = element_blank())

p_study


# posterior probabilities
pp_study = data.frame(
  hypothesis = c("attribution", "cue-based", "unconstrained"),
  postProb = PPs_study
)

p_study_pp = ggplot(data = pp_study, aes(x = hypothesis, y = postProb)) +
  geom_bar(stat = "identity") +
  theme_classic() + 
  scale_y_continuous(limits = c(0,1.3), breaks = seq(0,1,.25), name = "posterior probabilities") +
  geom_abline(slope = 0, intercept = 0.90, linetype = 2, colour = "red") +
  geom_segment(x = 1, xend = 3, y = 1.2, yend = 1.2) +
  geom_segment(x = 2, xend = 3, y = 1.10, yend = 1.10) +
  geom_segment(x = 1, xend = 2, y = 1, yend = 1) +
  geom_text(x = 2, y = 1.25, 
            label = ifelse(BFs_study[1] > 100,
                          "BF > 100", 
                          paste("BF = ", format(round(BFs_study[1],2), nsmall = 2)))) +
  geom_text(x = 2.5, y = 1.15, 
            label = ifelse(BFs_study[1] > 100,
                           "BF > 100", 
                           paste("BF = ", format(round(BFs_study[2],2), nsmall = 2)))) +
  geom_text(x = 1.5, y = 1.05, 
            label = ifelse(BFs_study[1]/BFs_study[2] > 100,
                           "BF > 100",
                           paste("BF = ", format(round(BFs_study[1]/BFs_study[2],2), nsmall = 2))))
  
p_study_pp



# 2.2 Trust in the Authors --------------------------------------------------

# 2.2.1 BAIN Test ---------------------------------------------------------

# run the BAIN analysis
lm_authors = lm(trust_authors ~ cond-1, data = data_COI)
fit_authors = bain(lm_authors, bain_hypotheses) 

# extract BFs for the two hypotheses against the unconstrained model
# and add the BF for the unconstrained model as 1
BFs_authors = c(fit_authors$fit$BF.u[1:(length(fit_authors$hypotheses))], 1)
#extract posterior probabilities including unconstrained model
PPs_authors = fit_authors$fit$PMPb[1:(length(fit_authors$hypotheses)+1)]

# inspect the Bayes factor and compute BF comparison for the two hypotheses
BFs_authors

BFs_authors[1]/BFs_authors[2]


# inspect the posterior probabilities
PPs_authors



# 2.2.2 plot --------------------------------------------------------------

# trust by condition
p_authors = ggplot(data = data_COI, aes(x = COI, y = trust_study, fill = result)) +
  geom_violin() +
  geom_point(stat = "summary", fun = mean, position = position_dodge(width = 0.9)) +
  geom_errorbar(stat = "summary", fun.data = mean_cl_normal, position = position_dodge(width = 0.9)) +
  theme_classic() +
  scale_fill_manual(values = c("#00A1E1", "#F18903"),
                    labels = c("positive result", "negative result")) +
  scale_y_continuous(limits = c(0,8), breaks = 1:7, name = "trust in the authors") +
  theme(legend.position = "inside", legend.position.inside = c(0.85, 0.85),
        legend.title = element_blank())


p_authors


# posterior probabilities
pp_authors = data.frame(
  hypothesis = c("attribution", "cue-based", "unconstrained"),
  postProb = PPs_authors
)

p_authors_pp = ggplot(data = pp_authors, aes(x = hypothesis, y = postProb)) +
  geom_bar(stat = "identity") +
  theme_classic() + 
  scale_y_continuous(limits = c(0,1.3), breaks = seq(0,1,.25), name = "posterior probabilities") +
  geom_abline(slope = 0, intercept = 0.90, linetype = 2, colour = "red") +
  geom_segment(x = 1, xend = 3, y = 1.2, yend = 1.2) +
  geom_segment(x = 2, xend = 3, y = 1.10, yend = 1.10) +
  geom_segment(x = 1, xend = 2, y = 1, yend = 1) +
  geom_text(x = 2, y = 1.25, 
            label = ifelse(BFs_authors[1] > 100,
                           "BF > 100", 
                           paste("BF = ", format(round(BFs_authors[1],2), nsmall = 2)))) +
  geom_text(x = 2.5, y = 1.15, 
            label = ifelse(BFs_authors[1] > 100,
                           "BF > 100", 
                           paste("BF = ", format(round(BFs_authors[2],2), nsmall = 2)))) +
  geom_text(x = 1.5, y = 1.05, 
            label = ifelse(BFs_authors[1]/BFs_authors[2] > 100,
                           "BF > 100",
                           paste("BF = ", format(round(BFs_authors[1]/BFs_authors[2],2), nsmall = 2))))

p_authors_pp


# 2.3 Trust in the Journal --------------------------------------------------

# 2.3.1 BAIN Test ---------------------------------------------------------

# run the BAIN analysis
lm_journal = lm(trust_journal ~ cond-1, data = data_COI)
fit_journal = bain(lm_journal, bain_hypotheses) 

# extract BFs for the two hypotheses against the unconstrained model
# and add the BF for the unconstrained model as 1
BFs_journal = c(fit_journal$fit$BF.u[1:(length(fit_journal$hypotheses))], 1)
#extract posterior probabilities including unconstrained model
PPs_journal = fit_journal$fit$PMPb[1:(length(fit_journal$hypotheses)+1)]

# inspect the Bayes factor and compute BF comparison for the two hypotheses
BFs_journal

BFs_journal[1]/BFs_journal[2]


# inspect the posterior probabilities
PPs_journal



# 2.3.2 plot --------------------------------------------------------------

# trust by condition
p_journal = ggplot(data = data_COI, aes(x = COI, y = trust_journal, fill = result)) +
  geom_violin() +
  geom_point(stat = "summary", fun = mean, position = position_dodge(width = 0.9)) +
  geom_errorbar(stat = "summary", fun.data = mean_cl_normal, position = position_dodge(width = 0.9)) +
  theme_classic() +
  scale_fill_manual(values = c("#00A1E1", "#F18903"),
                    labels = c("positive result", "negative result")) +
  scale_y_continuous(limits = c(0,8), breaks = 1:7, name = "trust in the journal") +
  theme(legend.position = "inside", legend.position.inside = c(0.85, 0.85),
        legend.title = element_blank())


p_journal


# posterior probabilities
pp_journal = data.frame(
  hypothesis = c("attribution", "cue-based", "unconstrained"),
  postProb = PPs_journal
)

p_journal_pp = ggplot(data = pp_journal, aes(x = hypothesis, y = postProb)) +
  geom_bar(stat = "identity") +
  theme_classic() + 
  scale_y_continuous(limits = c(0,1.3), breaks = seq(0,1,.25), name = "posterior probabilities") +
  geom_abline(slope = 0, intercept = 0.90, linetype = 2, colour = "red") +
  geom_segment(x = 1, xend = 3, y = 1.2, yend = 1.2) +
  geom_segment(x = 2, xend = 3, y = 1.10, yend = 1.10) +
  geom_segment(x = 1, xend = 2, y = 1, yend = 1) +
  geom_text(x = 2, y = 1.25, 
            label = ifelse(BFs_journal[1] > 100,
                           "BF > 100", 
                           paste("BF = ", format(round(BFs_journal[1],2), nsmall = 2)))) +
  geom_text(x = 2.5, y = 1.15, 
            label = ifelse(BFs_journal[1] > 100,
                           "BF > 100", 
                           paste("BF = ", format(round(BFs_journal[2],2), nsmall = 2)))) +
  geom_text(x = 1.5, y = 1.05, 
            label = ifelse(BFs_journal[1]/BFs_journal[2] > 100,
                           "BF > 100",
                           paste("BF = ", format(round(BFs_journal[1]/BFs_journal[2],2), nsmall = 2))))

p_journal_pp


# 2.4 Trust in the Company --------------------------------------------------

# 2.4.1 BAIN Test ---------------------------------------------------------

# run the BAIN analysis
lm_funder = lm(trust_funder ~ cond-1, data = data_COI)
fit_funder = bain(lm_funder, bain_hypotheses) 

# extract BFs for the two hypotheses against the unconstrained model
# and add the BF for the unconstrained model as 1
BFs_funder = c(fit_funder$fit$BF.u[1:(length(fit_funder$hypotheses))], 1)
#extract posterior probabilities including unconstrained model
PPs_funder = fit_funder$fit$PMPb[1:(length(fit_funder$hypotheses)+1)]

# inspect the Bayes factor and compute BF comparison for the two hypotheses
BFs_funder

BFs_funder[1]/BFs_funder[2]


# inspect the posterior probabilities
PPs_funder



# 2.4.2 plot --------------------------------------------------------------

# trust by condition
p_funder = ggplot(data = data_COI, aes(x = COI, y = trust_funder, fill = result)) +
  geom_violin() +
  geom_point(stat = "summary", fun = mean, position = position_dodge(width = 0.9)) +
  geom_errorbar(stat = "summary", fun.data = mean_cl_normal, position = position_dodge(width = 0.9)) +
  theme_classic() +
  scale_fill_manual(values = c("#00A1E1", "#F18903"),
                    labels = c("positive result", "negative result")) +
  scale_y_continuous(limits = c(0,8), breaks = 1:7, name = "trust in the company") +
  theme(legend.position = "inside", legend.position.inside = c(0.85, 0.85),
        legend.title = element_blank())


p_funder


# posterior probabilities
pp_funder = data.frame(
  hypothesis = c("attribution", "cue-based", "unconstrained"),
  postProb = PPs_funder
)

p_funder_pp = ggplot(data = pp_funder, aes(x = hypothesis, y = postProb)) +
  geom_bar(stat = "identity") +
  theme_classic() + 
  scale_y_continuous(limits = c(0,1.3), breaks = seq(0,1,.25), name = "posterior probabilities") +
  geom_abline(slope = 0, intercept = 0.90, linetype = 2, colour = "red") +
  geom_segment(x = 1, xend = 3, y = 1.2, yend = 1.2) +
  geom_segment(x = 2, xend = 3, y = 1.10, yend = 1.10) +
  geom_segment(x = 1, xend = 2, y = 1, yend = 1) +
  geom_text(x = 2, y = 1.25, 
            label = ifelse(BFs_funder[1] > 100,
                           "BF > 100", 
                           paste("BF = ", format(round(BFs_funder[1],2), nsmall = 2)))) +
  geom_text(x = 2.5, y = 1.15, 
            label = ifelse(BFs_funder[1] > 100,
                           "BF > 100", 
                           paste("BF = ", format(round(BFs_funder[2],2), nsmall = 2)))) +
  geom_text(x = 1.5, y = 1.05, 
            label = ifelse(BFs_funder[1]/BFs_funder[2] > 100,
                           "BF > 100",
                           paste("BF = ", format(round(BFs_funder[1]/BFs_funder[2],2), nsmall = 2))))

p_funder_pp



# 2.5 estimated replicability ---------------------------------------------

# 2.5.1 BAIN Test ---------------------------------------------------------

# run the BAIN analysis
lm_replicability = lm(est_replicability ~ cond-1, data = data_COI)
fit_replicability = bain(lm_replicability, bain_hypotheses) 

# extract BFs for the two hypotheses against the unconstrained model
# and add the BF for the unconstrained model as 1
BFs_replicability = c(fit_replicability$fit$BF.u[1:(length(fit_replicability$hypotheses))], 1)
#extract posterior probabilities including unconstrained model
PPs_replicability = fit_replicability$fit$PMPb[1:(length(fit_replicability$hypotheses)+1)]

# inspect the Bayes factor and compute BF comparison for the two hypotheses
BFs_replicability

BFs_replicability[1]/BFs_replicability[2]


# inspect the posterior probabilities
PPs_replicability



# 2.5.2 plot --------------------------------------------------------------

# trust by condition
p_replicability = ggplot(data = data_COI, aes(x = COI, y = est_replicability, fill = result)) +
  geom_violin() +
  geom_point(stat = "summary", fun = mean, position = position_dodge(width = 0.9)) +
  geom_errorbar(stat = "summary", fun.data = mean_cl_normal, position = position_dodge(width = 0.9)) +
  theme_classic() +
  scale_fill_manual(values = c("#00A1E1", "#F18903"),
                    labels = c("positive result", "negative result")) +
  scale_y_continuous(limits = c(0,110), breaks = seq(0, 100, 25), name = "estimated replicability") +
  theme(legend.position = "inside", legend.position.inside = c(0.85, 0.85),
        legend.title = element_blank())


p_replicability


# posterior probabilities
pp_replicability = data.frame(
  hypothesis = c("attribution", "cue-based", "unconstrained"),
  postProb = PPs_replicability
)

p_replicability_pp = ggplot(data = pp_replicability, aes(x = hypothesis, y = postProb)) +
  geom_bar(stat = "identity") +
  theme_classic() + 
  scale_y_continuous(limits = c(0,1.3), breaks = seq(0,1,.25), name = "posterior probabilities") +
  geom_abline(slope = 0, intercept = 0.90, linetype = 2, colour = "red") +
  geom_segment(x = 1, xend = 3, y = 1.2, yend = 1.2) +
  geom_segment(x = 2, xend = 3, y = 1.10, yend = 1.10) +
  geom_segment(x = 1, xend = 2, y = 1, yend = 1) +
  geom_text(x = 2, y = 1.25, 
            label = ifelse(BFs_replicability[1] > 100,
                           "BF > 100", 
                           paste("BF = ", format(round(BFs_replicability[1],2), nsmall = 2)))) +
  geom_text(x = 2.5, y = 1.15, 
            label = ifelse(BFs_replicability[1] > 100,
                           "BF > 100", 
                           paste("BF = ", format(round(BFs_replicability[2],2), nsmall = 2)))) +
  geom_text(x = 1.5, y = 1.05, 
            label = ifelse(BFs_replicability[1]/BFs_replicability[2] > 100,
                           "BF > 100",
                           paste("BF = ", format(round(BFs_replicability[1]/BFs_replicability[2],2), nsmall = 2))))

p_replicability_pp


