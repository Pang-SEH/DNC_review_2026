#### Virtual species testing and creation ## - ##
library(popbio)
library(Rage)
library(data.table)

#### Creating 2 virtual test species matrix ####
# stats to matrix function #
stat2mat <- function(pop_stats) {
  matrix(c(pop_stats$s_j * (1 - pop_stats$g_j), pop_stats$f + pop_stats$r_a * pop_stats$s_a,
           pop_stats$s_j * pop_stats$g_j, pop_stats$s_a*(1 - pop_stats$r_a)),
         nrow = 2, byrow = T)
}

### species pop
# population statistics (2x2) # "s_j","s_a","g_j","r_a","f"
pop_stats <- list(s_j = 0.9,   # fecundity/survival of stage1
                  s_a = 0,    # adult stay survival at env=0 (observed)
                  g_j = 1,    # juvenile -> adult at env=0 (observed)
                  r_a = 0,       # retrogression of adults back to s
                  f  = 2.5,        # fecundity of stage2
                  type = "std")      
## Hypothetical species with simple life cycle
# 2 stage life cycle (juv + adult)
# Annual species with fixed growth from stage 1 to 2 (g_j = 1)
# No retrogression
# Adults do not survive (annual)
# Only 2 real vital rates matter, survival of juvenile (stage 1s)
# and fecundity or reproduction (f, stage 1s obtained from stage 2s)

# fit into matrix  
pop_mat <- stat2mat(pop_stats)
pop_mat

# testing pgr
lambda(pop_mat) # Manually adjusted rates till lambda 1.5 is achieved
elasticity(pop_mat)
sensitivity(pop_mat)

#### visualising some RespFunc ####
env_seq <- c(seq(0, 1, length.out = 101))
## baseline survival ##
# using logit scale to bound 0-1
# so logit(surv(env)) = a + b * env
# so surv(env) = invlogit(a + b * env)
# inverse logit function to bound survival or 0-1 vals
invlogit <- function(x) 1 / (1 + exp(-x))
# for a data.table, extract the relevant vr, make a matrix and calculate lambda
stat2lambda <- function(x) {
  lambda_list <- NULL
  for(i in 1:nrow(x)) {
    mat <- stat2mat(as.list(x[i,]))
    lambda <- popbio::lambda(mat)
    lambda_list <- c(lambda_list, lambda)
  }
  return(lambda_list)
}

#### creating data.table ####
mono_pred_logit <- function(vr, env, slope = NA) invlogit(qlogis(vr) + slope * env)
mono_pred_log <- function(vr, env, slope = NA) exp(log(vr) + slope * env)

# creating sp env table
vsp_dt <- cbind(data.table(env = env_seq), as.data.table(pop_stats))
# creating the response to env for slope = 1
vsp_dt[, f := mono_pred_log(pop_stats$f, env_seq, -2)]
# vsp_dt[, g_j := mono_pred(Jgrow_int, env_seq, Jgrow_c)]
vsp_dt[, s_j := mono_pred_logit(pop_stats$s_j, env_seq, -3)]

# calculating lambda
vsp_dt[, lambda := stat2lambda(vsp_dt)]
vsp_dt <- melt(vsp_dt, id.vars = "env", measure.vars = c("s_j", "f", "lambda"),
               variable.name = "VitalRate", value.name = "rate")
vsp_dt[, lograte := log(rate)]

library(ggplot2)
ggplot(vsp_dt, aes(x = env, y = lograte, colour = VitalRate)) +
  geom_line()
ggplot(vsp_dt, aes(x = env, y = rate, colour = VitalRate)) +
  geom_line()

## where only s_j varies
# "#F8766D" "#00BA38" "#619CFF"
vsp_sj <- cbind(data.table(env = env_seq), as.data.table(pop_stats))
vsp_sj[, s_j := mono_pred_logit(pop_stats$s_j, env_seq, -3)]
vsp_sj[, lambda := stat2lambda(vsp_sj)]
vsp_sj <- melt(vsp_sj, id.vars = "env", measure.vars = c("s_j", "f", "lambda"),
               variable.name = "VitalRate", value.name = "rate")
vsp_sj[, lograte := log(rate)]
vsp_sj[VitalRate == 'lambda' & rate >= 1, ] # 0.86
ggplot(vsp_sj, aes(x = env, y = lograte, colour = VitalRate)) + geom_line() + geom_vline(xintercept = 0.86, colour = '#F8766D', linetype = 'dashed')
ggplot(vsp_sj, aes(x = env, y = rate, colour = VitalRate)) + geom_line() + geom_vline(xintercept = 0.86, colour = '#F8766D', linetype = 'dashed')

## where only f varies
vsp_f <- cbind(data.table(env = env_seq), as.data.table(pop_stats))
vsp_f[, f := mono_pred_log(pop_stats$f, env_seq, -2)]
vsp_f[, lambda := stat2lambda(vsp_f)]
vsp_f <- melt(vsp_f, id.vars = "env", measure.vars = c("s_j", "f", "lambda"),
               variable.name = "VitalRate", value.name = "rate")
vsp_f[, lograte := log(rate)]
vsp_f[VitalRate == 'lambda' & rate >= 1, ] # 0.86
ggplot(vsp_f, aes(x = env, y = lograte, colour = VitalRate)) + geom_line() + geom_vline(xintercept = 0.4, colour = '#00BA38', linetype = 'dashed')
ggplot(vsp_f, aes(x = env, y = rate, colour = VitalRate)) + geom_line() + geom_vline(xintercept = 0.4, colour = '#00BA38', linetype = 'dashed')

## again running for all
vsp_dt <- cbind(data.table(env = env_seq), as.data.table(pop_stats))
vsp_dt[, f := mono_pred_log(pop_stats$f, env_seq, -2)]
vsp_dt[, s_j := mono_pred_logit(pop_stats$s_j, env_seq, -3)]
vsp_dt[, lambda := stat2lambda(vsp_dt)]
vsp_dt <- melt(vsp_dt, id.vars = "env", measure.vars = c("s_j", "f", "lambda"),
               variable.name = "VitalRate", value.name = "rate")
vsp_dt[, lograte := log(rate)]
vsp_dt[VitalRate == 'lambda' & rate >= 1, ] # 0.32

library(ggplot2)
ggplot(vsp_dt, aes(x = env, y = lograte, colour = VitalRate)) + geom_line() +
  geom_vline(xintercept = 0.86, colour = '#F8766D', linetype = 'dashed') + 
  geom_vline(xintercept = 0.4, colour = '#00BA38', linetype = 'dashed') +
  geom_vline(xintercept = 0.32, colour = '#619CFF', linetype = 'dashed')
ggplot(vsp_dt, aes(x = env, y = rate, colour = VitalRate)) +  geom_line() +
  geom_vline(xintercept = 0.86, colour = '#F8766D', linetype = 'dashed') + 
  geom_vline(xintercept = 0.4, colour = '#00BA38', linetype = 'dashed') +
  geom_vline(xintercept = 0.32, colour = '#619CFF', linetype = 'dashed')

#### Trying with unimodal setting ####
env_seq <- seq(-1,1, length.out = 101)
unimod_pred_logit <- function(vr, env, slope = NA, mu = 0) invlogit(qlogis(vr) - slope * (env - mu)^2)
unimod_pred_log <- function(vr, env, slope = NA, mu = 0) exp(log(vr) - slope * (env - mu)^2)

plot(env_seq, unimod_pred_log(2.3, env_seq, slope = 3, mu = 0), ylim = c(0,2.5))
# creating sp env table
vsp_dt <- cbind(data.table(env = env_seq), as.data.table(pop_stats))
vsp_dt[, f := unimod_pred_log(pop_stats$f, env_seq, 2, 0)]
vsp_dt[, s_j := unimod_pred_logit(pop_stats$s_j, env_seq, 2, 0)]

# calculating lambda
vsp_dt[, lambda := stat2lambda(vsp_dt)]
vsp_dt <- melt(vsp_dt, id.vars = "env", measure.vars = c("s_j", "f", "lambda"),
               variable.name = "VitalRate", value.name = "rate")
vsp_dt[, lograte := log(rate)]

library(ggplot2)
ggplot(vsp_dt, aes(x = env, y = lograte, colour = VitalRate)) +
  geom_line()
ggplot(vsp_dt, aes(x = env, y = rate, colour = VitalRate)) +
  geom_line()

## where only s_j varies
# "#F8766D" "#00BA38" "#619CFF"
vsp_sj <- cbind(data.table(env = env_seq), as.data.table(pop_stats))
vsp_sj[, s_j := unimod_pred_logit(pop_stats$s_j, env_seq, 4, 0.1)]
vsp_sj[, lambda := stat2lambda(vsp_sj)]
vsp_sj[lambda >= 0.95 & lambda < 1.1, ] 
vsp_sj <- melt(vsp_sj, id.vars = "env", measure.vars = c("s_j", "f", "lambda"),
               variable.name = "VitalRate", value.name = "rate")
vsp_sj[, lograte := log(rate)]
ggplot(vsp_sj, aes(x = env, y = lograte, colour = VitalRate)) + geom_line() + geom_vline(xintercept = c(-0.7,0.9), colour = '#F8766D', linetype = 'dashed')
ggplot(vsp_sj, aes(x = env, y = rate, colour = VitalRate)) + geom_line() + geom_vline(xintercept = c(-0.7,0.9), colour = '#F8766D', linetype = 'dashed')

## where only f varies
vsp_f <- cbind(data.table(env = env_seq), as.data.table(pop_stats))
vsp_f[, f := unimod_pred_log(pop_stats$f, env_seq, 2, -0.2)]
vsp_f[, lambda := stat2lambda(vsp_f)]
vsp_f[lambda >= 0.95 & lambda < 1.1, ] 
vsp_f <- melt(vsp_f, id.vars = "env", measure.vars = c("s_j", "f", "lambda"),
              variable.name = "VitalRate", value.name = "rate")
vsp_f[, lograte := log(rate)]
ggplot(vsp_f, aes(x = env, y = lograte, colour = VitalRate)) + geom_line() + geom_vline(xintercept = c(-0.82, 0.42), colour = '#00BA38', linetype = 'dashed')
ggplot(vsp_f, aes(x = env, y = rate, colour = VitalRate)) + geom_line() + geom_vline(xintercept = c(-0.82, 0.42), colour = '#00BA38', linetype = 'dashed')

## again running for all
vsp_dt <- cbind(data.table(env = env_seq), as.data.table(pop_stats))
vsp_dt[, f := unimod_pred_log(pop_stats$f, env_seq, 2, -0.2)]
vsp_dt[, s_j := unimod_pred_logit(pop_stats$s_j, env_seq, 4, 0.1)]
vsp_dt[, lambda := stat2lambda(vsp_dt)]
vsp_dt[lambda >= 0.95 & lambda < 1.1, ] 
vsp_dt <- melt(vsp_dt, id.vars = "env", measure.vars = c("s_j", "f", "lambda"),
               variable.name = "VitalRate", value.name = "rate")
vsp_dt[, lograte := log(rate)]

library(ggplot2)
ggplot(vsp_dt, aes(x = env, y = lograte, colour = VitalRate)) + geom_line() +
  geom_vline(xintercept = c(-0.7,0.9), colour = '#F8766D', linetype = 'dashed') + 
  geom_vline(xintercept = c(-0.82, 0.42), colour = '#00BA38', linetype = 'dashed') +
  geom_vline(xintercept = c(-0.6, 0.40), colour = '#619CFF', linetype = 'dashed')
ggplot(vsp_dt, aes(x = env, y = rate, colour = VitalRate)) +  geom_line() +
  geom_vline(xintercept = c(-0.7,0.9), colour = '#F8766D', linetype = 'dashed') + 
  geom_vline(xintercept = c(-0.82, 0.42), colour = '#00BA38', linetype = 'dashed') +
  geom_vline(xintercept = c(-0.6, 0.40), colour = '#619CFF', linetype = 'dashed')

ggplot() +  
  geom_line(data = vsp_dt[VitalRate != 'lambda', ], aes(x = env, y = rate, colour = VitalRate)) +
  scale_color_manual(values = c('#F8766D','#00BA38')) +
  geom_line(data = vsp_dt[VitalRate == 'lambda', ], aes(x = env, y = lograte), colour = '#619CFF') +
  geom_vline(xintercept = c(-0.7,0.9), colour = '#F8766D', linetype = 'dashed') + 
  geom_vline(xintercept = c(-0.82, 0.42), colour = '#00BA38', linetype = 'dashed') +
  geom_vline(xintercept = c(-0.6, 0.40), colour = '#619CFF', linetype = 'dashed') +
  scale_y_continuous(limits = c(-0.2,NA))
