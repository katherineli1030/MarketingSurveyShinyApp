# to around decimal
round.numerics <- function(x, digits){
  if(is.numeric(x)){
    x <- round(x = x, digits = digits)
  }
  return(x)
}

# to make it percentage 
percentage.table <- function(x, digits = 1){
  tab <- table(x)
  percentage.tab <- 100*tab/(sum(tab))
  rounded.tab <- round(x = percentage.tab, digits = digits)
  return(rounded.tab)
}

#function to calcuate the distribution all levels of catagoriacal varaible
var_pct <- function(dat,var,digits){
  dat[,.("percentage"=.N/rater_num),keyby=var] %>%
    datatable() %>% formatPercentage("percentage",digits)
}

#function to get top Nth lead board of a variable
lead_5 <- function(dat,var,digits){
  setorderv(x = dat, cols = var, order=-1)
  dat[1:5] %>% datatable() %>% 
    formatPercentage(var,digits)
}


#top n product by engagement
top_product_by_engage <- function(dat,engage.name,n){
  t_dat <- dat[, .(average = 100*mean(get(engage.name),na.rm=TRUE)),keyby=product.name]
  setorderv(x = t_dat, cols = "average", order=-1)
  t_dat[1:n]
}

#top n product by brand perception 
top_product_by_bp <- function(dat,n,a,b){
  t_dat_bp <- dat[,lapply(.SD, function(x) mean(x,na.rm=TRUE)),
                  keyby=Product,.SDcols=a][,(b):=lapply(.SD, function(x) 10-x),
                                                      .SDcols=b][,.(mean_bp = rowMeans(.SD)),keyby=Product]
  setorderv(x = t_dat_bp, cols = "mean_bp", order=-1)
  t_dat_bp[1:n]
}

#funtion to get gap between two variables
gap <- function(dat,var1,var2, groupby, n, digits){
  var1<- dat[,.(mean1=mean(get(var1), na.rm=TRUE)), keyby=groupby]
  var2<- dat[,.(mean2=mean(get(var2),na.rm=TRUE)), keyby=groupby]
  merge = merge(var1,var2)
  gap = merge[,.(gap=100*(mean1-mean2)), keyby=groupby]
  setorderv(x=gap, cols='gap', order=-1)
  gap[1:n,lapply(X=.SD, FUN='round.numerics',digits=digits)]
}

#model functions
logistic.regression.summary <- function(glm.mod, digits = 3, alpha = 0.05){
  library(data.table)
  glm.coefs <- as.data.table(summary(glm.mod)$coefficients, keep.rownames = TRUE)
  setnames(x = glm.coefs, old = "rn", new = "Variable")
  z <- qnorm(p = 1-alpha/2, mean = 0, sd = 1)
  glm.coefs[, Odds.Ratio := exp(Estimate)]
  glm.coefs[, OR.Lower.95 := exp(Estimate - z * `Std. Error`)]
  glm.coefs[, OR.Upper.95 := exp(Estimate + z * `Std. Error`)]
  return(glm.coefs[])
}

linear.regression.summary <- function(lm.mod, digits = 3, alpha = 0.05){
  library(data.table)
  lm.coefs <- as.data.table(summary(lm.mod)$coefficients, keep.rownames = TRUE)
  setnames(x = lm.coefs, old = "rn", new = "Variable")
  
  z <- qnorm(p = 1-alpha/2, mean = 0, sd = 1)
  lm.coefs[, Coef.Lower.95 := Estimate - z * `Std. Error`]
  lm.coefs[, Coef.Upper.95 := Estimate + z * `Std. Error`]
  return(lm.coefs)
}

fit.model <- function(dt, outcome.name, input.names, model.type, digits = 3){
  library(formulaic)
  the.formula <- create.formula(outcome.name = outcome.name, input.names = input.names, dat = dt, reduce = TRUE)
  
  if(model.type == "logistic"){
    mod <- glm(formula = the.formula, family = "binomial", data = dt)
    mod.summary <- logistic.regression.summary(glm.mod = mod, digits = digits)
  }
  if(model.type == "linear"){
    mod <- lm(formula = the.formula, data = dt)
    mod.summary <- linear.regression.summary(lm.mod = mod, digits = digits)
  }
  mod.summary.rounded <- mod.summary[, lapply(X = .SD, FUN = "round.numerics", digits = digits)]
  return(mod.summary.rounded)
}
