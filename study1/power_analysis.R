# Power analysis for thesis study 1

# install effectsize package
install.packages("effectsize")
library(effectsize)

# install pwr package
install.packages("pwr")
library(pwr)
pwr.t.test(n= NULL, d = 0.53, sig.level = 0.05, power = 0.8, type = c("paired"), alternative = c("two.sided"))

pwr.t.test(n= NULL, d = 0.5, sig.level = 0.05, power = 0.8, type = "two.sample", alternative = "two.sided")
