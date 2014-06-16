# Load your csv file downloaded from the trait database
data_raw <- read.csv("data/traits.csv", as.is=TRUE)

# Load the "reshape2" package for R.  This package must initially be downloaded from CRAN
if (!"reshape2" %in% rownames(installed.packages())) install.packages("reshape2", repos="http://cran.csiro.au", dependencies=TRUE)
library(reshape2)

# Develop your aggregation rules function for the "acast" function
my_aggregate_rules <- function(x) {
  if (length(x) > 1) {               # Does a species by trait combination have more than 1 value?
    x <- type.convert(x, as.is=TRUE)
    if (is.character(x)) {
      return(x[1])                   # If values are strings (characters), then return the first value
    } else {
      return(as.character(mean(x)))  # If values are numbers, then return the mean (converted back to character)
    }
  } else {
    return(x)                        # If a species by trait combination has 1 value, then just return that value 
  }
}

# Reshape your data using "acast".  Fill gaps with NAs
data_reshaped <- acast(data_raw, coral~trait_name, value.var="value", fun.aggregate=my_aggregate_rules, fill="")

data_reshaped[data_reshaped == ""] <- NA

# If desired, convert the reshaped data into a data frame for analysis in R
data_final <- data.frame(data_reshaped, stringsAsFactors=FALSE)

# Note that all variables are still character-formatted.  Use as.numeric() and as.factor() accordingly.  For example,


dat <- data_final[,c(1, 3, 4)]#[!is.na(data_final$Growth.rate),]

dat$Growth.rate <- as.numeric(dat$Growth.rate)
dat$Growth.rate.log <- log10(dat$Growth.rate)

dat$Growth.form <- as.factor(dat$Growth.form)
dat$Clade.Fukami.Huang.2012 <- as.factor(dat$Clade.Fukami.Huang.2012)

dat$species <- rownames(dat)

# Danwei's phylogenetic analysis

# I believe this is the data you wanted, Damwei?  Mean growth rates for species. Growth is log transformed.  I think it's okay to ignore growth form, because presumably closely related species have similar groth strategies (?)

dan <- aggregate(dat["Growth.rate.log"], list(Species=rownames(dat)), mean)







# Josh's glm fit and plot
mod <- glm(Growth.rate.log ~ Growth.form + Clade.Fukami.Huang.2012, data=dat)
drop1(mod, test="Chisq")

source("R/make_josh_figure.r")


