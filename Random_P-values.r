# R script to reproduce histograms in Halsey LG, 
# Curran-Everett D, Vowler SL, Drummond GB.
# The fickle P value generates irreproducible results.
# Nat Methods. 2015; 12(3):179-185

rnd.t <- function(n, nc, df) { # random P-values
    1-pt(qt(runif(n), ncp=nc, df=df), df=df)
}

fx <- function(x, nc, df) { # P-value PDF
  y <- qt(x, df=df, lower.tail=FALSE)
  (dt(y, df=df, ncp=nc) / dt(y, df=df))
}

Fx <- function(p, nc, df) {
    1-pt(qt(1-p, df=df), ncp=nc, df=df)
}

# Reproduce Figure 4, second histogram
# from Halsey et al 
N1 = N2 = 30 # set sample sizes for two sample t-test
N = 1/(1/N1 + 1/N2) # sample size part of noncentrality
Df = N1 + N2 - 2 # degrees of freedom
Effect = 0.5 # mean difference (effect size assuming variance=1) 
Nonc = Effect * sqrt(N) # noncentrality parameter
x = seq(0, 1, length.out=200)
y = fx(x, Nonc, Df)
p = rnd.t(5e4, Nonc, Df) # Generate P-values directly
hist(p, freq=FALSE, 50) # P-value histogram
lines(x, y, lwd=4) # Plot theoretical P-value density on top of the histogram
pow.emp <- ecdf(p)(0.05/2) # Empirical power from the generated P-values
pow.CDF <- Fx(0.05/2, Nonc, Df) # 
cat("Empirical power from the generated P-values:", pow.emp, "\n")
cat("Power from P-value CDF:", pow.CDF, "\n")
