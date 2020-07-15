library(dhlvm)

# from the SDA Codebook
na.codes <- c(0, 8, 9, 98, 99)
vars <- c("PAGO", "PEXP", "RINC", "BAGO", "BEXP", "BUS12", "BUS5", "UNEMP",
          "GOVT", "RATEX", "PX1Q1", "DUR", "HOM", "CAR")

data <- read.csv("data/michigan/mich_raw.csv")

#check which variables are not available in the first year:
data.input <- clean_data(data, na.codes)
group.input <- as.numeric(factor(data$YYYYMM))
data.input <- data.input[, vars]

#forecast the contents of a new document

checkSparsity(data.input)

N = nrow(data.input)
J = ncol(data.input)
L = apply(data.input, MARGIN=2, FUN=function(x) return(length(unique(x))))


K=4

eta= list()
for(j in 1:J) {
  eta[[j]] = matrix(1,nrow=K,ncol=L[j])
  for(k in 1:K) {
    if ( k <= L[j]) {
      eta[[j]][k,k] = 10
    }
  }
}

print("starting")
set.seed(1)
v0=10
s0=1
steps = 3000
burn = 1000
skip = 10
tune=0.01

posterior = dhlcModel(data.input, group.input, eta, v0, s0, tune, K, steps, burn, skip)
post.ev <- posteriorMeans(posterior)
save(post.ev, file="posteriors/mich_estimate.RData")

#check BIC
print(bic(data.input, group.input, post.ev$pi, post.ev$beta, dynamic=T))

# can be used to check eigen values of Y
Y <-xtoAdjacency(data.input, group.input)


#K_2: 28
#K_3: 27
#K_4: 26.61
#K_5: 26.6
#K_6: 27.86
