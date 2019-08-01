# generate structural relationship between X (covariate of interest), Z (high dimensional auxiliary data), and Y (outcome of interest)

N=1000 
Z = rnorm(N)
K=2 
theta= c(0.3,0.7)
epsilon = rnorm(N)
X = 0.3*Z + rnorm(N) 
beta = 10  
Y = X*0.05 + beta*Z + epsilon 



model = lm(Y~X+Z,data=data.frame(X=X,Y=Y,Z=Z))