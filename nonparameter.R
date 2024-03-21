library(gtools)
library(dplyr)

## questionn1
# (1)
x = c(10, 15, 50)
y = c(12, 17, 19)
idx = combinations(n=6, r=3)
xy = c(x,y) # the combined data set
permut = NULL # the permuted data set 
for(i in 1:nrow(idx)){
  permut = rbind(permut, c(xy[idx[i,]], xy[-idx[i,]]))
}
permut.x = permut[, 1:3] # the permuted X matrix (20*3)
permut.y = permut[, 4:6] # the permuted Y matrix (20*3)
delta = apply(permut.x, 1, mean) - apply(permut.y, 1, mean)
delta.obs = mean(x) - mean(y)
hist(delta)
abline(v = quantile(delta, 0.95), col = "red")
table(delta)

pvalue = sum(delta >= delta.obs)/nrow(idx)
# or :pval1.upper = mean(delta >= delta.obs) #upper-tailed

# (2)
tx = apply(permut.x, 1, sum)
tx.obs = sum(x)
pvalue = sum(tx >= tx.obs)/nrow(idx)

#(4)
delta2 = apply(permut.x, 1, median) - apply(permut.y, 1, median)
delta2.obs = median(x) - median(y)
table(delta2)
pvalue = sum(delta2 >= delta2.obs)/nrow(idx)




## question2 permutation test

#load the data set:
x = c(5, 11, 16, 8, 12)
y = c(17, 14, 15, 21, 19, 13)

idx = combinations(n=11, r=5)

xy = c(x,y) # the combined data set
permut = NULL # the permuted data set 
for(i in 1:nrow(idx)){
  permut = rbind(permut, c(xy[idx[i,]], xy[-idx[i,]]))
}
permut.x = permut[, 1:5] # the permuted X matrix (20*3)
permut.y = permut[, 6:11] # the permuted Y matrix (20*3)
delta1 = apply(permut.x, 1, mean) - apply(permut.y, 1, mean)
delta2 = apply(permut.x, 1, median) - apply(permut.y, 1, median)
delta1.obs = mean(x)-mean(y)
delta2.obs = median(x) - median(y)

#pvalue for permutation of sample mean
pval1.upper = mean(delta1 >= delta1.obs) #upper-tailed
pval1.2sided = mean(abs(delta1) >= abs(delta1.obs)) #two-tailed

#pvalue for permutation of sample median
pval2.upper = mean(delta2 >= delta2.obs) #upper-tailed
pval2.2sided = mean(abs(delta2) >= abs(delta2.obs)) #two-tailed





## question3
# (1) wilcoxon rank-sum
x = c(3, 2, 1, 1, 2, 1, 3, 2, 2, 2, 2, 5, 1, 4, 1, 1, 1, 1, 6, 2, 2, 2, 1, 1)
y = c(1, 0, 1, 1, 0, 0, 1, 1, 1, 8, 1, 1, 1, 0, 1, 1, 2)
# sort the combined data
xy =c(x,y)
sort(xy)
#obtain the ranks of x
(m=length(x))
(n=length(y))
(rank.x = rank(xy)[1:m])
#rank sum statistic
(W=sum(rank.x))

result <- wilcox.test(x, y)


# (2)two sample t-test
t.test(x, y, var.equal=T, alternative="two.sided")


## question5
x = c(21.9, 20.2, 19.4, 20.3, 19.6, 20.4, 18.4, 20.1, 22, 18.9)
y = c(20.2, 13.8, 21.8, 19.2, 19.6, 25.5, 17, 17.6, 19.5, 22.2)
sd(x)
sd(y)
xy = c(x,y)
sort(xy)
srank.xy = siegelrank(xy,ansari=F)
#sum of ranks of X group
m = length(x)
n = length(y)
W = sum(srank.xy[1:m])

# the critical value
# qwilcox gives the quantiles of M-W's U distribution

# the lower critical value for W (works if m and n<50)
qwilcox(p=0.025, m, n) + m*(m+1)/2-1
# the lower critical value for Wx
qwilcox(p=0.975, m, n) + m*(m+1)/2+1
## p-values based on the exact permutation distribution of $W$

perm.W = perm.dist.sum(srank.xy, n1=5)
pval.1sided = mean(perm.W<=W)
pval.2sided = 2*mean(perm.W<=W)

pval.1sided
pval.2sided

siegel.tukey(x,y)