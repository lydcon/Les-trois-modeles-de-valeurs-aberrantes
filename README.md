# Les-trois-modeles-de-valeurs-aberrantes
# vi kører det med outlier package.
j'utilise que 3 mais il y'on a bcp d'autres
1+1
dat<-GOOG.returns$Adj.Close
dat
n<-length(dat)
n
ret<-log(dat[2:n]/dat[1:(n-1)])
ret
mean(ret)
var(ret)
sd(ret)
plot(ret)
x<-as.ts(ret)
plot(x)

#trouvez les aberrantes: k difference from the mean
aberrantes<- function (x, opposite = FALSE, logical = FALSE) 
{
    if (is.matrix(x)) 
        apply(x, 2, outlier, opposite = opposite, logical = logical)
    else if (is.data.frame(x)) 
        sapply(x, outlier, opposite = opposite, logical = logical)
    else {

  if (xor(((x[length(x)] - mean(x)) < (mean(x) - x[1])),opposite)) 
		{
			if (!logical) min(x)
			else x == min(x)
		}
		else 
		{
			if (!logical) max(x)
			else x == max(x)
		}
	} 
}

aberrantes (x, opposite = FALSE, logical = FALSE)



#enlevez les aberrantes

enlevez <- function (x, fill = FALSE, median = FALSE, opposite = FALSE) 
 {
    if (is.matrix(x)) 
         apply(x, 2, enlevez, fill = fill, median = median, opposite = opposite)
   else if (is.data.frame(x)) 
         as.data.frame(sapply(x, enlevez, fill = fill, median = median, opposite = opposite))
     else {
  res <- x
 	if (!fill) res[-which(x == aberrantes (x,opposite))]
 	else {
 		if (median) res[which(x == aberrantes (x,opposite))]<-median(x[-which(x == outlier(x,opposite))])
 		else res[which(x == outlier(x,opposite))]<-mean(x[-which(x == aberrantes (x,opposite))])
 	res
 	}
 }
 }

enlevez (x, fill = FALSE, median = FALSE, opposite = FALSE)

x
length(x)
plot(x)



#et puis on trouve les scores

SC<-function (x, type = c("z","t","chisq","iqr","mad"), prob = NA, lim = NA) 
{
    if (is.matrix(x)) 
        apply(x, 2, scores, type = type, prob = prob, lim = lim)
    else if (is.data.frame(x)) 
        as.data.frame(sapply(x, scores, type = type, prob = prob, lim = lim))
else {
  n <- length(x)
	s <- match.arg(type)
	ty <- switch(s, z=0, t=1, chisq=2, iqr=3, mad=4)

if (ty == 0) {
	res <- (x - mean(x))/sd(x)
	if (is.na(prob)) res
	else {
		if (prob == 1) pnorm(res)
	else	if (prob == 0) abs(res) > (n-1)/sqrt(n)
	else abs(res) > qnorm(prob)
		}
}
else if (ty == 1) {
	t <- (x - mean(x))/sd(x)
	res <- (t*sqrt(n-2))/sqrt(n-1-t^2)
	if (is.na(prob)) res
	else {
		if (prob == 1) pt(res,n-2)
	else	if (prob == 0) abs(res) > (n-1)/sqrt(n)
	else abs(res) > qt(prob,n-2)
		}

}
else if (ty == 2) {
	res <- (x - mean(x))^2/var(x)
	if (is.na(prob)) res
	else {
		if (prob == 1) pchisq(res,1)
	else abs(res) > qchisq(prob,1)
		}
}
else if (ty == 3) {
	res <- x
	Q1 <- quantile(x,0.25)
	Q3 <- quantile(x,0.75)
	res[x > Q1 & res < Q3] <- 0
	res[x < Q1] <- (res[x < Q1]-Q1)/IQR(x)
	res[x > Q3] <- (res[x > Q3]-Q3)/IQR(x)
	if (is.na(lim)) res
	else abs(res) > lim
}
else if (ty == 4) {
	res <- (x - median(x))/mad(x)
	if (is.na(prob)) res
	else {
		if (prob == 1) pnorm(res)
	else	if (prob == 0) abs(res) > (n-1)/sqrt(n)
	else abs(res) > qnorm(prob)
		}
}

}
}

SC = c("z")
SC(x, type = c("z", "t", "chisq", "iqr", "mad"), prob = NA, lim = NA)


score<-SC(x, type = c("z", "t", "chisq", "iqr", "mad"), prob = NA, lim = NA)
score
plot(score)

