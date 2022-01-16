``

# Week 12

## Types of Data
- Univariate data -- graphs for display and compare distributions
  - Histogram -- for checking symmetry
  - Box-and-whiskers plot -- summarizing the distribution
  - Quantile plot -- a detailed display of data distributions
  - Normal quantile plot (normal Q-Q) -- comparing the data distribution to the normal distribution
    - Q-Q math -- comparing to a specific distribution
  - Quantile-quantile plot (Q-Q) -- comparing two distributions
- Basic types of differneces when comparing two distributions
  - additive difference
  - multiplicative difference
  - the relationship between additive and multiplicative
- Fit and residuals -- understanding the difference among groups and within groups
  When two data distributions are known to differ by an additive shift, we know that the difference between them lies in their locations.  The two most commonly used candidates for location estimation are the median and the mean.  One way to understand a distribution is to seperate the location estimate from the data.  That is, each data point can be represented as a sum of two parts: the location and the difference between the data point and the location.  
  $$
  y_i = \bar{y} + \epsilon_i
  $$
When comparing multiple data sets:
$$
  y_{ij} = \bar{y}_j + \epsilon_{ij}
$$
for $j = 1, \cdots, J$. The location estimates $\bar{y}_j$ is called the "fit" and $\epsilon_{ij}$ is called the "residual". When the differences among the $J$ variables are additive, the residual distributions are identical.  One convenient way to check for additive shift is to examine residual distributions.


```r
book.2.12 <- function(){
    dotplot(tapply(singer$height,singer$voice.part,mean), 
            aspect=1,
            sub = list("Figure 2.12",cex=.8),
            xlab="Mean Height (inches)")
}

book.2.12()
```

![](Week12_files/figure-latex/unnamed-chunk-1-1.pdf)<!-- --> 

```r
ggVD_2.12 <- function(){
    meandot <- data.frame(tapply(singer$height, singer$voice.part, mean))
    singer.means = data.frame(
        voice.part=ordered(rownames(meandot),
                           levels=rev(c("Soprano 1", "Soprano 2",
                                        "Alto 1", "Alto 2" ,
                                        "Tenor 1", "Tenor 2",
                                        "Bass 1", "Bass 2"))),
        height=meandot[,1])
    
    ggplot(singer.means, aes(height,voice.part)) + geom_point() +
        labs(x = "Mean Height (inches)")  + theme(aspect.ratio=1)
}

ggVD_2.12()
```

![](Week12_files/figure-latex/unnamed-chunk-1-2.pdf)<!-- --> 

```r
book.2.13 <- function(){
    bwplot(voice.part ~ oneway(height~voice.part, spread = 1)$residuals,
           data = singer,
           aspect=0.75,
           panel = function(x,y){
               panel.bwplot(x,y)
               panel.abline(v=0)
           },
           sub = list("Figure 2.13",cex=.8),
           xlab = "Residual Height (inches)")
}

book.2.13()
```

![](Week12_files/figure-latex/unnamed-chunk-1-3.pdf)<!-- --> 

```r
ggVD_2.13 <- function(){
    res.height <- oneway(height ~ voice.part, data = singer,
                         spread = 1)$residuals 
    
    ggplot(singer, aes(voice.part, res.height)) + geom_boxplot() +
        coord_flip( )+ labs(xlab = "Residual Height (inches)") +
        theme(aspect.ratio=0.75) 
}
ggVD_2.13()
```

![](Week12_files/figure-latex/unnamed-chunk-1-4.pdf)<!-- --> 
The decomposition of a data point into a fit and a residual special allows us to separated the variation in singer's heights into two components. The fitted values (the fits) account for the variation in the heights attributed to the voice-part variable and the residuals are the remaining variation in the data after the variation due to the shifting means has been removed. When we are convinced that the residual distributions are identical, we can pool all residuals together to better characterize the distribution.  Another way to use the pooled residuals is to check for the homogeneity assumption (residual distributions are identical). 


```r
book.2.14 <- function()
{
    res.height <- oneway(height ~ voice.part, data = singer,
                         spread = 1)$residuals
    qqmath(~ res.height | singer$voice.part, 
           distribution = substitute(function(p) quantile(res.height, p)),
           panel=function(x){
               panel.grid()
               panel.qqmathline(x)
               panel.qqmath(x)
           },
           aspect=1,
           layout=c(2,4),
           sub = list("Figure 2.14",cex=.8),
           xlab = "Pooled Residual Height (inches)",
           ylab = "Residual Height (inches)")
}
book.2.14()
```

![](Week12_files/figure-latex/unnamed-chunk-2-1.pdf)<!-- --> 

```r
ggVD_2.14 <- function(){
    res.height <- oneway(height ~ voice.part, data = singer,
                         spread = 1)$residuals
    
    ggplot(singer, aes(sample =res.height )) +
        stat_qq(distribution = qnorm) +
        geom_qq_line() +
        facet_wrap(~voice.part, ncol = 2) +
        labs(x = "Pooled Residual Height (inches)",
             y = "Residual Height (inches)") +
        theme(aspect.ratio=1)
}
ggVD_2.14()
```

![](Week12_files/figure-latex/unnamed-chunk-2-2.pdf)<!-- --> 

### Checking for normality of the pooled residuals

```r
book.2.15 <- function()
{
    qqmath(~ oneway(height ~ voice.part, spread = 1)$residuals, 
           data = singer,
           distribution = qunif,
           aspect = 1,
           sub = list("Figure 2.15",cex=.8),
           xlab = "f-value",
           ylab = "Residual Height (inches)")
}

book.2.15()
```

![](Week12_files/figure-latex/unnamed-chunk-3-1.pdf)<!-- --> 

```r
ggVD_2.15 <- function()
{
    res.height.f <- ((1:dim(singer)[1]) -.5)/(dim(singer)[1])
    singer$res.height.f <- res.height.f 
    singer$res.height <- oneway(height ~ voice.part,
                                data = singer, spread = 1)$residuals
    
    ggplot(singer, aes(sample = res.height)) +
        stat_qq(distribution = qunif) +
        labs(x = "f-value", y = "Residual Height (inches)")+
        theme(aspect.ratio=1)
}

ggVD_2.15()
```

![](Week12_files/figure-latex/unnamed-chunk-3-2.pdf)<!-- --> 

```r
book.2.16 <- function(){
    qqmath(~ oneway(height~voice.part, spread = 1)$residuals,
           data = singer,
           prepanel = prepanel.qqmathline, 
           panel = function(x, ...) {
               panel.grid()
               panel.qqmathline(x, distribution = qnorm)
               panel.qqmath(x,...)
           },
           aspect=1,
           sub = list("Figure 2.16",cex=.8),
           xlab = "Unit Normal Quantile",
           ylab="Residual Height (inches)")
}    

book.2.16()
```

![](Week12_files/figure-latex/unnamed-chunk-3-3.pdf)<!-- --> 

```r
ggVD_2.16 <- function(){
    data <- singer
    data$res.height <- oneway(height~voice.part, 
                              spread = 1, data=singer)$residuals
    ggplot(data, aes(sample = res.height)) +
    stat_qq(distribution = qnorm) +
    geom_qq_line()+labs(x = "f-value", y = "Residual")+
        theme(aspect.ratio=1)
}

ggVD_2.16()
```

![](Week12_files/figure-latex/unnamed-chunk-3-4.pdf)<!-- --> 

### The `rfs` plot



```r
book.2.17 <- function(){
    rfs(oneway(height~voice.part, data = singer, spread = 1), 
        aspect=1, 
        sub = list("Figure 2.17",cex=.8),
        ylab = "Height (inches)")
}
book.2.17()
```

![](Week12_files/figure-latex/unnamed-chunk-4-1.pdf)<!-- --> 

```r
ggVD_2.17 <- function(){
    Fitoneway <- oneway(height~voice.part, data = singer, spread = 1)
    fitmean <- Fitoneway$fitted-mean(Fitoneway$fitted)
    singer.rfs <- data.frame(Fitted_minus_mean=fitmean,
                             Residual=Fitoneway$residuals) 
    singer.m <-reshape2::melt(singer.rfs) 
    ggplot(singer.m)+stat_qq(aes(sample = value),
                             distribution = qunif) +
        facet_wrap(.~variable) +
        labs(y = "Height (inches)")+
        theme(aspect.ratio=1)
}

ggVD_2.17()
```

```
## No id variables; using all as measure variables
```

![](Week12_files/figure-latex/unnamed-chunk-4-2.pdf)<!-- --> 

## Additive versus multiplicative shifts

An example of multiplicative shift











