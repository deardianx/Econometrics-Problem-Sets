#import data
data<- read_excel("C:/Users/dxu/OneDrive - London Business School/Desktop/2023Fall/Econometrics/wage.xlsx")

#calculating parameters
n1=sum(data$male)
n2=length(data$male)-n1
y<-log(data$wage)
ybar<-mean(y)
s1sq=var(log(data$wage[data$male==1]))
s2sq=var(log(data$wage[data$male==0]))

#calculate kurtosis
k<-(n1+n2)*sum((y-ybar)^4)/(sum((y-ybar)^2))^2

#calculate test statistics
T<-(n1*n2/(n1+n2))^(1/2)*(log(s1sq)-log(s2sq))
t=T/(k-1)^(1/2)
#t=-0.9423021 > critical value -3.289707 so we fail to reject H0 with statistic T
qnorm(0.05,mean=0,sd=2)

F<-s1sq/s2sq
#F=0.9209199 < critical value 0.9221371 so we can reject H0 with statistic F
qf(0.05,df1=n1, df2=n2)
