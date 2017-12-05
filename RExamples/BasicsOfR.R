a <- 3
b <- sqrt(a*a+3)
ls()

a <- c(1,2,3,4,5)

a+1

mean(a)

var(a)

typeof(a)

a <- "hello"

b <- c("hello","there")

tree<-trees

a <- c(1,2,3,4)
b <- c(2,4,6,8)
levels <- factor(c("A","B","A","B"))
df <- data.frame(a,b,levels)

summary(df)


df$a

logicalvar = TRUE
typeof(logicalvar)

is.numeric(a)



matdf<-matrix(c(70,120,65,140),ncol=2,byrow=TRUE)

#t(matdf)
#Plot the graph

colors = c("red", "yellow", "green", "violet", "orange")
hist(tree$Height,main="Distribution of height",xlab="height",col=colors)
