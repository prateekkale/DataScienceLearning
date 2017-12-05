data("airquality")
cor(airquality)

names(airquality)
str(airquality)


plot(airquality$Ozone~airquality$Solar.R)

#mean of ozone concentration
mean.Ozone<-mean(airquality$Ozone,na.rm = TRUE)
abline(h=mean.Ozone)

#lm function to fit regression line in this data

lmmodel<-lm(airquality$Ozone~airquality$Solar.R)

lmmodel
abline(lmmodel,col="red")

plot(lmmodel)


termplot(lmmodel)

summary(lmmodel)

coplot(Ozone~Solar.R|Wind,panel=panel.smooth,airquality)


model2<-lm(Ozone~Solar.R*Wind,airquality)
plot(model2)


summary(model2)


termplot(model2)

summary(airquality$Solar.R)
predict(model2,data.frame(Solar.R=100,Wind=10))



p1<-predict(model2,data.frame(Solar.R=mean(airquality$Solar.R,na.rm = TRUE),Wind=1:20))

p2<-predict(model2,data.frame(Solar.R=100,Wind=1:20))

p3<-predict(model2,data.frame(Solar.R=300,Wind=1:20))


plot(Ozone~Wind,airquality)
lines(1:20,p1)
lines(1:20,p2)
lines(1:20,p3)








