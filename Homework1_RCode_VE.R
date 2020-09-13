library(readr)
cars_test<-read_csv("~/Desktop/CARS.csv")

cars_test['MPG_Combo']<- cars_test['MPG_City']*.55 + cars_test['MPG_City']*.45
print(head(cars_test))

boxplot(cars_test$MPG_Combo)

boxplot(MPG_Combo~Type,
        data=cars_test,
        main="MPG_Combo for each Type",
        xlab="Type",
        ylab="MGP_Combo",
        col="pink",
        border="green")

summary(cars_test$Invoice)

qqnorm(cars_test$Invoice); qqline(cars_test$Invoice, col = 3)

shapiro.test(cars_test$Invoice)

boxplot(Invoice ~ Origin, data=cars_test, main="Invoice by Origin", xlab="Origin", ylab="Invoice")

shapiro.test(cars_test$Invoice[cars_test$Origin=="Asia"]);

shapiro.test(cars_test$Invoice[cars_test$Origin == "Europe"])

shapiro.test(cars_test$Invoice[cars_test$Origin == "USA"]);

tran_set <- cars_test[cars_test$Origin=="Europe" | cars_test$Origin=="Asia", c("Invoice")]

normalized <- sqrt(tran_set)
normalized <- unlist(normalized,recursive=TRUE)
typeof(normalized)

shapiro.test(normalized)

summary(airquality)

july<-airquality[airquality$Month==7,c('Wind')]
aug<-airquality[airquality$Month==8,c('Wind')]

qqnorm(july);qqline(july)

qqnorm(aug);qqline(aug)

shapiro.test(july)

shapiro.test(aug)

t.test(july, aug)


