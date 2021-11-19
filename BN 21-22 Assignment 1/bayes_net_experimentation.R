library(lavaan)
library(dagitty)
library(bnlearn)
dataset = read.csv("heart_failure_clinical_records_dataset.csv")

#dataset <-as.data.frame(scale(dataset[,c("anaemia","creatinine_phosphokinase","diabetes","ejection_fraction","high_blood_pressure","age","platelets","serum_creatinine","serum_sodium","sex","smoking","time","DEATH_EVENT")]))

dag1 = graphLayout(dagitty("dag {
DEATH_EVENT -> serum_sodium
age -> DEATH_EVENT
creatinine_phosphokinase -> serum_creatinine
diabetes -> high_blood_pressure
ejection_fraction -> DEATH_EVENT
high_blood_pressure -> ejection_fraction
platelets -> high_blood_pressure
serum_creatinine -> DEATH_EVENT
serum_creatinine -> anaemia
sex -> DEATH_EVENT
smoking -> diabetes
smoking -> high_blood_pressure
time -> DEATH_EVENT
}
"))

plot(dag1)

impliedConditionalIndependencies(dag1)

localTests(dag1, dataset, type = 'cis.chisq')

dataset$age <- cut(dataset$age, c(39,50,60,70,80,90,100))
levels(dataset$age) = c("40-50","50-60","60-70","70-80","80-90","90-100")
dataset$age <- ordered(dataset$age, levels=c("40-50","50-60","60-70","70-80","80-90","90-100"))

M <- lavCor(dataset)
M

localTests(dag1, sample.cov = M, sample.nobs = nrow(dataset))


cg <- coordinates(dag1)
fit <- sem(toString(dag1,"lavaan"), sample.cov=M, sample.nobs=nrow(dataset))

summary(fit)

fg <- lavaanToGraph(fit, digits = 2)
coordinates(fg) <- cg
plot(fg, show.coefficients=TRUE)





summary(lm(DEATH_EVENT ~ anaemia + serum_creatinine, dataset))


net <- model2network(toString(dag1,"bnlearn"))
fit <- bn.fit(net, dataset)
fit

predict(fit,node="diabetes", data=data.frame(smoking =as.double(1:100)))


lvsem <- toString(dag1,"lavaan")
lvsem.fit <- sem(lvsem, dataset)
summary(lvsem.fit)
