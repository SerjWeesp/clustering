library(arules)
library(arulesViz)
library(arulesCBA)

#The dataset consists the data from one belarussian local fast-food restaurants. Apriori algorythm was used to build some rules and offer some sets ideas. 

#load the data
orders<-read.transactions("D:\\UW\\1st semester\\Unsupervised learning\\projects\\orders.csv", format="single", sep=",", cols=c("Order","Meal"), header=TRUE)

#inspect the data
inspect(orders[1:10])
size(head(orders))
itemFrequency(head(orders), type="relative")
itemFrequency(head(orders), type="absolute")
mean(size((orders))) #average number of items in a single order is 3-4 items

ctab<-crossTable(orders, measure="count", sort=TRUE) 
#ctab

ptab<-crossTable(orders, measure="probability", sort=TRUE)
#round(ptab,3)

chi2tab<-crossTable(orders, measure="chiSquared", sort=TRUE)
#round(chi2tab,2) #the dependent pairs are GOLD STAR burger and Cola 0.4 as well as French fries and Cola 0.4

#plots
itemFrequencyPlot(orders, support = 0.13)
itemFrequencyPlot(orders, topN = 10)
itemFrequencyPlot(orders, topN=10, type="absolute", main="Item Frequency") 
itemFrequencyPlot(orders, topN=10, type="relative", main="Item Frequency") #there aren't any sweets is top10 meals
image(sample(orders, 100))


#apriori algorythm
rules <- apriori(orders, parameter = list(support = 0.05, confidence = 0.5, minlen = 2))
inspect(rules)
summary(rules)

#reordering
inspect(sort(rules, by = "support")) #GOLD star burger, fries and Cola has some common rules
inspect(sort(rules, by = "confidence")) 
inspect(sort(rules, by = "lift"))

#rules with orders id
supportingTransactions(rules, orders)
inspect(supportingTransactions(rules, orders))

#set up specific rules for popcakes and nuggets
rules.vancake<-apriori(data=orders, parameter=list(supp=0.005,conf = 0.15), 
                       appearance=list(default="lhs", rhs="Vanilla cupcake"), control=list(verbose=F)) 
rules.vancake.byconf<-sort(rules.vancake, by="confidence", decreasing=TRUE)
inspect(head(rules.vancake.byconf))

rules.vancakeopp<-apriori(data=orders, parameter=list(supp=0.01,conf = 0.005), 
                          appearance=list(default="rhs", lhs="Vanilla cupcake"), control=list(verbose=F)) 
rules.vancakeopp.byconf<-sort(rules.vancakeopp, by="confidence", decreasing=TRUE) 
inspect(head(rules.vancakeopp.byconf))

rules2 <- apriori(orders, parameter = list(support = 0.01, confidence = 0.1, minlen = 2))
popcakesrules <- subset(rules2, items %in% "Pops cake")
inspect(popcakesrules) #Cakes and Cappuccino are perspective sets

#plot apriori results
plot(rules)
plot(rules, method="grouped") 
plot(rules, method="graph")
plot(rules, method="paracoord", control=list(reorder=TRUE))

#closed itemsets
rules.closed<-apriori(orders, parameter=list(target="closed frequent itemsets", support=0.15))
inspect(rules.closed)

#tests
is.significant(rules, orders, adjust="holm")
is.redundant(rules)
is.superset(rules) 
is.maximal(rules)
orders.sel<-orders[,itemFrequency(orders)>0.05] # selected transations
d.jac.i<-dissimilarity(orders.sel, which="items") # Jaccard as default
round(d.jac.i,2) 

#Conclusion
#According to analysis the is a significant chance, that Combo sets, including:
#GOLD STAR burger + French fries + Cola 0.4l
#Cakes + Cappuccino
#Also cheese sause is the most suitable for french fries
