# Liberías necesarias para resolver el ejercicio
library(arules)
library(arulesViz)
# Lista de compras
market_basket <-  
  list(  
    c("apple", "beer", "rice", "meat"),
    c("apple", "beer", "rice"),
    c("apple", "beer", "rice"),
    c("apple", "beer"), 
    c("apple", "pear"),
    c("apple", "beer", "rice", "pear"),
    c("milk", "beer", "rice", "meat"), 
    c("apple", "beer", "rice"), 
    c("apple", "rice", "pear"),
    c("milk", "beer", "rice", "meat"), 
    c("milk", "rice"), 
    c("apple", "beer"),
    c("milk", "rice"), 
    c("milk", "beer"),
    c("milk", "pear")
  )

# nombramos las compras 
names(market_basket) <- paste("C", c(1:length(market_basket)), sep = "")

# Transformación
trans <- as(market_basket, "transactions")

# Lista de productos
itemLabels(trans)

# Resumen de los datos
summary(trans)

# Visualización
image(trans)

# Frecuencia relativa de los productos
itemFrequencyPlot(trans, topN=10,  cex.names=1)

# A-Priori

#Min Support 0.3, confidence 0.5.
rules <- apriori(trans, 
                 parameter = list(supp=0.3, conf=0.5, 
                                  maxlen=10, 
                                  target= "rules"))

summary(rules)

# Producto `beer`
beer_rules_lhs <- apriori(trans, 
                          parameter = list(supp=0.3, conf=0.5, 
                                           maxlen=10, 
                                           minlen=2),
                          appearance = list(lhs="beer", default="rhs"))

inspect(beer_rules_lhs)


# Visualizar las reglas
subrules <- head(rules, n=10, by = "confidence")

plot(subrules, method = "graph",  engine = "htmlwidget")

plot(subrules, method="paracoord")
