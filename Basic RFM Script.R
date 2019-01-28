library(XLConnect)
library(readr)
library(plyr)
library(stats)
library(stringr)
library(data.table)
library(dplyr)
library(ISOweek)
library(rpivotTable)

source("/home/c.stienen/Info/DelphiDBIconnect.R")
data <- dbGetQuery(Delphi,"SELECT        OMZETDATUM.STATISTIEKJAAR_NUMERIEK, OMZETDATUM.STATISTIEKWEEK, FACTUURREGEL.ORDERDATUM_ID, FACTUURREGEL.ORDERNUMMER, 
                         SUBKLANT.KLANTNUMMER + SUBKLANT.SUBKLANTNUMMER AS CustomerId, SUBKLANT.KLANTNUMMER, SUBKLANT.PRIJSLIJNNUMMER, SUBKLANT.ACTIEPRIJZEN, SK_DOELGROEP.DGPCODE, 
                   SUBKLANT.MARKETING_NUMMER, SUBKLANT.MARKETING_OMSCHRIJVING, SUBKLANT.CC_NUMMER_KLANT, ARTIKEL.ARTIKELNUMMER, ARTIKEL.MERKNAAM_VOLUIT, 
                   CASE WHEN (ARTIKEL.SUBPRODUKTGROEP_NIVEAU_6 = '75') THEN 'TRUE' ELSE 'FALSE' END AS PL, ARTIKEL.INDICATIE_COMPANY_WIDE_SANEREN, PRODUCT_HIERARCHIE.NIVEAU_01, 
                   PRODUCT_HIERARCHIE.NIVEAU_02, PRODUCT_HIERARCHIE.NIVEAU_03, SUM(FACTUURREGEL.NETTO_FACTUURREGELBEDRAG) AS nettoFactuurOmzet, COUNT(FACTUURREGEL.FACTUURREGEL_ID) AS aantalFactuurregels, 
                   SUM(FACTUURREGEL.AANTAL) AS aantalVkeh, SUM(CASE WHEN FACTUURREGEL.TE_FACTUREREN_GEWICHT = 0 THEN FACTUURREGEL.AANTAL_STANDAARD_EENH ELSE FACTUURREGEL.TE_FACTUREREN_GEWICHT END)
                   AS aantalSteh
                   FROM            Budget.dbo.Doelgroep AS SK_DOELGROEP RIGHT OUTER JOIN
                   Dwh.DIM_SUBKLANT AS SUBKLANT ON SK_DOELGROEP.MKSCODE = SUBKLANT.MARKETINGSEGMENTCODE INNER JOIN
                   Dwh.FACT_FACTUURREGEL AS FACTUURREGEL ON SUBKLANT.SUBKLANT_ID = FACTUURREGEL.SUBKLANT_ID INNER JOIN
                   Dwh.DIM_FACTURERINGSSOORT AS FACTURERINGSSOORT ON FACTURERINGSSOORT.FACTURERINGSSOORT_ID = FACTUURREGEL.FACTURERINGSSOORT_ID INNER JOIN
                   Dwh.DIM_DATUM AS OMZETDATUM ON OMZETDATUM.DATUM_ID = FACTUURREGEL.OMZETDATUM_ID INNER JOIN
                   Dwh.DIM_ARTIKEL AS ARTIKEL ON FACTUURREGEL.ARTIKEL_ID = ARTIKEL.ARTIKEL_ID INNER JOIN
                   Dwh.DIM_PRODHIERARCHIE AS PRODUCT_HIERARCHIE ON PRODUCT_HIERARCHIE.PRODUCTHIERARCHIE_CODE = ARTIKEL.PRODUCTHIERARCHIE
                   WHERE        (OMZETDATUM.AANTAL_STATISTIEK_WEKEN_GELEDEN BETWEEN - 52 AND - 1) AND (FACTURERINGSSOORT.FACTURERINGSSOORT NOT IN ('005', '009')) AND (ARTIKEL.HOOFDPRODUKTGROEP_NIVEAU_1 <> '07') AND 
                   (SUBKLANT.KLANTCODE <> '6')
                   GROUP BY OMZETDATUM.STATISTIEKJAAR_NUMERIEK, OMZETDATUM.STATISTIEKWEEK, FACTUURREGEL.ORDERDATUM_ID, FACTUURREGEL.ORDERNUMMER, SUBKLANT.KLANTNUMMER + SUBKLANT.SUBKLANTNUMMER, 
                   SUBKLANT.KLANTNUMMER, SUBKLANT.PRIJSLIJNNUMMER, SUBKLANT.ACTIEPRIJZEN, SK_DOELGROEP.DGPCODE, SUBKLANT.MARKETING_NUMMER, SUBKLANT.MARKETING_OMSCHRIJVING, 
                   SUBKLANT.CC_NUMMER_KLANT, ARTIKEL.ARTIKELNUMMER, ARTIKEL.MERKNAAM_VOLUIT, CASE WHEN (ARTIKEL.SUBPRODUKTGROEP_NIVEAU_6 = '75') THEN 'TRUE' ELSE 'FALSE' END, 
                   ARTIKEL.INDICATIE_COMPANY_WIDE_SANEREN, PRODUCT_HIERARCHIE.NIVEAU_01, PRODUCT_HIERARCHIE.NIVEAU_02, PRODUCT_HIERARCHIE.NIVEAU_03")
rm(Delphi)
#View(data)


#Date format change
data$date <- str_extract_all(data$ORDERDATUM_ID, "^[0-9]{4}")
data$date2 <- str_extract_all(data$ORDERDATUM_ID, "^[0-9]{6}")
data$date2 <- str_extract_all(data$ORDERDATUM_ID, "[0-9]{2}$")
data$date3 <- str_extract_all(data$ORDERDATUM_ID, "[0-9]{2}$")
data$date <- paste(data$date, data$date2, data$date3, sep = "-")
data$`ORDERDATUM_ID` <- as.Date(data$date, format = "%Y-%M-%d")

#Create backup for testing
dataBackup <- data
data <- dataBackup
head(data)


data <- subset(data, (PRIJSLIJNNUMMER == 40 & 
                        ACTIEPRIJZEN == "J" &
                        !(MARKETING_OMSCHRIJVING %in% as.vector(c("CONTR.CAT BEDRIJVEN", "CONTR.CAT RECREATIE", "CONTR.CAT ZORG",
                                                                  "CONTR/EVENTCATERING", "AZIATISCH CAFETARIA")) &
                            DGPCODE != 3)))

colnames(data)[colnames(data) == 'nettoFactuurOmzet'] <- 'Revenue'
colnames(data)[colnames(data) == 'ORDERDATUM_ID'] <- 'Orderdate'
colnames(data)[colnames(data) == 'MARKETING_OMSCHRIJVING'] <- 'Salesgroup'
colnames(data)[colnames(data) == 'ORDERNUMMER'] <- 'OrderId'

length(unique(data$CustomerId))
sum(is.na(data$CustomerId))
data <- subset(data, !is.na(data$CustomerId))

length(unique(data$`OrderId`))

length(unique(data$`Orderdate`))
range(data$`Orderdate`)
#data$ISOweek <- date2ISOweek(data$Orderdate)

# Create customer-level dataset #
customers <- as.data.frame(unique(data$`CustomerId`))
names(customers) <- "CustomerId"

# Recency #
data$recency2 <- as.Date(Sys.time())
data$Orderdate <- as.character(data$Orderdate)
data$recency <- as.integer(difftime(data$recency2, data$Orderdate, units="weeks"))
recency <- unique(setDT(data)[order(data$recency)], by = "CustomerId")
recency <- subset(recency, select=c("CustomerId", "recency"))
customers <- merge(customers, recency, by="CustomerId", all=TRUE, sort=TRUE)
customers$recency <- (customers$recency) # log can't handle a recency value of 0
rm(recency)

# Frequency #)
annual.orders <- subset(data, select=c("CustomerId", "OrderId"))
annual.orders <- distinct(annual.orders)
annual.orders$frequency <- "1"
annual.orders$frequency <- as.integer(annual.orders$freq)
customers2 <- aggregate(frequency ~ CustomerId, data=annual.orders, FUN=sum, na.rm=TRUE)
customers <- merge(customers, customers2, by="CustomerId", all=TRUE, sort=TRUE)
rm(annual.orders)
rm(customers2)

range(customers$frequency)
table(customers$frequency)

# Monetary #
annual.sales <- aggregate(Revenue ~ CustomerId, data=data, FUN=sum, na.rm=TRUE)
colnames(annual.sales)[colnames(annual.sales) == 'Revenue'] <- 'monetary'
customers <- merge(customers, annual.sales, by="CustomerId", all=TRUE, sort=TRUE)
rm(annual.sales)

# Modeling #
pareto.cutoff <- 0.8 * sum(customers$monetary)
customers$pareto <- ifelse(cumsum(customers$monetary) <= pareto.cutoff, "Top 20%", "Bottom 80%")
customers$pareto <- factor(customers$pareto, levels=c("Top 20%", "Bottom 80%"), ordered=TRUE)
levels(customers$pareto)
round(prop.table(table(customers$pareto)), 2)
remove(pareto.cutoff)

# Log-transform positively-skewed variables
customers$recency.log <- log(customers$recency)
customers$frequency.log <- log(customers$frequency)
customers$monetary.log <- customers$monetary + 0.1 # can't take log(0), so add a small value to remove zeros
customers$monetary.log <- log(customers$monetary.log)
customers <- subset(customers, recency.log != "-Inf")
customers <- subset(customers, recency.log != "NaN")

# Z-scores
customers$recency.z <- scale(customers$recency.log, center=TRUE, scale=TRUE)
customers$frequency.z <- scale(customers$frequency.log, center=TRUE, scale=TRUE)
customers$monetary.z <- scale(customers$monetary.log, center=TRUE, scale=TRUE)


library(ggplot2)
library(scales)

# Original scale
scatter.1 <- ggplot(customers, aes(x = frequency, y = monetary))
scatter.1 <- scatter.1 + geom_point(aes(colour = recency, shape = pareto))
scatter.1 <- scatter.1 + scale_shape_manual(name = "80/20 Designation", values=c(17, 16))
scatter.1 <- scatter.1 + scale_colour_gradient(name="Recency\n(Days since Last Purchase))")
scatter.1 <- scatter.1 + scale_y_continuous(label=dollar)
scatter.1 <- scatter.1 + xlab("Frequency (Number of Purchases)")
scatter.1 <- scatter.1 + ylab("Monetary Value of Customer (Annual Sales)")
scatter.1

# Log-transformed
scatter.2 <- ggplot(customers, aes(x = frequency.log, y = monetary.log))
scatter.2 <- scatter.2 + geom_point(aes(colour = recency.log, shape = pareto))
scatter.2 <- scatter.2 + scale_shape_manual(name = "80/20 Designation", values=c(17, 16))
scatter.2 <- scatter.2 + scale_colour_gradient(name="Log-transformed Recency")
scatter.2 <- scatter.2 + xlab("Log-transformed Frequency")
scatter.2 <- scatter.2 + ylab("Log-transformed Monetary Value of Customer")
scatter.2

# How many customers are represented by the two data points in the lower left-hand corner of the plot? 18
delete <- subset(customers, monetary.log < 0)
no.value.custs <- unique(delete$CustomerId)
delete2 <- subset(data, CustomerId %in% no.value.custs)
delete2 <- delete2[order(delete2$CustomerId, delete2$Orderdate),]
remove(delete, delete2, no.value.custs)

# Scaled variables
scatter.3 <- ggplot(customers, aes(x = frequency.z, y = monetary.z))
scatter.3 <- scatter.3 + geom_point(aes(colour = recency.z, shape = pareto))
scatter.3 <- scatter.3 + scale_shape_manual(name = "80/20 Designation", values=c(17, 16))
scatter.3 <- scatter.3 + scale_colour_gradient(name="Z-scored Recency")
scatter.3 <- scatter.3 + xlab("Z-scored Frequency")
scatter.3 <- scatter.3 + ylab("Z-scored Monetary Value of Customer")
scatter.3

remove(scatter.1, scatter.2, scatter.3)
preprocessed <- subset(customers, select = c(CustomerId, recency.z, frequency.z, monetary.z))
preprocessed <- subset(preprocessed, !is.na(preprocessed$recency.z))
preprocessed <- subset(preprocessed, !is.na(preprocessed$frequency.z))
preprocessed <- subset(preprocessed, !is.na(preprocessed$monetary.z))
customers <- subset(customers, CustomerId %in% as.vector(preprocessed$CustomerId))
preprocessed <- subset(preprocessed, select = c(recency.z, frequency.z, monetary.z))

j <- 16 # specify the maximum number of clusters you want to try out

models <- data.frame(k=integer(),
                     tot.withinss=numeric(),
                     betweenss=numeric(),
                     totss=numeric(),
                     rsquared=numeric())

for (k in 1:j ) {
  
  print(k)
  
  # Run kmeans
  output <- kmeans(preprocessed, centers = k, nstart = 20)
  
  
  # Add cluster membership to customers dataset
  var.name <- paste("cluster", k, sep="_")
  customers[,(var.name)] <- output$cluster
  customers[,(var.name)] <- factor(customers[,(var.name)], levels = c(1:k))
  
  # Graph clusters
  cluster_graph <- ggplot(customers, aes(x = frequency.log, y = monetary.log))
  cluster_graph <- cluster_graph + geom_point(aes(colour = customers[,(var.name)]))
  colors <- c('red','orange','green3','deepskyblue','blue','darkorchid4','violet','pink1',
              'tan3','black', 'darkgreen', 'brown', 'darkred', 'skyblue', 'limegreen', 'yellow')
  cluster_graph <- cluster_graph + scale_colour_manual(name = "Cluster Group", values=colors)
  cluster_graph <- cluster_graph + xlab("Log-transformed Frequency")
  cluster_graph <- cluster_graph + ylab("Log-transformed Monetary Value of Customer")
  title <- paste("k-means Solution with", k, sep=" ")
  title <- paste(title, "Clusters", sep=" ")
  cluster_graph <- cluster_graph + ggtitle(title)
  print(cluster_graph)
  
  # Cluster centers in original metrics
  library(plyr)
  print(title)
  cluster_centers <- ddply(customers, .(customers[,(var.name)]), summarize,
                           monetary=round(median(monetary),2),# use median b/c this is the raw, heavily-skewed data
                           frequency=round(median(frequency),1),
                           recency=round(median(recency), 0))
  names(cluster_centers)[names(cluster_centers)=="customers[, (var.name)]"] <- "Cluster"
  print(cluster_centers)
  cat("\n")
  cat("\n")
  
  # Collect model information
  models[k,("k")] <- k
  models[k,("tot.withinss")] <- output$tot.withinss # the sum of all within sum of squares
  models[k,("betweenss")] <- output$betweenss
  models[k,("totss")] <- output$totss # betweenss + tot.withinss
  models[k,("rsquared")] <- round(output$betweenss/output$totss, 3) # percentage of variance explained by cluster membership
  assign("models", models, envir = .GlobalEnv)
  
  remove(output, var.name, cluster_graph, cluster_centers, title, colors)
  
}


library(ggplot2)
library(scales)

# Graph variance explained by number of clusters
r2_graph <- ggplot(models, aes(x = k, y = rsquared))
r2_graph <- r2_graph + geom_point() + geom_line()
r2_graph <- r2_graph + scale_y_continuous(labels = scales::percent)
r2_graph <- r2_graph + scale_x_continuous(breaks = 1:j)
r2_graph <- r2_graph + xlab("k (Number of Clusters)")
r2_graph <- r2_graph + ylab("Variance Explained")
r2_graph

# Graph within sums of squares by number of clusters
# Look for a "bend" in the graph, as with a scree plot
ss_graph <- ggplot(models, aes(x = k, y = tot.withinss))
ss_graph <- ss_graph + geom_point() + geom_line()
ss_graph <- ss_graph + scale_x_continuous(breaks = 1:j)
ss_graph <- ss_graph + scale_y_continuous(labels = scales::comma)
ss_graph <- ss_graph + xlab("k (Number of Clusters)")
ss_graph <- ss_graph + ylab("Total Within SS")
ss_graph

remove(j, r2_graph, ss_graph)

# Custers NbClust

#library(NbClust)
#set.seed(20180308)
#nc <- NbClust(preprocessed, min.nc=2, max.nc=7, method="kmeans")
#table(nc$Best.n[1,])

#nc$All.index # estimates for each number of clusters on 26 different metrics of model fit

#barplot(table(nc$Best.n[1,]),
#        xlab="Number of Clusters", ylab="Number of Criteria",
#        main="Number of Clusters Chosen by Criteria")

#remove(preprocessed)

library(car)
library(rgl)

colors <- c('red','orange','green3','deepskyblue','blue','darkorchid4','violet','pink1',
            'tan3','black', 'darkgreen', 'purple','brown', 'darkred', 'skyblue', 'limegreen', 'yellow')
scat3D <- scatter3d(
          x = customers$frequency.log,
          y = customers$monetary.log,
          z = customers$recency.log,
          groups = customers$cluster_15,
          xlab = "Frequency (Log-transformed)",
          ylab = "Monetary Value (log-transformed)",
          zlab = "Recency (Log-transformed)",
          surface.col = colors,
          axis.scales = FALSE,
          surface = TRUE, # produces the horizonal planes through the graph at each level of monetary value
          fit = "smooth",
          # ellipsoid = TRUE, # to graph ellipses uses this command and comment out "surface = TRUE"
          grid = TRUE,
          #options(rgl.printRglwidget = TRUE),
          #webgl = TRUE,
          axis.col = c("black", "black", "black"))
        #remove(colors)
print(scat3D)

summary(customers)


########################################################################
customers <- subset(customers, select=c("CustomerId", "cluster_15"))
data <- merge(data, customers, by = "CustomerId", all=TRUE, sort=TRUE, na.rm=TRUE, no.dups = TRUE)
colnames(data)[colnames(data) == 'cluster_15'] <- 'Cluster'

#colnames(data)[colnames(data) == 'Count Order Line Items'] <- 'Orderlines'
   
clusters <- subset(data, select=c("CustomerId", "Cluster"))
clusters$frequency <- "1"
clusters$frequency <- as.integer(clusters$frequency)
clusters <- unique(setDT(clusters)[order(clusters$Cluster)], by = "CustomerId")
clusters <- aggregate(frequency ~ Cluster, data=clusters, FUN=sum, na.rm=TRUE, sort=TRUE)
print(clusters)

data <- subset(data, !is.na(data$Cluster))
colnames(data)[colnames(data) == 'NIVEAU_01'] <- 'Lvl01'
colnames(data)[colnames(data) == 'NIVEAU_02'] <- 'Lvl02'
colnames(data)[colnames(data) == 'NIVEAU_03'] <- 'Lvl03'
data <- subset(data, data$Lvl01 < 200)
#write.csv2(data2, "data2.csv")

# Create customer-level datasets for exploration & visualisation #
data2 <- subset(data, select=c("CustomerId", "Lvl01", "aantalFactuurregels", "Salesgroup"))
data2$aantalFactuurregels <- as.integer(data2$aantalFactuurregels)
Lvl01 <- as.list(unique(data$Lvl01))
data2 <- merge(data2, customers, by="CustomerId", all=TRUE, sort=TRUE)
data2 <- aggregate(aantalFactuurregels ~ ., data = data2, FUN=sum, na.rm=TRUE)

rpivotTable(data = data2,
            rows = c("Lvl01"),
            cols = "cluster_15",
#           aggregatorName = ,
            vals = "aantalFactuurregels",
            rendererName = "Table")


rpivotTable(data = data2,
            rows = c("Salesgroup"),
            cols = "cluster_15",
            #           aggregatorName = ,
            vals = "aantalFactuurregels",
            rendererName = "Table")




qplot(data2$Cluster)
scatter.1 <- ggplot(data2, aes(x = data2$Cluster, y = data2$Orderlines))
colors <- c('red','orange','green3','deepskyblue','blue','darkorchid4','violet','pink1','tan3','black', 'darkgreen')
scatter.1 <- scatter.1 + geom_point(aes(colour = data2$Lvl01))
mid <- mean(data2$Lvl01)
#data2$Lvl01 <- as.factor(data2$Lvl01)
scatter.1 + scale_color_gradient2(midpoint = mid, low = muted ("blue"), mid ="white", 
                                  high = muted ("red"), space ="Lab", guide = "colourbar")
print(scatter.1)



#scatter.2<-ggplot(data, aes(x=Cluster, y=Quantity, color=as.factor(Lvl01))) + geom_point() +
#scale_fill_manual(values=c('orange','green3','deepskyblue','blue','darkorchid4','violet','pink1',
#                           'tan3','black', 'darkgreen', 'purple','brown', 'darkred', 'skyblue',
#                           'limegreen', 'yellow'))
#print(scatter.2)

# Purchase List revenue
#clusters <- subset(data, select=c("CustomerId", "Cluster", "Revenue", "Bestellijst Revenue""))
#clusters$frequency <- "1"
#clusters$frequency <- as.integer(clusters$frequency)
#clusters <- unique(setDT(clusters)[order(clusters$Cluster)], by = "CustomerId")
#clusters <- aggregate(frequency ~ Cluster, data=clusters, FUN=sum, na.rm=TRUE, sort=TRUE)
#print(clusters)

