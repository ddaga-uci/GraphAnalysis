library(igraph)
library(plyr)
library(dplyr)
library(sqldf)
library(readxl)

# import data
products <- read.csv("products.csv", header=T)
copurchase <- read.csv("copurchase.csv", header=T)
attach(products)
attach(copurchase)

# 1. Delete products that are not books from “products” and “copurchase” files
products <- products %>% filter(group=='Book') %>% filter(salesrank<=150000) %>%
        filter(salesrank>-1)
copurchase <- copurchase %>% filter(Source %in% products$id) %>% 
        filter(Target %in% products$id)

# create copurchase network
conet <- graph.data.frame(copurchase, directed=T)
V(conet)
E(conet)

# 2. show how many “Source” products people who buy “Target” products buy
in_degree <- degree(conet, mode='in')

# 3. show how many “Target” products people who buy “Source” product also buy
out_degree <- degree(conet, mode='out')

# 4.Pick up one of the products (in case there are multiple) with highest 
# degree (in-degree + out-degree), and find its subcomponent
all_degree <- degree(conet, mode='all')
highest <- max(all_degree) 
all_degree[all_degree==highest] # 4429, 33

incident(conet, '4429', mode='all')
incident(conet, '33', mode='all')


#5.Visualize the subcomponent using iGraph
net <- graph.data.frame(d=copurchase, vertices=products, directed=T)
subnet <- subcomponent(net, '33', mode='all')
graph <- induced.subgraph(net, subnet)

V(graph)$label <- V(graph)$name
V(graph)$degree <- degree(graph)

# Kamada graph
# visualize the graph
set.seed(1)
plot(graph,
     vertex.color=rainbow(52),
     vertex.size=V(graph)$degree*0.8,
     vertex.label.color='black',
     vertex.label.cex=0.4,
     vertex.label=NA,
     edge.arrow.size=0.06,
     layout=layout.kamada.kawai)
# find the diameter
diameter(graph, directed = T)
dia <- get.diameter(graph, directed=T, weights=NA)
dia
as.vector(dia)

library(grDevices)
adjustcolor( "gray40", alpha.f = 0.1)
vcol <- rep("#6666661A", vcount(graph))
vcol[dia] <- "gold"
ecol <- rep("gray80", ecount(graph))
ecol[E(graph, path=dia)] <- "orange" 
# E(graph, path=diam) finds edges along a path, here 'dia'
set.seed(1)
plot(graph,
     vertex.color=vcol,
     vertex.size=3,     
     vertex.label.color='black',
     vertex.label.cex=0.02,
     #vertex.label=NA,
     edge.color=ecol,
     edge.arrow.size=0.06)
diagraph <- induced.subgraph(graph, dia)
plot(diagraph,
     vertex.label.color='black',
     vertex.label.cex=0.5,
     edge.arrow.size=0.2)

# drl graph
set.seed(1)
plot(graph,
     #vertex.color=rainbow(52),
     vertex.size=V(graph)$degree*0.8,
     edge.arrow.size=0.08,
     #vertex.label.cex=0.8,
     vertex.label=NA,
     layout=layout.drl)
# find the diameter
#diameter(graph, directed = T)
#dia

#vcol <- rep("gray40", vcount(graph))
#vcol[dia] <- "gold"
#ecol <- rep("gray80", ecount(graph))
#ecol[E(graph, path=dia)] <- "orange" 
# E(graph, path=diam) finds edges along a path, here 'dia'
#plot(graph, vertex.color=vcol, 
     #vertex.label.color='black',
     #vertex.label.cex=0.4,
     #vertex.label=NA,
     #edge.color=ecol,
     #edge.arrow.size=0.06,)

# 6. Compute various statistics about this network (i.e., subcomponent)
# including degree distribution, density, and centrality (degree centrality, closeness centrality and between centrality)
# hub/authority scores

# degree distribution
deg <- degree(graph, mode="all")
hist(deg, main="Histogram of node degree")

# density
edge_density(graph, loops=F)

# Reciprocity
# The proportion of reciprocated ties (for a directed network).
reciprocity(graph)

# Closeness (centrality based on distance to others in the graph)
# Inverse of the node's average geodesic distance to others in the network
closeness <- closeness(graph, mode="all", weights=NA) 

# Betweenness (centrality based on a broker position connecting others)
# (Number of geodesics that pass through the node or the edge)
betweenness <- betweenness(graph, directed=T, weights=NA)
edge_betweenness(graph, directed=T, weights=NA)

# hub/authority scores
hub_score <- hub.score(graph)$vector
authority_score <- authority.score(graph)$vector

par(mfrow=c(1,2))
set.seed(123)
plot(graph,
     vertex.size=hub_score*30,
     main = 'Hubs',
     vertex.color = rainbow(52),
     vertex.label=NA,
     edge.arrow.size=0.03,
     layout = layout.kamada.kawai)

set.seed(123)
plot(graph,
     vertex.size=authority_score*30,
     main = 'Authorities',
     vertex.color = rainbow(52),
     vertex.label=NA,
     edge.arrow.size=0.03,
     layout = layout.kamada.kawai)
par(mfrow=c(1,1))


# 7.
# find means
rating<-copurchase %>%
        group_by(Target) %>%
        inner_join(products, by=c('Source'='id'))%>%
        transmute(nghb_mn_rating=mean(rating))
rating <-as.data.frame(rating)

rank<-copurchase %>%
        group_by(Target) %>%
        inner_join(products,by=c('Source'='id'))%>%
        transmute(nghb_mn_salesrank=mean(salesrank))
rank <- as.data.frame(rank)

reviews<-copurchase %>%
        group_by(Target) %>%
        inner_join(products,by=c('Source'='id'))%>%
        transmute(nghb_mn_review_cnt=mean(review_cnt))
reviews <- as.data.frame(reviews)

# match id in product with id in subcomponent (subnet)
# convert subnet to dataframe
subnet1 <- as_ids(subnet)

subnet1 <- products[products$id %in% subnet1,]
combined <- copurchase %>% 
        group_by(Target) %>% 
        inner_join(subnet1, by = c('Source' = 'id'))

nghb_mn_rating <- mean(combined$rating) #3.885337
nghb_mn_salesrank <- mean(combined$salesrank) #73975.71
nghb_mn_review_cnt <- mean(combined$review_cnt) #27.06479

# make a dataframe of means



# match id with copurchase target


#colnames(copurchase) <- c("Source", "id")
#combined <- subnet1 %>%
#  filter(id %in% copurchase$Target)
#combined <- as.integer(combined$id)


#colnames(rating) <- c("id", "nghb_mn_rating")
#colnames(rank) <- c("id", "nghb_mn_salesrank")
#colnames(reviews) <- c("id", "nghb_mn_review_cnt")

#all_means <- join_all(list(combined, rating, rank, reviews), by='id',type = 'left')

#all <- copurchase %>%
# group_by(Target) %>%
#inner_join(combined, by = c('Source' = 'id'))


#rating <- mean(all_means$nghb_mn_rating)
#rank <- mean(all_means$nghb_mn_salesrank)
#reviews <- mean(all_means$nghb_mn_review_cnt)


# 8.
#convert to data frames
in_degree1 <- as.data.frame(in_degree)
in_degree1 <- cbind(col = rownames(in_degree1), in_degree1)
colnames(in_degree1) <- c("Nodes", "in_degree")

out_degree1 <- as.data.frame(out_degree)
out_degree1 <- cbind(col = rownames(out_degree1), out_degree1)
colnames(out_degree1) <- c("Nodes", "out_degree")

closeness1 <- as.data.frame(closeness)
closeness1 <- cbind(col = rownames(closeness1), closeness1)
colnames(closeness1) <- c("Nodes", "closeness")

betweenness1 <- as.data.frame(betweenness)
betweenness1 <- cbind(col = rownames(betweenness1), betweenness1)
colnames(betweenness1) <- c("Nodes", "betweenness")

hub_score1 <- as.data.frame(hub_score)
hub_score1 <- cbind(col = rownames(hub_score1), hub_score1)
colnames(hub_score1) <- c("Nodes", "hub_score")

authority_score1 <- as.data.frame(authority_score)
authority_score1 <- cbind(col = rownames(authority_score1), authority_score1)
colnames(authority_score1) <- c("Nodes", "authority_score")

# put neighbor's mean into 1 data frame by id 
combined <- copurchase %>% group_by(Target) %>% 
        inner_join(subnet1, by = c('Source' = 'id')) %>%
        summarise(nghb_mn_rating=mean(rating),
                  nghb_mn_salesrank=mean(salesrank),
                  nghb_mn_review_cnt=mean(review_cnt))

colnames(combined)[1] <- "Nodes"
colnames(products)[1] <- "Nodes"

# merge dataframes
x <- join_all(list(hub_score1, authority_score1, betweenness1, closeness1, 
                   in_degree1,out_degree1,products, combined), 
              by ="Nodes", type = "left")


## poisson regression
poisson = summary(salesrating_prediction<- glm(salesrank ~ review_cnt + rating + downloads+
                                                       in_degree+ out_degree+betweenness + hub_score+
                                                       authority_score + closeness + nghb_mn_rating + nghb_mn_salesrank + nghb_mn_review_cnt,
                                               family="poisson", data=x))
poisson

