library(devtools)
library(movienetdata)
library(network)
library(sna)
library(igraph)
library(intergraph)
library(RColorBrewer)
library(GGally)
library(tidyverse)




#-----------------------------------------------------------------------
#PART A

#KLUBBDATA

#load klubbdata

klubbdata_nodes <- read.csv("klubbdata/klubbdata_nodes.csv")
klubbdata_edges <- read.csv("klubbdata/klubbdata_edges.csv")

klubbdata_igraph = graph_from_data_frame(d = klubbdata_edges, 
                                  vertices = klubbdata_nodes, 
                                  directed=FALSE)

klubbdata_net = network(klubbdata_edges,
                        klubbdata_nodes,
                        directed=FALSE)

#----------------------------------------------------------------------
# 1. Is the network directed or undirected?

summary(klubbdata_net)


#----------------------------------------------------------------------
# 2. How many actors and ties there are?

# 34 vertices
# 78 edges
#according to the summary


#----------------------------------------------------------------------
# 3. What is the density of the network?

# 0.1390374
#again, according to the summary


#----------------------------------------------------------------------
# 4. What is the average degree of the network?

mean(degree(klubbdata_igraph))/2


#----------------------------------------------------------------------
# 5. What is the average shortest path of the network?

mean_distance(klubbdata_igraph)

# 2.4082


#----------------------------------------------------------------------
# 6. Who are the 2 most central characters in this network?

klubbdata_betweenness <- betweenness(klubbdata_igraph)
klubbdata_names <- network.vertex.names(klubbdata_net)

klubbdata_bet_names <- bind_cols(klubbdata_betweenness, klubbdata_names)
 
# 1
# 34


#----------------------------------------------------------------------
# 7. Are there different communities in the network 
#    (use Louvain algorithm)? Describe and visualise the results. 
#    Report correlation with actual club membership.

klubb_comm <- cluster_louvain(klubbdata_igraph)
klubb_membership_num <- klubb_comm$membership
klubb_membership_fac <- as_factor(klubb_membership_num)

klubbdata_igraph_attr <- vertex.attributes(klubbdata_igraph)
klubb_faction_num <- klubbdata_igraph_attr$faction
klubb_faction_num <- as.numeric(klubb_faction_num)
klubb_faction_fac <- as_factor(klubb_faction_num)

cor.test(klubb_faction_num, klubb_membership_num, method = "spearman")

ggnet2(klubbdata_igraph,
       shape = klubb_membership_fac,
       shape.legend = "Community",
       color = klubb_faction_fac,
       color.legend = "Faction",
       label = TRUE,
       label.size = 3)


#----------------------------------------------------------------------
# 8. Perform clustering based on edge betweenness 
#    (use cut-off to 2 clusters). How well do the clustering results 
#    match with the actual club membership? Report correlation and 
#    create a network visualisation that shows both to which cluster 
#    and club actors belong to

klubb_cluster_ed_bet <- cluster_edge_betweenness(klubbdata_igraph)
klubb_2clusters_num <- cut_at(klubb_cluster_ed_bet, no = 2)
klubb_2clusters_fac <- as_factor(klubb_2clusters_num)

cor.test(klubb_faction_num, klubb_2clusters_num, method = "spearman")

ggnet2(klubbdata_net, 
       shape = klubb_2clusters_fac,
       shape.legend = "Cluster",
       color = klubb_faction_fac,
       color.legend = "Faction",
       label = TRUE,
       label.size = 3)

# link to the ggnet2 stuff, could be useful  
#https://briatte.github.io/ggnet/#node-legends















#STARWARS

#load data

Sys.setenv(R_REMOTES_STANDALONE="true")
remotes::install_github("pj398/movienetdata")

data("starwars_tfa", "movienetdata")
data(package = "movienetdata")
force(starwars_tfa)

# Notice that there are several objects and attributes available:
# starwars_tfa$event_list – we will not use this
# starwars_tfa$node_list
# starwars_tfa$adjacency
# Create network object from adjacency matrix

starwars_net <- network(starwars_tfa$adjacency)

#----------------------------------------------------------------------
# 9. Is the network directed or undirected?

summary(starwars_net)


#----------------------------------------------------------------------
# 10. How many actors and ties there are?

summary(starwars_net)

#31 vertices
#149 edges


#----------------------------------------------------------------------
# 11. What is the density of the network?

gden(starwars_net, mode="graph")
network.density(starwars_net)

#0.1602151, same for both


#----------------------------------------------------------------------
# 12. What is the average degree of the network?


mean(degree(starwars_igraph))



#----------------------------------------------------------------------
# 13. What is the average shortest path of the network?

starwars_igraph <- asIgraph(starwars_net)
mean_distance(starwars_igraph, directed = TRUE)

# 2.297619


#----------------------------------------------------------------------
# 14. Who are the 3 most important characters in this movie?

starwars_betweenness <- betweenness(starwars_igraph)
starwars_names <- network.vertex.names(starwars_net)

starwars_bet_names <- bind_cols(starwars_betweenness, starwars_names)

# according the betweenness centrality: 
#   1. Poe Dameron
#   2. Kylo Ren
#   3. Han Solo

starwars_in <- degree(starwars_igraph, mode = "in")

starwars_out <- degree(starwars_igraph, mode= "out")

starwars_in_out_names <- bind_cols(starwars_in, 
                                   starwars_out, 
                                   starwars_names)

# according to the degrees:
#   1. Poe Dameron
#   2. Han Solo
#   3. Finn

#----------------------------------------------------------------------
# 15. Who does Rey interact with in the movie?

network.vertex.names(starwars_net)
#Rey is number 7

get.neighborhood(starwars_net, 7)
# 3 Kylo Ren
# 5 Finn
# 14 Maz Kanata
# 10 Han Solo
# 6 Unkar


#----------------------------------------------------------------------
# 16. Visualise the network so that node size depends on some centrality 
#     measure and node colour corresponds to the sex of the character.

starwars_degree <- degree(starwars_igraph)

starwars_nodelist <- starwars_tfa$node_list
starwars_sex <- starwars_nodelist$char_female
starwars_sex <- as_factor(starwars_sex)

ggnet2(starwars_net,
       color = starwars_sex,
       color.legend = "Is Female",
       size = starwars_degree) +
  guides(size = FALSE)
       

#----------------------------------------------------------------------
# 17. Briefly compare the two networks to other real-life networks 
#     (use the examples from the last slide in Lecture 2)


















#----------------------------------------------------------------------
# PART B (10 points)



# Create a social network (e.g. friends, relatives, classmates etc.) 
# with at least 16 actors.

# empty graph
net <- graph.empty(directed = FALSE)

# vertices
net <- add_vertices(
  net, 
  18, 	
  name=c("Vendy", "Vojta",
         "Anet", "Kuba",
         "Tyna", "Tom", "Tonda",
         "Klarka", "Honza", "Majda",
         "Peta", "Luky",
         "Radka", "Tomas", "Stepa",
         "Sima",
         "Bernas", "Bety"))

# edges
net <- add_edges(
  net,
  c("Vendy", "Vojta",
    "Anet", "Kuba",
    "Tyna", "Tom",
    "Klarka", "Honza",
    "Peta", "Luky",
    "Radka", "Tomas",
    "Bernas", "Bety",
    "Vendy", "Radka",
    "Vendy", "Tomas",
    "Vendy", "Stepa",
    "Vojta", "Sima",
    "Tyna", "Tonda",
    "Tom", "Tonda",
    "Tom", "Peta",
    "Klarka", "Majda",
    "Honza", "Majda",
    "Radka", "Stepa",
    "Tomas", "Stepa",
    "Bety", "Vojta",
    "Bernas", "Vojta",
    "Vendy", "Bernas",
    "Majda", "Tonda",
    "Vendy", "Kuba",
    "Vendy", "Anet",
    "Tyna", "Kuba")
)

# 4. plot
ggnet2(net,
       label = TRUE)

summary(net)


# 18. Compare your network to a random network and small world network 
# of the same size (also set seed). 
# Provide a brief description of network, including a table 
# with the main descriptives and figures of degree distribution for 
# all 3 networks.

set.seed(6)
random <- erdos.renyi.game(18, 25, type = "gnm")

set.seed(6)
small_world <- watts.strogatz.game(1, 18, 2, 0.6, 
                                   loops = FALSE, 
                                   multiple = FALSE)

networks_table <- data.frame(Random_network = c(
  round(edge_density(random), 3),
  as.character(is.directed(random)),
  round(mean(degree(random)), 3),
  round(mean_distance(random, directed = FALSE), 3)),
  Small_world = c(
    round(edge_density(small_world), 3),
    as.character(is.directed(small_world)),
    round(mean(degree(small_world)), 3),
    round(mean_distance(small_world, directed = FALSE), 3)),
  My_network = c(
    round(edge_density(net), 3),
    as.character(is.directed(net)),
    round(mean(degree(net)), 3),
    round(mean_distance(net, directed = FALSE), 3)),
  row.names = c("Density", 
                "Directed", 
                "Mean degree",
                "Mean distance"))



hist(degree(random), breaks=4, xlab = "Degree", main = "Random network")

hist(degree(small_world), breaks=5, xlab = "Degree", main = "Small world")

hist(degree(net), breaks=5, xlab = "Degree", main = "My network")


# 19. Present a very basic visualisation of all 3 networks 
# (just to capture the basic structure of connections).

random_degree <- degree(random)

ggnet2(random,
       size = random_degree,
       label = TRUE,
       label.size = 3)  +
  guides(size = FALSE)

small_world_degree <- degree(small_world)

ggnet2(small_world,
       size = small_world_degree,
       label = TRUE,
       label.size = 3) +
  guides(size = FALSE)

net_degree <- degree(net)

ggnet2(net,
       size = net_degree,
       label = TRUE,
       label.size = 3) +
  guides(size = FALSE)


# 20. Create a list of top 5 members by 3 centrality measures for your network. 
# In your network:
#   a) Who you consider the key figure to contact for distributing 
#      information? Why?
#   b) Who should get vaccinated first to avoid quick spreading of contagious 
#      disease? Why?
#   21. Create a visualisation of your social network (use labels, colours etc).


net_net <- asNetwork(net)

net_betweenness <- betweenness(net)
net_names <- network.vertex.names(net_net)
net_bet_names <- bind_cols(net_betweenness, net_names)

net_closeness <- closeness(net)

net_degrees <- degree(net)
net_deg_names <- bind_cols(net_degrees, net_names)



net <- set.vertex.attribute(net, "gender", value=c("female", "male",
                                                    "female", "male",
                                                    "female", "male",
                                                    "male", "female",
                                                    "male", "female",
                                                    "female", "male",
                                                    "female", "male",
                                                    "male", "female",
                                                    "male", "female"))

net <- set.vertex.attribute(net, "leader", value = c("yes", "no",
                                                     "yes", "yes",
                                                     "yes", "yes",
                                                     "no", "yes",
                                                     "yes", "no",
                                                     "yes", "no",
                                                     "no", "no",
                                                     "yes", "no",
                                                     "no", "no"))

net <- set.edge.attribute(net, "status", value = c("dating",
                                                    "dating",
                                                    "married",
                                                    "married",
                                                    "dating",
                                                    "married",
                                                    "dating",
                                                    "related",
                                                    "related",
                                                    "related",
                                                    "related",
                                                    "related",
                                                    "related",
                                                    "related",
                                                    "related",
                                                    "related",
                                                    "related",
                                                    "related",
                                                    "friends",
                                                    "friends",
                                                    "friends",
                                                    "friends",
                                                    "friends",
                                                    "friends",
                                                    "friends"))

ggnet2(net,
       label = TRUE,
       label.size = 3,
       color = "gender",
       shape = "leader",
       edge.label = "status",
       edge.label.size = 2)




