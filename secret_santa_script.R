###########
## Setup ##
###########

# import the libraries
library(dplyr)
library(RMySQL)
library(reshape2)
library(clue)

# connect to the MySQL database
con_mysql <- src_mysql(dbname = "noel")

#################
## Data import ##
#################

# extract the relevant data from MySQL
choices <- con_mysql %>% 
  tbl(sql("SELECT user.id AS Id
          , user.email AS Name
          , undesirable.email AS Veto
          FROM user
          LEFT JOIN undesirable
          ON user.id = undesirable.user_id
          ORDER BY user.id")) %>% 
  collect()

######################
## Data preparation ##
######################

# create a squared matrix with all the registered participants
size_matrix <- length(unique(choices$Name))
adj_matrix <- matrix(ncol = size_matrix, nrow = size_matrix)
# and format it
colnames(adj_matrix) <- rownames(adj_matrix) <- sample(unique(choices$Name))
# and populate it with default values
diag(adj_matrix) <- 1
adj_matrix[is.na(adj_matrix)] <- 0

# create dataset contaning only the vetos
vetos <- choices %>% 
  filter(!is.na(Veto))

# place a 1 at the [gifter,giftee] coordinates in the matrix if a gifter vetoes a giftee
for(i in 1:nrow(vetos)){

  choice_subs <- vetos[i,]
  try(adj_matrix[choice_subs$Name,choice_subs$Veto] <- 1, silent = TRUE)

}

###############
## Algorithm ##
###############

# optimal assignment of column to rows by minimizing the sum of assigned costs
# the matrix obs with 1 are strongly penalized (ie, only selected if no other choice is possible)
# the method aims to minimize the sum of the costs of all the assignments
res <- solve_LSAP(adj_matrix, maximum = FALSE)

# create the final dataset
assignation_secret_santa<- data.frame(gifter = colnames(adj_matrix)[seq_along(res)], giftee = colnames(adj_matrix)[res])

#################
## Data export ##
#################

# write the dataset as a csv at //path//
write.csv2(assignation_secret_santa, file = "assignation_secret_santa.csv", row.names = FALSE)
