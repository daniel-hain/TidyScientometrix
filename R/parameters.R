
#####################################################################################################
# Parameters
#####################################################################################################

################# Restrict ninimum degree in network
cutof.bib <- 0.10 # <- no cutoff
cutof.cit <- 0.10 # <- no cutoff

################# Restrict community size
res.bib <- 250 # Bibliographic coupling community
res.cit <- 1000 # Cocitation community 

################# Set the time periods
time <- c(1990, 2000, 2010, 2020) # Here we set the decades ect we want to look at
#periods <- length(time)
start <- 1980
end <- 2018

################# the number of topics
k <- 20

################# Diversity cutoff
div.cut <- 0.01

#####################################################################################################
# Descriptions
#####################################################################################################

################# Research Areas
com.names.bib <- c("1: Innovation Systems",
                   "2: Systainability Transitions",
                   "3: ",
                   "4: ",
                   "5: ",
                   "6: ",
                   "7: ",
                   "8: ")



################# Knowledge BAses

com.names.cit <- c("1: Classics",
                   "2: Transitions",
                   "3: Innovation Systems",
                   "4: Envirromental")

################# Topics

top.names<- c("1: ",
              "2: ",
              "3: ",
              "4: ",
              "5: ",
              "6: ",
              "7: ",
              "8: ",
              "9: ",
              "10: ")
        
  






