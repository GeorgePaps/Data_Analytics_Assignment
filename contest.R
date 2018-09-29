library(R6)
#erase
Agent <- R6Class("Agent",
                 
                 public = list(
                   bid = NULL,
                   book = NULL,
                   greeting = "H!",
                   id = NULL,
                   opponent_id = NULL,
                   round = NULL, 
                   response = NULL,
                   
                   set_book = function(book=NA) {
                     self$book <- book
                   },
                   
                   set_id = function(id=NA) {
                     self$id = id
                   },
                   
                   set_opponent_id = function(opponent_id=NA) {
                     self$opponent_id = opponent_id
                   },
                   
                   set_response = function(response=NA) {
                     self$ response <-response 
                   },
                   
                   set_round = function(round=NA) {
                     self$round <- round
                   },
                   
                   
                   get_bid = function() {
                     #print(self$response)
                     if(self$response == "Lemon!"){
                       self$bid = "defect"
                     } else {
                       
                       self$bid <- "defect"
                     }
                   }
                 )
)

Agenta <- R6Class("Agenta",
                 
                 public = list(
                   bid = NULL,
                   book = NULL,
                   greeting = "Hi!",
                   id = NULL,
                   opponent_id = NULL,
                   round = NULL, 
                   response = NULL,
                   
                   set_book = function(book=NA) {
                     self$book <- book
                   },
                   
                   set_id = function(id=NA) {
                     self$id = id
                   },
                   
                   set_opponent_id = function(opponent_id=NA) {
                     self$opponent_id = opponent_id
                   },
                   
                   set_response = function(response=NA) {
                     self$ response <-response 
                   },
                   
                   set_round = function(round=NA) {
                     self$round <- round
                   },
                   
                   
                   get_bid = function() {
                     if(self$response == "Lemon!"){
                       self$bid = "defect"
                     } else {
                       #bid_vector <- c("cooperate","defect","defect","defect")
                       #self$bid <- sample(bid_vector,1)
                       self$bid = "cooperate"
                     }
                   }
                 )
)

Agentb <- R6Class("Agentb",
                  
                  public = list(
                    bid = NULL,
                    book = NULL,
                    greeting = "Hii!",
                    id = NULL,
                    opponent_id = NULL,
                    round = NULL, 
                    response = NULL,
                    
                    set_book = function(book=NA) {
                      self$book <- book
                    },
                    
                    set_id = function(id=NA) {
                      self$id = id
                    },
                    
                    set_opponent_id = function(opponent_id=NA) {
                      self$opponent_id = opponent_id
                    },
                    
                    set_response = function(response=NA) {
                      self$ response <-response 
                    },
                    
                    set_round = function(round=NA) {
                      self$round <- round
                    },
                    
                    
                    get_bid = function() {
                      if(self$response == "Lemon!"){
                        self$bid = "defect"
                      } else {
                        #bid_vector <- c("cooperate","defect","defect","defect")
                        #self$bid <- sample(bid_vector,1)
                        self$bid = "cooperate"
                      }
                    }
                  )
)

Agentc <- R6Class("Agentc",
                  
                  public = list(
                    bid = NULL,
                    book = NULL,
                    greeting = "Hiiii!",
                    id = NULL,
                    opponent_id = NULL,
                    round = NULL, 
                    response = NULL,
                    
                    set_book = function(book=NA) {
                      self$book <- book
                    },
                    
                    set_id = function(id=NA) {
                      self$id = id
                    },
                    
                    set_opponent_id = function(opponent_id=NA) {
                      self$opponent_id = opponent_id
                    },
                    
                    set_response = function(response=NA) {
                      self$ response <-response 
                    },
                    
                    set_round = function(round=NA) {
                      self$round <- round
                    },
                    
                    
                    get_bid = function() {
                      if(self$response == "Lemon!"){
                        self$bid = "defect"
                      } else {
                        self$bid <- "cooperate"
                      }
                    }
                  )
)

#book = read.table("tournament.csv",header=TRUE,sep=",")
first = Agent$new()
first$set_id(1)
second = Agenta$new()
second$set_id(2)
third = Agentb$new()
second$set_id(3)
fourth = Agentc$new()
second$set_id(4)

agent_list = list()
agent_list[[1]] = first
agent_list[[2]] = second
agent_list[[3]] = third
agent_list[[4]] = fourth 


print(agent_list)
book = data.frame(id1=integer(),id2=integer(),round=integer(),bid1=character(),bid2=character(),points1=integer(),points2=integer())

#df = data.frame(bid1=character(),bid2=character(),payoff1=integer(),payoff2=integer())
x = c("cooperate","cooperate","defect","defect")
y = c("defect","cooperate","defect","cooperate")
z = c(0,4,2,5)
pame = c(5,4,2,0)
df = data.frame(bid1=x,bid2=y,payoff1=z,payoff2=pame)
print(df)
u = c(1,2,3,4)
iade = c(0,0,0,0)
payoffs = data.frame(payoff1=0,payoff2=0)
test_payoff = data.frame(id=u,pointss=iade)

test_payoff$pointss[2]


#The following code determines the matches, who is going to face whom 
# ===================================================================================
# each agent gets to play 1000 times
# matches are paired opportunities for agents to compete
matches <- c()

for (i in 1:1000) {
  
  # There will be an even number of agents, up to about 40
  # each agent has a unique id
  id_list<-1:4
  
  # shuffle the list in place
  shuffle <- sample(id_list)
  
  while (length(shuffle) > 0) {
    
    # get two agents off the list
    el1 <- shuffle[[1]]
    el2 <- shuffle[[2]]
    
    # remove two agents off the front of the list
    shuffle <- shuffle[-1]
    shuffle <- shuffle[-1]
    
    # contestants shows the pairing
    contestants <- c(el1,el2)
    
    # add your contest to the end of the match list
    matches[[length(matches)+1]] <- contestants
    
  }
}

#====================================================================================

# Run the Tournament
x <- length(matches)
#print(x)
#print(contestants)

# Contestants is a list of list
# The outer list is a list of contests
# The inner list is a list of two agents
payofft = data.frame(id=1:4,point=0)
payoff1 = 0 
payoff2 = 0
for (i in 1:x) {
  
  # find the first contest
  # get the list, not the slice
  contest <- matches[[i]]
  #print(contest)
  # get the ids
  id1 <- contest[[1]]
  id2 <- contest[[2]]

  # get the agents
  agent1 = agent_list[[id1]]
  agent2 = agent_list[[id2]]
  # set the agent ids
  #agent1$set_id(id1)
  #agent2$set_id(id2)
  
  # set the opponent number
  agent1$set_opponent_id(id2)
  agent2$set_opponent_id(id1)
  
  # get the greetings
  greeting1 = agent1$greeting
  greeting2 = agent2$greeting
  
  #send the greetings
  agent1$set_response(greeting2)
  agent2$set_response(greeting1)
  # set the round number
  agent1$set_round(i) 
  agent2$set_round(i)
  
  # get the bids
  agent1$get_bid()
  agent2$get_bid()
  
  mybid1 <- agent1$bid
  #print(mybid1)
  mybid2 <- agent2$bid
  #print(mybid2)
 
  
  # find the payoffs
  payoffs <-subset(df, bid1 == mybid1 & bid2 == mybid2)
  #print(payoffs)
  
  # record the transaction
  book = rbind(book,data.frame("id1"=id1,"id2"=id2,"round"= i,"bid1"=mybid1,"bid2"=mybid2,"points1"=payoffs$payoff1,"points2"=payoffs$payoff2))
  #print(payoffs$payoff1)

}
pointlist = data.frame(id=integer(),points=integer())

#print(book)

for(i in 1:4){
  list1 = book[book$id1 == i,c("points1")]
  list2 = book[book$id2 == i,c("points2")]
  pointlist = rbind(pointlist, data.frame("id"=i,"points"=sum(list1) + sum(list2)))
  if(i==1){
    maxi = pointlist$points[[i]]
    ind = i
  } else{
    if(maxi < pointlist$points[[i]]){
      maxi = pointlist$points[[i]]
      ind = i
    }
  }
}

print(book)
print(pointlist)
print(ind)
