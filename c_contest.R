library(R6)

#Below are the different types of agents

Lemon <- R6Class("Lemon",
                 
                 public = list(
                   bid = NULL,
                   book = NULL,
                   my_greeting = "Lemon!",
                   my_id = NULL,
                   opponent_id = NULL,
                   round = NULL, 
                   greeting_response = NULL,
                   
                   set_book = function(book=NA) {
                     self$book <- book
                   },
                   
                   set_id = function(id=NA) {
                     self$my_id = id
                   },
                   
                   set_opponent_id = function(opponent_id=NA) {
                     self$opponent_id = opponent_id
                   },
                   
                   set_response = function(response=NA) {
                     self$greeting_response <-response 
                   },
                   
                   set_round = function(round=NA) {
                     self$round <- round
                   },
                   
                   get_bid = function() {
                     self$bid <- "defect"
                   }
                 )
)

Cherry <- R6Class("Cherry",
                 
                 public = list(
                   bid = NULL,
                   book = NULL,
                   my_greeting = "Cherry!",
                   my_id = NULL,
                   opponent_id = NULL,
                   round = NULL, 
                   greeting_response = NULL,
                   
                   set_book = function(book=NA) {
                     self$book <- book
                   },
                   
                   set_id = function(id=NA) {
                     self$my_id = id
                   },
                   
                   set_opponent_id = function(opponent_id=NA) {
                     self$opponent_id = opponent_id
                   },
                   
                   set_response = function(response=NA) {
                     self$greeting_response <-response 
                   },
                   
                   set_round = function(round=NA) {
                     self$round <- round
                   },
                   
                   get_bid = function() {
                     if(self$greeting_response == "Lemon!"){
                       self$bid = "defect"
                     } else{
                       if(runif(1) > 0.3){
                         self$bid <- "cooperate"
                         } else{
                           self$bid = "defect"
                         }
                     }
                   }
                   )
)

Apple <- R6Class("Apple",
                  
                  public = list(
                    bid = NULL,
                    book = NULL,
                    my_greeting = "Apple!",
                    my_id = NULL,
                    opponent_id = NULL,
                    round = NULL, 
                    greeting_response = NULL,
                    
                    set_book = function(book=NA) {
                      self$book <- book
                    },
                    
                    set_id = function(id=NA) {
                      self$my_id = id
                    },
                    
                    set_opponent_id = function(opponent_id=NA) {
                      self$opponent_id = opponent_id
                    },
                    
                    set_response = function(response=NA) {
                      self$greeting_response <-response 
                    },
                    
                    set_round = function(round=NA) {
                      self$round <- round
                    },
                    
                    get_bid = function() {
                      if(self$greeting_response == "Lemon!"){
                        self$bid = "defect"
                      } else {
                        if(runif(1) > 0.7){
                          self$bid <- "cooperate"
                          } else{
                            self$bid = "defect"
                          } 
                      }
                    }
                  )
)

Peach <- R6Class("Peach",
                  
                  public = list(
                    bid = NULL,
                    book = NULL,
                    my_greeting = "Peach!",
                    my_id = NULL,
                    opponent_id = NULL,
                    round = NULL, 
                    greeting_response = NULL,
                    
                    set_book = function(book=NA) {
                      self$book <- book
                    },
                    
                    set_id = function(id=NA) {
                      self$my_id = id
                    },
                    
                    set_opponent_id = function(opponent_id=NA) {
                      self$opponent_id = opponent_id
                    },
                    
                    set_response = function(response=NA) {
                      self$greeting_response <-response 
                    },
                    
                    set_round = function(round=NA) {
                      self$round <- round
                    },
                    
                    get_bid = function() {
                      if(self$greeting_response == "Lemon!"){
                        self$bid = "defect"
                      } else{
                      bid_vector <- c("cooperate","defect")
                      self$bid <- sample(bid_vector,1)
                      }
                    }
                  )
)

Agent <- R6Class("Agent",
                 
                 public = list(
                   bid = NULL,
                   book = NULL,
                   my_greeting = "Bella Ciao!",
                   my_id = NULL,
                   opponent_id = NULL,
                   round = NULL, 
                   greeting_response = NULL,
                   
                   set_book = function(book=NA) {  # this function passes the tournament book to our agent
                     self$book <- book
                   },
                   
                   set_id = function(id=NA) {     # this function passes the id to our agent
                     self$my_id = id
                   },
                   
                   set_opponent_id = function(opponent_id=NA) { # this function passes our opponents id to our agent
                     self$opponent_id = opponent_id
                   },
                   
                   set_response = function(response=NA) {    # this function passes our opponents greeting to our agent
                     self$greeting_response <-response 
                   },
                   
                   set_round = function(round=NA) {          # this function passes the round number to our agent
                     self$round <- round
                   },
                   
                   get_bid = function() {                  # this function gives the our bid 
                     
                     # if we encounter fake agents (greeting with "Lemon!") we defect
                     if(self$greeting_response == "Lemon!"){
                       if (length(private$lemon_list) < 4){ # check if all 4 lemons have been found
                         private$lemon_ident()              # if not: call lemon_ident() to update lemon_list
                       }
                       self$bid = "defect"
                     } else {                             # if our opponent is not a lemon, implement our strategy
                       self$bid = private$our_strategy()  # our_strategy is the function containing our strategy 
                     }
                   }
                 ),
                 
                 # We use a private list to store our strategy and the related functions 
                 private = list(     
                   
                   lemon_list = c(),     # a vector that contains the ids of the lemons 
                   delemonized_book = NULL,   # this is a book that contains the history of matches that fake agents did not take part
                   
                   last_interact = function() {   #interaction between two agents self$id, self$opponent_id
                     t = self$book  
                     rec1 = t[(t$id1 == self$my_id & t$id2 == self$opponent_id), c("round","bid2")]  #rec1 is a vector that contains all the lines that (t$id1 == self$id & t$id2 == self$opponent_id) == TRUE, but has only the "round" and "bid2" columns
                     rec2 = t[(t$id1 == self$opponent_id & t$id2 == self$my_id), c("round","bid1")]
                     
                     names(rec1) = c("round","bid")   # change names of variables because otherwise there is problem when you will try to combine the two vectors in one
                     names(rec2) = c("round","bid")
                     all_rec = rbind(rec1,rec2)       # this vector contains all the encounters of agents self$id, self$opponent_id
                     
                     if (nrow(all_rec) > 0) {         # if the two agents have met in the past
                       round = all_rec$round          # a vector of all the rounds
                       n = max(round)                 # the largest number in this vector is the last time the two agents met
                       last = all_rec[all_rec$round == n,"bid"]
                     } else{                          # if the agents haven't met yet
                       last = NULL
                     }
                     return(last)
                   },
                   
                   
                   lemon_ident = function() {  # this function creates and updates the list that contains the lemon's IDs
                     
                     if( !(self$opponent_id %in% private$lemon_list) ){
                       private$lemon_list = c(private$lemon_list,self$opponent_id)
                     }
                     
                   },
                   
                   delemonize_book = function() {  # this function removes the matches that the fake agents(lemons) took part from the tournament book   
                     
                     t = self$book
                     private$delemonized_book = t[ !(t$id1 %in% private$lemon_list) & !(t$id2 %in% private$lemon_list),]
                     
                   },
                   
                   fraction_op = function(){  # this function returns the defection ratio of our opponents. 
                     
                     if( length(private$lemon_list) > 0 ){                   # if we have found all the lemons then "delemonize" the book
                       private$delemonize_book()
                       t = private$delemonized_book
                     } else{
                       t = self$book
                     }
                     
                     
                     rec1 = t[ t$id1 == self$opponent_id, c("round","bid1")]
                     rec2 = t[ t$id2 == self$opponent_id, c("round","bid2")]
                     names(rec1) = c("round","bid")
                     names(rec2) = c("round","bid")
                     rec = rbind(rec1,rec2)             
                     rec = rec[order(rec$round),]      # after pasting rec1 and rec2 into rec, we order rec (round wise) so that we can extract the most recent rounds
                     
                     if(nrow(rec) > 720){                           #if we are after the 720 round we keep only the recent history (after 700 round)
                       rec = rec[c(701:nrow(rec)),]                 # because we assume that some agents may change their behavior in the last rounds
                     }
                     
                     n_def = nrow(rec[rec$bid == "defect",])      # find the number of times that our opponent defected
                     tot_rounds = nrow(rec)                       # total number of round that our opponent played
                     def_frac = n_def/tot_rounds                  # calculate the defection fraction
                     
                     return(def_frac)
                     
                   },
                   
                   
                   our_strategy = function() {               # function containing our strategy
                     their_last = private$last_interact()    # find our opponent's previous move against us
                     if (is.null(their_last)) {               # if this is our first encounter with this opponent: cooperate
                       bid = "cooperate"
                     } else {
                       if( their_last == "defect"){            # if their last move with us was "defect":
                         if( private$fraction_op() < 0.35){    # then check if they are usually "friendly", which means their defection factor is low
                           bid = "cooperate"                   # reestablish cooperation: We give them a second chance if it is a "friendly" agent 
                         } else{                              # if it is not "friendly": defect
                           bid = "defect"                      
                         }
                       } else{                                 # if their last move with us was "cooperate"
                         bid = "cooperate"                     # then "cooperate" 
                       }        
                     }
                     
                     return(bid)
                   }
                 )
)

#this agent defects at the end
Agent_alt <- R6Class("Agent_alt",
                 
                 public = list(
                   bid = NULL,
                   book = NULL,
                   my_greeting = "Hi!",
                   my_id = NULL,
                   opponent_id = NULL,
                   round = NULL, 
                   greeting_response = NULL,
                   
                   set_book = function(book=NA) {
                     self$book <- book
                   },
                   
                   set_id = function(id=NA) {
                     self$my_id = id
                   },
                   
                   set_opponent_id = function(opponent_id=NA) {
                     self$opponent_id = opponent_id
                   },
                   
                   set_response = function(response=NA) {
                     self$greeting_response <-response 
                   },
                   
                   set_round = function(round=NA) {
                     self$round <- round
                   },
                   
                   get_bid = function() {
                     
                     #if we encounter fake agents, greeting with "Lemon!", we defect
                     if(self$greeting_response == "Lemon!"){
                       self$bid = "defect"
                     } else {
                       self$bid = private$tit_for_tat()
                     }
                   }
                 ),
                 
                 private = list(
  
                   
                   last_interact = function() {   #interaction between two agents self$id, self$opponent_id
                     t = self$book  
                     rec1 = t[(t$id1 == self$my_id & t$id2 == self$opponent_id), c("round","bid2")]  #rec1 is a vector that contains all the lines that (t$id1 == self$id & t$id2 == self$opponent_id) == TRUE, but has only the "round" and "bid2" columns
                     rec2 = t[(t$id1 == self$opponent_id & t$id2 == self$my_id), c("round","bid1")]
                     
                     names(rec1) = c("round","bid")   # change names of variables because otherwise there is problem when you will try to combine the two vectors in one
                     names(rec2) = c("round","bid")
                     all_rec = rbind(rec1,rec2)       # this vector contains all the encounters of agents self$id, self$opponent_id
                     
                     if (nrow(all_rec) > 0) {         # if the two agents have met in the past
                       round = all_rec$round          # a vector of all the rounds
                       n = max(round)                 # the largest number in this vector is the last time the two agents met
                       last = all_rec[all_rec$round == n,"bid"]
                     } else{                          # if the agents haven't met yet
                       last = NULL
                     }
                     return(last)
                   },
                   
                   end_is_near = function() {
                     t = self$book
                     
                     my_round = nrow(t[ (t$id1 == self$my_id & t$id2 == self$my_id), ])
                     if( my_round > 500){
                       return(TRUE)
                     } else {
                       return(FALSE)
                     }
                     
                   },
                  
                   
                   tit_for_tat = function() {
                     their_last = private$last_interact()
                     if (is.null(their_last)) {
                       bid = "cooperate"
                     } else {
                       if( private$end_is_near() == FALSE){
                         bid = their_last
                       } else{
                         bid = "defect"
                       }
                     }
                     return(bid)
                   }
                 )
)


#==============================================================================================================================================================

# create a list of agents
agent_list = list()

# create and initialize agents
agent_1 = Lemon$new()
agent_1$set_id(1)

agent_2 = Lemon$new()
agent_2$set_id(2)

agent_3 = Peach$new()
agent_3$set_id(3)

agent_4 = Apple$new()
agent_4$set_id(4)

agent_5 = Agent$new()
agent_5$set_id(5)

agent_6 = Agent$new()
agent_6$set_id(6)

agent_7 = Agent_alt$new()
agent_7$set_id(7)

agent_8 = Agent$new()
agent_8$set_id(8)

# put agents in a list
agent_list[[1]] = agent_1
agent_list[[2]] = agent_2
agent_list[[3]] = agent_3
agent_list[[4]] = agent_4
agent_list[[5]] = agent_5
agent_list[[6]] = agent_6
agent_list[[7]] = agent_7
agent_list[[8]] = agent_8
#=========================================================================================================================================================================


# create the payoff table
df = data.frame(bid1=c("cooperate","cooperate","defect","defect"),bid2=c("defect","cooperate","defect","cooperate"),payoff1= c(0,4,2,5),payoff2= c(5,4,2,0))

# create the tournament's book
book = data.frame(id1=integer(),id2=integer(),round=integer(),bid1=character(),bid2=character())

# create a ledger to keep the points of every agent, it should have the same number of "0" as the number of agents
ledger = c(0,0,0,0,0,0,0,0)

# create the schedule of the tournament, who is going to face whom in each round
matches <- c()

for (i in 1:800) {
  
  # There will be an even number of agents, up to about 40
  # each agent has a unique id, this should have the same number as the agents
  id_list<-1:8
  
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

# x is the total number of games that will be played
x <- length(matches)

# Run the tournament
for (i in 1:x) {
  
  contest = matches[[i]]
  
  # get the ids
  id1 <- contest[[1]]
  id2 <- contest[[2]]

  # get the agents
  agent1 = agent_list[[id1]]
  agent2 = agent_list[[id2]]
  
  # set the opponent number
  agent1$set_opponent_id(id2)
  agent2$set_opponent_id(id1)
  
  # get the greetings
  greeting1 = agent1$my_greeting
  greeting2 = agent2$my_greeting
  
  # send the greetings
  agent1$set_response(greeting2)
  agent2$set_response(greeting1)
  
  # set the round number
  agent1$set_round(i) 
  agent2$set_round(i)
  
  # pass the book to agents
  agent1$set_book(book)
  agent2$set_book(book)
  
  # get the bids
  agent1$get_bid()
  agent2$get_bid()
 
  # store the bid in a variable
  mybid1 <- agent1$bid
  mybid2 <- agent2$bid
  
  # find the payoffs
  payoffs <-subset(df, bid1 == mybid1 & bid2 == mybid2)
  
  # update ledger
  ledger[[id1]] = ledger[[id1]] + payoffs$payoff1
  ledger[[id2]] = ledger[[id2]] + payoffs$payoff2
  
  # update the book 
  book = rbind(book, data.frame("id1"=id1,"id2"=id2,"round"= i,"bid1"=mybid1,"bid2"=mybid2))
  
}

print(book)
print(ledger)
print(which.max(ledger))
