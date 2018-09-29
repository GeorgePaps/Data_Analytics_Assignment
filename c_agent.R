#it includes lemon check, first encounter -> "cooperate" and tit for tat strategy
Agent <- R6Class("Agent",
                 
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
                   
                   tit_for_tat = function() {
                     their_last = private$last_interact()
                     if (is.null(their_last)) {
                       bid = "cooperate"
                     } else {
                       bid = their_last
                     }
                     return(bid)
                   }
                 )
)
