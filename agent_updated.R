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