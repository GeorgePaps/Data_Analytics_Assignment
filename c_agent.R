library(R6)
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
                     } else {                                   #if the agent we encount is not fake
                       
                       #if we encounter an opponent for the first time, we cooperate
                       #find out if this is the first time we encounter an agent
                       print(self$book)
                       print(self$my_id)
                       print(self$opponent_id)
                       num_enc1 = nrow( self$book[( self$book$id1 == self$my_id & self$book$id2 == self$opponent_id),] )
                       num_enc2 = nrow( self$book[( self$book$id2 == self$my_id & self$book$id1 == self$opponent_id),] )
                       
                       if( num_enc1 + num_enc2 == 0){
                         first_enc = TRUE
                       } else {
                         first_enc = FALSE
                       }
                       
                       if( first_enc == TRUE){
                         self$bid = "cooperate"
                       } else{                                 #if this is not our first encounter
                         
                         self$bid <- "defect"
                         
                       }
                       
                     }
                     
                     
                     
                   }
                 )
)

