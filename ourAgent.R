library(R6)

Agent <- R6Class("Agent",
                 
                 public = list(
                   bid = NULL,
                   book = NULL,
                   greeting = "Hi!",
                   id = NULL,
                   opponent_id = NULL,
                   round = NULL, 
                   response = NULL,
	           clean_list = NULL, 
                   lemon_list = list(),
                   
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
                   
		    clean_book = function() {
                     t = self$book
                     id = self$lemon_list
                     cleaned_book = NULL
                     if (is.null(self$lemon_list)) {
                       return (self$book)
                     }
                     else {
                       cleaned_book = t[(t$id1 != id[0]) & (t$id2 != id[0]),]
                                         & (t$id1 != id[1]) & (t$id2 != id[1])
                                         & (t$id1 != id[2]) & (t$id2 != id[2])
                                         & (t$id1 != id[3]) & (t$id2 != id[3])]
               
                       #for (id in self$lemon_list) {
                         #cleaned_book = rbind(cleaned_book, t[(t$id1 != id) & (t$id1 != id),])
                       #}
                     return (cleaned_book)
                     }
                   },
			 
		   # function to test whether opponent uses tit-tat-strategy
                   # if yes, return TRUE
                   # else returns FALSE
                   # only use it after a minimum of rounds playes
	           is_tit_tat = function() {
                     t = self$book
                     
                     # loop to check all encounter of our opponent with other agents (opop)
                     for (id_opop in range(from=0, to=24)) {
                     
                     rec1 = t[(t$id1 == self$opponent_id) & (t$id1 == self$id_opop),]  # all rounds opponent played as id1 and other agent (opop)
                     rec2 = t[(t$id2 == self$opponent_id) & (t$id1 == self$id_opop),]   # same but opponent as id2
                     
                     names(rec1) = c("round", "op", "opop", "bid_op", "bid_opop")  # rec1 contains round no., opponent id, opponent's opponent's id, opponent's bid, and opponent's opponent's bid
                     names(rec2) = c("round", "opop", "op", "bid_opop", "bid_op")
                     rec2 = rec2[,c("round", "op", "opop", "bid_op", "bid_opop")]  # change order of coloumns so that it is the same as in rec1
                     all_rec = rbind(rec1,rec2)       # contains all encounters from opponent (op) with other agents- (opop)
                     
        
                     # check all encounters one by one, if op's next bid is equal to opop's previous bid
                       for (i in 1:length(all_rec)) {
                         if (all_rec[i+1,"bid_op"] == all_rec[i, "bid_opop"]) {
                           return (FALSE) 
                         }
                       }
                     }
         
                   get_bid = function() {
                     if(self$response == "Lemon!"){
                       self$bid = "defect"
		       self$lemon_list = c(self$lemon_list, self$opponent_id)
                     } else {
                       bid_vector <- c("cooperate","defect")
                       self$bid <- sample(bid_vector,1)
                     }
                     # if more than 700 rounds always "defect"
                     if (self$round > 700) {
                       self$bid = "defect"
                     }
                   }
                 ),
		 
		 private = list(		 
				fraction_op = function(){
					rec1 = self$book[(self$book)$id1 == self$opponent_id, c("round","bid1")]
					rec2 = self$book[self$book$id2 == self$opponent_id, c("round","bid2")]				
					names(rec1) = c("round","bid")
					names(rec2) = c("round","bid")
					rec = rbind(rec1,rec2)  
					if(nrow(rec) == 0){
						return(-1)  #if this is the first game of the opponent, then it returns -1
						} else{
						n_def = nrow(rec[rec$bid == "defect",])
						n_cop = nrow(rec[rec$bid == "cooperate",])
						tot_rounds = nrow(rec)                 
						def_frac = n_def/tot_rounds
						return(def_frac)
						}
				}
		 )
)

book = read.table("data/tournament.csv",header=TRUE,sep=",")
first = Agent$new()
first$response
first$set_round(2)
first$round
first$set_book(book)
first$set_response("Lemon!")
first$get_bid()
first$bid
first$set_id(5)

# test is_tit_tat_function
for (i in 1:24) {
  first$set_opponent_id(i)
  print(first$is_tit_tat())
}
