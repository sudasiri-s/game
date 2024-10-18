## install.packages("crayon")
library(crayon)

play_game <- function() {
  
  rounds <- 9
  sum_score <- 0
  your_score <- 0
  com_score <- 0
  options <- c("hammer", "scissor", "paper")
  greeting_option <- c("Bravo!", "Yehh!", "Hurray!")
  sad_option <- c("Oh, no!", "Oops!", "OUCH!")
  
  # Helper function to get result (win, lose, or tie)
  determine_winner <- function(user_select, computer_select) {
    if (user_select == computer_select) {
      return("tie")
    } else if ((user_select == "hammer" & computer_select == "scissor") |
               (user_select == "scissor" & computer_select == "paper") |
               (user_select == "paper" & computer_select == "hammer")) {
      return("win")
    } else {
      return("lose")
    }
  }
  
  # Play the game for a set number of rounds
  for (round in 1:rounds) {
    
    cat(yellow(paste("\nROUND", round,"!\n")))
    user_select <- ""
    
    # Input validation loop ตรวจสอบว่าค่าที่ผู้เล่นเลือก (user_select) อยู่ในตัวเลือกที่ถูกต้องหรือไม่
    while (!user_select %in% options) {
      cat(yellow("Choose one (hammer, scissor, paper): "))
      user_select <- tolower(trimws(readLines("stdin", 1)))  # Sanitize input
      if (!user_select %in% options) {
        cat(red("Invalid input. Please choose from hammer, scissor, paper.\n"))
      }
    }
    
    computer_select <- sample(options, 1)
    
    cat(paste("Your option :", user_select), "\n")
    cat(paste("Computer option:", computer_select), "\n")
    
    result <- determine_winner(user_select, computer_select)
    
    # Handle different results (win, lose, tie)
    if (result == "win") {
      greeting <- sample(greeting_option, 1)
      cat(green(paste(greeting, "You get the point!\n")))
      your_score <- your_score + 1
    } else if (result == "lose") {
      sad <- sample(sad_option, 1)
      cat(red(paste(sad, "Computer gets the point.\n")))
      com_score <- com_score + 1
    } else {
      cat(blue("It's a tie! No points awarded.\n"))
    }
    
    # Display current score
    cat(paste("Your Score:", your_score), "\n")
    cat(paste("Computer Score:", com_score), "\n")
  }
  
  # Display the final result
  cat("\nGame Over!\n")
  if (your_score > com_score) {
    cat(green("CONGRATULATIONS!! YOU WIN!"))
  } else if (your_score < com_score) {
    cat(red("SORRY, YOU LOSE."))
  } else {
    cat(blue("IT'S A DRAW!!"))
  }
}

play_game()
