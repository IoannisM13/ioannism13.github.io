# Monty Hall Problem

```r
# Define function
monty_hall_sim <- function(change = TRUE) {
  
  # Define slots
  slots <- c(1:3)
  
  # Make 1st selection
  selection_1 <- sample(slots, 1)
  
  # Select winning slot 
  win_slot <- sample(slots, 1)
  
  # Non-winning slots
  non_win_slots <- setdiff(slots, win_slot)
  
  # Open non-winning slot that is also not players' selection
  open_lose_slot <- 
    if (win_slot == selection_1) {
      sample(non_win_slots, 1)
    } else {
      setdiff(non_win_slots, selection_1)
    }
  
  slots_remaining <- setdiff(slots, open_lose_slot)
  
  selection_2_final <- 
    if (change == TRUE) {
      setdiff(slots_remaining, selection_1)
    } else {
      selection_1
    }
  
  result <- 
    if_else(win_slot == selection_2_final, 1, 0)  
  
  return(result)

}

## Run simulation
### Set N trials 
N <- 100000

result <-  
  mean(
    replicate(
      N,
      monty_hall_sim(change = FALSE)
    )
  )

result
```

