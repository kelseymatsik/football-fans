source("run_epoch.R")

# This is our top level function. This is the function we will call when we want
# to compute an expected points for a certain "state" of the game.  For now,
# the states are: down, ytg, and fp.

get_EP <- function(down, ytg, fp) {
  # create a vector to store points from our simulation.  For now, we will run
  # 1000.  This is something that a user might want to control, so later we 
  # will allow it to be passed in as an argument.
  
  n <- 1000 # Always try to avoid "hard coding" by putting numbers that _could_
  # change into the "working part" of the code, especially if that
  # number will need to appear more than once, which it will here.
  # Otherwise, you have to make sure you change it everywhere in your
  # code if you want to change it.  By defining it here, we need only
  # change it once.
  
  # create a vector to store our simulations
  points <- rep(NA, n)
  
  # for now we can just use a for loop to actually run the simulations.  There
  # are more efficient ways to do this, but we aren't expecting simulations to 
  # take _that_ long.
  
  for(i in 1:n) {
    points[i] <- run_epoch(down, ytg, fp)
  }
  
  # And now we return the mean!  You should avoid "return" statements.  They 
  # require a bunch of overhead and are very inefficient.  Instead, just don't
  # save the last thing you do to a variable.
  
  mean(points)
  
}

################################################################################
############################# Test that get_EP works ###########################
################################################################################

# We don't really need to test this at this point.  Remember we are really
# working on the run_epoch code in the main branch.  If you want to test the
# get_EP function, you should really go to the "working_get_EP_function" branch.
get_EP(2, 5, 34)
get_EP(1, 10, 61)

# This is the run_drive function.  For now, our only goal is to make sure that
# run_epoch is working, so we don't really need anything like a realistic
# run_drive function.  We just need one that will give us outcomes we can
# play with.  The run_drive function will output a state.

run_drive <- function(down, ytg, fp) {
  # the end of a drive will either result in a score or a 1st and 10 for the
  # other team.  We will convey a score through fp (100 < fp <= 110, touchdown; 
  # 110 < fp <= 120, field goal).  If a score happens, down and ytg are
  # irrelevant, so it does not hurt us to still set thouse to 1st and 10.
  
  # so for now, to check our run_epoch function, we really just need to sample
  # a field position.  It doesn't even matter what they are, we just have to
  # sample something that will result in a new drive being run, something that
  # will result in a touchdown, and something that will result in a field goal.
  
  # sample new field position.  Because this is only temporary, I'm not worried
  # about hardcoding here.  We will sample a fp that results in a new drive with
  # probability .9, a touchdown with probability .05, and a field goal with 
  # probability .05
  
  # These probabilities aren't reasonable, but they will produce a variety of
  # outputs so that you can check that your run_epoch function works in a
  # reasonable way.  You can change the probabilities to "force" certain
  # outcomes to check something specific.
  
  new_fp <- sample(c(80, 105, 115), 1, prob=c(.9, .05, .05))
  
  # and we return the new state, as a list (see branch state, where the state
  # is always a list!)
  
  list(down=1, ytg=10, fp=new_fp)
}

source("run_drive.R")
source("utils.R")

# Now we will focus on the epoch function.  Once we are done, we can more or
# less leave it alone unless there are some project-specific changes we need to
# make.  

run_epoch <- function(down, ytg, fp) {
  # We need to do 3 things: run drives, track which team is on offense during
  # the drive, and determine if there was a score or not.  What is getting fed 
  # into the run_epoch function is the "state" we are interested in calculating
  # an EP for, so that team is the "original offense."  If that team scores,
  # the score for the epoch should be positive.  If the other team scores, the
  # score for the epoch should be negative.  This suggests we can keep track of 
  # the team in possession of the ball with a status of 1 ("original offense") 
  # and -1 (opponent). Then, we can just multiply the team status and the score
  # (when there is one) to obtain our output from this function.
  
  # initialize team status to -1.  Why not 1, since the "original offense" runs
  # the first drive?  The while loop will always have to toggle the team in 
  # possession, regardless of if there was a score or not (I suppose it doesn't
  # _have_ to, but that is overly complicated).  So we will set it to -1 and 
  # then immediately switch it when we enter the while loop.
  team_status <- -1
  
  # we can use a while loop to run our epoch.  In general, I like to use
  # flags when things are not as straightforward, and not check conditions 
  # as a part of the while loop.  This is a little tidyer.  So instead of
  # having the while loop check if the output from the drive is a score,
  # I will set a flag has_scored and monitor it.  We want to upper bound the 
  # number of iterations we are allowing our while loop to run, otherwise it
  # could run forever (in theory, but if we set everything up right this would
  # be highly unlucky!).  So for now we will restrict our while loop to run
  # a maximum of ten drives
  
  # set max number of drives
  max_drives <- 10 # again, avoid hardcoding
  
  # set drive counter
  cumulative_drives <- 0 # based on the code below, it's a little less 
  # cumbersome to index starting at 0.  Main thing is to
  # make sure your indexing and your verification yield
  # a maximum of max_drives.  We can interpret this 
  # counter as "the number of drives that have already 
  # been run."
  
  # initialize flag
  no_score <- TRUE 
  
  # Quick reminder when reading the printed output that the starting state 
  # should be the first state.
  
  print("Remember, the state of the first drive should be the input state.")
  
  # run loop
  while(no_score & (cumulative_drives < max_drives)) {
    
    # flip the team status immediately!  The first iteration of the while loop
    # will set team_status to 1, which is the "original offense."  If we kick
    # out of the while loop after just one drive, then the score multiplier
    # will be 1, which is exactly what we want.
    team_status <- team_status * -1
    
    # for similar reasons, we can go ahead and update the cumulative drives now.
    # This only serves to make our print statments nice...
    
    # increment the run drive
    cumulative_drives <- cumulative_drives + 1
    
    
    # for testing purposes, we may want to track the state of the "game" as it
    # progresses, so we will print here to show that we are running a drive, 
    # and the state of the game.  Since our "run_drive" function has only one 
    # value for fp that will instigate another call to the function, we should
    # always see fp=80, except for the first printout, which will be the state
    # we are deriving an EP for.  If we see anything else, we are not exiting 
    # the while loop properly!
    
    print(paste0("starting down: ", down, ", ytg: ", ytg, ", fp: ", fp, 
                 ", drive number: ", cumulative_drives,
                 ", team status flag: ", team_status,"."))
    
    # run drive
    tmp_state <- run_drive(down, ytg, fp)
    
    # reassign variables (look at branch "state" in the directory to see a 
    # better (in my opinion) way to do this)
    down <- tmp_state$down
    ytg <- tmp_state$ytg
    fp <- tmp_state$fp
    
    # aside: why don't I just have run_drive return just the fp?  1) I like to 
    # keep the state variables together, and 2) I don't want the run_epoch
    # function to do anything that doesn't fit its story.  1st and 10 is a
    # consequence of the end of a drive, not a part of the management of an 
    # epoch, at least in my opinion.
    
    # flip the score flag if there was a score
    no_score <- (fp <= 100)
    
    
    
    
    # That's it for the while loop!  If cumulative_drives < 10 and no_score is
    # true, the while loop will just run again, this time with the updated
    # downs, ytg, and fp settings.  Otherwise, it will exit.
  }
  
  # Now we just need to compute the points from the field position once the 
  # while loop has exited.  I decided to create a helper function to do this to
  # keep the code uncluttered (my documentation is already clutter enough!).
  # I've called it compute_score() and put it in a utils file.
  
  score <- team_status * compute_score(fp)
  
  # after we compute the score, I'd like to print out the scoring fp (0 
  # indicates loop stopped before a score), as well as the score.  
  # This allows me to check that it is scoring properly.
  
  print(paste0("final fp: ", fp, 
               ", scoring drive number: ", cumulative_drives,
               ", team status flag: ", team_status, ", score: ", score, "."))
  
  # and then finally return the score for the get_EP function to use.
  
  score
  
}


################################################################################
############################# Test that run_epoch works ########################
################################################################################

run_epoch(2, 7, 34) # To check different aspects of the code, you can modify
# the probabilities in the run_drive function to "force"
# certain outcomes to be more probable.

# helper function to compute the score.  Takes field position.

compute_score <- function(fp){
  
  # order here is important.  If the first condition is satisfied, it does not
  # look at the other statements.  So the only time it looks at the other
  # statements is if fp > 100, so we need not check any more if fp > 100!  If 
  # the second condition is not satisfied, then we know that the fp must be >110
  # and by construction of our output, the only thing it can be is a field goal.
  
  if(fp <= 100) {
    0
  } else if (fp <= 110) {
    7
  } else {
    3
  }
  
}
