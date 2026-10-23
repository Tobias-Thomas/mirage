A session with valid commands. The prompt has no newline, so each output line
starts with it. In cram files an indented "> " would continue the command, so
the prompt is renamed with sed.

  $ printf 'load coin.mrg\nseed 1\nsample 100\nobserve c false\nunobserve c\nsample 10\n' | mirage | sed 's/> /mirage> /g'
  mirage> 	Loaded program
  mirage> 	Updated random seed
  mirage> 	Done sampling
  mirage> 	Replaced observation for c
  mirage> 	Removed observation for c
  mirage> 	Done sampling
  mirage> 

Errors are reported (on stderr) and the session continues.

  $ printf 'smaple 10\n\nsample 10\nload missing.mrg\nload coin.mrg\nsample 10\n' | mirage 2>&1 | sed 's/> /mirage> /g'
  mirage> smaple is not a known command
  mirage> The command should not be empty
  mirage> Can not sample without loading a program
  mirage> missing.mrg: No such file or directory
  mirage> 	Loaded program
  mirage> 	Done sampling
  mirage> 

End of input (Ctrl-D) ends the session with exit code 0.

  $ printf '' | mirage > /dev/null

quit ends the session; the commands after it are not run. A timed quit quits
too, and "quit" with arguments is an error.

  $ printf 'seed 1\nquit\nseed 2\n' | mirage | sed 's/> /mirage> /g'
  mirage> 	Updated random seed
  mirage> 

  $ printf 'time quit\nseed 2\n' | mirage | sed 's/> /mirage> /g'
  mirage> 

  $ printf 'quit now\nseed 2\n' | mirage 2>&1 | sed 's/> /mirage> /g'
  mirage> Quit can only stand alone
  mirage> 	Updated random seed
  mirage> 
