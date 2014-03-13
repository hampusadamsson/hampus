#include <stdio.h>	// fgets()
#include <stdlib.h>	// EXIT_FAILURE, EXIT_SUCCESS
#include <string.h>	// strcmp() 
#include <unistd.h>	// execvp(), fork(), getpid(), getppid(),
			// STDIN_FILENO, STDOUT_FILENO, STDERR_FILENO
#include <sys/wait.h>   // wait()
#include <signal.h>     // SIGPIPE

#include "parse.h"      // next_command(), enum cmd_pos, pos2str()


#define MAX_ARGV_SIZE 10
#define COMMAND_LINE_BUFFER_SIZE 1024



#define DO_DEBUG 0  // To enable   debug printouts set this to 1. 
		    // To dissable debug printouts set this to 0.


// Use the DBG macro for debug printouts. 

#if DO_DEBUG == 1
#define DBG(format, ...) fprintf(stderr, format, __VA_ARGS__);fflush(stderr)
#define DBG_ONLY(statement) statement

#else
#define DBG(format, ...) 
#define DBG_ONLY(statement)
#endif



void sigpipe_handler(int signum) {
  printf("ERROR: signal %d received!\n", signum);
  exit(EXIT_FAILURE);
}


void run_command(char* argv[]) {
  if (execvp(argv[0], argv) == -1) {
    perror("No such command");
    exit(EXIT_FAILURE);
  }
}


/**
 * DESCRIPTION: 
 *
 * Once a new process is created for a command, this function will
 * take care of actually running the command. To do this, STDIN and
 * STDOU may need to be redirected to write and read from one of the
 * pipes. Unused pipe descriptors must be closed.
 *
 * 
 * INPUT:
 *
 *    pos  the relative position of the command. This should be one
 *         of {single, first, middle, last}.
 *
 *    argv the argv for the command. 
 *
 *    left_pipe-read_fd
 *         To enable one command to be aware of the pipe "to the left" 
 *         the read descriptor of this pipe must be given as an argument 
 *         to this function. 
 *
 *    right_pipe
 *         A pair of descritors for the pipe "to the right" of this command. 
 *
 *
 * NOTE: 
 * 
 * You must add code at various places, see TODO comments.
 */
void child_command(enum cmd_pos pos, char* argv[], int left_pipe_read_fd, int right_pipe[]) {

  // TODO: Register a the sigpipe_hanlder signal handler in order to detect if we 
  //       make any misstakes and receives a SIGPIPE.
  signal(SIGPIPE,sigpipe_handler);

  switch(pos){ //{single, first, middle, last}
  case single: //stdin + stdout
    break; 
  case first://stdin + pipeout
    if (dup2(right_pipe[1], STDOUT_FILENO) == -1) {
      perror("dup2-1");
      exit(1);
    }
    close(left_pipe_read_fd);
    close(right_pipe[0]);
    break; 

  case middle: //pipein + pipeout
    if (dup2(left_pipe_read_fd, STDIN_FILENO) == -1){
    perror("dup2-2");
    exit(2);
    }
    if (dup2(right_pipe[1], STDOUT_FILENO) == -1){
      perror("dup2-se1");
      exit(11);
    }
    close(right_pipe[0]);
    break;
  case last: //pipein + stdout
    if (dup2(left_pipe_read_fd, STDIN_FILENO)== -1){
      perror("dup2-se");
      exit(1);
    }
    close(right_pipe[1]);
    break; 
  default:
    perror("error");
    exit(EXIT_FAILURE);
    break;}

  
  
  DBG("CHILD  <%ld> %s left_pipe_read_fd = %d, right_pipe_read_fd = %d, right_pipe_write_fd = %d\n", 
      (long) getpid(), argv[0], left_pipe_read_fd, right_pipe[0], right_pipe[1]);
  
  // REMEMBER: The child inherits all open file descriptors from the parent. 
  
  // TODO: Close descritors if necessary. Redirect STDIN and STDOUT if necessary. 
  


  
  // Now we're ready to run the command. 
  run_command(argv);
}


/**
 * DESCRIPTION: 
 *
 * Command lines read from the user have the following generic format: 
 * 
 *	cmd0 | cmd1 | cmd2 | ... | cmdN
 * 
 * , i.e., a serie of command connected with pipes. In this sequence,
 * the command have the following relatve positions:
 *
 *	cmd0 - first
 *	cmd1 - middle
 *	cmd2 - middle
 *	cmdN - last
 *
 * If the command line only have a single command as in this example: 
 * 
 *	cmd 
 *
 * , the position of this lonely command is defined as single.
 *   
 * To communicate, the commands writes and read data to various
 * pipes. In the fist example above:
 * 
 *   cmd0 - must redirect STDOUT to write to the pipe 
 *          "to the right" of cmd0.
 *   cmd1 - must redirect STDID  to read read from the pipe 
 *          "to the left" of cmd1. 
 *	    must redirect STDOUT to write to the pipe 
 *          "to the right" of cmd1.
 *    .
 *    .
 *    .
 *   cmdN - must redirct STDID  to read read from the pipe 
 *          "to the left" of cmdN. 
 * 
 * This function must create pipes and rediret STDIN and STDOUT as
 * described above and unused pipe descriptors must be closed. Once
 * this is done, a new process for the command can be created.
 *
 * 
 * INPUT:
 *
 *    pos  the relative position of the command. This should be one
 *         of {single, first, middle, last}.
 *
 *    argv the argv for the command. 
 *
 *    left_pipe-read_fd
 *         To enable one command to be aware of the pipe "to the left" 
 *         the read descriptor of this pipe must be given as an argument 
 *         to this function. 
 *
 *
 * OUTPUT: 
 *
 *    The read descripor of the newly created pipe (right pipe) is
 *    returned. This descriptor will then become the read descriptor
 *    to the left pipe for the next command.
 *
 *
 * NOTE: 
 * 
 * You must add code at various places, see TODO comments.
 */
int pipe_and_fork(enum cmd_pos pos, char* argv[], int left_pipe_read_fd) {
  
  // A pair of file descriptors used for the pipe. 
  
  int new_pipe[2];  

  
  DBG("pos = %s\n", pos2str(pos));
  
  // TODO: The pipe must be created before the fork(), but only create
  //       a pipe if needed (use pos to make decision).
  
  if(pos==first){
    if (pipe(new_pipe)==-1){
      perror("er-pipe");
      exit(1);
      //errror
    }
  }else if(pos==middle){
    if (pipe(new_pipe)==-1){
      //error
      perror("ex2-pipe");
      exit(2);
    }
  }
    
  // Once the pipe is created we can fork a child process for the command. 
  
  pid_t pid = fork();
  
  switch(pid) {
  
  case 0: // == CHILD ==
    
    DBG("CHILD  <%ld> %s says HELLO!\n", (long) getpid(), argv[0]);
    
    // 
    child_command(pos, argv, left_pipe_read_fd, new_pipe);
    
    // This will never be reached when you have sucessfully managed to call execv().
    exit(EXIT_FAILURE);
    
  case -1: // == ERROR ==
    
    perror("======= ERROR ====> Forking child process failed.");
    exit(EXIT_FAILURE);
    
  default: // == PARENT ==
    
    DBG("PARENT <%llu> just forked child <%llu> %s\n", (long long unsigned)getpid(), (long long unsigned)pid, argv[0]);
    
    // TODO: Close descriptors if necessary. 
      
    close(new_pipe[1]);
    
    // Return the read descriptor of the newly created pipe. This
    // descriptor is needed by the next child. This is the left_pipe_read_fd 
    // for the next command. 

    return new_pipe[0]; 
  } // end-switch(pid)
}




/**
 * DESCRIPTION: 
 *
 * Prints a promt to the screen. Reads a command line of the following format from the  the user:
 * 
 *	cmd0 -a00 -b01 ... -z0n | cmd1 | cmd2 | ... | cmdN -an0 ... -znn
 *
 * For each command, the fork_and_pipe() function is called with the
 * appropiate argv and relative position of the command.
 * 
 * NOTE: 
 * 
 * You only have to add code at the end of main, see TODO comment. 
 */
int main() {
  
  // Allocate a buffer for the command line read from the user. 

  char line_buffer[COMMAND_LINE_BUFFER_SIZE];
  
  
  // We parse the command line using the next_command() parser. The
  // parser will populate the following array with the result of each call
  // to the parse.
  
  char* argv[MAX_ARGV_SIZE];  
    
  // Count the number of non empty command lines.
  
  int line_nr = 0;
 
  
  // Position of each command in a command line.
  
  enum cmd_pos pos;  // {single, first, middle, last}
  
  
  // Pipe read descriptor to the pipe to the "left".
  
  int left_pipe_read_fd = -1;

  // Count the number of children forked for each command line. 

  int children = 0; 
  

  while(1) {

    do {
      
      // Print the command prompt including the command line counter. 
      printf(" %d> ", line_nr);
      fflush(NULL);
      
      
      // Read input from the user. 
      if ( fgets(line_buffer, COMMAND_LINE_BUFFER_SIZE, stdin) == NULL) {
	perror("====== ERROR ====> READING COMMAND LINE FAILED :(");
	exit(EXIT_FAILURE);
      }

      // Exit if the user types "exit" and presses enter. 
      if (strcmp(line_buffer, "exit\n") == 0) {
	printf("     Goodbye!\n");
	exit(EXIT_SUCCESS);
      }
      
      // If the user presses enter without typing anything else, don't
      // update the command line counter, just start over again.
      
    } while (empty_line(line_buffer));
    
    // We got some input from the user.
    line_nr++;
    
    // Parse the command line
    do {
      pos = next_command(line_buffer, argv);
      
      // After the call to the next_command() parser function, the
      // command possition within the command line is now available in the pos
      // varialble.
      
      DBG("\n%6s command %s\n", pos2str(pos), argv[0]);
      
      // Loop through the argv and print the command data. 
          
      int i = 0;
      
      
      while (argv[i] != NULL) {
	DBG("         argv[%d] @ [0x%x] = \"%s\"\n", i, (unsigned int) argv[i], argv[i]);
	i++;
      }
      
      
      // Create a pipe fork a new process for the command. We also
      // must remember the read descriptor to the new pipe. This
      // descriptor will become the read descriptor to the pipe to the
      // "left" for the next command (if any).
      
      left_pipe_read_fd = pipe_and_fork(pos, argv, left_pipe_read_fd);
      
      children++;

      // When are we done?
    } while (pos != single && pos != last);
    
    
    DBG("\nAfter calling next_command(), line [0x%X]  ==>%s<==\n",  
	(unsigned int) &line_buffer, line_buffer);
    
    
    // The parent goes here after after all children have been 
    // created for the command. 
    
    // TODO: Make sure shell doesn't print a new prompt until 
    // all the command processes (children) have terminated.
    while(children>0){
      wait(NULL);   
      children--;
    }
    
  } // end while(1)
} // end of main()
