/**
 * Game of luck: Implementation of the Gamemaster
 *
 * Course: Operating Systems and Multicore Programming - OSM lab
 * assignment 1: game of luck.
 *
 * Author: Nikos Nikoleris <nikos.nikoleris@it.uu.se>
 *
 */

#include <stdio.h> /* I/O functions: printf() ... */
#include <stdlib.h> /* rand(), srand() */
#include <unistd.h> /* read(), write() calls */
#include <assert.h> /* assert() */
#include <time.h>   /* time() */
#include <signal.h> /* kill(), raise() and SIG???? */

#include <sys/types.h> /* pid */
#include <sys/wait.h> /* waitpid() */

#include "common.h"

#define READ_END 0
#define WRITE_END 1


int main(int argc, char *argv[])
{
  int i, seed, k;

  /* TODO: Use the following variables in the exec system call. Using the
   * function sprintf and the arg1 variable you can pass the id parameter
   * to the children
   */

  char arg0[] = "./shooter";
  char arg1[10];
  char *args[] = {arg0, arg1, NULL};

  /* TODO: initialize the communication with the players */

  int pfd[NUM_PLAYERS][2];
  int pfd_score[NUM_PLAYERS][2];
  pid_t pid[NUM_PLAYERS];

  for (i = 0; i < NUM_PLAYERS; i++) {
    pipe(pfd[i]);
    pipe(pfd_score[i]);
  }

  for (i = 0; i < NUM_PLAYERS; i++) {
    pid_t temp_pid = fork(); //????

    switch(temp_pid){
    case -1:
      perror("error");
      exit(EXIT_FAILURE);
      break; 
    case 0:
      /*I barnet, föräldern trycker in en int i barnet och barnet trycker tillbaka in en int till föräldern*/
      
      dup2(pfd[i][0], STDIN_FILENO);
      dup2(pfd_score[i][1], STDOUT_FILENO);
      //shooter(i, pfd[i][0], pfd_score[i][1]);
      sprintf(arg1, "%d", i);
      execve(args[0], args, NULL);
      

      exit(EXIT_SUCCESS);
      for (k = 0; k < NUM_PLAYERS; k++) {
	if(k != i){
	  /*stänger fd som vi har använt*/
	  close(pfd_score[k][1]);
	  close(pfd_score[k][0]);

	  close(pfd[k][0]);
	  close(pfd[k][1]);
	}
      }
      break; 
      /*Glöm inte att fork returnerar två gånger, ett barn och föräldern. Vi kommer alltså komma in i två case-satser, i case 0 och default, i default får vi tillbaka föräldern och då vill vi även spara pid där och inte i varje barn, då blir det fel*/
    default: 
      pid[i] = temp_pid;
      /* passar på att stänga fd här också*/
      close(pfd[i][0]);
      close(pfd_score[i][1]);
      break; 
    }
  } 

	seed = time(NULL);
	for (i = 0; i < NUM_PLAYERS; i++) {
		seed++;
		/*Trycker in int i rören till barnen, write genom pipsen*/
		write(pfd[i][1], &seed, sizeof(int));
		close(pfd[i][1]);
		/* TODO: send the seed to the players */
	}

	/* TODO: get the dice results from the players, find the winner */
	
	/*jämför direkt värdena när jag läser in vilken som är störst */
	int current_score = 0; 
	int current_max = 0; 
	for (i = 0; i < NUM_PLAYERS; i++) {
	  //läser in (0) pfd_score på plats i. 
	  read(pfd_score[i][0], &current_score, sizeof(int));
	  close(pfd_score[i][0]);
	  if(current_score > current_max){
	    current_max = current_score;
	    
	    winner = i; 
	  }
	}
	printf("Winner: %d\n", winner);
	printf("master: player %d WINS\n", winner);
	
	/* TODO: signal the winner */
	kill(pid[winner], SIGUSR1);
	
	/* TODO: signal all players the end of game */
	for (i = 0; i < NUM_PLAYERS; i++) {
	  kill(pid[i], SIGUSR2);
	}

	printf("master: the game ends\n");

	/* TODO: cleanup resources and exit with success */
        int status;
        pid_t pid_temp;
	for (i = 0; i < NUM_PLAYERS; i++) {
	  //????????????????????????? stäng fd. 
	  //stäng alla eftersom!
	  pid_temp = wait(&status);
          waitstat(pid_temp, status);
	}
	//	while(1) {};
	return 0;
}
