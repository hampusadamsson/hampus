#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "db.h"

int main(int argc, char *argv[]){
  if (argc < 2){
    puts("Usage: db [FILE]");
    return -1;
  }
  Node list;
  list = readDatabase(argv[1]);

  puts("Welcome to");
  puts(" ____    ____       ");
  puts("/\\  _`\\ /\\  _`\\     ");
  puts("\\ \\ \\/\\ \\ \\ \\L\\ \\   ");
  puts(" \\ \\ \\ \\ \\ \\  _ <\\ ");
  puts("  \\ \\ \\_\\ \\ \\ \\L\\ \\ ");
  puts("   \\ \\____/\\ \\____/ ");
  puts("    \\/___/  \\/___/  ");
  puts("");

  
  // Main loop
  int choice = -1;
  while(choice != 0) {
    puts("Please choose an operation");
    puts("1. Query a key");
    puts("2. Update an entry");
    puts("3. New entry");
    puts("4. Remove entry");
    puts("5. Print database");
    puts("0. Exit database");
    printf("? ");
    scanf("%d", &choice);
    while(getchar() != '\n'); // Clear stdin
    //int found;
    // Node cursor;
char buffer[128];
    switch(choice){
    case 1: //query
      queryDb(list, buffer);
     break;

    case 2: //update
      updateDb(list, buffer);
      break;

    case 3: //insert
      list = insertDb(list, buffer);
      break;

    case 4: // Delete
      list = deleteDb(list, buffer);
      break;

    case 5: //print
      printDb(list);
      break;

    case 0: //Fel - prova igen
      // Exit
      puts("Good bye!");
      break;
    default:
      // Please try again
      puts("Could not parse choice! Please try again");
    }
    puts("");
  }
  return 0;
}


