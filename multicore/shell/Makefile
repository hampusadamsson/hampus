DEBUG=y

CC=gcc
CFLAGS=-Werror -Wall 
LDFLAGS= 

ifeq ($DEBUG), y)
	CFLAGS += -DDEBUG -g
else
	CFLAGS += -O2
endif



all: bin/shell

bin/shell: obj/shell.o obj/parse.o
	gcc $^ -o $@

obj/%.o: src/%.c
	gcc -c $(CFLAGS) $< -o $@

clean:
	$(RM) *~ src/*~ src/#* obj/*.o bin/* 

# 	
.PHONY: all clean dist

