.PHONY: all run

CC = gcc
CCFLAGS = -Wall -Wextra
SRC = *.c
BUILD = boiler

all:
	$(CC) $(SRC) -o $(BUILD) $(CCFLAGS)

run:
	./$(BUILD)
