all:
	llc *.bc
	gcc main.c *.s -lm -o main
	./main

clean:
	rm *.bc *.ll *.s main