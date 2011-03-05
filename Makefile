ERL = erl -boot start_clean

all: compile

compile: 
	erl -make

run: compile
	${ERL} -pa src -s ememcached start

clean:	
	rm -rf *.beam erl_crash.dump

