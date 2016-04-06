
REGRESSES = --regress=cycles:iters --regress=mutatorWallSeconds:iters --regress=cpuTime:iters

# These don't make sense because its an external process:
# REGRESSES += --regress=bytesCopied:iters --regress=gcWallSeconds:iters --regress=numGcs:iters --regress=allocated:iters 

.PHONY: all rebuild run

all: ./bin/criterion-external run

./bin/criterion-external:
	$(MAKE) rebuild

rebuild:
	mkdir -p ./bin
	stack install --local-bin-path=bin

run:
	./bin/criterion-external ./timeme.sh -h
	./bin/criterion-external ./timeme.sh -- --list
	./bin/criterion-external +RTS -T -RTS ./timeme.sh -- $(REGRESSES) -o bash_loop.html 

# Here we lose GC statistics that we would have if it were intra-process criterion:
	./bin/criterion-external +RTS -T -RTS ./bin/simple-haskell-io-loop -- $(REGRESSES) -o haskell_IO_loop.html 
	./bin/criterion-external +RTS -T -RTS ./bin/simple-haskell-pure-loop -- $(REGRESSES) -o haskell_pure_loop.html 
