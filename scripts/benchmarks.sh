
case "$1" in 
	--sync) 	
		scripts/run_program.sh benchmarks/fib.bolt
		cd benchmarks
		javac Fibonacci.java
		echo "Bolt"
		for i in {1..10}
		do
			time ./fib
		done	
		echo "Java"

		for i in {1..3} # warmup JIT
		do
		 java Fibonacci >nul
		done
		for i in {1..10}
		do
			time java Fibonacci
		done
		;;
	--async)
		scripts/run_program.sh benchmarks/asyncFib.bolt
		cd benchmarks
		javac AsyncFibonacci.java
		echo "Bolt"
		for i in {1..10}
		do
			time ./asyncFib
		done
		echo "Java"
		for i in {1..3}
		do
		 java AsyncFibonacci >nul
		done
			for i in {1..10}
		do
			time java AsyncFibonacci
		done
		;;
esac