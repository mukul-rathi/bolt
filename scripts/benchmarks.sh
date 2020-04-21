case "$1" in 
	--sync) 	
		scripts/run_program.sh benchmarks/fib.bolt
		cd benchmarks
		javac Fibonacci.java
		echo "Bolt"
		gtime ./fib
		gtime ./fib
		gtime ./fib
		gtime ./fib
		gtime ./fib
		echo "Java"
		gtime java Fibonacci
		gtime java Fibonacci
		gtime java Fibonacci
		gtime java Fibonacci
		gtime java Fibonacci
		;;
	--async)
		scripts/run_program.sh benchmarks/asyncFib.bolt
		cd benchmarks
		javac AsyncFibonacci.java
		echo "Bolt"
		gtime ./asyncFib
		gtime ./asyncFib
		gtime ./asyncFib
		gtime ./asyncFib
		gtime ./asyncFib
		
		echo "Java"
		gtime java AsyncFibonacci
		gtime java AsyncFibonacci
		gtime java AsyncFibonacci
		gtime java AsyncFibonacci
		gtime java AsyncFibonacci
		;;
esac