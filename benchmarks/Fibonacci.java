public class Fibonacci{

	static int fib(int n){
		if (n ==0 || n==1){
			return 1;
		}
		else return fib(n-1)+fib(n-2);
	}

	public static void main(String[] args){
			System.out.print("Fib of 10 is");
			System.out.println(fib(44));
	}



}
