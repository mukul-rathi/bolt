public class AsyncFibonacci{
	int left;
	int right;
	int n;
	AsyncFibonacci(int n){
		this.n = n;
	}
	static int fib(int n){
		if (n ==0 || n==1){
			return 1;
		}
		else return fib(n-1)+fib(n-2);
	}


	static int asyncFib(int n){
		if (n < 35){ // for small cases use synchronous version
			return fib(n);
		}
		AsyncFibonacci x = new AsyncFibonacci(n);
		Thread t = new Thread() {
                @Override
                public void run() {
                    x.left = asyncFib(x.n - 1);
								}
		};
		t.start();
		x.right = asyncFib(x.n-2);
		try{
		t.join();
		} catch(InterruptedException e){}// do nothing as shouldn't be thrown
		return  x.left + x.right;
	}


	public static void main(String[] args){
			System.out.print("Fib of 46 is: ");
			System.out.println(asyncFib(46));
	}



}
