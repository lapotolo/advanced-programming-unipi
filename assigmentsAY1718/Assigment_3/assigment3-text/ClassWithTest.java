public class ClassWithTest{
    public ClassWithTest(){
	/* Va invocato */
	System.out.println("Tests ready to run");
    }

    public void testAbs(){
	/* Va invocato */
	System.out.println("Invocation of " + Thread.currentThread().getStackTrace()[1].getMethodName());
	System.out.println("abs(-2) = 2: " + testResult(Math.abs(-2), 2));
	System.out.println("abs(-3) = -2: " + testResult(Math.abs(-3), -2));
    }

    public void testRound(){
	/* Va invocato */
	System.out.println("Invocation of " + Thread.currentThread().getStackTrace()[1].getMethodName());
	System.out.println("round(0.7) = 1: " + testResult(Math.round(0.7), 1));
	System.out.println("round(0.2) = 1: " + testResult(Math.round(0.2), 1));
    }

    public void testMin(){
	/* Va invocato */
	System.out.println("Invocation of " + Thread.currentThread().getStackTrace()[1].getMethodName());
	System.out.println("min(7,3) = 3: " + testResult(Math.min(7,3), 3));
	System.out.println("min(5,7) = 7: " + testResult(Math.min(5,7), 7));
    }

    public void testMax(){
	/* Va invocato */
	System.out.println("Invocation of " + Thread.currentThread().getStackTrace()[1].getMethodName());
	System.out.println("max(7,14) = 14: " + testResult(Math.max(7,14), 14));
	System.out.println("max(5,7) = 5: " + testResult(Math.max(5,7), 5));
    }

    public void TestDiv(){
	/* Non andreabbe chiamato */
	System.out.println("Invocation of " + Thread.currentThread().getStackTrace()[1].getMethodName());
	System.out.println("14 / 2 = 7: " + testResult(14 / 7, 3));
	//	assert(14 / 0 == 10);
    }

    public void testMult(int x){
	/* Non andreabbe chiamato */
	System.out.println("Invocation of " + Thread.currentThread().getStackTrace()[1].getMethodName());
	System.out.printf("2 * %d = %s:", x, testResult(2 * x, 2 * x));
	//       assert(false);
    }

    public void otherTest(){
	/* Non andreabbe chiamato */
	System.out.println("Invocation of " + Thread.currentThread().getStackTrace()[1].getMethodName());
	System.out.printf("1 = 1:", testResult(1, 1));
	//       assert(false);
    }

    private void testAll(){
	/* Non dovrebbe essere chiamato */
	System.out.println("Invocation of " + Thread.currentThread().getStackTrace()[1].getMethodName());
	testAbs();
	testRound();
	testMin();
	testMax();
    }

    private static String testResult(double result, double expected){
	return (result == expected ? "ok" : "no");
    }

    public static void main(String[] args){
	(new ClassWithTest()).testAll();
    }
}
