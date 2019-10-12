static String __info__(Object obj){
    return String.format("[%s:%d]> %s",
			 Thread.currentThread().getStackTrace()[2].getFileName(),
			 Thread.currentThread().getStackTrace()[2].getLineNumber(),
			 obj.toString());
}
