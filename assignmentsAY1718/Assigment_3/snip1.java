public static String insertLength(Object obj){
    String line = obj.toString();
    return String.format("[%d]> %s", line.length(), line);
}
