import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;

public class ExecTest {
    
    public static void main(String[] args) {
        for(String className : args) {
            try {
                Class<?> c = Class.forName(className);
                Object t = c.newInstance();
                for(Method method : c.getDeclaredMethods()) {
                    if(!Modifier.isPrivate(method.getModifiers()) 
                       && method.getParameterTypes().length == 0 
                       && method.getName().startsWith("test")) {
                        method.invoke(t);
                    }
                }
            } catch (InstantiationException 
                    | IllegalAccessException 
                    | ClassNotFoundException 
                    | IllegalArgumentException 
                    | InvocationTargetException e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }
        }
    }

}