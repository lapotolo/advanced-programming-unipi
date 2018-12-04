import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Constructor;
import java.lang.reflect.Executable;
import java.lang.reflect.Parameter;
import java.util.ArrayList;

public class CheckNPE {

    public static void main(String[] args) {
        for (String className : args) {
            try {
                NullPointerExceptionSensibility(Class.forName(className));
            } catch (ClassNotFoundException e) {
                e.printStackTrace();
            }

        }
    }

    private static void NullPointerExceptionSensibility(Class<?> c){
        Object t = null;
        printClassInfos(c);
        System.out.println("");
        checkConstructors(c, t);
        System.out.println("");
        checkMethods(c, t);
    }

    private static void checkConstructors(Class<?> c, Object obj) {
        boolean isNpe = false;
        for(Constructor<?> constr : c.getDeclaredConstructors()) {
            constr.setAccessible(true);
            if(constr.getParameterCount() > 0) {
                try {
                    System.out.println("- constructor");
                    obj = constr.newInstance(getParametersSetToDefault(constr));
                } 
                catch(InvocationTargetException e) {
                    if(NullPointerException.class.isInstance(e.getCause())) {
                        isNpe = true;
                    }
                    
                }
                catch (InstantiationException 
                        | IllegalAccessException 
                        | IllegalArgumentException  e) {
                    e.printStackTrace();
                }
                System.out.println("IS IT NPE? " + isNpe);
                isNpe = false;
            }   
        }
    }

    private static void checkMethods(Class<?> c, Object obj) {
        if(obj == null) {
            try {
                // we need to create the object if all constructors din't satisfied the requirements
                obj = c.newInstance();
            } catch (InstantiationException | IllegalAccessException e) {
                e.printStackTrace();
            }
        }
        for(Method m : c.getDeclaredMethods()) {
            m.setAccessible(true);
            if(m.getParameterCount() > 0) {
                try {
                    System.out.println("Method: " + m.getName());
                    m.invoke(obj, getParametersSetToDefault(m));
                    System.out.println("IS IT NPE SENSIBLE? " + false);
                }
                catch(InvocationTargetException e){
                    if(NullPointerException.class.isInstance(e.getCause())) {
                        System.out.println("IS IT NPE SENSIBLE? " + true);
                    }
                }
                catch (IllegalAccessException | IllegalArgumentException e) {
                    e.printStackTrace();
                }
            }
        }
    }

    private static void printClassInfos(Class<?> c){
        System.out.println("class " + c.getName() +
                "\nThere is/are " + c.getDeclaredConstructors().length + " declared constructor/s" +
                "\nThere is/are " + c.getDeclaredMethods().length + " declared method/s");

    }

    private static Object[] getParametersSetToDefault(Executable funct) {
        ArrayList<Object> defaultValueParameters = new ArrayList<>();
        Object defaultValue;
        for(Parameter p : funct.getParameters()) {
            defaultValue = null;
            Class<?> paramType = p.getType();
            if(paramType.isPrimitive()) {
                if(paramType.getName().equals("boolean")) {
                    defaultValue = false;
                } else {
                    defaultValue = 0;
                }
            }
            defaultValueParameters.add(defaultValue);
            System.out.println("-- parameter type: " + paramType);
        }
        return defaultValueParameters.toArray();
    }

}