
package tempsensorvetoable;

import java.beans.*;

/**
 *
 * @author lapo
 */
public class TempSensorVetoable extends TempSensor {
    
    private static final float MIN_TEMP_VETO = 0.0f;
    private static final float MAX_TEMP_VETO = 30.0f;
    private static final String VETO_ALARM_MSG = "Temperature out of bounds";
    
    
    private VetoableChangeSupport vetoTempSupporter;
    
    public TempSensorVetoable() {
        super();
        vetoTempSupporter = new VetoableChangeSupport(this);
    }
        
    @Override
    public void setCurrentTemp(float newTemp) {                
        try {
            vetoTempSupporter.fireVetoableChange(PROPERTY_CURRENT_TEMP, getCurrentTemp(), newTemp);
            super.setCurrentTemp(newTemp);
            
        } catch (PropertyVetoException e) {}

    }
    
    public void addVetoableChangeListener(VetoableChangeListener listener) {
        vetoTempSupporter.addVetoableChangeListener(listener);
    }
    
    public void removeVetoableChangeListener(VetoableChangeListener listener) {
        vetoTempSupporter.removeVetoableChangeListener(listener);
    }
    
    public float getMinTempVeto(){
        return TempSensorVetoable.MIN_TEMP_VETO;
    }
    
    public float getMaxTempVeto(){
        return TempSensorVetoable.MAX_TEMP_VETO;
    }
    
    public String getAlarmMsgVeto(){
        return TempSensorVetoable.VETO_ALARM_MSG;
    }
}