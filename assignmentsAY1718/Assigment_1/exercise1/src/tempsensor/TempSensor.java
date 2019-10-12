/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package tempsensor;

/**
 *
 * @author lapo
 */

import java.beans.*;
import java.io.Serializable;
import java.util.Random;
import java.util.Timer;
import java.util.TimerTask;

public class TempSensor implements Serializable {

    private static final int SENSING_RATE_MILLISECS = 1000;
    private static final float MAX_TEMP = 50.0f;
    private static final float MIN_TEMP = 20.0f;


    public static final String PROPERTY_CURRENT_TEMP = "currentTemp";
    
    private float currentTemp;
    private final PropertyChangeSupport currentTempSupporter;

    // *******************************************
    // *** Implementation of bean requirements *********************
    
// default constructuor
    public TempSensor() {
        currentTempSupporter = new PropertyChangeSupport(this);
    }

    // currentTemp properties getter and setter
    public float getCurrentTemp() {
        return currentTemp;
    }

    public void setCurrentTemp(float newTemp) {
        float oldValue = this.currentTemp;
        this.currentTemp = newTemp;
        currentTempSupporter.firePropertyChange(PROPERTY_CURRENT_TEMP, oldValue, newTemp);
    }

    // currentTemp is a bound property so "observer" methods are required
    public void addCurrentTempChangeListener(PropertyChangeListener listener) {
        currentTempSupporter.addPropertyChangeListener(listener);
    }

    public void removeCurrentTempChangeListener(PropertyChangeListener listener) {
        currentTempSupporter.removePropertyChangeListener(listener);
    }

    //***********************************************************
    
    public void go() {
        sense(SENSING_RATE_MILLISECS);
    }

    private void sense(long sensingRate) {
        new Timer().scheduleAtFixedRate(new TimerTask() 
        {
            @Override
            public void run() {
                setCurrentTemp(MIN_TEMP + new Random().nextFloat() * (MAX_TEMP - MIN_TEMP) );
            }
        }, 0, sensingRate);
    }
}
