using System;
using System.Collections;
using System.Collections.Generic;
using System.Diagnostics;
using System.Runtime.CompilerServices;
using Unity.Collections;
using UnityEditor.ShaderGraph.Drawing;
using UnityEngine;
using Debug = UnityEngine.Debug;

public class GenericMaterialModifier : MonoBehaviour
{
    public GameObject objectToModify;
    public bool lerpFromCurrent = false;
    public float initialValue;
    private float valueToChange;
    public float targetValue;
    public float overDuration = 5;
    public string propertyToChange = "_DisplacementPower";


    private Renderer rend;

    
    
    private void Awake()
    {
        rend = objectToModify.GetComponent<Renderer>();

    }

    void OnEnable()
    {
        if (lerpFromCurrent == true)
        {
            initialValue = rend.material.GetFloat(propertyToChange);
            //Debug.Log(initialValue);
        } 
        
        StartCoroutine(LerpFunction(targetValue, overDuration));
    }

    IEnumerator LerpFunction(float endValue, float duration)
    {
        float time = 0;
        valueToChange = initialValue;
        float startValue = initialValue;

        while (time < duration)
        {
            valueToChange = Mathf.Lerp(startValue, endValue, time / duration);
            time += Time.deltaTime;
            yield return null;
            
            //Set property
            rend.material.SetFloat(propertyToChange, valueToChange);
        }
        valueToChange = endValue;
        
        //Set final value
        rend.material.SetFloat(propertyToChange, valueToChange);

    }
}


