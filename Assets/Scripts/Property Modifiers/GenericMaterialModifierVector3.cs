using System;
using System.Collections;
using System.Collections.Generic;
using System.Diagnostics;
using System.Runtime.CompilerServices;
using TMPro;
using Unity.Collections;
using UnityEditor.ShaderGraph.Drawing;
using UnityEngine;

public class GenericMaterialModifierVector3 : MonoBehaviour
{
    public GameObject objectToModify;
    public bool lerpFromCurrent = false;
    public Vector3 initialValue;
    private Vector3 valueToChange;
    public Vector3 targetValue;
    public float overDuration = 5;
    public string propertyToChange = "_MainColor";

    private Renderer rend;

    private void Awake()
    {
        rend = objectToModify.GetComponent<Renderer>();

    }

    void OnEnable()
    {
        if (lerpFromCurrent == true)
        {
            initialValue = rend.material.GetVector(propertyToChange);
            //Debug.Log(initialValue);
        } 
        
        StartCoroutine(LerpFunction(targetValue, overDuration));
    }

    IEnumerator LerpFunction(Vector3 endValue, float duration)
    {
        float time = 0;
        valueToChange = initialValue;
        Vector3 startValue = initialValue;

        while (time < duration)
        {
            valueToChange = Vector3.Lerp(startValue, endValue, time / duration);
            time += Time.deltaTime;
            yield return null;
            
            //Set property
         rend.material.SetVector(propertyToChange, valueToChange);
        }
        valueToChange = endValue;
        
        //Set final value
        rend.material.SetVector(propertyToChange, valueToChange);

    }
}
