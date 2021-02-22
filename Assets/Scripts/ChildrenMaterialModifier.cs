using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class ChildrenMaterialModifier : MonoBehaviour
{
    [TextArea] public string notes;

    [Header("Parent Object with Material to Modify")]
    public GameObject objectToModify;

    [Header("Property of Material to Modify")]
    public string propertyToChange = "_DisplacementPower";

    [Header("Values to Modify")] [Tooltip("If enabled, Initial Value will be ignored")]
    public bool lerpFromCurrent = false;

    public float initialValue;
    private float valueToChange;
    public float targetValue;
    [Header("Duration (s)")] public float overDuration = 5;



    //private Renderer rend;

    private Renderer[] rends;


    private void Awake()
    {
        // rend = objectToModify.GetComponent<Renderer>();

        rends = objectToModify.GetComponentsInChildren<Renderer>();

    }

    void OnEnable()
    {
        if (lerpFromCurrent == true)
        {
            initialValue = rends[0].material.GetFloat(propertyToChange);
            //Debug.Log(initialValue);
        }


        foreach (Renderer rend in rends)
        {
            StartCoroutine(LerpFunction(targetValue, overDuration, rend));
        }
    }

    IEnumerator LerpFunction(float endValue, float duration, Renderer rend)
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