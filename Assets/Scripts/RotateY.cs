using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class RotateY : MonoBehaviour
{
    public float degreesPerSecond = 1; 

    // Update is called once per frame
    void Update()
    {
       transform.Rotate(Vector3.up * Time.deltaTime * degreesPerSecond);
        
    }
}
