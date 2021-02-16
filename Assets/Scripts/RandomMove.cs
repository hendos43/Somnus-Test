using System;
using UnityEngine;
using System.Collections;
using System.Collections.Generic;
using System.Security.Cryptography.X509Certificates;
using UnityEngine.Rendering;
using UnityEngine.UIElements;

public class RandomMove : MonoBehaviour
{
    // Range over which height varies.
    public float xPosMax = 100.0f;
    public float yPosMax = 100.0f;
    public float zPosMax = 100.0f;

    public float xPosMin = -100f;
    public float yPosMin = 0f;
    public float zPosMin = -100f;

    // public float xRotRange = 360f;
    // public float yRotRange = 360f;
    // public float zRotRange = 360f;

    // Distance covered per second along X axis of Perlin plane.
    public float xSpeed = 0.1f;
    public float ySpeed = 0.1f;
    public float zSpeed = 0.1f;

    private float xSeed, ySeed, zSeed;
    


    private void Start()
    {
    xSeed = UnityEngine.Random.Range(0f, 100f);
    ySeed = UnityEngine.Random.Range(0f, 100f);
    zSeed = UnityEngine.Random.Range(0f, 100f);

    }

    void Update()
    {
        float xDir = Mathf.PerlinNoise(xSeed + (Time.time * xSpeed), 0f);
        float yDir = Mathf.PerlinNoise(ySeed + (Time.time * ySpeed), 0f);
        float zDir = Mathf.PerlinNoise(zSeed + (Time.time * zSpeed), 0f);


        float xPosMinScaled = xPosMin / xPosMax;
        float yPosMinScaled = yPosMin / yPosMax;
        float zPosMinScaled = zPosMin / zPosMax;

        xDir = map(xDir, 0, 1, xPosMinScaled, 1);
        yDir = map(yDir, 0, 1, yPosMinScaled, 1);
        zDir = map(zDir, 0, 1, zPosMinScaled, 1);

        
        
         xDir = xPosMax * xDir;
         yDir = yPosMax * yDir;
         zDir = zPosMax * zDir;


 
        Vector3 pos = transform.position;

        pos.x = xDir;
        pos.y = yDir;
        pos.z = zDir;
        
        transform.position = pos;

       
       


        //transform.eulerAngles = pos;
    }
    
    float map(float s, float a1, float a2, float b1, float b2)
    {
        return b1 + (s-a1)*(b2-b1)/(a2-a1);
    }
}