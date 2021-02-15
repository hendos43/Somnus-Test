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
    public float xPosRange = 20.0f;
    public float yPosRange = 20.0f;
    public float zPosRange = 20.0f;

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
        float xDir = xPosRange * Mathf.PerlinNoise(xSeed + (Time.time * xSpeed), 0f);
        float yDir = yPosRange * Mathf.PerlinNoise(ySeed + (Time.time * ySpeed), 0f);
        float zDir = zPosRange * Mathf.PerlinNoise(zSeed + (Time.time * zSpeed), 0f);
        
        Vector3 pos = transform.position;

        pos.x = xDir;
        pos.y = yDir;
        pos.z = zDir;
        
        transform.position = pos;

       
       


        //transform.eulerAngles = pos;
    }
}