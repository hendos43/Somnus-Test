using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class RandomRotate : MonoBehaviour
{
    public float xRotRange = 360f;
    public float yRotRange = 360f;
    public float zRotRange = 360f;

    // Distance covered per second along X axis of Perlin plane.
    public float xSpeed = 0.1f;
    public float ySpeed = 0.1f;
    public float zSpeed = 0.1f;
    
    private float xSeed, ySeed, zSeed;
    
    // Start is called before the first frame update
    void Start()
    {
        xSeed = UnityEngine.Random.Range(0f, 100f);
        ySeed = UnityEngine.Random.Range(0f, 100f);
        zSeed = UnityEngine.Random.Range(0f, 100f);

    }

    // Update is called once per frame
    void Update()
    {
        float xRot = xRotRange * Mathf.PerlinNoise(xSeed + (Time.time * xSpeed), 0f);
        float yRot = yRotRange * Mathf.PerlinNoise(ySeed + (Time.time * ySpeed), 0f);
        float zRot = zRotRange * Mathf.PerlinNoise(zSeed + (Time.time * zSpeed), 0f);

        Vector3 rot = transform.eulerAngles;
        
        rot.x = xRot;
        rot.y = yRot;
        rot.z = zRot;

        transform.eulerAngles = rot;
    }
}
