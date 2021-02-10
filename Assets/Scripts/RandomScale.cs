using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class RandomScale : MonoBehaviour
{
    
    public float xScaleRange = 3.0f;
    public float yScaleRange = 3.0f;
    public float zScaleRange = 3.0f;
    
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
        
        
        float xScale = xScaleRange * Mathf.PerlinNoise(xSeed + (Time.time * xSpeed), 0f);
        //float yScale = yScaleRange * Mathf.PerlinNoise(ySeed + (Time.time * ySpeed), 0f);
        //float zScale = zScaleRange * Mathf.PerlinNoise(zSeed + (Time.time * zSpeed), 0f);
        
        Vector3 newScale = transform.localScale;

        newScale.x = xScale;
        //newScale.y = yScale;
        //newScale.z = zScale;
        
        transform.localScale = new Vector3(newScale.x, newScale.x, newScale.x);
    }
}
