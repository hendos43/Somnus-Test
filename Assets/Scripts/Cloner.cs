// Instantiates 10 copies of Prefab each 2 units apart from each other

using System;
using UnityEngine;
using System.Collections;
using System.Collections.Generic;
using Random = UnityEngine.Random;

public class Cloner : MonoBehaviour
{
    public int numClones = 10;
    public float minXRange = -50f;
    public float maxXRange = 50f;
    public float minYRange = 0f;
    public float maxYRange = 20f;
    public float minZRange = -50f;
    public float maxZRange = 50f;

    public GameObject prefab;
    
    public Vector3 cloneScale = new Vector3(1, 1, 1);
    
    private List<GameObject> clones = new List<GameObject>();
    

    void OnEnable()
    {
        for (int i = 0; i < numClones; i++)
        {
            GameObject thisClone = Instantiate(prefab, 
                new Vector3(Random.Range(minXRange, maxXRange), Random.Range(minYRange, maxYRange), Random.Range(minZRange, maxZRange)), 
                Quaternion.Euler(new Vector3(Random.Range(0, 360),Random.Range(0, 360),Random.Range(0, 360)))
                );
            
            thisClone.transform.localScale = new Vector3(cloneScale.x, cloneScale.y, cloneScale.z);
            thisClone.transform.parent = gameObject.transform;
            
            clones.Add(thisClone);

        }
    }

    private void OnDisable()
    {
        foreach (GameObject clone in clones)
        {
            Destroy(clone);
        }
        
        clones.Clear();
        
    }
}