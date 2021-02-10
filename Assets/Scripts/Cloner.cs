// Instantiates 10 copies of Prefab each 2 units apart from each other

using UnityEngine;
using System.Collections;

public class Cloner : MonoBehaviour
{
    public int numClones = 10;
    public float minXRange = -50f;
    public float maxXRange = 50f;
    public float minYRange = 0f;
    public float maxYRange = 20f;
    public float minZRange = -50f;
    public float maxZRange = 50f;
    
    public Transform prefab;
    void OnEnable()
    {
        for (int i = 0; i < numClones; i++)
        {
            var newObject = Instantiate(prefab, 
                new Vector3(Random.Range(minXRange, maxXRange), Random.Range(minYRange, maxYRange), Random.Range(minZRange, maxZRange)), 
                Quaternion.Euler(new Vector3(Random.Range(0, 360),Random.Range(0, 360),Random.Range(0, 360)))
                );

            newObject.transform.parent = gameObject.transform;
        }
    }
}