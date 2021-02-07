// Instantiates 10 copies of Prefab each 2 units apart from each other

using UnityEngine;
using System.Collections;

public class Cloner : MonoBehaviour
{
    public int numClones = 10;
    
    public Transform prefab;
    void Start()
    {
        for (int i = 0; i < numClones; i++)
        {
            var newObject = Instantiate(prefab, 
                new Vector3(Random.Range(0f, 10f), Random.Range(0f, 10f), Random.Range(0f, 10f)), 
                Quaternion.Euler(new Vector3(Random.Range(0, 360),Random.Range(0, 360),Random.Range(0, 360)))
                );

            newObject.transform.parent = gameObject.transform;
        }
    }
}