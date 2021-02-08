using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class CloneToMeshPoints : MonoBehaviour
{
    public GameObject meshObject;
    
    public GameObject cloneObject;
    
    
    
    // Start is called before the first frame update
    void Start()
    {

        Mesh mesh = meshObject.GetComponent<MeshFilter>().mesh;
        
        Vector3[] vertices = mesh.vertices;

        for (int i = 0; i < vertices.Length; i++)
        {
            vertices[i] += Vector3.up;
        }

        mesh.vertices = vertices;
        mesh.RecalculateBounds();

        for (int vertId = 0; vertId < vertices.Length; vertId++)
        {
            Instantiate(cloneObject, vertices[vertId], transform.rotation);
        }


    }

    // Update is called once per frame
    void Update()
    {
        
    }
}
