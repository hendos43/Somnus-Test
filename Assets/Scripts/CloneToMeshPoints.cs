using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class CloneToMeshPoints : MonoBehaviour
{
    public GameObject meshObject;
    
    public GameObject cloneObject;
    
    public Vector3 cloneScale = new Vector3(1, 1, 1);
    
    // Start is called before the first frame update
    void Start()
    {

        Mesh mesh = meshObject.GetComponent<MeshFilter>().mesh;
        
        Vector3[] vertices = mesh.vertices;
        Vector3[] normals = mesh.normals;

        // for (int i = 0; i < vertices.Length; i++)
        // {
        //     vertices[i] += Vector3.up;
        // }
        //
        // mesh.vertices = vertices;
        // mesh.RecalculateBounds();
        
        

        for (int vertId = 0; vertId < vertices.Length; vertId++)
        {
           
           
           GameObject newObject = Instantiate(cloneObject, meshObject.transform.TransformPoint(vertices[vertId]),  Quaternion.FromToRotation(Vector3.up, normals[vertId]));
           newObject.transform.localScale = new Vector3(cloneScale.x, cloneScale.y, cloneScale.z);
           newObject.transform.parent = gameObject.transform;

        }


    }

    // Update is called once per frame
    void Update()
    {
        
    }
}
