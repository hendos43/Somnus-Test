using System;
using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class CloneToMeshPoints : MonoBehaviour
{
    public GameObject meshObject;
    
    public GameObject cloneObject;
    
    public Vector3 cloneScale = new Vector3(1, 1, 1);

    private List<GameObject> clones = new List<GameObject>();
    
    public Vector3 worldSpaceOffset = new Vector3(0,0,0);
    
    // Start is called before the first frame update
    void OnEnable()
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
           
           
           GameObject thisClone = Instantiate(cloneObject, meshObject.transform.TransformPoint(vertices[vertId]+worldSpaceOffset),  Quaternion.FromToRotation(Vector3.up, normals[vertId]));
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

    // Update is called once per frame
    void Update()
    {
        
    }
}
