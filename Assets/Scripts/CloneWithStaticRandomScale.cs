using System;
using System.Collections;
using System.Collections.Generic;
using System.Timers;
using Property_Modifiers;
using UnityEngine;
using Random = System.Random;

public class CloneWithStaticRandomScale : MonoBehaviour
{
    public GameObject meshObject;
    
    public GameObject cloneObject;
    
    //public Vector3 cloneScale = new Vector3(1, 1, 1);

    public List<GameObject> clones = new List<GameObject>();
    
    public Vector3 worldSpaceOffset = new Vector3(0,0,0);
    
    public Vector3 rotationOffset = new Vector3();

    [Header("Select Game Object to control Fade Out:")]
    public GameObject pushListToFader;
    
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

            float thisCloneScale = UnityEngine.Random.Range(0.5f, 1.5f);
            //float thisCloneScale = 1f;

            
           // Vector3 randomRotation = new Vector3(1, UnityEngine.Random.Range(0f,360f), 1);

            GameObject thisClone = Instantiate(cloneObject, meshObject.transform.TransformPoint(vertices[vertId]+worldSpaceOffset),  Quaternion.FromToRotation(Vector3.up, normals[vertId] + rotationOffset));
            thisClone.transform.localScale = new Vector3(thisCloneScale, thisCloneScale, thisCloneScale);
            thisClone.transform.parent = gameObject.transform;
            clones.Add(thisClone);

            if (pushListToFader != null)
            {
                if (pushListToFader.TryGetComponent(out FadeOut fader))
                {
                    fader.clones.Add(thisClone);
                }
            }



        }


    }

    private void OnDisable()
    {
        
        foreach (GameObject clone in clones)
        {
            Destroy(clone);
        }
       
        clones.Clear();


        if (pushListToFader != null)
        {
            if (pushListToFader.TryGetComponent(out FadeOut fader))
            {
                foreach (GameObject clone in fader.clones)
                {
                    Destroy(clone);
                }

                fader.clones.Clear();
            }
        }
    }

        

        







}