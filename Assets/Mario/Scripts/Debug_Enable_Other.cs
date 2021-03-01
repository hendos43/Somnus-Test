using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class Debug_Enable_Other : MonoBehaviour
{
    public float eventStartsAt;
    public float eventEndsAt;
    [HideInInspector]
    public bool canTrigger;
    public GameObject objectToEnable;

    private void Update()
    {
        objectToEnable.SetActive(canTrigger);
    }
}
