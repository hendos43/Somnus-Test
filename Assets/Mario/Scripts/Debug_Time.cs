using System;
using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class Debug_Time : MonoBehaviour
{
    [SerializeField][Range(0,10)] private float skipSpeed = 0.5f;
    public static double timer;
    [Header("AUTO")]
    [SerializeField] private float myTime;
    [TextArea(4, 4)]
    [SerializeField]
    private string com =
"Right Arrow For Fast Forward \n" +
"Left Arrow For Reverse \n" +
"Space simulates player looking";

    private List<Debug_Global_Event> all_Global_Events = new List<Debug_Global_Event>();
    private List<Debug_Enable_Other> all_Enable_Other = new List<Debug_Enable_Other>();

    private void Start()
    {
        for (int i = 0; i < FindObjectsOfType<Debug_Global_Event>().Length; i++)
        {
            all_Global_Events.Add(FindObjectsOfType<Debug_Global_Event>()[i]);
        }

        for (int i = 0; i < FindObjectsOfType<Debug_Enable_Other>().Length; i++)
        {
            all_Enable_Other.Add(FindObjectsOfType<Debug_Enable_Other>()[i]);
        }
    }

    private void Update()
    {
        timer += Time.deltaTime;
        myTime = (float)timer;

        foreach (Debug_Global_Event debugEvent in all_Global_Events)
        {

            if (myTime >= debugEvent.eventStartsAt)
                debugEvent.gameObject.SetActive(true);
            else
                debugEvent.gameObject.SetActive(false);

            if (myTime >= debugEvent.eventEndsAt)
                debugEvent.gameObject.SetActive(false);

            if (Input.GetKey(KeyCode.Space))
            {
                debugEvent.transform.GetChild(1).gameObject.SetActive(true);
            }
            else
            {
                debugEvent.transform.GetChild(1).gameObject.SetActive(false);
            }
        }

        foreach (Debug_Enable_Other debugEvent in all_Enable_Other)
        {

            if (myTime > debugEvent.eventStartsAt)
                debugEvent.canTrigger = true;
            else
                debugEvent.canTrigger = false;

            if (myTime > debugEvent.eventEndsAt)
                debugEvent.canTrigger = false;
        }

        if (Input.GetKey(KeyCode.RightArrow))
            timer += 1f * skipSpeed;

        if (Input.GetKey(KeyCode.LeftArrow))
        {
            if(timer >= 1)
                timer -= 1f * skipSpeed;
        }

    }
}
