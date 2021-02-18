using System.Collections;
using UnityEngine;

namespace Property_Modifiers
{
    public class GenericMaterialModifierVector3 : MonoBehaviour
    {
        [TextArea] public string notes;
        [Header("Object with Material to Modify")]
        public GameObject objectToModify;
        [Header("Property of Material to Modify")]
        public string propertyToChange = "_MainColor";
        [Header("Values to Modify")]
        [Tooltip("If enabled, Initial Value will be ignored")]public bool lerpFromCurrent = false;
        public Vector3 initialValue;
        private Vector3 valueToChange;
        public Vector3 targetValue;
        [Header("Duration (s)")]
        public float overDuration = 5;

        private Renderer rend;

        private void Awake()
        {
            rend = objectToModify.GetComponent<Renderer>();

        }

        void OnEnable()
        {
            if (lerpFromCurrent == true)
            {
                initialValue = rend.material.GetVector(propertyToChange);
                //Debug.Log(initialValue);
            } 
        
            StartCoroutine(LerpFunction(targetValue, overDuration));
        }

        IEnumerator LerpFunction(Vector3 endValue, float duration)
        {
            float time = 0;
            valueToChange = initialValue;
            Vector3 startValue = initialValue;

            while (time < duration)
            {
                valueToChange = Vector3.Lerp(startValue, endValue, time / duration);
                time += Time.deltaTime;
                yield return null;
            
                //Set property
                rend.material.SetVector(propertyToChange, valueToChange);
            }
            valueToChange = endValue;
        
            //Set final value
            rend.material.SetVector(propertyToChange, valueToChange);

        }
    }
}
