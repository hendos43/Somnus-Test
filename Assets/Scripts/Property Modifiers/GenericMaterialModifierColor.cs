using System.Collections;
using UnityEngine;

namespace Property_Modifiers
{
    public class GenericMaterialModifierColor : MonoBehaviour
    {
        [TextArea] public string notes;
        [Header("Object with Material to Modify")]
        public GameObject objectToModify;
        [Header("Property of Material to Modify")]
        public string propertyToChange = "_MainColor";
        [Header("Values to Modify")]
        [Tooltip("If enabled, Initial Value will be ignored")]public bool lerpFromCurrent = false;
        public Color initialValue;
        private Color valueToChange;
        public Color targetValue;
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
                initialValue = rend.material.GetColor(propertyToChange);
                //Debug.Log(initialValue);
            } 
        
            StartCoroutine(LerpFunction(targetValue, overDuration));
        }

        IEnumerator LerpFunction(Color endValue, float duration)
        {
            float time = 0;
            valueToChange = initialValue;
            Color startValue = initialValue;

            while (time < duration)
            {
                valueToChange = Color.Lerp(startValue, endValue, time / duration);
                time += Time.deltaTime;
                yield return null;
            
                //Set property
                rend.material.SetColor(propertyToChange, valueToChange);
            }
            valueToChange = endValue;
        
            //Set final value
            rend.material.SetColor(propertyToChange, valueToChange);

        }
    }
}
