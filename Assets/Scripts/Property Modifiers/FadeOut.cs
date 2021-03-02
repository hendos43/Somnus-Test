using System;
using System.Collections;
using System.Collections.Generic;
using Property_Modifiers;
using UnityEngine;

public class FadeOut : MonoBehaviour
{
    
    public float fadeDuration = 10;

    public List<GameObject> clones = new List<GameObject>();
    private void OnEnable()
    {
        StartCoroutine(FadeToClear());
    }

    
    IEnumerator FadeToClear(){
        foreach (GameObject clone in clones)
        {
            if (clone.TryGetComponent(out GrassFader currentFader)) //if it's grass that needs fading
            {
                currentFader.enabled = false;
                currentFader.overDuration = fadeDuration;
                currentFader.lerpFromCurrent = true;
                currentFader.targetValue = 1;
                currentFader.enabled = true;
            }
            else if (clone.TryGetComponent(out GenericMaterialModifier currentGenericMaterialModifier)) //If it's scaffolding that needs fading
            {
                currentGenericMaterialModifier.enabled = false;
                currentGenericMaterialModifier.overDuration = fadeDuration;
                currentGenericMaterialModifier.lerpFromCurrent = true;
                currentGenericMaterialModifier.targetValue = 0.5f;
                currentGenericMaterialModifier.enabled = true;
            }
            else{
                yield return null;
            }
        }
    }
}
