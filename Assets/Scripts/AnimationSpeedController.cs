using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class AnimationSpeedController : MonoBehaviour
{
    private Animator m_animator;

   [Range(-1, 1)] public float animationSpeed = 0;
    
    void Start()
    {

        m_animator = gameObject.GetComponent<Animator>();

    }

    void Update()
    {
     
        if (m_animator.GetCurrentAnimatorStateInfo(0).normalizedTime < 0)
        {
            
            m_animator.Play(0,0,0);
            
        }
        else if(m_animator.GetCurrentAnimatorStateInfo(0).normalizedTime > 1)
        {
            
            m_animator.Play(0,0,1);

        }

        m_animator.SetFloat("animSpeedFactor", animationSpeed);

    }
}
