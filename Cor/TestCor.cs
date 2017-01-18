using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class TestCor : MonoBehaviour {
    CoroutineFactory _cor = new CoroutineFactory();

    [ContextMenu("test_orign_1")]
    void TestCorOr1()
    {
        StartCoroutine(test_1());
    }

    [ContextMenu("test_1")]
    void TestCor1()
    {
        _cor.Start(test_1());
    }

    IEnumerator test_loop()
    {
        //if (!bSkip)
            yield return null;
        Debug.LogFormat("test_loop 1111 frame:{0}", Time.renderedFrameCount);

        _cor.StopAll();
        StopAllCoroutines();

        Debug.LogFormat("test_loop 2222 frame:{0}", Time.renderedFrameCount);
    }

    IEnumerator test_1()
    {
        //Debug.LogFormat("test_1       111 frame:{0}", Time.renderedFrameCount);
        //yield return test_1_1();
        Debug.LogFormat("test_1       222 frame:{0}", Time.renderedFrameCount);
        yield return test_1_2();
        Debug.LogFormat("test_1       333 frame:{0}", Time.renderedFrameCount);
    }

    IEnumerator test_1_1()
    {
        Debug.LogFormat("test_1_1     111 frame:{0}", Time.renderedFrameCount);
        yield return null;
        Debug.LogFormat("test_1_1     111 frame:{0}", Time.renderedFrameCount);
    }

    IEnumerator test_1_2()
    {
        Debug.LogFormat("test_1_2     111 frame:{0}", Time.renderedFrameCount);
        yield return test_loop();
        Debug.LogFormat("test_1_2     222 frame:{0}", Time.renderedFrameCount);
    }

    [ContextMenu("test_2")]
    void TestCor2()
    {
        _cor.Start(test_2());
    }

    IEnumerator test_2()
    {
        Debug.LogFormat("test_2       111 frame:{0}", Time.renderedFrameCount);
        yield return _cor.Start(test_2_1());
        Debug.LogFormat("test_2       222 frame:{0}", Time.renderedFrameCount);
        yield return _cor.Start(test_2_2());
        Debug.LogFormat("test_2       333 frame:{0}", Time.renderedFrameCount);
        yield return _cor.Start(test_2_3());
    }

    IEnumerator test_2_1()
    {
        Debug.LogFormat("test_2_1     111 frame:{0}", Time.renderedFrameCount);
        yield return null;
        Debug.LogFormat("test_2_1     111 frame:{0}", Time.renderedFrameCount);
    }

    IEnumerator test_2_2()
    {
        Debug.LogFormat("test_2_2     111 frame:{0}", Time.renderedFrameCount);
        yield return test_loop();
        Debug.LogFormat("test_2_2     222 frame:{0}", Time.renderedFrameCount);
    }

    public bool bSkip = true;
    IEnumerator test_2_3()
    {
        //if (!bSkip)
            yield return null;
        //Debug.LogFormat("test_2_3     222 frame:{0}", Time.renderedFrameCount);
    }

    [ContextMenu("test_3")]
    void TestCor3()
    {
        //Debug.LogFormat("test3      111 frame:{0}", Time.renderedFrameCount);
        _cor.Start(test_3());
        //Debug.LogFormat("test3      222 frame:{0}", Time.renderedFrameCount);
    }

    IEnumerator test_3()
    {
        yield return _cor.Start(test_2_3());
        //Debug.Log("11111");
        //yield return _cor.Start(test_2_3());
        //yield return null;
    }
}
