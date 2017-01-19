using System;
using System.Collections;
using System.Collections.Generic;
using UnityEngine;

static class Tool
{
    static int renderFrame = 0;
    public static void PushFrame()
    {
        renderFrame = Time.renderedFrameCount;
    }

    public static void Log(string name, int step, int tab = 0)
    {
        const int TAB = 4;
        string strStep = string.Format("step:{0}", step);
        Debug.LogFormat("{0}{1}{2}{3}{4}frame:{5}",
            new string(' ', tab * tab),
            name,
            new string(' ', TAB + name.Length % TAB),
            strStep,
            new string(' ', TAB + strStep.Length % TAB),
            Time.renderedFrameCount - renderFrame);
    }
}

interface CorMaker
{
    Coroutine Create(IEnumerator iter);
    void Stop(Coroutine cor);
    void StopAll();
}

class CorInner : CorMaker
{
    MonoBehaviour _mono = null;
    public CorInner(MonoBehaviour mono)
    { _mono = mono; }

    public Coroutine Create(IEnumerator iter)
    { return _mono.StartCoroutine(iter); }

    public void Stop(Coroutine cor)
    { _mono.StopCoroutine(cor); }

    public void StopAll()
    { _mono.StopAllCoroutines(); }
}

class CorMy : CorMaker
{
    CoroutineFactory _cor;
    public CorMy(CoroutineFactory cor)
    { _cor = cor; }

    public Coroutine Create(IEnumerator iter)
    { return _cor.Start(iter); }

    public void Stop(Coroutine cor)
    { _cor.Stop(cor); }

    public void StopAll()
    { _cor.StopAll(); }
}

class TestNomal
{
    CorMaker _maker;

    public TestNomal(CorMaker maker)
    {
        _maker = maker;
    }

    public void Test()
    {
        Tool.PushFrame();
        _maker.Create(Test1());
    }

    IEnumerator Test1()
    {
        yield return Step_1();
        yield return Step_2();
    }

    IEnumerator Step_1()
    {
        Tool.Log("Step_1", 1);
        yield return Step_Inner();
        Tool.Log("Step_1", 2);
    }

    IEnumerator Step_2()
    {
        Tool.Log("Step_2", 1);
        yield return Step_Inner();
        Tool.Log("Step_2", 2);
        yield return Step_Inner();
    }

    IEnumerator Step_Inner()
    {
        Tool.Log("Step_Inner", 1);
        yield return null;
        Tool.Log("Step_Inner", 2);
    }
}

class TestCreate
{
    CorMaker _maker;

    public TestCreate(CorMaker maker)
    {
        _maker = maker;
    }

    public void Test()
    {
        Tool.PushFrame();
        _maker.Create(test_cor());
    }

    IEnumerator test_cor()
    {
        yield return _maker.Create(Step_1());
        yield return _maker.Create(Step_2());
    }

    IEnumerator Step_1()
    {
        Tool.Log("Step_1", 1);
        yield return _maker.Create(Step_Inner());
    }

    IEnumerator Step_2()
    {
        Tool.Log("Step_2", 1);
        yield return _maker.Create(Step_Inner());
        Tool.Log("Step_2", 2);
        yield return _maker.Create(Step_Inner());
        Tool.Log("Step_2", 3);
        yield return Step_Inner();
        Tool.Log("Step_2", 4);
    }

    IEnumerator Step_Inner()
    {
        Tool.Log("Step_Inner", 1);
        yield return null;
        Tool.Log("Step_Inner", 2);
    }
}

class TestStop
{
    CorMaker _maker;

    public TestStop(CorMaker maker)
    { _maker = maker; }

    Coroutine _inner = null;
    Coroutine _outside = null;

    public void Test()
    {
        Tool.PushFrame();
        _maker.Create(test_inner());
        _outside = _maker.Create(test_outside());
    }

    IEnumerator test_inner()
    {
        Tool.Log("inner", 1);
        _inner = _maker.Create(test_inner_1());
        yield return _inner;
        Tool.Log("inner", 2);
    }

    IEnumerator test_inner_1()
    {
        Tool.Log("test_inner", 1, 1);
        yield return null;
        yield return _maker.Create(test_stop(true));
        Tool.Log("test_inner", 2, 1);
        yield return null;
    }

    IEnumerator test_outside()
    {
        yield return new WaitForSeconds(1f);
        Tool.Log("outside", 1);
        yield return _maker.Create(test_outside_1());
        Tool.Log("outside", 2);
    }

    IEnumerator test_outside_1()
    {
        Tool.Log("test_outside", 1, 1);
        yield return null;
        yield return _maker.Create(test_stop(false));
        Tool.Log("test_outside", 2, 1);
        yield return null;
    }

    IEnumerator test_stop(bool inner)
    {
        yield return null;
        Tool.Log(string.Format("stop coroutine {0}", inner ? "inner" : "outside"), 1);
        if (inner)
            _maker.Stop(_inner);
        else
            _maker.Stop(_outside);
        Tool.Log(string.Format("stop coroutine {0}", inner ? "inner" : "outside"), 2);
    }
}

class TestStopAll
{
    CorMaker _maker;

    public TestStopAll(CorMaker maker)
    { _maker = maker; }

    public void Test()
    {
        Tool.PushFrame();
        _maker.Create(test_1());
    }

    IEnumerator test_1()
    {
        Tool.Log("test_1", 1);
        yield return  _maker.Create(test_2());
        Tool.Log("test_1", 2);
        yield return _maker.Create(test_3());
    }

    IEnumerator test_2()
    {
        Tool.Log("test_2", 1, 1);
        _maker.Create(test_4());
        Tool.Log("test_2", 2, 1);
        yield return _maker.Create(test_stop());
        Tool.Log("test_2", 3, 1);
        yield return null;
    }

    IEnumerator test_3()
    {
        Tool.Log("test_3", 1, 1);
        yield return null;
        Tool.Log("test_3", 2, 1);
    }

    IEnumerator test_4()
    {
        Tool.Log("test_4", 1, 2);
        yield return new WaitForSeconds(2f);
        Tool.Log("test_4", 2, 2);
    }

    IEnumerator test_stop()
    {
        Tool.Log("stop all coroutine ", 1);
        _maker.Create(test_3());
        Tool.Log("stop all coroutine ", 2);
        _maker.StopAll();
        Tool.Log("stop all coroutine ", 3);
        yield return null;
    }
}

public class TestCor : MonoBehaviour {
    CorInner _inner;
    CorMy _my;

    void Awake()
    {
        _inner = new CorInner(this);
        _my = new CorMy(new CoroutineFactory());
    }

    [ContextMenu("test normal original")]
    void test_original_cor()
    { new TestNomal(_inner).Test(); }

    [ContextMenu("test normal factory")]
    void test_cor_factory()
    { new TestNomal(_my).Test(); }

    [ContextMenu("test create original")]
    void test_create_ogrinal()
    { new TestCreate(_inner).Test(); }

    [ContextMenu("test create factory")]
    void test_create_my()
    { new TestCreate(_my).Test(); }

    [ContextMenu("test stop original")]
    void test_stop_ogrinal()
    { new TestStop(_inner).Test(); }

    [ContextMenu("test stop factory")]
    void test_stop_my()
    { new TestStop(_my).Test(); }

    [ContextMenu("test stop all original")]
    void test_stopall_ogrinal()
    { new TestStopAll(_inner).Test(); }

    [ContextMenu("test stop all factory")]
    void test_stopall_my()
    { new TestStopAll(_my).Test(); }

    [ContextMenu("test all")]
    void test_all()
    {
        _my.Create(test_all_cor());
    }

    IEnumerator test_all_cor()
    {
        Debug.LogError("TestNormal Original");
        new TestNomal(_inner).Test();
        yield return new WaitForSeconds(5f);

        Debug.LogError("TestNormal Factory");
        new TestNomal(_my).Test();
        yield return new WaitForSeconds(5f);

        Debug.LogError("TestCreate Original");
        new TestCreate(_inner).Test();
        yield return new WaitForSeconds(5f);

        Debug.LogError("TestCreate Factory");
        new TestCreate(_my).Test();
        yield return new WaitForSeconds(5f);

        Debug.LogError("TestStop Original");
        new TestStop(_inner).Test();
        yield return new WaitForSeconds(5f);

        Debug.LogError("TestStop Factory");
        new TestStop(_my).Test();
        yield return new WaitForSeconds(5f);

        Debug.LogError("TestStopAll Original");
        new TestStopAll(_inner).Test();
        yield return new WaitForSeconds(5f);

        Debug.LogError("TestStopAll Factory");
        new TestStopAll(_my).Test();
    }
}
