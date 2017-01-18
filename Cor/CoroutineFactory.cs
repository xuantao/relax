using System.Collections;
using System.Collections.Generic;
using UnityEngine;

/// <summary>
/// 协程对象外包管理器
/// 增加方便的接口对象管理协程
/// 额外增加一种命名协程,方便使用
/// </summary>
public class CoroutineFactory
{
    // map of name coroutine
    class CorName
    {
        public ICorNode Node;
        public string Name;
    }

    Dictionary<Coroutine, CorName> _dicCors = new Dictionary<Coroutine, CorName>();
    Dictionary<string, CorName> _dicNames = new Dictionary<string, CorName>();
    ICorNode _starting = null;

    public int Count
    {
        get { return _dicCors.Count; }
    }

    public bool IsActive(Coroutine cor)
    {
        return _dicCors.ContainsKey(cor);
    }

    public bool IsActive(string name)
    {
        return _dicNames.ContainsKey(name);
    }

    public Coroutine GetCoroutine(string name)
    {
        CorName cn = null;
        if (_dicNames.TryGetValue(name, out cn))
            return cn.Node.Cor;
        return null;
    }

    public Coroutine Start(IEnumerator iter)
    {
        if (iter == null)
            return null;

        return DoStart(string.Empty, iter);
    }

    public Coroutine Start(string name, IEnumerator iter)
    {
        if (string.IsNullOrEmpty(name) || iter == null)
        {
            return null;
        }
        if (_dicNames.ContainsKey(name))
        {
            Debug.LogErrorFormat("Start Coroutine failed, name conflict [{0}]", name);
            return null;
        }
        return DoStart(name, iter);
    }

    public void Stop(Coroutine cor, bool includeChildren = false)
    {
        CorName cn;
        if (_dicCors.TryGetValue(cor, out cn))
            DoStop(cn.Node, includeChildren);
    }

    public void Stop(string name, bool includeChildren = false)
    {
        CorName cn = null;
        if (_dicNames.TryGetValue(name, out cn))
            DoStop(cn.Node, includeChildren);
    }

    public void StopAll()
    {
        if (_starting != null)
            DoStop(_starting, false);

        List<Coroutine> cors = new List<Coroutine>(_dicCors.Keys);
        for (int i = 0; i < cors.Count; ++i)
        {
            if (IsActive(cors[i]))
                Stop(cors[i]);
        }
    }

    Coroutine DoStart(string name, IEnumerator iter)
    {
        _starting = CoroutineManager.Instance.Create(iter, OnFinish);
        _starting.Start();

        if (_starting == null || _starting.State != CorState.Running)
            return null;

        Coroutine cor = _starting.Cor;
        CorName cn = new CorName();
        cn.Node = _starting;
        cn.Name = name;
        _dicCors[cor] = cn;
        if (!string.IsNullOrEmpty(name))
            _dicNames[name] = cn;

        _starting = null;
        return cor;
    }

    void DoStop(ICorNode node, bool includeChildren)
    {
        while (includeChildren && node.WaitingFor != null)
            node = node.WaitingFor;
        node.Stop();
    }

    void OnFinish(ICorNode node)
    {
        if (node == _starting)
            _starting = null;

        CorName cn;
        if (node.Cor != null && _dicCors.TryGetValue(node.Cor, out cn))
        {
            if (!string.IsNullOrEmpty(cn.Name))
                _dicNames.Remove(cn.Name);
            _dicCors.Remove(node.Cor);
        }
    }
}
