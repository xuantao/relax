using UnityEngine;
using System;
using System.Collections;
using System.Collections.Generic;

public enum CorState
{
    None,
    Running,
    Completed,
}

public interface ICorNode
{
    CorState State { get; }
    Coroutine Cor { get; }
    ICorNode GetDeepest();
    Coroutine Start();
    void Stop();
    void Reset();
}

/// <summary>
/// 提供全局、统一的协程运行基础
/// 那些需要使用协程，而找不到协程启动器的对象，不用单独创建一个全局的GameObject
/// </summary>
public class CoroutineManager : MonoBehaviour
{
    static CoroutineManager _instance = null;
    public static CoroutineManager Instance
    {
        get { return _instance; }
    }

    Dictionary<Coroutine, CorNode> _cors = new Dictionary<Coroutine, CorNode>();

    public int count
    {
        get { return _cors.Count; }
    }

    void Awake()
    {
        _instance = this;
    }

    public ICorNode Create(IEnumerator routine, Action<ICorNode> onFinish = null)
    {
        return new CorNode(routine, onFinish);
    }

    class CorNode : ICorNode
    {
        class Stepper
        {
            bool _stopped = false;
            Stepper _next = null;
            IEnumerator _iter = null;

            public Stepper(IEnumerator iter)
            { _iter = iter; }

            public object Current
            {
                get { return _next != null ? _next.Current : _iter.Current; }
            }

            public bool Move()
            {
                if (_next != null)
                {
                    if (_next.Move())
                        return true;
                    else
                        _next = null;
                }

                if (_stopped || !_iter.MoveNext())
                {
                    return false;
                }

                if (_iter.Current is IEnumerator)
                {
                    _next = new Stepper(_iter.Current as IEnumerator);
                    return Move();
                }
                return true;
            }

            public void Stop()
            {
                Stepper next = this;
                while (next != null)
                {
                    next._stopped = true;
                    next = next._next;
                }
            }

            public void Reset()
            {
                _stopped = false;
                _iter.Reset();
                _next = null;
            }
        }
        class Iter : IEnumerator
        {
            CorNode _impl;
            public Iter(CorNode cor)
            { _impl = cor; }
            public object Current
            { get { return _impl._stepper.Current; } }
            public bool MoveNext()
            { return _impl.MoveNext(); }
            public void Reset()
            { throw new NotImplementedException(); }
        }

        Stepper _stepper = null;
        Action<ICorNode> _onFinish = null;     // callback on finish
        CorState _state = CorState.None;

        List<CorNode> _prev = null;             // those are waiting for me
        CorNode _next = null;                   // waiting for
        Coroutine _cor = null;                  // 
        Iter _iter = null;

        public CorState State
        { get { return _state; } }

        public Coroutine Cor
        { get { return _cor; } }

        public CorNode(IEnumerator iter, Action<ICorNode> onFinish)
        {
            _stepper = new Stepper(iter);
            _onFinish = onFinish;
        }

        public ICorNode GetDeepest()
        {
            CorNode node = this;
            while (node._next != null)
                node = node._next;
            return node;
        }

        public Coroutine Start()
        {
            if (_state != CorState.None)
                return null;

            _iter = new Iter(this);
            _state = CorState.Running;
            _cor = _instance.StartCoroutine(_iter);
            if (_cor != null)
                _instance._cors.Add(_cor, this);
            return _cor;
        }

        public void Stop() 
        {
            if (_state != CorState.Running)
                return;

            // stop all coroutines from outside
            if (_prev != null)
            {
                for (int i = 0; i < _prev.Count; ++i)
                    _prev[i].Stop();
            }

            _stepper.Stop();
            OnComplete(false);
        }

        public void Reset()
        {
            _stepper.Reset();
            _state = CorState.None;
            _iter = null;
            _prev = null;
            _cor = null;
            _next = null;
        }

        bool MoveNext()
        {
            // catch the exception
            try
            {
                if (!_stepper.Move())
                {
                    // normal complete
                    OnComplete(true);
                    return false;
                }
            }
            catch (Exception ex)
            {
                Debug.LogErrorFormat("Coroutine Exception exception:{0}", ex);
                Stop();
                return false;
            }

            // build the connection
            if (_stepper.Current is Coroutine)
            {
                if (_instance._cors.TryGetValue(_stepper.Current as Coroutine, out _next))
                {
                    if (_next._prev == null) _next._prev = new List<CorNode>();
                    _next._prev.Add(this);
                }
                else
                {
                    Debug.LogErrorFormat("Where is the Coroutine come from? stack:{0}");
                }
            }

            return true;
        }

        void OnComplete(bool nomal)
        {
            if (_state != CorState.Running)
                return;

            Coroutine cor = _cor;
            _cor = null;
            _state = CorState.Completed;

            if (_next != null)
                _next._prev.Remove(this);
            _next = null;

            // stop from back to front
            if (_prev != null)
            {
                for (int i = 0; i < _prev.Count; ++i)
                    _prev[i]._next = null;
                _prev = null;
            }

            if (cor != null)
                _instance._cors.Remove(cor);

            if (_onFinish != null)
                _onFinish(this);

            if (!nomal && cor != null)
                _instance.StopCoroutine(cor);
        }
    }
}
