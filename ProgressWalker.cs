/// author:	xuantao
/// date:	2014-03-15   11:03
/// node:	异步加载使用进度计数
using System;
using System.Collections.Generic;
using UnityEngine;

public class ProgressWalker {
	public class Segment {
        ProgressWalker _walker;
		float _precent = 0f;
		float _seg = 0f;

        public Segment(ProgressWalker walker, float seg) {
            _walker = walker;
            _seg = seg;
        }

        public ProgressWalker owner {
            get { return _walker; }
        }

        public float length {
            get { return _seg; }
        }

        public float percent {
			get { return _precent; }
			set {
                float p = Mathf.Clamp01(value);
                if (p != _precent) {
                    _precent = p;
                    _walker.onChange();
                }
            }
		}

        public string job {
            get { return _walker._desc; }
            set { if (_walker._desc != value) {
                    _walker._desc = value;
                    _walker.onChange();
                }
            }
        }
	}

	float _total = 0f;
    string _desc = "";
    Action<ProgressWalker> _callback;
    List<Segment> _segs = new List<Segment>();

	public float percent {
		get { return _total; }
	}

    public string job {
        get { return _desc; }
    }

	public ProgressWalker(Action<ProgressWalker> call, string content = "") {
        _callback = call;
        _desc = content;
        _segs.Add(new Segment(this, 1f));
	}

	public Segment Top() {
		if (0 == _segs.Count) {
			Debug.LogError("stack is empty");
			return null;
		}
		return _segs[_segs.Count - 1];
	}

	public Segment Push(float seg) {
		Segment p = new Segment(this, seg);
		_segs.Add(p);
		return p;
	}

	public void Pop() {
		if (1 >= _segs.Count)
			return;
		
		Segment p = _segs[_segs.Count - 1];
		_segs.RemoveAt(_segs.Count - 1);

		Segment r = _segs[_segs.Count - 1];
		r.percent += p.length;
	}

	void onChange() {
		float seg = 1f;
		_total = 0f;
		for (int i = 0; i < _segs.Count; ++i) {
			Segment p = _segs[i];
			seg *= p.length;
			_total += seg * p.percent;
		}

		if (null != _callback) {
            _callback(this);
        }
	}
}
