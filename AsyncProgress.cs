/// author:	xuantao
/// date:	2014-03-15   11:03
/// node:	异步加载使用进度计数
using System;
using System.Collections.Generic;

public class AsyncProgress {
	public class Progress {
		AsyncProgress m_async;
		float m_precent = 0f;
		float m_block = 0f;

		public float Precent {
			get { return m_precent; }
			set { set(value); }
		}

		public float Block {
			get { return m_block; }
		}

		public AsyncProgress AsyncProgress {
			get { return m_async; }
		}

		public Progress(AsyncProgress async, float block) {
			m_async = async;
			m_block = block;
		}

		void set(float p) {
			p = Math.Clamp01(p);
			if (p != m_precent) {
				m_precent = p;
				m_async._OnChange();
			}
		}
	}

	float m_total = 0f;
	List<Progress> m_lstProgress = new List<Progress>();

	public delegate void FnOnChange(float percent);
	public event FnOnChange onChange;

	public float Percent {
		get { return m_total; }
	}

	public AsyncProgress() {
		Reset();
	}

	/// <summary>
	/// 在每次使用前需要手动清掉之前的数据存留
	/// </summary>
	public void Reset() {
		m_lstProgress.Clear();
		m_total = 0f;
		m_lstProgress.Add(new Progress(this, 1f));
	}

	public Progress Top() {
		if (0 == m_lstProgress.Count) {
			Debug.LogError("stack is empty");
			return null;
		}
		return m_lstProgress[m_lstProgress.Count - 1];
	}

	public Progress PushProgress(float block) {
		Progress p = new Progress(this, block);
		m_lstProgress.Add(p);
		return p;
	}

	public void PopProgress()
	{
		if (1 >= m_lstProgress.Count)
			return;
		
		Progress p = m_lstProgress[m_lstProgress.Count - 1];
		m_lstProgress.RemoveAt(m_lstProgress.Count - 1);

		Progress r = m_lstProgress[m_lstProgress.Count - 1];
		r.Precent += p.Block;
	}

	void _OnChange() {
		float block = 1f;
		m_total = 0f;
		for (int i = 0; i < m_lstProgress.Count; ++i) {
			Progress p = m_lstProgress[i];
			block *= p.Block;
			m_total += block * p.Precent;
		}

		if (null != onChange) onChange(m_total);
	}
}
